{ pkgs, lib, home-manager, defaultManualPath, system, json2nix, ... }:
let
  usage = pkgs.writeText "home-manager-search-usage" ''
    Usage:
    home-manager-search [ -j | -n ] [-f=<flake-url> [-r=<git-reference>]]

    Options:
    -h   | --help        Display this message.
    -j   | --json        Display raw data in json format.
    -np  | --no-preview  Don't display a preview.
    -nc  | --no-color    Turn off ANSI colors.
    -f=* | --flake=*     Use a flake url to an instance (e.g. a fork) of home-manager,
                         generate its options, and use those instead of the global ones.
    -r=* | --ref=*       If a flake url is specified, this option might be used to choose
                         a specific reference (branch, tag, commit, etc.) to use for
                         generating the docs.

    Example usage:
    home-manager-search
    home-manager-search -j
    home-manager-search --flake="github:nix-community/home-manager"
    home-manager-search --flake="github:nix-community/home-manager" --ref="release-22.05"
  '';

  inherit (pkgs.callPackage ../internals/lib.nix { })
    jq fzf gomplate nixfmt bat perl blinkred bold red green yellow blue magenta
    clear flatten;

  # TODO: Preprocess all XML tags in description.
  substitutionsColor = let s = "\\s*";
  in {
    "<code>([^>]*)</code>" = "${bold}`$1`${clear}";
    "<command>([^>]*)</command>" = "${bold}`$1`${clear}";
    "<filename>([^>]*)</filename>" = "${yellow}$1${clear}";
    "<emphasis>([^>]*)</emphasis>" = "${bold}$1${clear}";
    "<literal>([^>]*)</literal>" = "${red}$1${clear}";
    "<varname>([^>]*)</varname>" = "${red}$1${clear}";
    "</para><para>" = "\\n";
    "<link${s}xlink:href=\"([^>]*)\"${s}/>" = "${blue}$1${clear}";
    "<link${s}xlink:href=\"([^>]*)\"${s}>([^<]*)</link>" =
      "${bold}$2 ${clear}(${blue}$1${clear})";
    "<xref${s}linkend=\"opt-([^>]*)\"${s}/>" = "${blue}$1${clear}";
  };

  substitutions = let s = "\\s*";
  in {
    "<code>([^>]*)</code>" = "`$1`";
    "<command>([^>]*)</command>" = "`$1`";
    "<filename>([^>]*)</filename>" = "`$1`";
    "<emphasis>([^>]*)</emphasis>" = "`$1`";
    "<literal>([^>]*)</literal>" = "`$1`";
    "<varname>([^>]*)</varname>" = "`$1`";
    "</para><para>" = "\\n";
    "<link${s}xlink:href=\"([^>]*)\"${s}/>" = "`$1`";
    "<link${s}xlink:href=\"([^>]*)\"${s}>([^<]*)</link>" = "`$2 ($1)`";
    "<xref${s}linkend=\"opt-([^>]*)\"${s}/>" = "`$1`";
  };

  perlArgsColor = with lib;
    pipe substitutionsColor [
      (mapAttrsToList (name: value: "s|${name}|${value}|gm"))
      (concatStringsSep ";")
      (x: "-pe '${x}'")
    ];

  perlArgs = with lib;
    pipe substitutions [
      (mapAttrsToList (name: value: "s|${name}|${value}|gm"))
      (concatStringsSep ";")
      (x: "-pe '${x}'")
    ];

  optionTemplateColor =
    pkgs.callPackage ../templates/option-preview-template-color.nix { };

  optionTemplate =
    pkgs.callPackage ../templates/option-preview-template.nix { };

  # TODO: This preview does not respect the color option...
  previewJson = pkgs.writers.writeBash "preview-home-manager-attrs-json" ''
    OPTION_KEY=$1
    JSON_MANUAL_PATH=$2

    ${jq} -C ".\"$OPTION_KEY\"" $JSON_MANUAL_PATH
  '';

  previewGomplate = isColorized:
    let
      # TODO: Color management here needs a refactoring badly...
      pArgs = if isColorized then perlArgsColor else perlArgs;
      colorSuffix = if isColorized then "-color" else "";
      batColorArg = if isColorized then "--color=always " else "";
      template = if isColorized then optionTemplateColor else optionTemplate;
    in pkgs.writers.writeBash
    "preview-home-manager-attrs-gomplate${colorSuffix}" ''
      OPTION_KEY=$1
      JSON_MANUAL_PATH=$2

      JSON_DATA=$(${jq} ".\"$OPTION_KEY\"" $JSON_MANUAL_PATH)
      export DESCRIPTION=$(echo $JSON_DATA | ${jq} -r ".description" | ${perl} ${pArgs})

      EXAMPLE_DATA=$(echo $JSON_DATA | ${jq} -r ".example.text" 2>/dev/null | ${nixfmt})
      if [ $? != 0 ]; then
        EXAMPLE_DATA=$(echo $JSON_DATA | ${jq} -r ".example" | ${json2nix}/bin/json2nix)
      fi
      export EXAMPLE=$(echo $EXAMPLE_DATA | ${bat} ${batColorArg}--style=numbers)

      export DEFAULT=$(echo $JSON_DATA | ${jq} -r ".default" | ${json2nix}/bin/json2nix | ${bat} ${batColorArg}--style=numbers)
      echo $JSON_DATA | ${gomplate} --datasource opt=stdin:?type=application/json --file ${template}
    '';

in pkgs.writers.writeBash "search-home-manager-attrs" ''
  JSON_MANUAL_PATH="${defaultManualPath}"

  for i in "$@"; do
    case $i in
      -h|--help)
        cat ${usage}
        exit 0
        ;;
      -j|--json)
        PRINT_JSON=1
        shift
        ;;
      -np|--no-preview)
        NO_PREVIEW=1
        shift
        ;;
      -nc|--no-color)
        NO_COLOR=1
        shift
        ;;
      -f=*|--flake=*)
        FLAKE="''${i#*=}"
        shift
        ;;
      -r=*|--ref=*)
        REF="''${i#*=}"
        shift
        ;;
      *|-*|--*)
        echo "Unknown option $i"
        cat ${usage}
        exit 1
        ;;
    esac
  done

  if [ -v PRINT_JSON  ] && [ -v NO_PREVIEW ]; then
    echo "Cannot preview as json with no-preview enabled"
    cat ${usage}
    exit 1
  fi

  if [ -v FLAKE ]; then
    FLAKE_URL="''${FLAKE}"

    if [ -v REF ]; then
      FLAKE_URL="''${FLAKE_URL}?ref=$REF"
    fi

    FLAKE_URL="''${FLAKE_URL}#docs-json"
    echo "Building docs from $FLAKE_URL"

    OUT_PATH=$(${pkgs.nix}/bin/nix build "$FLAKE_URL" --no-link --print-out-paths --no-write-lock-file)
    JSON_MANUAL_PATH="$OUT_PATH/share/doc/home-manager/options.json"
    echo "Using docs located at $JSON_MANUAL_PATH"
  fi

  if [ -v NO_PREVIEW ]; then
    ${jq} -r 'keys | .[] | .' $JSON_MANUAL_PATH | ${fzf}
  elif [ -v PRINT_JSON ]; then
    ${jq} -r 'keys | .[] | .' $JSON_MANUAL_PATH | ${fzf} --preview "${previewJson} {} $JSON_MANUAL_PATH"
  elif [ -v NO_COLOR ]; then
    ${jq} -r 'keys | .[] | .' $JSON_MANUAL_PATH | ${fzf} --preview "${
      previewGomplate false
    } {} $JSON_MANUAL_PATH"
  else
    ${jq} -r 'keys | .[] | .' $JSON_MANUAL_PATH | ${fzf} --preview "${
      previewGomplate true
    } {} $JSON_MANUAL_PATH"
  fi
''

