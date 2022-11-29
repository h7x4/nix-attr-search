# TODO:
{ pkgs, lib, nixpkgs, defaultManualPath, system, json2nix, xmldoc2txt, ... }:
let
  usage = pkgs.writeText "nix-option-search-usage" ''
    Usage:
    nix-option-search [ -j | -n ] [-f=<flake-url> [-r=<git-reference>]]

    Options:
    -h   | --help        Display this message.
    -j   | --json        Display raw data in json format.
    -np  | --no-preview  Don't display a preview.
    -nc  | --no-color    Turn off ANSI colors.
    -f=* | --flake=*     Use a flake url to an instance (e.g. a fork) of nixpkgs,
                         generate its options, and use those instead of the global ones.
    -r=* | --ref=*       If a flake url is specified, this option might be used to choose
                         a specific reference (branch, tag, commit, etc.) to use for
                         generating the docs.

    Example usage:
    nix-option-search
    nix-option-search -j
    nix-option-search --flake="github:NixOS/nixpkgs"
    nix-option-search --flake="github:NixOS/nixpkgs" --ref="nixos-22.05"
  '';

  inherit (pkgs.callPackage ../internals/lib.nix { })
    jq fzf gomplate nixfmt bat perl blinkred bold red green yellow blue magenta
    clear flatten;

  optionTemplateColor =
    pkgs.callPackage ../templates/option-preview-template-color.nix { };

  optionTemplate =
    pkgs.callPackage ../templates/option-preview-template.nix { };

  # TODO: This preview does not respect the color option...
  previewJson = pkgs.writers.writeBash "preview-nix-option-attrs-json" ''
    OPTION_KEY=$1
    JSON_MANUAL_PATH=$2

    ${jq} -C ".\"$OPTION_KEY\"" $JSON_MANUAL_PATH
  '';

  previewGomplate = isColorized:
    let
      # TODO: Color management here needs a refactoring badly...
      colorSuffix = if isColorized then "-color" else "";
      batColorArg = if isColorized then "--color=always " else "";
      xmldoc2textColorArg = if isColorized then "-C " else "";
      template = if isColorized then optionTemplateColor else optionTemplate;
    in pkgs.writers.writeBash
    "preview-nix-option-attrs-gomplate${colorSuffix}" ''
      OPTION_KEY=$1
      JSON_MANUAL_PATH=$2

      JSON_DATA=$(${jq} ".\"$OPTION_KEY\"" $JSON_MANUAL_PATH)
      export DESCRIPTION=$(echo $JSON_DATA | ${jq} -r ".description" | ${xmldoc2txt}/bin/xmldoc2txt ${xmldoc2textColorArg})

      EXAMPLE_DATA=$(echo $JSON_DATA | ${jq} -r ".example.text" 2>/dev/null)
      if [ $? != 0 ]; then
        EXAMPLE_DATA=$(echo $JSON_DATA | ${jq} -r ".example" | ${json2nix}/bin/json2nix)
      fi
      export EXAMPLE=$(echo $EXAMPLE_DATA | ${nixfmt} | ${bat} ${batColorArg}--style=numbers -lnix)

      export DEFAULT=$(echo $JSON_DATA | ${jq} -r ".default" | ${json2nix}/bin/json2nix | ${nixfmt} | ${bat} ${batColorArg}--style=numbers -lnix)
      echo $JSON_DATA | ${gomplate} --datasource opt=stdin:?type=application/json --file ${template}
    '';

in pkgs.writers.writeBash "search-nix-option-attrs" ''
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

    echo "Building docs from $FLAKE_URL"

    OUT_PATH=$(${pkgs.nix}/bin/nix build --impure --no-link --print-out-paths --no-write-lock-file --expr "let nixpkgs=(builtins.getFlake \"$FLAKE_URL\"); in (import \"\''${nixpkgs}/nixos/release.nix\" { inherit nixpkgs; }).options")
    echo $OUT_PATH
    JSON_MANUAL_PATH="$OUT_PATH/share/doc/nixos/options.json"
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

