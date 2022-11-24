{ pkgs, lib, home-manager, system, ... }:
let

  # TODO: Preprocess XML tags in description.

  # TODO: Make non-literal examples have better syntax highlighting

  # TODO: Colorize preview

  usage = pkgs.writeText "home-manager-search-usage" ''
    Usage:
    home-manager-search [ -j | -n ] [-f=<flake-url> [-r=<git-reference>]]

    Options:
    -h   | --help        Display this message.
    -j   | --json        Display raw data in json format.
    -n   | --no-preview  Don't display a preview. 
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

  inherit (pkgs.callPackage ./shared.nix { })
    jq fzf gomplate blinkred clear flatten;

  template = pkgs.writeText "preview-home-manager-attrs-template" (flatten ''
    {{ with (ds "opt")}}
      {{- $title := join .loc "." }}
      {{- $title }}
      {{ strings.Repeat (strings.RuneCount $title) "=" }}

      {{ if .readOnly }}
        ${blinkred}READONLY${clear}
      {{ end }}

      Type: {{ .type }}

      {{ if has . "description" }}
        DESCRIPTION
        {{ strings.Repeat 20 "=" }}
        {{ .description }}
      {{ end }}

      {{- if has . "example" }}
        EXAMPLE
        {{ strings.Repeat 20 "=" }}
        {{ if and (has .example "_type") }}
        {{ else }}
          {{ data.ToJSONPretty "  " .example }}
        {{ end }}
      {{ end }}

      {{- if has . "default" }}
        DEFAULT
        {{ strings.Repeat 20 "=" }}
        {{ .default }}
      {{ end }}

      CONFIGURED IN:
      {{ strings.Repeat 20 "=" }}
      {{ range .declarations }}
        {{- .path }}
      {{ end }}

    {{ else }}
      ERROR: Could not find datasource
    {{ end }}
  '');

  previewJson = pkgs.writers.writeBash "preview-home-manager-attrs-json" ''
    OPTION_KEY=$1
    JSON_MANUAL_PATH=$2

    ${jq} -C ".\"$OPTION_KEY\"" $JSON_MANUAL_PATH
  '';

  previewGomplate =
    pkgs.writers.writeBash "preview-home-manager-attrs-gomplate" ''
      OPTION_KEY=$1
      JSON_MANUAL_PATH=$2

      ${jq} ".\"$OPTION_KEY\"" $JSON_MANUAL_PATH | ${gomplate} --datasource opt=stdin:?type=application/json --file ${template}
    '';

  defaultManualPath = "${
      home-manager.packages.${system}.docs-json
    }/share/doc/home-manager/options.json";

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
      -n|--no-preview)
        NO_PREVIEW=1
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
  else
    ${jq} -r 'keys | .[] | .' $JSON_MANUAL_PATH | ${fzf} --preview "${previewGomplate} {} $JSON_MANUAL_PATH"
  fi
''

