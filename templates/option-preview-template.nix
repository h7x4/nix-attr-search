{ pkgs }:
let inherit (pkgs.callPackage ../internals/lib.nix { }) flatten;
in pkgs.writeText "nix-attrs-search-internal-option-template" (flatten ''
  {{ with (ds "opt")}}
    {{- $origTitle := join .loc "." }}
    {{- $title := join .loc "." }}
    {{- $title }}
    {{ strings.Repeat (strings.RuneCount $origTitle) "=" }}

    {{ if .readOnly }}
      READONLY
    {{ end }}

    Type: {{ .type }}

    {{ if has . "description" }}
      DESCRIPTION
      {{ strings.Repeat 20 "=" }}
      {{ getenv "DESCRIPTION" "Gomplate error: could not fetch description" }}
    {{ end }}

    {{- if has . "example" }}
      EXAMPLE
      {{ strings.Repeat 20 "=" }}
      {{ getenv "EXAMPLE" "Gomplate error: could not fetch example" }}
    {{ end }}

    {{- if has . "default" }}
      DEFAULT
      {{ strings.Repeat 20 "=" }}
      {{ getenv "DEFAULT" "Gomplate error: could not fetch default value" }}
    {{ end }}

    {{- if has . "declarations" }}
      CONFIGURED IN
      {{ strings.Repeat 20 "=" }}
      {{ range .declarations }}
        {{- .path }}
      {{ end }}
    {{ end }}

    {{- if has . "relatedPackages" }}
      RELATED PACKAGES:
      {{ strings.Repeat 20 "=" }}
      {{ range .relatedPackages }}
        {{- .attrName }}
        {{- .description }}
      {{ end }}
    {{ end }}

  {{ else }}
    ERROR: Could not find datasource
  {{ end }}
'')
