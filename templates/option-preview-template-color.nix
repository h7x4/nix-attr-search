{ pkgs }:
let
  inherit (pkgs.callPackage ../internals/lib.nix { })
    flatten red bold blinkred green yellow magenta blue clear;
in pkgs.writeText "nix-attr-search-internal-option-template" (flatten ''
  {{ define "colorize" -}}
    {{ strings.ShellQuote . | strings.Trim "'" }}
  {{- end -}}
  {{ with (ds "opt")}}
    {{- $origTitle := join .loc "." }}
    {{- $title := join .loc "." | regexp.Replace "(<name>)" "${red}$1${clear}${bold}" }}
    {{- template "colorize" "${bold}" -}}
    {{- template "colorize" $title }}
    {{ strings.Repeat (strings.RuneCount $origTitle) "=" }}
    {{- template "colorize" "${clear}" }}

    {{ if .readOnly }}
      {{ template "colorize" "${blinkred}READONLY${clear}" }}
    {{ end }}

    {{ template "colorize" "${green}" -}}
      Type: {{ .type }}
    {{- template "colorize" "${clear}" }}

    {{ if has . "description" }}
      {{ template "colorize" "${yellow}" -}}
      DESCRIPTION
      {{ strings.Repeat 20 "=" }}
      {{- template "colorize" "${clear}" }}
      {{ getenv "DESCRIPTION" "Gomplate error: could not fetch description" }}
    {{ end }}

    {{- if has . "example" }}
      {{ template "colorize" "${magenta}" -}}
      EXAMPLE
      {{ strings.Repeat 20 "=" }}
      {{- template "colorize" "${clear}" }}
      {{ getenv "EXAMPLE" "Gomplate error: could not fetch example" }}
    {{ end }}

    {{- if has . "default" }}
      {{ template "colorize" "${blue}" -}}
      DEFAULT
      {{ strings.Repeat 20 "=" }}
      {{- template "colorize" "${clear}" }}
      {{ getenv "DEFAULT" "Gomplate error: could not fetch default value" }}
    {{ end }}

    {{- if has . "declarations" }}
      {{ template "colorize" "${red}" -}}
      CONFIGURED IN
      {{ strings.Repeat 20 "=" }}
      {{- template "colorize" "${clear}" }}
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
