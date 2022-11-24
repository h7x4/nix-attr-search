{ pkgs, lib, ... }: {

  # Executables
  jq = "${pkgs.jq}/bin/jq";
  fzf = "${pkgs.fzf}/bin/fzf";
  gomplate = "${pkgs.gomplate}/bin/gomplate";

  # ANSI Colors
  blinkred = "\\033[31;5m";
  clear = "\\033[0m";

  # Misc functionality

  # All empty lines, and all prefixed whitespace is removed.
  flatten = with lib;
    let
      stripPrefixSpace = s:
        if hasPrefix " " s then
          stripPrefixSpace ((substring 1 (stringLength s)) s)
        else
          s;
    in (flip pipe) [
      (splitString "\n")
      (remove "")
      (map stripPrefixSpace)
      (concatStringsSep "\n")
    ];
}
