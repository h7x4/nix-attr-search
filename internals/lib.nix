{ pkgs, lib, ... }: {
  # Executables
  jq = "${pkgs.jq}/bin/jq";
  fzf = "${pkgs.fzf}/bin/fzf";
  gomplate = "${pkgs.gomplate}/bin/gomplate";
  nixfmt = "${pkgs.nixfmt}/bin/nixfmt";
  bat = "${pkgs.bat}/bin/bat";
  perl = "${pkgs.perl}/bin/perl";

  # ANSI Colors
  bold = "\\x1b[1m";
  blinkred = "\\x1b[31;5m";
  red = "\\x1b[31m";
  green = "\\x1b[32m";
  yellow = "\\x1b[33m";
  blue = "\\x1b[34m";
  magenta = "\\x1b[35m";
  clear = "\\x1b[0m";

  # Misc functionality

  # flatten takes a string that might be formatted like this:
  #
  # ''
  #   A string
  #      with some random indents
  #
  #   and
  #
  #   newlines
  # ''
  #
  # And turns it into this:
  #
  # ''
  #   A string
  #   with some random indents
  #   and
  #   newlines
  # ''
  #
  # Useful for keeping track of gomplates logic
  # while keeping the template flat
  #
  # flatten :: String -> String
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
