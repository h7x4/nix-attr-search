# Nix Attr Search

**NOTE: This is still in alpha**

These are some tools i made for ease of searching for stuff while writing my nix configs.

They are made with `fzf`, so they can easily be piped into other programs.

Also, they all create a derivation containing all of the attributes.

---

## Try it out!

### Nix Packages

TODO:

### Nix Options

TODO:

### Homemanager

```
nix run github:h7x4/nix-attr-search#home-manager-search
```

Or a more advanced version:

```
nix run github:h7x4/nix-attr-search#home-manager-search -- --flake="github:nix-community/home-manager" --ref="release-22.05"
```

---

## Some piping ideas

### Misc

- Pipe the option into the clipboard
- Bind shortcut keys to open a terminal with the program, and paste the chosen option directly into the current program (probably a code editor of some sort, with your nix configs open).

### Repiping values into `jq`
- Repipe the chosen value into `jq`, and choose either the default value, the example value.
- Repipe the chosen value into `jq`, choosing its referenced configs, pipe these into `fzf` once more to choose one referenced config, then pipe this relative file path into `rg` to (hopefully) find the specific location the value was set, and then open the file at its specific position in an editor.