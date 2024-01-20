with import <nixpkgs> {};
mkShell {
    buildInputs = [
        nodejs
        ocaml
        ocamlPackages.ocp-indent
    ];
    shellHook = ''
    '';
}
