with (import ./lib.nix);
{ withHoogle ? true
, env ? null
}:
let
  default = import ./default.nix {};

  scripts-env = pkgs.stdenv.mkDerivation {

    name = "cardano-node-scripts-${env}";

    buildInputs = with pkgs; with default; with environments.${env};
      let
        run-cardano-node-script = writeScriptBin "run-cardano-node" ''
          #!${pkgs.runtimeShell}
          exec ${scripts.${env}.node} $@
        '';

        edge-topology = builtins.toFile "${env}-topology-edge-only.yaml" (builtins.toJSON [
            {
              nodeId = 0;
              nodeAddress =  {
                addr = edgeHost;
                port = edgePort;
              };
              producers = [];
            }
          ]);

        env-opts =
        ''--genesis-file ${genesisFile} \
          --genesis-hash ${genesisHash} \
          --topology ${edge-topology} \
          --node-id "0"'';

        submit-tx-script = writeScriptBin "submit-tx" ''
          #!${pkgs.runtimeShell}
          exec ${nix-tools.exes.cardano-node}/bin/cardano-cli real-pbft \
            submit-tx \
            ${env-opts} \
            --tx $@
        '';

      in [
        nix-tools.exes.cardano-node
        run-cardano-node-script
        submit-tx-script
      ];
    shellHook = ''
      echo "Cardano-Node Demo" \
      | ${pkgs.figlet}/bin/figlet -f banner -c \
      | ${pkgs.lolcat}/bin/lolcat
    '';
  };
in
default.nix-tools.shellFor {
  packages    = ps: with ps; [ cardano-node ];
  withHoogle  = withHoogle;
  buildInputs =
  (with default.nix-tools._raw; [
    cabal-install.components.exes.cabal
    ghcid.components.exes.ghcid
  ]) ++
  (with default.nix-tools._raw._config._module.args.pkgs; [
    tmux
  ]);
} // { inherit scripts-env; }
