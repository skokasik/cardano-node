############################################################################
# Docker image builder
#
# To test it out, use:
#
#   docker load -i $(nix-build -A dockerImage --no-out-link)
#   docker run cardano-node
#
############################################################################

{ runtimeShell, writeScriptBin, runCommand, dockerTools

# The main contents of the image.
, cardano-node

# Other things to include in the image.
, glibcLocales, iana-etc, cacert
, bashInteractive, coreutils, utillinux, iproute, iputils, curl, socat

# Used to generate the docker image names
, repoName ? "inputoutput/cardano-node"
}:

let

  startScript = writeScriptBin "start-cardano-node" ''
    #!${runtimeShell}
     set -euo pipefail

     mkdir -p /data /config

     exec ${cardano-node}/bin/cardano-node --config /config/config.yaml --database-path /data/db --genesis-file /config/genesis.json --host-addr 127.0.0.1 --port 3001 --socket-dir /data/ipc/socket --topology /config/topology.json "$@"
  '';

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [
      glibcLocales iana-etc cacert
      bashInteractive coreutils utillinux iproute iputils curl socat
    ];
    # set up /tmp (override with TMPDIR variable)
    extraCommands = "mkdir -m 0777 tmp";
  };

in
  dockerTools.buildImage {
    name = repoName;
    ### tag = "${cardano-node.version}";
    tag = "tag04";
    fromImage = baseImage;
    contents = [
      cardano-node
      startScript
    ];
    created = "now";   # Set creation date to build time. Breaks reproducibility
    config = {
      EntryPoint = [ "start-cardano-node" ];
      ExposedPorts = {
        "3001/tcp" = {};  # Cardano node to node port
      };
    };
  }  #### // { inherit (cardano-node) version; }
