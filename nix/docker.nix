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
    tag = "yolo";
    fromImage = baseImage;
    contents = [
      cardano-node
    ];
    created = "now";   # Set creation date to build time. Breaks reproducibility
    config = {
      EntryPoint = ''
        "cardano-node","--config","/configuration/config.yaml",
        "--database-path","/db","--genesis-file","/configuration/genesis.json",
        "--host-addr","127.0.0.1","--port","3001","--socket-dir","/socket",
        "--topology","/configuration/topology.yaml"
      '';
      ExposedPorts = {
        "3001/tcp" = {};  # Cardano node to node port
      };
      WorkingDir = "/cardano-node";
      Volumes = {
        "/cardano-node" = {};
        "node-db:/db" = {};
        "node-socket:/socket" = {};
      };
    };
  }  #### // { inherit (cardano-node) version; }
