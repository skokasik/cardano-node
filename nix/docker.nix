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
  defaultPort = "3001";

  startScript = writeScriptBin "start-cardano-node" ''
    #!${runtimeShell}
    set -euo pipefail

    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    exec ${cardano-node}/bin/cardano-node "$@"
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
    tag = "yolo";
    fromImage = baseImage;
    contents = [
      cardano-node
      startScript
    ];
    config = {
      EntryPoint = [ "start-cardano-node" ];
      ExposedPorts = {
        "${defaultPort}/tcp" = {}; # wallet api
      };
    };
  }  #### // { inherit (cardano-node) version; }
