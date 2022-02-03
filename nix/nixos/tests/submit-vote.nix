{ config, pkgs, system, lib, ... }:
let

  cardano-cli = (import sources.cardano-node { inherit system; }).cardano-cli;
  jormungandr = (import sources.iohk-nix { inherit system; }).jormungandrLib.environments.beta.packages.jcli;
  voter-registration = (pkgs.callPackage ../../.. {}).voter-registration;

  cardanoNodePort = 3001;
in
{
  name = "vote-submission-test";

  nodes = {
    machine = {
      imports = [
        "${sources.cardano-node}/nix/nixos"
      ];

      environment.systemPackages = [
        jormungandr
        voter-registration
        cardano-cli
      ];

      services.cardano-node = {
        environment = "testnet";
        enable = true;
        port = cardanoNodePort;
        systemdSocketActivation = true;
      };
    };
  };

  testScript = { nodes, ...}: let
      environments = nodes.machine.config.services.cardano-node.environments;
      environment = nodes.machine.config.services.cardano-node.environment;
      envNodeCfg = environments."${environment}".nodeConfig;
      shelleyGenesisParams = __fromJSON (__readFile envNodeCfg.ShelleyGenesisFile);
      envFlag = if nodes.machine.config.services.cardano-node.environment == "mainnet" then "--mainnet" else "--testnet-magic ${toString shelleyGenesisParams.networkMagic}";
    in ''
    start_all()

    machine.wait_for_unit("cardano-node.service")
    machine.succeed("stat /run/cardano-node")
    machine.succeed("stat /run/cardano-node/node.socket")
    machine.wait_for_open_port(3001)
    machine.succeed("systemctl status cardano-node")

    # Generate keys
    machine.succeed("${cardano-cli}/bin/cardano-cli address key-gen \
        --verification-key-file payment.vkey \
        --signing-key-file payment.skey")
    machine.succeed("${cardano-cli}/bin/cardano-cli stake-address key-gen \
        --verification-key-file stake.vkey \
        --signing-key-file stake.skey")
    machine.succeed("${cardano-cli}/bin/cardano-cli address build \
        --payment-verification-key-file payment.vkey \
        --out-file payment.addr \
        ${envFlag}")
    machine.succeed("${cardano-cli}/bin/cardano-cli address build \
        --payment-verification-key-file payment.vkey \
        --out-file payment.addr \
        ${envFlag}")
    machine.succeed("${cardano-cli}/bin/cardano-cli stake-address build \
        --stake-verification-key-file stake.vkey \
        --out-file stake.addr \
        ${envFlag}")
    machine.succeed("${jormungandr}/bin/jcli key generate \
        --type ed25519extended \
        > vote.skey")
    machine.succeed("${jormungandr}/bin/jcli key to-public \
        < vote.skey \
        > vote.pub")

    machine.succeed("CARDANO_NODE_SOCKET_PATH=/run/cardano-node/node.socket ${voter-registration}/bin/voter-registration \
        --payment-signing-key payment.skey \
        --payment-address $(cat payment.addr) \
        --stake-signing-key stake.skey \
        --rewards-address $(cat stake.addr) \
        --vote-public-key vote.pub \
        ${envFlag} \
        --out-file vote-tx.signed \
        --alonzo-era \
        --sign")
    machine.succeed("${cardano-cli}/bin/cardano-cli transaction submit \
        --tx-file tx.signed \
        ${envFlag}")
  '';
}
