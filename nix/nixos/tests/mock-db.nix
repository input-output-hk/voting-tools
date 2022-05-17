{ config, pkgs, system, lib, inputs, ... }:
let
  nixpkgs = pkgs.lib.cleanSource pkgs.path;

  sources = import ../../sources.nix;

  cardanoNodePort = 3001;

  mock-data =
    pkgs.runCommandLocal "mock-data" {}
      ''
        mkdir -p $out

        cp -r ${./mock-db}/* $out/
      '';

  slotNo = 41778925;

  votingToolsPkg = inputs.self.packages."${system}".voting-tools;
in
{
  name = "vote-submission-test";

  nodes = {
    machine = {
      nixpkgs.pkgs = pkgs;

      # Import cardano-db-sync nixos module so we can get access to database schema:
      #   config.services.cardano-db-sync.dbSyncPkgs.schema.
      imports = [
        inputs.cardano-db-sync.nixosModules.cardano-db-sync
      ];

      # Ensure we have a postgres instance running with a db_sync database.
      services.postgresql = {
        enable = true;
        enableTCPIP = false;
        identMap = ''
          # map-name system-username database-username
          explorer-users postgres postgres
          explorer-users db-sync db-sync
          explorer-users root db-sync
        '';
        authentication = ''
          local all all ident map=explorer-users
          local all all trust
        '';
        ensureDatabases = [
          "db_sync"
          "hdb_catalog"
        ];
        ensureUsers = [
          {
            name = "db-sync";
            ensurePermissions = {
              "DATABASE db_sync" = "ALL PRIVILEGES";
              "DATABASE hdb_catalog" = "ALL PRIVILEGES";
              "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
              "ALL TABLES IN SCHEMA information_schema" = "SELECT";
              "ALL TABLES IN SCHEMA pg_catalog" = "SELECT";
            };
          }
        ];
        initialScript = pkgs.writeText "init.sql" ''
          CREATE USER db-sync WITH SUPERUSER;
        '';
      };

    };
  };

  testScript = { nodes, ...}: ''
    start_all()

    # Ensure database has correct schema by running cardano-db-sync migrations.
    machine.wait_for_unit("postgresql.service")
    machine.succeed("echo 'Running db_sync migrations...'")
    machine.succeed("for file in ${nodes.machine.config.services.cardano-db-sync.dbSyncPkgs.schema}/*; do psql -U db-sync -d db_sync -f $file; done")

    # Copy over mock data
    machine.succeed("psql -U db-sync -d db_sync -c \"\\copy slot_leader FROM '${mock-data}/slot_leader.csv' DELIMITER ',' HEADER CSV\"")
    machine.succeed("psql -U db-sync -d db_sync -c \"\\copy block FROM '${mock-data}/block.csv' DELIMITER ',' HEADER CSV\"")
    machine.succeed("psql -U db-sync -d db_sync -c \"\\copy tx FROM '${mock-data}/tx.csv' DELIMITER ',' HEADER CSV\"")
    machine.succeed("psql -U db-sync -d db_sync -c \"\\copy stake_address FROM '${mock-data}/stake_address.csv' DELIMITER ',' HEADER CSV\"")
    machine.succeed("psql -U db-sync -d db_sync -c \"\\copy tx_in FROM '${mock-data}/tx_in.csv' DELIMITER ',' HEADER CSV\"")
    machine.succeed("psql -U db-sync -d db_sync -c \"\\copy tx_out FROM '${mock-data}/tx_out.csv' DELIMITER ',' HEADER CSV\"")
    machine.succeed("psql -U db-sync -d db_sync -c \"\\copy tx_metadata FROM '${mock-data}/tx_metadata.csv' DELIMITER ',' HEADER CSV\"")

    # Run voting-tools
    machine.succeed("${votingToolsPkg}/bin/voting-tools --mainnet --db db_sync --db-user db-sync --out-file out.json --slot-no ${toString slotNo}")

    # Add a newline to the end of the JSON file if it doesn't already exist
    machine.succeed("sed -i -e '$a\\' out.json")

    # Ensure generated file matches golden file
    # Ensure sorted for comparison purposes
    machine.succeed("diff <(jq -c 'sort_by(.stake_public_key)' ${mock-data}/out.json) <(jq -c 'sort_by(.stake_public_key)' out.json)")
  '';
}
