{ config, pkgs, system, lib, inputs, ... }:
let
  integrationTest = inputs.self.packages.x86_64-linux."voting-tools:test:integration-tests";
in
{
  name = "property-tests-db";

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

      virtualisation.diskSize = pkgs.lib.mkDefault 4096;

    };
  };

  testScript = { nodes, ...}: ''
    start_all()

    # Ensure database has correct schema by running cardano-db-sync migrations.
    machine.wait_for_unit("postgresql.service")
    machine.succeed("echo 'Running db_sync migrations...'")
    machine.succeed("for file in ${nodes.machine.config.services.cardano-db-sync.dbSyncPkgs.schema}/*; do psql -U db-sync -d db_sync -f $file; done")

    # Run voting-tools tests
    machine.succeed('${integrationTest}/bin/integration-tests --db-name db_sync --db-user db-sync --db-host /run/postgresql')
  '';
}
