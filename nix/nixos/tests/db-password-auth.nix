# Test that voting-tools supports password authentication to connect to the
# cardano-db-sync database.
{ config, pkgs, system, lib, inputs, ... }:
let
  voting-tools = inputs.self.packages.x86_64-linux."voting-tools:exe:voting-tools";

  dbPassword = "openSesame";
in
{
  name = "db-password-auth";

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
        # Only allow connnections over socket or localhost
        enableTCPIP = false;
        # Disallow socket connections, only allow TCP/IP connections with password
        authentication = ''
          # _    database    user       address    auth-method
          host   db_sync     db-sync    all        scram-sha-256
        '';
        ensureDatabases = [
          "db_sync"
          "hdb_catalog"
        ];
        # Create a role that must login with a password
        initialScript = pkgs.writeText "init.sql" ''
          CREATE ROLE "db-sync" WITH LOGIN PASSWORD '${dbPassword}';
        '';
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
        # Ensure we support the scram auth method
        settings = {
          password_encryption = "scram-sha-256";
        };
      };
    };
  };

  testScript = { nodes, ...}: ''
    start_all()

    # Ensure database has correct schema by running cardano-db-sync migrations.
    machine.wait_for_unit("postgresql.service")
    machine.succeed("echo 'Running db_sync migrations...'")
    machine.succeed("for file in ${nodes.machine.config.services.cardano-db-sync.dbSyncPkgs.schema}/*; do PGPASSWORD=${dbPassword} psql -U db-sync -d db_sync -h localhost -f $file; done")

    # Run voting-tools tests
    # Succeed with password authentication
    machine.succeed("${voting-tools}/bin/voting-tools --db db_sync --db-user db-sync --db-host localhost --db-pass ${dbPassword} --mainnet --out-file out.json")
    machine.succeed("cat out.json 1>&2")

    # Fail with wrong password
    machine.fail("${voting-tools}/bin/voting-tools --db db_sync --db-user db-sync --db-host localhost --db-pass wrongPassword --mainnet --out-file out.json")

    # Fail with peer authentication
    machine.fail("${voting-tools}/bin/voting-tools --db db_sync --db-user db-sync --db-host /run/postgresql --mainnet --out-file out.json")
  '';
}
