{ pkgs }:

let
  # Single source of truth for all tutorial constants
  database      = "postgres";
  schema        = "api";
  table         = "todos";
  username      = "authenticator";
  password      = "mysecretpassword";
  webRole       = "web_anon";
  postgrestPort = 3000;

in
  import "${pkgs.path}/nixos/tests/make-test-python.nix" ({ pkgs, ...}: {
    system = "x86_64-linux";

    nodes = {
      server = { config, pkgs, ... }: {
        # Open the default port for `postgrest` in the firewall
        networking.firewall.allowedTCPPorts = [ postgrestPort ];

        services.postgresql = {
          enable = true;

          initialScript = pkgs.writeText "initialScript.sql" ''
            create schema ${schema};

            create table ${schema}.${table} (
              id serial primary key,
              done boolean not null default false,
              task text not null,
              due timestamptz
            );

            insert into ${schema}.${table} (task) values
              ('finish tutorial 0'), ('pat self on back');

            create role ${webRole} nologin;

            grant usage on schema ${schema} to ${webRole};
            grant select on ${schema}.${table} to ${webRole};

            create role ${username} inherit login password '${password}';
            grant ${webRole} to ${username};
          '';
        };

        users = {
          mutableUsers = false;

          users = {
            # For ease of debugging the VM as the `root` user
            root.password = "";

            # Create a system user that matches the database user so that we
            # can use peer authentication.  The tutorial defines a password,
            # but it's not necessary.
            "${username}".isSystemUser = true;
          };
        };

        systemd.services.postgrest = {
          wantedBy = [ "multi-user.target" ];

          after = [ "postgresql.service" ];

          script =
            let
              configuration = pkgs.writeText "tutorial.conf" ''
                db-uri = "postgres://${username}:${password}@localhost:${toString config.services.postgresql.port}/${database}"
                db-schema = "${schema}"
                db-anon-role = "${username}"
              '';

            in
              ''
                ${pkgs.haskellPackages.postgrest}/bin/postgrest ${configuration}
              '';

          serviceConfig.User = username;
        };

        # Uncomment the next line for running QEMU on a non-graphical system
        # virtualisation.graphics = false;
      };

      client = { };
    };

    testScript =
      ''
      import json
      import sys

      start_all()

      server.wait_for_open_port(${toString postgrestPort})

      expected = [
          {"id": 1, "done": False, "task": "finish tutorial 0", "due": None},
          {"id": 2, "done": False, "task": "pat self on back", "due": None},
      ]

      actual = json.loads(
          client.succeed(
              "${pkgs.curl}/bin/curl http://server:${toString postgrestPort}/${table}"
          )
      )

      if expected != actual:
          sys.exit(1)

      # (tf, vkey_file) = tempfile.mkstemp()
      # # cli_args = [ "cardano-cli", "shelley", "key", "verification-key", "--signing-key-file", skey_file, "--verification-key-file", vkey_file ]
      # cli_args = [ "vote-registration-fetch" ]
      # p = subprocess.run(cli_args, capture_output=True, text=True)
      # if p.returncode != 0:
      #   print(p.stderr)
      #   raise Exception("Unknown error deriving cardano vkey from skey")
      # result = p.stdout.rstrip()
      '';
  })
