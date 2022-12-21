let
  ciInputName = "GitHub push";
  repository = "input-output-hk/voting-tools";
in {
  tasks.ci = {
    config,
    lib,
    ...
  }: {
    preset = {
      nix.enable = true;

      github.ci = {
        # Tullia tasks can run locally or on Cicero.
        # When no facts are present we know that we are running locally and vice versa.
        # When running locally, the current directory is already
        # bind-mounted into the container, so we don't need to fetch the
        # source from GitHub and we don't want to report a GitHub status.
        enable = config.actionRun.facts != {};
        repository = "input-output-hk/voting-tools";
        remote = config.preset.github.lib.readRepository ciInputName null;
        revision = config.preset.github.lib.readRevision ciInputName null;
      };
    };

    command.text = config.preset.github.status.lib.reportBulk {
      bulk.text = ''
        nix eval .#hydraJobs --json \
          --apply 'jobs: __attrNames (removeAttrs jobs [ "required" ])' \
        | nix-systems -i
      '';
      each.text = ''nix build -L .#hydraJobs."$1".required'';
      skippedDescription = lib.escapeShellArg "No nix builder available for this system";
    };

    memory = 1024 * 8;

    nomad = {
      resources.cpu = 10000;

      driver = "exec";
    };
  };

  actions."voting-tools/ci" = {
    task = "ci";
    io = ''
      // This is a CUE expression that defines what events trigger a new run of this action.
      // There is no documentation for this yet. Ask SRE if you have trouble changing this.

      #lib.io.github_push
      #input: "${ciInputName}"
      #repo: "${repository}"
    '';
  };
}
