# Catalyst Voting Tools

<p align="center">
  <a href="https://buildkite.com/input-output-hk/voting-tools"><img src="https://img.shields.io/buildkite/7ea3dac7a16f066d8dfc8f426a9a9f7a2131e899cd96c444cf/master?label=BUILD&style=for-the-badge"/></a>
  <a href="https://hydra.iohk.io/jobset/Cardano/voting-tools#tabs-jobs"><img src="https://img.shields.io/endpoint?style=for-the-badge&url=https%3A%2F%2Fhydra.iohk.io%2Fjob%2FCardano%2Fvoting-tools%2Frequired%2Fshield" /></a>
</p>

A library and series of executables to aid programmers and users to
interact with the voting capabilities of Catalyst.

The "voter-registration" executable creates transaction metadata in the correct format for submission with a transaction, to register a user to vote.

The "voting-tools" creates a snapshot of the voting power of registrations and outputs it as JSON file.

## Obtaining

### Static binary

A static binary for the "voter-registration" executable is provided. Due to limitations in our cross-compilation infrastructure (specifically, static binaries of postgresql libraries cannot be produced), a static binary cannot be provided for the "voting-tools" executable.

The latest static binary can be found [here](https://hydra.iohk.io/job/Cardano/voting-tools/native.voterRegistrationTarball.x86_64-linux/latest/download/1/voter-registration.tar.gz).

Or built with:

```
nix build .#voterRegistrationTarball
```

### Build it yourself

#### With Nix

```
nix build .#voter-registration -o voter-registration
nix build .#voting-tools -o voting-tools

./voting-tools/bin/voting-tools --mainnet --db-user cardano-node --out-file snapshot.json
```

## How to Register to Vote from Scratch

See [Manual](https://github.com/input-output-hk/voting-tools/blob/master/MANUAL.org).

## Development

```
# Launch a ghcid session for the given target
make dev target=lib:voting-tools
make dev target=exe:voting-tools
make dev target=exe:voter-registration
# Launch a ghci session for the given target
make repl target=lib:voting-tools
```
