# Catalyst Voting Tools

A library and series of executables to aid programmers and users to
interact with the voting capabilities of Catalyst.

The "voter-registration" executable aids the user in registering to
vote. The executable creates a transaction, with the vote registration
encoded as transaction metadata, signs the transaction, and then
returns this signed transaction to the user, so they can submit it to
the chain.

The "voting-tools" executable has two functions: "genesis" and
"rewards". The "genesis" tool creates a genesis JSON file from a
"genesis-template.json" file, populating the file with the initial
funds and block-zero date. The "rewards" tool calculates the voting
rewards that should be given to each user and optionally generates the
appropriate MIR certificates.

## Registering to vote

Register to vote on the testnet with protocol magic id of "1097911063".

- Requires:
  - cardano-cli executable from the cardano-node project
  - jcli executable from the jormungandr project
  - voter-registration executable from this project

```
cardano-cli address key-gen \
        --verification-key-file payment.vkey \
        --signing-key-file payment.skey
cardano-cli stake-address key-gen \
        --verification-key-file stake.vkey \
        --signing-key-file stake.skey
cardano-cli address build \
        --payment-verification-key-file payment.vkey \
        --out-file payment.addr \
        --testnet-magic 1097911063
cardano-cli stake-address build \
        --stake-verification-key-file stake.vkey \
        --out-file stake.addr \
        --testnet-magic 1097911063
jcli key generate \
        --type ed25519extended \
        > vote.skey
jcli key to-public \
        < vote.skey \
        > vote.pub

CARDANO_NODE_SOCKET_PATH=/run/cardano-node/node.socket voter-registration \
        --payment-signing-key payment.skey \
        --payment-address $(cat payment.addr) \
        --stake-signing-key stake.skey \
        --rewards-address $(cat stake.addr) \
        --vote-public-key vote.pub \
        --testnet-magic 1097911063 \
        --out-file vote-tx.signed \
        --alonzo-era
        --sign
CARDANO_NODE_SOCKET_PATH=/run/cardano-node/node.socket cardano-cli transaction submit \
        --tx-file vote-tx.signed \
        --testnet-magic 1097911063
```

## Running

The latest pre-built statically linked tarball can be found at https://hydra.iohk.io/job/Cardano/voting-tools/native.voterRegistrationTarball.x86_64-linux/latest-finished/download/1/voter-registration.tar.gz

```
make build-voter-registration

CARDANO_NODE_SOCKET_PATH=state-node-testnet/node.socket \
  ./voter-registration/bin/voter-registration \
  --payment-signing-key payment.skey \
  --stake-signing-key stake.skey \
  --vote-public-key vote.pub \
  --payment-address "addr_test..." \
  --testnet-magic 1097911063 > meta.txsigned

make build-voting-tools

./voting-tools/bin/voting-tools genesis --mainnet --db-user cardano-node --out-file genesis.json

./voting-tools/bin/voting-tools rewards --mainnet --db-user cardano-node --total-rewards 8000 --out-file rewards.json

```

## Development

```
# Launch a ghcid session for the given target
make dev target=lib:voting-tools
make dev target=exe:voting-tools
make dev target=exe:voter-registration
# Launch a ghci session for the given target
make repl target=lib:voting-tools
```

## Distribution

A static binary for the "voter-registration" executable is provided.
Due to limitations in our cross-compilation infrastructure
(specifically, static binaries of postgresql libraries cannot be
produced), a static binary cannot be provided for the "voting-tools"
executable.

The latest static binary can be found
[here](https://hydra.iohk.io/job/Cardano/voting-tools/native.voterRegistrationTarball.x86_64-linux/latest/download/1/voter-registration.tar.gz),
or built with Nix using:

```
nix-build -A voterRegistrationTarball
```
