# Catalyst Voting Tools

A library and series of executables to aid programmers and users to
interact with the voting capabilities of Catalyst.

The "voter-registration" executable creates transaction metadata in the correct format for submission with a transaction, to register a user to vote.

The "voting-tools" executable has two functions: "genesis" and
"rewards". The "genesis" tool creates a genesis JSON file from a
"genesis-template.json" file, populating the file with the initial
funds and block-zero date. The "rewards" tool calculates the voting
rewards that should be given to each user and optionally generates the
appropriate MIR certificates.

## Obtaining

### Static binary

A static binary for the "voter-registration" executable is provided. Due to limitations in our cross-compilation infrastructure (specifically, static binaries of postgresql libraries cannot be produced), a static binary cannot be provided for the "voting-tools" executable.

The latest static binary can be found [here](https://hydra.iohk.io/job/Cardano/voting-tools/native.voterRegistrationTarball.x86_64-linux/latest/download/1/voter-registration.tar.gz).

Or built with:

```
nix-build -A voterRegistrationTarball
```

### Build it yourself

#### With Nix

```
nix-build -A voter-registration -o voter-registration
nix-build -A voting-tools -o voting-tools

./voting-tools/bin/voting-tools genesis --mainnet --db-user cardano-node --out-file genesis.json

./voting-tools/bin/voting-tools rewards --mainnet --db-user cardano-node --total-rewards 8000 --out-file rewards.json
```

## Registering to vote

- Requires:
  - cardano-cli executable from the cardano-node project
  - jcli executable from the jormungandr project
  - voter-registration executable from this project
  - jq

### Generating vote registration transaction metadata

First we'll need to generate our keys and addresses, or use existing ones:

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
```

From here, we can generate our vote registration, encoded in transaction metadata:

```
export CARDANO_NODE_SOCKET_PATH=/run/cardano-node/node.socket
# Or use --mainnet
export NETWORK_ID="--testnet-magic 1097911063"
export SLOT_TIP=$(cardano-cli query tip $NETWORK_ID | jq '.slot')

voter-registration \
    --rewards-address $(cat stake.addr) \
    --vote-public-key-file vote.pub \
    --stake-signing-key-file stake.skey \
    --slot-no $SLOT_TIP \
    --json > metadata.json
```

Both CBOR (--cbor) and JSON (--json) formats exist.

### Submission of vote registration

Next, we need to add this transaction metadata to a transaction and submit it to the chain.

First we'll grab the protocol parameters:

```
cardano-cli query protocol-parameters \
    $NETWORK_ID \
    --out-file protocol.json
```

And find some funds to use:

```
export PAYMENT_ADDR=$(cat payment.addr)

echo "UTxOs available:"
cardano-cli query utxo \
    $NETWORK_ID \
    --address $PAYMENT_ADDR
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b9579d53f3fd77679874a2d1828e2cf40e31a8ee431b35ca9347375a56b6c39b     0        999821651 lovelace + TxOutDatumNone

```

Here we're just using the first TxHash and TxIx we find, you should choose an appropriate UTxO and TxIx.

```
export AMT=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $3;}')
export UTXO=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $1;}')
export UTXO_TXIX=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $2;}')
echo "UTxO: $UTXO#$UTXO_TXIX"
```

Here we'll make draft transaction for the purposes of fee estimation. This transaction will simply send the entire UTxO value back to us, minus the fee. We don't need to send money anywhere else, we simply have to make a valid transaction with the metadata attached.

```
cardano-cli transaction build-raw \
    --tx-in $UTXO#$UTXO_TXIX \
    --tx-out $(cat payment.addr)+0 \
    --invalid-hereafter 0 \
    --fee 0 \
    --out-file tx.draft \
    --metadata-json-file metadata.json

export FEE=$(cardano-cli transaction calculate-min-fee \
    $NETWORK_ID \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 1 \
    --protocol-params-file protocol.json | awk '{print $1;}')

export AMT_OUT=$(expr $AMT - $FEE)
```

Then we have to decide on a TTL for the transaction, and build the final transaction:

```
export TTL=$(expr $SLOT_TIP + 200)

cardano-cli transaction build-raw \
    --tx-in $UTXO#$UTXO_TXIX \
    --tx-out $PAYMENT_ADDR+$AMT_OUT \
    --invalid-hereafter $TTL \
    --fee $FEE \
    --out-file tx.raw
```

Then we can sign the transaction:

```
cardano-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    $NETWORK_ID \
    --out-file tx.signed
```

And finally submit our transaction:

```
cardano-cli transaction submit \
    --tx-file tx.signed \
    $NETWORK_ID
```

We'll have to wait a little while for the transaction to be incorporated into the chain:

```
cardano-cli query utxo --address $(cat payment.addr) $NETWORK_ID
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b9579d53f3fd77679874a2d1828e2cf40e31a8ee431b35ca9347375a56b6c39b     0        999821651 lovelace + TxOutDatumNone

cardano-cli query utxo --address $(cat payment.addr) $NETWORK_ID
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
4fbd6149f9cbbeb8f91b618ae3813bc451c22059c626637d3b343d3114cb92c5     0        999642026 lovelace + TxOutDatumNone
```

But once we've confirmed the transaction has entered the chain, we're registered!

## Development

```
# Launch a ghcid session for the given target
make dev target=lib:voting-tools
make dev target=exe:voting-tools
make dev target=exe:voter-registration
# Launch a ghci session for the given target
make repl target=lib:voting-tools
```
