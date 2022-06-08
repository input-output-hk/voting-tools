# Register to Vote from Scratch

## Preamble

This manual requires:

-   `cardano-cli` executable from the [cardano-node](https://github.com/input-output-hk/cardano-node) project
-   `jcli` executable from the [jormungandr](https://github.com/input-output-hk/jormungandr) project
-   `voter-registration` executable from this project
-   `voting-tools` executable from this project
-   A running [cardano-db-sync](https://github.com/input-output-hk/cardano-db-sync) instance, fully synced

Run this example on testnet (Use "--mainnet" if you want to run this on mainnet):

``` shell
export NETWORK_ID="--testnet-magic 1097911063"
```

Let `cardano-cli` know where the node socket is:

``` shell
export CARDANO_NODE_SOCKET_PATH="/run/cardano-node/node.socket"
```

## Generate Stake Address

Generate a stake address from a stake key:

``` shell
cardano-cli stake-address key-gen \
    --verification-key-file stake.vkey \
    --signing-key-file stake.skey
cardano-cli stake-address build \
    --stake-verification-key-file stake.vkey \
    $NETWORK_ID \
    --out-file stake.addr
```

## Generate Base Address

Generate a [base address](https://docs.cardano.org/core-concepts/cardano-addresses#baseaddresses) from a payment key and a stake key:

``` shell
cardano-cli address key-gen \
    --verification-key-file payment.vkey \
    --signing-key-file payment.skey
cardano-cli address build \
    $NETWORK_ID \
    --payment-verification-key-file payment.vkey \
    --stake-verification-key-file stake.vkey \
    --out-file payment.addr

export PAYMENT_ADDR=$(cat payment.addr)
```

We will use the base address to:

-   Pay transaction fees
-   Pay stake address registration fees
-   Hold value for the associated stake key (that is later translated into voting power)

## Get Funds

We need to add funds to the base address in order to pay fees and hold value (i.e. voting power):

-   On testnet, add funds using the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/), using the base address (`$(cat payment.addr)`) as the address.
-   On mainnet, add funds using an exchange or transfer money from elsewhere.

## Register Stake Address

The stake address must be registered on-chain in order for the snapshot tool to find it.

To do so, first generate a registration certificate:

``` shell
cardano-cli stake-address registration-certificate \
    --stake-verification-key-file stake.vkey \
    --out-file stake.cert
```

Then submit the registration certificate in a transaction:

``` shell
export UTXO=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $1;}')
export UTXO_TXIX=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $2;}')
echo "UTxO: $UTXO#$UTXO_TXIX"

cardano-cli query protocol-parameters \
    $NETWORK_ID \
    --out-file protocol.json

cardano-cli transaction build  \
    $NETWORK_ID \
    --tx-in $UTXO#$UTXO_TXIX \
    --change-address $PAYMENT_ADDR \
    --certificate-file stake.cert \
    --protocol-params-file protocol.json  \
    --out-file tx.raw \
    --witness-override 2

cardano-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --signing-key-file stake.skey \
    $NETWORK_ID \
    --out-file tx.signed

cardano-cli transaction submit \
    --tx-file tx.signed \
    $NETWORK_ID
```

Note that we pay not only a transaction fee, but a deposit for registering the stake address.

The deposit amount is listed in the protocol parameters under "stakeAddressDeposit".

## Register Voting Key

We must generate a voting key to use on the Catalyst side-chain:

``` shell
jcli key generate \
    --type ed25519extended \
    > vote.skey
jcli key to-public \
    < vote.skey \
    > vote.pub
```

And then generate metadata associating that voting key with our stake address:

``` shell
export SLOT_TIP=$(cardano-cli query tip $NETWORK_ID | jq '.slot')

voter-registration \
    --rewards-address $(cat stake.addr) \
    --vote-public-key-file vote.pub \
    --stake-signing-key-file stake.skey \
    --slot-no $SLOT_TIP \
    --json > metadata.json
```

The voting power on the Catalyst side-chain is derived from the value associated with this stake address (via our [base address](https://docs.cardano.org/core-concepts/cardano-addresses#baseaddresses)).

It is also possible to delegate your voting power between voting keys.

For example to split your voting power 1/3 to `vote1.pub` and 2/3 to `vote2.pub`, you could use the following invocation:

``` shell
voter-registration \
    --rewards-address $(cat stake.addr) \
    --delegate vote1.pub,1 \
    --delegate vote2.pub,2 \
    --stake-signing-key-file stake.skey \
    --slot-no $SLOT_TIP \
    --json > metadata.json
```

Submit the generated metadata to the blockchain in a transaction:

``` shell
export UTXO=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $1;}')
export UTXO_TXIX=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $2;}')
echo "UTxO: $UTXO#$UTXO_TXIX"

cardano-cli transaction build  \
    $NETWORK_ID \
    --tx-in $UTXO#$UTXO_TXIX \
    --change-address $PAYMENT_ADDR \
    --metadata-json-file metadata.json \
    --protocol-params-file protocol.json  \
    --out-file tx.raw

cardano-cli transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file payment.skey \
  $NETWORK_ID \
  --out-file tx.signed

cardano-cli transaction submit \
  --tx-file tx.signed \
  $NETWORK_ID

cardano-cli transaction txid --tx-file tx.signed
```

## Confirm Vote Power

The voting power associated with a stake address can be confirmed using the snapshot tool.

The snapshot tool requires a fully synced `cardano-db-sync` database to retrieve voting power.

Refer to [cardano-db-sync](https://github.com/input-output-hk/cardano-db-sync) for instructions on starting and building `cardano-db-sync`. In this example we assume it has been started using [docker-compose.yaml](https://github.com/input-output-hk/cardano-db-sync/blob/master/docker-compose.yml) (as described in the [Docker](https://github.com/input-output-hk/cardano-db-sync/blob/master/doc/docker.md) section).

``` shell
export REWARDS_ADDRESS=$(cardano-cli address info --address $(cat stake.addr) | jq -r .base16)
export DB=$(cat ../cardano-db-sync/config/secrets/postgres_db)
export DB_USER=$(cat ../cardano-db-sync/config/secrets/postgres_user)
export DB_PASS=$(cat ../cardano-db-sync/config/secrets/postgres_password)

voting-tools \
    $NETWORK_ID \
    --db $DB \
    --db-user $DB_USER \
    --db-pass $DB_PASS \
    --db-host localhost \
    --out-file voting-snaphot.json

cat voting-snaphot.json | jq --arg REWARDS_ADDRESS "$REWARDS_ADDRESS" '.[] | select(.rewards_address | contains($REWARDS_ADDRESS))'
```

## Return Funds to Faucet

It is polite to return funds used in the testnet to the faucet:

```shell
export ADA_LEFT=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $3;}')
export UTXO=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $1;}')
export UTXO_TXIX=$(cardano-cli query utxo $NETWORK_ID --address $PAYMENT_ADDR | tail -n1 | awk '{print $2;}')
export FAUCET_ADDR="addr_test1qqr585tvlc7ylnqvz8pyqwauzrdu0mxag3m7q56grgmgu7sxu2hyfhlkwuxupa9d5085eunq2qywy7hvmvej456flknswgndm3"

echo
echo "Building faucet refund transaction..."

cardano-cli transaction build-raw \
  --alonzo-era \
  --fee 0 \
  --tx-in $UTXO#$UTXO_TXIX \
  --tx-out "$FAUCET_ADDR+$ADA_LEFT" \
  --out-file return.raw

export FEE=$(cardano-cli transaction calculate-min-fee \
            $NETWORK_ID \
            --tx-body-file return.raw \
            --tx-in-count 1 \
            --tx-out-count 1 \
            --witness-count 1 \
            --protocol-params-file protocol.json | awk '{print $1;}')
export AMT_OUT=$(expr $ADA_LEFT - $FEE)

cardano-cli transaction build-raw \
            --alonzo-era \
            --fee $FEE \
            --tx-in $UTXO#$UTXO_TXIX \
            --tx-out "$FAUCET_ADDR+$AMT_OUT" \
            --out-file return.raw

cardano-cli transaction sign \
        --signing-key-file payment.skey \
        --tx-body-file return.raw \
            --out-file return.signed

cardano-cli transaction submit \
            $NETWORK_ID \
            --tx-file return.signed

echo
echo "Awaiting refund..."
sleep 60
cardano-cli query utxo \
            $NETWORK_ID \
            --address $PAYMENT_ADDR
```
