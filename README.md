# Catalyst Voting Tools

The goal of the tool is to aid the user in registering to vote. The
process for doing so is to create a transaction, with the vote
registration encoded as transaction metadata, and then to give this
transaction to the user, so they can submit it to the chain.

## Running

```
make build

CARDANO_NODE_SOCKET_PATH=state-node-testnet/node.socket \
  ./voting-tools/bin/voting-tools register \
  --payment-signing-key payment.skey \
  --stake-signing-key stake.skey \
  --vote-public-key vote.pub \
  --payment-address "addr_test..." \
  --testnet-magic 1097911063 > meta.txsigned
  
./voting-tools/bin/voting-tools genesis --mainnet --db-user cardano-node --out-file genesis.json

./voting-tools/bin/voting-tools rewards --mainnet --db-user cardano-node --total-rewards 8000 --out-file rewards.json

```

## Development

```
# Launch a ghcid session for the given target
make dev target=lib:voting-tools
make dev target=exe:voting-tools
# Launch a ghci session for the given target
make repl target=lib:voting-tools
```
