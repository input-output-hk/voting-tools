# Catalyst Voting Tools

The goal of the tool is to aid the user in registering to vote. The
process for doing so is to create a transaction, with the vote
registration encoded as transaction metadata, and then to give this
transaction to the user, so they can submit it to the chain.

## Running

```
make build
CARDANO_NODE_SOCKET_PATH=state-node-testnet/node.socket \
  ./voter-registration-tool/voter-registration/bin/voter-registration \
  --payment-signing-key payment.skey \
  --stake-signing-key stake.skey \
  --vote-public-key vote.pub \
  --payment-address "addr_test..." \
  --testnet-magic 1097911063 > meta.txsigned
```

## Development

```
# Launch a ghcid session for the given target
make dev target=lib:voter-registration
# Launch a ghci session for the given target
make repl target=lib:voter-registration
```
