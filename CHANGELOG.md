# Revision history for voting-tools

## 0.3.0.0 -- 2021-12-08

- The `voter-registration` tool now allows the user to delegate their voting power between voting keys using the `--delegate key,weight` syntax.
  - Previous `--vote-public-key-file` syntax still supported.
- The `voting-tools` tool now handles registrations made using the [CIP-36](https://cips.cardano.org/cips/cip36/) transaction metadata format (i.e. `delegations` and `voting purpose`)
  - Previous transaction metadata format still supported.
- Re-arranged modules to improve architecture.

## 0.2.0.0 -- 2021-12-08

- Updated voter-registration tool to work in the Alonzo era.
- Improved error messages if there isn't enough funds to meet the registration transaction fee.
- Updated cardano-node and cardano-db-sync dependencies.
- Changed voter-registration to only output transaction metadata, instead of a signed transaction. User must now create and sign transaction.
- Added --version flag to voter-registration and voting-tools executables.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
