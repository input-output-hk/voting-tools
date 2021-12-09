# Revision history for voting-tools

## 0.2.0.0 -- 2021-12-08

- Updated voter-registration tool to work in the Alonzo era.
- Improved error messages if there isn't enough funds to meet the registration transaction fee.
- Updated cardano-node and cardano-db-sync dependencies.
- Changed voter-registration to only output transaction metadata, instead of a signed transaction. User must now create and sign transaction.
- Added --version flag to voter-registration and voting-tools executables.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
