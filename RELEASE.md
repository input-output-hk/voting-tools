# Release Checklist

## Preparing the release

- Fetch the tip of `master`:

  ```shell
  $ git checkout master
  $ git pull
  ```

- Ensure that the versions of `voting-tools` and `voter-registration` have been bumped appropriately.

- Create and push a signed release tag on the `HEAD` of `master`.

  ```shell
  $ git tag -s -m v0.4.0.0 v0.4.0.0
  $ git push origin refs/tags/v0.4.0.0
  ```

  Where `v0.4.0.0` should be replaced by the actual date of the release.

- Wait for the Hydra build of that commit to finish and then check the evaluation for the "voterRegistrationTarball.x86_64-linux" build (e.g. https://hydra.iohk.io/build/15320965 ). Copy the URL and SHA-256 hash of that build (click "Details") for use in creating the release notes.

## Create the release notes

- Write release notes on the [release page](https://github.com/input-output-hk/voting-tools/releases).
  - Include the link to the `voter-registration.tar.gz` and SHA-256 hash from the previous step.

- Remove items that are irrelevant to users (e.g. pure refactoring, improved testing).

## Manual ad-hoc verifications

- Execute all *TODO* [manual scenarios]() on the binaries to be released.

## Publication

- Once everyone has signed off (i.e. Catalyst Team, Tech lead, QA & Release manager), publish the release draft.
