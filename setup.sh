#!/bin/bash

echo "*** Make sure you:"
echo "***     Run this like: source setup.sh"
echo "***     Run from root directory of the verbii source."

# NOTE: The following code (finding the absolute pathname of this script)
# is from https://www.baeldung.com/linux/bash-get-location-within-script

SCRIPT_PATH="${BASH_SOURCE}"
while [ -L "${SCRIPT_PATH}" ]; do
  SCRIPT_DIR="$(cd -P "$(dirname "${SCRIPT_PATH}")" >/dev/null 2>&1 && pwd)"
  SCRIPT_PATH="$(readlink "${SCRIPT_PATH}")"
  [[ ${SCRIPT_PATH} != /* ]] && SCRIPT_PATH="${SCRIPT_DIR}/${SCRIPT_PATH}"
done
SCRIPT_PATH="$(readlink -f "${SCRIPT_PATH}")"
SCRIPT_DIR="$(cd -P "$(dirname -- "${SCRIPT_PATH}")" >/dev/null 2>&1 && pwd)"

echo "*** Setting VERBII_BOOT ..."

# NOTE: path MUST include trailing slash to avoid cross-platform issues
# with \ vs /
export VERBII_BOOT=${SCRIPT_DIR}/lib/
