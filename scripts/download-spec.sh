#!/bin/bash

# Refreshes the pinned OpenAPI spec from the published one.
#
# The copy in inst/spec/ is the wire contract this hand-written client is built against, and the
# package reads it at runtime to check every answer, so a build stays reproducible and offline and
# the diff shows exactly which spec version the client was checked against. Run this deliberately,
# then commit the spec change alongside whatever it made you change, so a reviewer sees both.

set -euo pipefail

cd "$(dirname "$0")/.."

SPEC_URL="${SPEC_URL:-https://s3.vpndetection.io/vpndetection-public/openapi/openapi.json}"

mkdir -p inst/spec
curl -fsS "$SPEC_URL" -o inst/spec/openapi.json
echo "inst/spec/openapi.json <- ${SPEC_URL}"
python3 -c 'import json, sys; print("  version:", json.load(open(sys.argv[1]))["info"]["version"])' inst/spec/openapi.json
