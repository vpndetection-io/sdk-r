#!/bin/bash

# Runs the package's suite in the R image, the way CI runs it: installs the package into a scratch
# copy, its dependencies into a library cached under R_LIB_CACHE, and runs tests/testthat.
#
#   ./scripts/test.sh                 the whole suite
#   FILTER=transport ./scripts/test.sh   one file
#
# The conformance corpus is tests/testthat/testdata/testdata.json, the cases every SDK of this API
# answers identically; without it the conformance file fails. NOT_CRAN=true runs the clock-timed
# tests a CRAN check skips.

set -euo pipefail

pkg="$(cd "$(dirname "$0")/.." && pwd)"
image="${R_IMAGE:-rocker/r-ver:4.5.2}"
lib="${R_LIB_CACHE:-/tmp/rlib}"
mkdir -p "$lib"

docker run --rm --network host -e FILTER -e NOT_CRAN=true -v "${lib}:/rlib" -v "${pkg}:/pkg:ro" -w /tmp "$image" bash -c '
set -euo pipefail
Rscript -e "options(Ncpus = 4); need <- setdiff(c(\"curl\", \"jsonlite\", \"testthat\", \"webfakes\"), rownames(installed.packages(lib.loc = \"/rlib\"))); if (length(need)) install.packages(need, lib = \"/rlib\", quiet = TRUE)"
cp -r /pkg /tmp/pkg
R_LIBS=/rlib R CMD INSTALL /tmp/pkg > /tmp/install.log 2>&1 || { cat /tmp/install.log; exit 1; }
cd /tmp/pkg/tests
R_LIBS=/rlib Rscript -e "library(testthat); library(vpndetection); test_dir(\"testthat\", reporter = \"summary\", package = \"vpndetection\", load_package = \"installed\", stop_on_failure = TRUE, filter = if (nzchar(Sys.getenv(\"FILTER\"))) Sys.getenv(\"FILTER\") else NULL)"
'
