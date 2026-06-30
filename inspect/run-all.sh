#!/usr/bin/env sh
# Offline gate suite — table/cmap/shape vs vendored oracles (no Python needed).
set -e
here=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
export CL_SOURCE_REGISTRY="(:source-registry (:tree \"$here\") :ignore-inherited-configuration)"
exec sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "scribe"))' \
  --load "$here/inspect/run-all.lisp"
