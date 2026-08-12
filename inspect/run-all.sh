#!/usr/bin/env sh
# Offline gate suite — table/cmap/shape vs vendored oracles (no Python needed).
set -e
here=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
# Root the tree at the WORKSPACE, not at scribe: cram (the WOFF1 inflate) is a
# sibling checkout, and :ignore-inherited-configuration means we must not fall
# back to a local-projects symlink to find it.  Still deterministic — the tree
# resolves this scribe, not some other one.
ws=$(CDPATH= cd -- "$here/.." && pwd)
export CL_SOURCE_REGISTRY="(:source-registry (:tree \"$ws\") :ignore-inherited-configuration)"
exec sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "scribe"))' \
  --load "$here/inspect/run-all.lisp"
