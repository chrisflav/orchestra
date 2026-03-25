#!/usr/bin/env bash
set -euo pipefail

echo "==> Building..."
lake build

echo "==> Running listener-config-test..."
.lake/build/bin/listener-config-test

echo "==> All checks passed."
