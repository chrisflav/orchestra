#!/usr/bin/env bash
set -euo pipefail

echo "==> Building..."
lake build

echo "==> Running tests..."
lake test

echo "==> All checks passed."
