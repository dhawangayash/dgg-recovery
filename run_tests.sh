#!/usr/bin/env bash
set -euo pipefail

# Run from repo root (directory containing dgg-recovery.el and test/)
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Validate expected structure
if [[ ! -f "dgg-recovery.el" ]]; then
  echo "ERROR: dgg-recovery.el not found in: $PWD" >&2
  exit 1
fi
if [[ ! -f "test/dgg-recovery-test.el" ]]; then
  echo "ERROR: test/dgg-recovery-test.el not found in: $PWD/test" >&2
  exit 1
fi

# Prefer explicit emacs if available
if ! command -v emacs >/dev/null 2>&1; then
  echo "ERROR: emacs not found on PATH" >&2
  exit 1
fi

echo "Repo root: $PWD"
echo "Running ERT tests..."

emacs -Q --batch \
  -L "$PWD" -L "$PWD/test" \
  -l "$PWD/dgg-recovery.el" \
  -l "$PWD/test/dgg-recovery-test.el" \
  -f ert-run-tests-batch-and-exit

echo "OK: tests passed"
