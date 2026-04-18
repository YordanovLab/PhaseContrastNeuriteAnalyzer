#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
APP="${ROOT_DIR}/app.R"
PAGE="${ROOT_DIR}/frontend/index.html"

echo "============================================================"
echo "Neurite Analysis in Phase Contrast Images"
echo "============================================================"
echo
echo "Trying to start the browser app..."
echo

if command -v Rscript >/dev/null 2>&1; then
  if command -v xdg-open >/dev/null 2>&1; then
    xdg-open "http://127.0.0.1:3838" >/dev/null 2>&1 &
  elif command -v wslview >/dev/null 2>&1; then
    wslview "http://127.0.0.1:3838" >/dev/null 2>&1 &
  fi
  cd "$ROOT_DIR"
  exec Rscript "$APP"
fi

echo "Rscript was not found, so the launcher will open the static navigator instead."
if command -v xdg-open >/dev/null 2>&1; then
  xdg-open "$PAGE" >/dev/null 2>&1 &
elif command -v wslview >/dev/null 2>&1; then
  wslview "$PAGE" >/dev/null 2>&1 &
else
  echo "Could not find a browser opener. Open this file manually:"
  echo "$PAGE"
fi
