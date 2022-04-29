#!/bin/bash
if test -z "$EXECUTABLE" -a -z "$EXECUTABLE_USE_STACK_EXEC"; then
  CABAL_VERSION=$(cabal --numeric-version | cut -d. -f1)
  problem=0
  test -z "$CABAL_VERSION" && problem=1
  test "$problem" -eq 0 && test "$CABAL_VERSION" -lt 3 && problem=1
  if test "$problem" -eq 1; then
    echo >&2 "Your cabal version should be at least 3."
    full_version=$(cabal --numeric-version)
    test -n "$full_version" && echo >&2 "(It reports version $full_version)"
    echo >&2 "If you're on Linux, FreeBSD, MacOS or WSL2, we recommend using ghcup to install"
    echo >&2 "ghc, cabal, etc.:"
    echo >&2 "  https://www.haskell.org/ghcup/"
    echo >&2 "If you're on Windows and don't want to use WSL2 (or don't know what that is),"
    echo >&2 "we recommend using stack instead of cabal:"
    echo >&2 "  https://docs.haskellstack.org/en/stable/README/"
    echo >&2 "If using stack, use the run_tests_stack.sh file instead of this one."
    exit 1
  fi
fi

RUNHASKELL=${RUNHASKELL:-runhaskell}
if test "$EXECUTABLE_USE_STACK_EXEC" = "1"; then
  EXECUTABLE=( stack exec minhs-1 -- )
elif test -z "$EXECUTABLE"; then
  cabal_binname="$(cabal list-bin minhs1 2>/dev/null)"
  if test -n "$cabal_binname"; then
    EXECUTABLE=( "$cabal_binname" )
  else
    EXECUTABLE=( cabal exec minhs-1 -- )
  fi
fi

$RUNHASKELL -i./tests/driver ./tests/driver/Check.hs "$@" "${EXECUTABLE[@]}"
