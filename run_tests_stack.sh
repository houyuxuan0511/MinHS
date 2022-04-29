#!/bin/bash
export RUNHASKELL="stack exec runhaskell --"
stack_install_root="$(stack path --local-install-root 2>/dev/null)"
if test -n "$stack_install_root"; then
  export EXECUTABLE_USE_STACK_EXEC=
  export EXECUTABLE="${stack_install_root}/bin/minhs-1"
else
  export EXECUTABLE_USE_STACK_EXEC=1
  export EXECUTABLE=
fi
./run_tests_cabal.sh "$@"
