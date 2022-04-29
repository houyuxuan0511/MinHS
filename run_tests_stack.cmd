@echo off
setlocal enableextensions EnableDelayedExpansion
set runhaskell=stack exec runhaskell --
set runhaskell_set=1
set stack_install_root=
set stack_install_root_set=0
for /f "delims=" %%a in ('stack path --local-install-root 2^>nul') do (
	set stack_install_root=%%a
	set stack_install_root_set=1
)
if "!stack_install_root_set!" EQU "1" (
	set executable="!stack_install_root!\bin\minhs-1"
) else (
	set executable=stack exec minhs-1 --
)
set executable_set=1
run_tests_cabal.cmd %*