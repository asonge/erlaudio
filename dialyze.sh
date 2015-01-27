#!/bin/bash
DIRNAME=`pwd`
PROJNAME=`basename $DIRNAME`

if [[ ! -f "$HOME/.dialyzer_plt" ]];
  then
  echo "OTP plt not found"
  exit -1
fi

PLT="$HOME/.dialyzer_plt"

rebar compile skip_deps=true

if [[ -f $PROJNAME.plt ]];
  then
  dialyzer --check_plt --plt $PROJNAME.plt -r ebin/
  if [[ $? -ne 0 ]];
    then
    echo "Not up to date, dialyzing"
    dialyzer --add_to_plt --plt $PLT --output_plt $PROJNAME.plt -r ebin/
  fi
else
  echo "Dialyzing $PROJNAME"
  dialyzer --add_to_plt --plt $PLT --output_plt $PROJNAME.plt -r ebin/
fi

echo "Checking"
dialyzer -Werror_handling -Wrace_conditions -Wunderspecs --plt $PROJNAME.plt -r ebin/
