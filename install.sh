#!/bin/bash
echo
echo "[install.sh] building llama"
echo
stack install --fast
echo
echo "[install.sh] configuring chrony"
echo
curr_dir=$(pwd)
chrony_dir=$curr_dir/test/chrony
cd $chrony_dir
./configure
cd $curr_dir
echo
echo "[install.sh] ok, llama should be good to go!"
echo
