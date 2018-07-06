#!bin/sh

if [ -d "nonsmooth-joint-opt" ]
then
	echo Source code found. Doing nothing...
else
	echo Cloning git...
	git clone https://github.com/jjfeng/nonsmooth-joint-opt.git
	mkdir -p nonsmooth-joint-opt/results/sgl/tmp/
fi
mkdir -p results/HC results/HC0 results/GS results/RS results/NM results/iSGL results/iSGL0 data
