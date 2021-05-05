#!/bin/csh

#determine if ENI DTA time series script is running
set iRun = `ps -ef | grep processDTAtimeseries.csh | grep -v grep | wc -l`

#if not
if ($iRun < 1) then
	#remove the contents of the /tmp directory
	rm -Rvf /tmp/eniTSwork/*
	#re-run the script
	csh /usr/local/ldm/bin/eni/processDTAtimeseries.csh bg1 &
#if so
else
	#everything is okay
	echo "There are $iRun processes running..."
endif
