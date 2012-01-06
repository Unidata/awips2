#!/bin/sh

su - root -c "/home/kchrisma/logScripts/umap;/home/kchrisma/logScripts/map"
wait

perl scanErrors.pl dev1
wait
