#!/bin/sh

export ANT_OPTS="$*"
ant

sudo rsync -rugl tmp/test-reports/html/* root@awipscm:/var/www/html/junit
