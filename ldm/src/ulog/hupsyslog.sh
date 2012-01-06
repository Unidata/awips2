#! /bin/sh
#
# Script to send a SIGHUP to syslog(8), causing it to close and reopen
# all log files.
# NB: Must be run by root or the kill(1) will fail;
#
kill -HUP `cat /etc/syslog.pid` || exit 1
