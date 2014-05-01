#!/usr/bin/perl

chop($hostname = `uname -n`);
$ldmhome = "/usr/local/ldm";
$pq_size = "500M";
$pq_slots = "default";
$surf_size = "2000000";
$numlogs = "7";
$num_metrics = "4";
$metrics_files = "$ldmhome/logs/metrics.txt*";
$data_path = "$ldmhome/data";
$pq_path = "$data_path/ldm.pq";
$etc_path = "$ldmhome/etc";
$log_rotate = "1";
$ip_addr = "0.0.0.0";
$port = "388";
$max_clients = 256;
$max_latency = 3600;
$offset = 3600;
$delete_info_files = 0;
$check_time = 1;		# whether or not to check the system clock
$warn_if_check_time_disabled = 1;	# warn if time-checking is disabled
$ntpdate = "ntpdate";	# pathname of the ntpdate(1) utility
$ntpdate_timeout = 20;	# timeout for the ntpdate(1) utility
@time_servers = (
    "ntp.ucsd.edu",
    "ntp1.cs.wisc.edu",
    "ntppub.tamu.edu",
    "otc1.psu.edu",
    "timeserver.unidata.ucar.edu"
);
$check_time_limit = 10;		# maximum allowable time-difference in seconds
# Whether or not to abort on failure.  This should be enabled on a virgin
# installation and disabled for an update.
$abort_if_check_time_failure = 0;
$netstat = "netstat -A inet -t -n";

if ($ARGV[0]) {
    require $ARGV[0];
}

print "
# The fully-qualified hostname of the computer system.  If the name isn't
# fully-qualified (i.e., if it doesn't include the domain) then replace
# \"$hostname\" with the fully-qualified hostname.
\$hostname = \"$hostname\";

# The LDM home directory:
\$ldmhome = \"$ldmhome\";

# Requested size, in bytes, of the data portion of the LDM product-queue.  The
# actual size might be slightly greater than the requested size for performance
# reasons.  It is recommended that IDD sites keep at least one hour's worth of
# data in the queue.  This means that the queue size should depend upon how
# much data is requested.  After the queue has stabilized, use the pqmon(1)
# utility to monitor the age of the oldest product in the queue and adjust this
# value, if necessary.  Understood suffixes include \"K\", \"M\", and \"G\" for
# \"kilo\", \"mega\", and \"giga\", respectively.  The default requested size
# is 500 megabytes (i.e., \"500M\").
\$pq_size = \"$pq_size\";

# Number of slots in the LDM product-queue.  This is the maximum number
# of data-products that the product-queue can contain.  Because it is
# recommended that IDD sites keep at least one hour's worth of data in
# the queue, this value should equal the maximum expected number of
# data-products per hour.  If the value is \"default\", then the number
# of slots will be computed using the size of the product-queue and a
# mean data-product size of 4096 bytes.  Use the pqmon(1) utility to
# monitor the number of data-products in the product-queue and adjust
# this value, if necessary.
\$pq_slots = \"$pq_slots\";

# Size of the pqsurf(1) product-queue in bytes.  Do not use any suffixes.  This
# is only meaningful if your LDM configuration-file executes the pqsurf(1)
# utility.  You probably won't need to change this.
\$surf_size = \"$surf_size\";

# Default number of log files that the \"newlog\" command should keep around:
\$numlogs = $numlogs;

# File paths - everything here is based on the \$ldmhome variable by default:
\$bin_path = \"\$ldmhome/bin\";
\$etc_path = \"\$ldmhome/etc\";
\$log_path = \"\$ldmhome/logs\";
\$data_path = \"\$ldmhome/data\";
\$pq_path = \"\$data_path/ldm.pq\";;

# NOTE: If you change the following, then you must also modify the EXEC entry
# for the \"pqsurf(1)\" utility in the LDM configuration-file (etc/ldmd.conf)
# to ensure that the same pathname is used (via the \"-q path\" option).
\$surf_path = \"\$data_path/pqsurf.pq\";

# ldmadmin(1) file locations and names:
\$pid_file = \"\$ldmhome/ldmd.pid\";
\$lock_file = \"\$ldmhome/.ldmadmin.lck\";
\$log_file = \"\$log_path/ldmd.log\";
\$ldmd_conf = \"\$etc_path/ldmd.conf\";
\$pqact_conf = \"\$etc_path/pqact.conf\";
\$scour_file = \"\$etc_path/scour.conf\";

# Whether or not to rotate the LDM log files whenever the LDM is started or
# restarted.  They are rotated if and only if the value is non-zero.
\$log_rotate = $log_rotate;

# The IP address of the network interface to be used by the LDM server.
# Address \"0.0.0.0\" means that the LDM server will use all available network
# interfaces.
\$ip_addr = \"$ip_addr\";

# The port on which the LDM server will listen for connections:
\$port = $port;

################################################################################
# The following variables are new with LDM version 6.8.0:
################################################################################

# Maximum latency in seconds.  A data-product arriving with a latency greater
# than this will be rejected by any downstream LDM process.
\$max_latency = $max_latency;

# Request time-offset in seconds.  Nomally, a downstream LDM will request data
# starting with just after the last successfully received data-product.  If that
# product doesn't exist in the upstream LDM's product-queue, however, then the
# downstream LDM will request data starting from this many seconds ago.  The
# need for this parameter can arise if, for example, the downstream LDM has been
# offline long enough for the last successfully recieved data-product to be
# purged from the upstream LDM's product-queue.  The value must be less than or
# equal to \$max_latency.
\$offset = $offset;

# Whether or not to delete the product-information files when the product-queue
# is deleted (via the \"delqueue\" command).  If true, then the new
# product-queue will be initially populated by requests for data that go back
# \$offset seconds in time; otherwise, the requests for data that are the same
# as before will start with the last successfully-received data-product for
# each connection.  Relay nodes should probably set this variable to true in
# order to regenerate a buffer of data while leaf nodes should probably set it
# to false in order to avoid receiving (and processing) duplicate data-products.
\$delete_info_files = $delete_info_files;

# The maximum number of extant client connections the LDM server will allow
# before additional incoming connections requests are rejected.
\$max_clients = $max_clients;

################################################################################
# The following deal with verifying the accuracy of the system clock.

# Whether or not to check the system clock at all:
\$check_time = $check_time;

# Whether or not to print a warning if time-checking is disabled:
\$warn_if_check_time_disabled = $warn_if_check_time_disabled;

# Pathname of the ntpdate(1) utility:
\$ntpdate = \"$ntpdate\";

# Timeout, in seconds, for the ntpdate(1) utility:
\$ntpdate_timeout = $ntpdate_timeout;

# Time server hostnames.  Modify to suit your needs.  They're accessed in
# random order.
\@time_servers = (\n";
for (my $i = 0; $i <= $#time_servers; $i++) {
    print "    \"$time_servers[$i]\",\n";
}
print ");

# Maximum allowable time-difference in seconds:
\$check_time_limit = $check_time_limit;

# Whether or not to abort on failure:
\$abort_if_check_time_failure = $abort_if_check_time_failure;

################################################################################
# The following deal with LDM performance metrics.

# The netstat(1) command for printing numeric port numbers of TCP Internet
# connections:
\$netstat = \"$netstat\";

# The metrics file into which the \"addmetrics\" command appends data:
\$metrics_file = \"\$log_path/metrics.txt\";

# The file-pattern for the metrics files to be plotted by the \"plotmetrics\"
# command:
\$metrics_files = \"$metrics_files\";

# The number of metrics-files that the \"newmetrics\" command should keep
# around:
\$num_metrics = $num_metrics;

################################################################################
# The following is necessary because the last executable statement of a
# \"require\"ed file must have a non-zero value.
################################################################################
1;
";
