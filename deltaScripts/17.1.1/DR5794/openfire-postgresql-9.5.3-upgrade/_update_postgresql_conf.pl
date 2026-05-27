#!/usr/bin/env perl

# Update postgresql.conf (read from stdin)
# Accompanies upgrade from PostgreSQL 9.3.x to 9.5.x

use strict;
use warnings;

while (<>) {
    if (/^checkpoint_segments\s*=\s*/ ||
        /^autocommit\s*=\s*/ ||
        /^debug_assertions\s*=\s*/ ||
        /^ssl_renegotiation_limit\s*=\s*/) {
        # comment out options removed in PostgreSQL 9.5
        chomp;
        print "# " . $_ . "    # removed in 9.5\n";
    } else {
        print;
    }
}
