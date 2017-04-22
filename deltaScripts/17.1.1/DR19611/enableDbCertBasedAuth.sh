#!/bin/sh
# DR 19611 (part of DCS 18655) - Enabled certificate-based authentication for the
# database.

# Change "host" entries to "hostnossl" and add "hostssl" entries.
dir=$(cd "$(dirname "$0")"; pwd)
"$dir"/pg_hba_conf-tool.py --update-for-ssl
if [ "$SITE_TYPE" = rfc ]; then
    ssh ax "$dir"/pg_hba_conf-tool.py --update-for-ssl
fi

# Initialize site CA and install certificates into user and application
# directories.
a2pgca init-site && \
    a2pgca refresh


