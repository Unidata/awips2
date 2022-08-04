#!/bin/bash

# Omaha #6314
# Delete all md5 files in the localization tree, forcing EDEX to regenerate all md5 files.
# This will clean up any md5 files that do not match the corresponding localization file, which can
# lead to performance degradation if not removed.

# Only run this script on a single EDEX server (dv3 or dv4). Run only when all EDEX are down.

find /awips2/edex/data/utility -xdev -type f -regex '.*\.md5$' -delete
