#!/usr/bin/env python2

# #6065: Add entries for the new TAF IWXXM products to the site-level
# afos2awips.txt. Run on dx1 as root.
#
# Author: tgurney
# Mar 22, 2017

from __future__ import print_function

import errno
import os
import re
import subprocess
import sys

A2A_PATTERN = re.compile(r'(...)(...)(..?.?) (..)(....) (....)')
NEW_NNN = 'TML'

def get_new_entries(path):
    '''Return set of new entries to add to specified file'''
    new_entries = set()
    already_existing = set()
    with open(path) as f:
        for line in f:
            line = line.strip()
            m = re.match(A2A_PATTERN, line)
            if m:
                node, nnn, xxx, tt, aaii, cccc = m.groups()
                new_entry = '{}{}{} LT{} {}'.format(node, NEW_NNN, xxx, aaii, cccc)
                if line == new_entry:
                    already_existing.add(line)
                    continue
                if nnn == 'TAF' or tt == 'FT':
                    if tt == 'FT' and nnn != 'TAF':
                        print('WARN: {} has FT but not TAF. Ignoring this one'.format(line))
                        continue
                    elif tt != 'FT' and nnn == 'TAF':
                        print('WARN: {} has TAF but not FT. Ignoring this one'.format(line))
                        continue
                    new_entries.add(new_entry)
    # Do not add new entries that have already been added
    intersection = already_existing & new_entries
    if intersection:
        print('INFO: Skipping {} already existing entries'.format(len(intersection)))
    new_entries -= already_existing
    return new_entries

def main():
    no_files = True
    print('INFO: Starting afos2awips update')

    localization_root = '/awips2/edex/data/utility/common_static/'
    all_sites = set(
            os.listdir(localization_root + '/site') +
            os.listdir(localization_root + '/configured')
            )
    for site_id in sorted(all_sites):
        a2a_site_part = 'site/{}/afos2awips/afos2awips'.format(site_id)
        a2a_cfgd_part = 'configured/{}/afos2awips/afos2awips'.format(site_id)
        a2a_site_path = a2a_site_part + '.txt'
        a2a_site_blacklist_path = a2a_site_part + '.blacklist.txt'
        a2a_cfgd_path = a2a_cfgd_part + '.txt'
        a2a_cfgd_blacklist_path = a2a_cfgd_part + '.blacklist.txt'

        for path_part in (a2a_site_path, a2a_site_blacklist_path, a2a_cfgd_path, a2a_cfgd_blacklist_path):
            path = localization_root + path_part
            if not os.path.exists(path):
                continue
            no_files = False
            print('INFO: Checking ' + path_part)
            new_entries = set()
            try:
                new_entries = get_new_entries(path)
            except EnvironmentError as e:
                if e.errno == errno.ENOENT:
                    print("WARN: {}: {}".format(path, e.strerror))
                else:
                    print("ERROR: {}: {}".format(path, e.strerror))
            if new_entries:
                string_out = '\n'.join(item for item in sorted(new_entries))
                print('INFO: Adding the following new entries to {}:'.format(path_part))
                print(string_out)
                with open(path, 'a') as f:
                    f.write('\n' + string_out + '\n')
                print('INFO: Updated ' + path_part)
            else:
                print('INFO: No changes are necessary for {}'.format(path_part))

    if no_files:
        print('WARN: Found no SITE or CONFIGURED afos2awips files. Is localization mounted at /awips2/edex/data/utility?')

    print('INFO: Done with afos2awips update')

if __name__ == '__main__':
    main()
