##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
#
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
#
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
#
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Mar 10, 2022  8808     randerso  Initial Creation.
# Mar 21, 2022  8808     randerso  Add code to replace '""' and "''" with ""
#
##
import collections
from configparser import ConfigParser, _UNSET


class MultiOrderedDict(collections.OrderedDict):
    '''
       OrderedDict that joins values for duplicate keys into a comma separated list vs overwriting them.
    '''

    def __setitem__(self, key, value):

        if isinstance(value, list) and key in self:
            self[key].extend(value)
            new_value = ",".join(self[key])
            super().__setitem__(key, [new_value])
        else:
            super().__setitem__(key, value)

    def keys(self):
        return super().keys()


class AvnConfigParser(ConfigParser):
    '''
    ConfigParser subclass that merges duplicate keys into a comma separated list
    to better work with Java code that uses org.apache.commons.configuration
    and defaults to better match Python 2.7
    '''

    def __init__(self):
        ConfigParser.__init__(self, dict_type=MultiOrderedDict, strict=False, allow_no_value=True, interpolation=None)

    def get(self, section, option, *args, raw=False, vars=None, fallback=_UNSET):
        '''
        Replace '""' or "''" with an empty string.
        '''

        value = super().get(section, option, *args, raw=raw, vars=vars, fallback=fallback)
        if value in ("''", '""'):
            value = ""

        return value;
