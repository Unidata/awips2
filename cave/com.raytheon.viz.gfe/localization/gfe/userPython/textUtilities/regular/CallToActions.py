##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CallToActions.py
# 
# This module contains all of the call to action statements based on the
# VTEC phen/sig code. Sites can override this file to make modifications
# to the list of call to actions.
##

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

import DefaultCallToActions


class CallToActions(DefaultCallToActions.CallToActions):

    def init(self):
        pass # Override methods from defaultCallToActions here.

