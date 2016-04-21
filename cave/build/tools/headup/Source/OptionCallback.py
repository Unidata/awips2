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

#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2 Mar 2010      #3771         jelkins        Initial Creation.

def flagWithOption(option, opt, value, parser):
	""" Handles -f [OPTION] type of flags
	
	If the flag is specified the destination value will be "".  If the
	flag is specified with an additional option the destination value
	will be the given option.  Otherwise the flag will be None.
	"""
	
	if len(parser.rargs) == 0:
		return
	
	flagOption = parser.rargs[0]

	if ( (flagOption[:2] != "--" and len(flagOption) > 2 
		 or flagOption[:1] != "-" and len(flagOption) >1)
		 and len(parser.rargs) > 1 ):
		 del parser.rargs[0]
	else:
		flagOption = ""
		
	setattr(parser.values,option.dest,flagOption)
	
