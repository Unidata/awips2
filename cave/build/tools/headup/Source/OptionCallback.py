##
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
	
