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
##############################################################################
# Contains configuration information specific to the Subscription Manager tool
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/09/08        1709          mfegan         Initial Creation.
##############################################################################

##############################################################################
# service end-point definition dictionary:
# basic format is
#       'token':'endpoint'
##############################################################################
endpoint = {'subscribe':'/services/subscribe'}

##############################################################################
# defines the arguments that are needed with the add and read commands
##############################################################################
addread = ['type','trigger','runner']

##############################################################################
# defines the arguments that are allowed with the update command
##############################################################################
update = ['index','update','operation']

##############################################################################
# defines valid script runners
##############################################################################
runners = ['python','jscript','system','pil', 'ldad']

##############################################################################
# defines valid trigger types 
##############################################################################
triggers = ['timer','data','pil', 'ldad']

##############################################################################
# defines valid maintenance operations
##############################################################################
operations = ['read','add','delete','update']

##############################################################################
# Command line flag decoder dictionary:
# Basic format is
#       'flag':('command','help/usage text'
##############################################################################
flags = {'-r':('runner','Identifies which script runner to use',runners),
         '-o':('operation','Maintenance operation',operations),
         '-p':('trigger','Product Identifier/Trigger',['AFOS PIL','Data URI','Time Match']),
         '-s':('substitution','Key/value substitution pair to customize script'),
         '-t':('type','Identifies type of subscription trigger type', triggers),
         '-f':('file','Identifies the external containing the script'),
         '-c':('command','Identifies command line arguments to pass to external script'),
         '-i':('index','Identifies index of row to update/delete'),
         '-u':('update','Field name/value pair for update')}


