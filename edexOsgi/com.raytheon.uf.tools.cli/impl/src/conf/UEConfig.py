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
# Contains configuration information specific to the uEngine tool
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/28/08        1585          MW Fegan       Initial Creation.
#    12/09/08        1709          MW Fegan       Removed subscription related items.
#    05/18/10        NCEP230    mgamazaychikov    Added -m flag to flags dictionary.
##############################################################################

##############################################################################
# service end-point definition dictionary:
# basic format is
#       'token':'endpoint'
##############################################################################
endpoint = {'python':'/services/pyproductjaxb',
            'jscript':'/services/plgproductsrvjs',
            'system':'/services/plgproductsrvsys'}

##############################################################################
# defines valid script runners
##############################################################################
runners = ['python','jscript','system','ldad']

##############################################################################
# Command line flag decoder dictionary:
# Basic format is
#       'flag':('command','help/usage text'
##############################################################################
flags = {'-r':('runner','Identifies which script runner to use',runners),
         '-m':('fullMessage','Returns the response as full XML message'),
         '-s':('substitution','Key value substitution pair to customize script')}

##############################################################################
# XML path to "responses" element
##############################################################################
response = "body/responses"
##############################################################################
# response type decoder dictionary
# Basic format is '{response type}':{'tag':'[True|False]','name':'{sub-element name}',incTag:[True|False],'error':[True|False}
##############################################################################
responses = {'responseMessageURI':{'tag':True,'name':'productURI','incTag':False,'error':False},
             'responseMessageGeneric':{'tag':True,'name':'contents','incTag':True,'error':False},
             'responseMessageError':{'tag':False,'name':'errorMsg','incTag':False,'error':True,'optional':'errorCause'},
             'responseMessageCatalog':{'tag':True,'name':'items','incTag':True,'error':False}}
