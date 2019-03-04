##
##

#
# Sample script to show capability of executing specific awips query through
# localization
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/18/09                      rjpeter        Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

import BaseTool

class ExecuteAwipsQuery(BaseTool.BaseTool):
    
     def process(self, pythonTextEditorToolCallback, arg):
         wmoid = 'wmoid'
         site = 'site'
         awipsid = 'awipsid'
         hdrtime = 'hdrtime'
         bbbid = 'bbbid'
         lasthours = 'lasthours'
         fullread = 'fullread'
         hash = dict()
         
         argPairs = arg.split(',')
         for pair in argPairs:
             fieldVal = pair.split('=', 2)
             hash[fieldVal[0]] = fieldVal[1]

         if wmoid in hash:
             wmoid = hash[wmoid]
         else:
             wmoid = ''

         if site in hash:
             site = hash[site]
         else:
             site = ''

         if awipsid in hash:
             awipsid = hash[awipsid]
         else:
             awipsid = ''

         if hdrtime in hash:
             hdrtime = hash[hdrtime]
         else:
             hdrtime = ''

         if bbbid in hash:
             bbbid = hash[bbbid]
         else:
             bbbid = ''

         if lasthours in hash:
             lasthours = hash[lasthours]
         else:
             lasthours = ''

         if fullread in hash:
             fullread = hash[fullread]
         else:
             fullread = ''

              
         products = pythonTextEditorToolCallback.executeAwipsQuery(wmoid, site, awipsid, hdrtime, bbbid, lasthours, fullread)
         rval = ''

         if len(products) > 0:
             for prod in products:
                 if len(prod.getProduct()) > 0:
                     rval += prod.getProduct() + "\n"
                 else:
                     rval += prod.getWmoid() + ' ' + prod.getSite() + ' ' + prod.getHdrtime() + ' ' + prod.getBbbid() + '\n'
         else:
             pythonTextEditorToolCallback.displayMessage('No product in the database matches your request.');
         
         return rval
         