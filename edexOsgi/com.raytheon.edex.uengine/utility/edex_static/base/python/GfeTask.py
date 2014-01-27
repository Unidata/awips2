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


from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric


#
# Gfe Task script 
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/18/08        #875          bphillip      Initial Creation.
#    
# 


class GfeTask():

	def __init__(self, wsId, task):
		self.wsId = wsId
		self.task = task
	
	def addArgument(self, xml):
		self.task.addXml(xml)

	def execute(self):
		self.task.setWorkstationID(self.wsId)
		
		result = self.task.execute()
		payload = result.getPayload()
		messages = result.getMessages()
		notifications = result.getNotifications()
		response = ArrayList()

		if payload is not None:
			for i in range(payload.size()):				
				response.add(ResponseMessageGeneric(payload.get(i)))
	
		for i in range(messages.size()):
			response.add(ResponseMessageGeneric(messages.get(i)))
			
		for i in range(notifications.size()):
			response.add(ResponseMessageGeneric(notifications.get(i)))		
		
		return response
	