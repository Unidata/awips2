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
from com.raytheon.edex.uengine.tasks.process import SystemLog
from com.raytheon.uf.common.message.response import ResponseMessageGeneric

#
# This is a demonstration script that logs a message to the EDEX system log.
# Usage:
#   import HelloWorld
#   runner = HelloWorld.HelloWorld()
#   runner.setMessage("""Hello from Omaha!""")
#   return runner.execute()
#
class HelloWorld():

	def __init__(self):
		self.message = ""

	def setMessage(self,message):
		self.message = message.replace("EQUALS","",1).lstrip()

	def execute(self):
		logger = SystemLog()
		logger.log("info",self.message)
		return ResponseMessageGeneric(self.message)


