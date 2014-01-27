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
#    Name:
#       Startup.py
#       GFS1-NHD:A7820.0000-SCRIPT;1.2
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.2 (DELIVERED)
#         Created:  07-MAY-2005 11:38:19      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.1 (DELIVERED)
#         Created:  01-JUL-2004 14:44:39      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6832
#       	Action Date:       07-JUN-2005 13:13:53
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Add PVCS doc blocks
#       
#
# Startup.py
# Startup utilities for AvnFPS 
# This module should be imported only from main programs
# Author: George Trojan, SAIC/MDL, March 2004
# last update: 05/20/04

import atexit, logging, logging.config, os, signal, sys, time

class WeeklyFileHandler(logging.FileHandler):
	Mode = 'a'
	def __init__(self, dir, basename):
		self.basename = '%s/%s' % (dir, basename)
		self.__checkFileTime()
		logging.FileHandler.__init__(self, self.baseFilename, self.Mode)

	def __checkFileTime(self):
		self.baseFilename = '%s_%s' % (self.basename, \
			time.strftime('%a'))
		try:
			if time.time() - os.path.getmtime(self.baseFilename) \
				> 100000.0: # more than 1 day
				os.unlink(self.baseFilename)
		except OSError:
			pass

	def doRollover(self):
		self.stream.close()
		self.__checkFileTime()
		self.stream = file(self.baseFilename, self.Mode)
		self.stream.write('Log rollover at %s\n' % time.strftime('%X'))

	def emit(self, record):
		if time.strftime('%a') != self.baseFilename[-3:]:
			self.doRollover()
		logging.FileHandler.emit(self, record)

###############################################################################
def _exitfun():
	Logger.info('Terminating, user id = %d', os.getuid())
	logging.shutdown()

def _handler(signum, frame):
	for s in [signal.SIGINT, signal.SIGTERM]:
		signal.signal(s, _handler)
	Logger.info('Caught signal %d', signum)
	raise SystemExit

##############################################################################
# execute this on startup
_Fmt = '%(asctime)s %(levelname)-5s %(process)5d %(module)s: %(message)s'

# inserts WeeklyFileHandler into logging namespace
setattr(logging, 'WeeklyFileHandler', WeeklyFileHandler)

try:
	cfgfile = os.environ['LOGCFG']
	file(cfgfile).close()	# will raise exception if file does not exist
	logging.config.fileConfig(cfgfile)
	Logger = logging.getLogger()
except Exception, e:	
	# use built-in values
	hdlr = WeeklyFileHandler('%s/logs' % os.environ['TOP_DIR'], \
		os.path.basename(sys.argv[0]).split('.')[0])
	hdlr.setFormatter(logging.Formatter(_Fmt))
	Logger = logging.getLogger()
	Logger.addHandler(hdlr)
	Logger.setLevel(logging._levelNames[os.environ.get('LOGLEVEL', \
		'DEBUG')])

Logger.info('Starting, user id = %d', os.getuid())
for s in [signal.SIGINT, signal.SIGTERM]:
	signal.signal(s, _handler)
atexit.register(_exitfun)
