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
#       XmitServ.py
#       GFS1-NHD:A3661.0000-SCRIPT;33
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 33 (DELIVERED)
#         Created:  28-AUG-2008 15:29:44      OBERFIEL
#           Changed error msg to be more informative.
#       
#       Revision 32 (DELIVERED)
#         Created:  20-SEP-2007 08:04:38      OBERFIEL
#           Attempt to fix PCMS Header substitution feature
#       
#       Revision 31 (INITIALIZE)
#         Created:  20-SEP-2007 08:00:11      OBERFIEL
#           Added trailing newline character after check of empty
#           report.
#       
#       Revision 30 (DELIVERED)
#         Created:  30-AUG-2007 11:43:25      OBERFIEL
#           Fixed VFT product produced by transmission server; Fixed
#           cvt3.py not to touch cig/vsby thresholds
#           also added new package to preserve ordering and comments in
#           configuration files.
#       
#       Revision 29 (DELIVERED)
#         Created:  10-AUG-2007 14:47:23      OBERFIEL
#           Updated to include newline character for last line of VFT.
#           Update install script to apply just the patch.
#       
#       Revision 28 (DELIVERED)
#         Created:  06-JAN-2007 11:59:31      OBERFIEL
#           Update to allow TAFs with numbers in the ICAO identifier to
#           be disseminated.
#       
#       Revision 27 (DELIVERED)
#         Created:  21-OCT-2005 19:53:28      TROJAN
#           spr 7047
#       
#       Revision 26 (DELIVERED)
#         Created:  06-SEP-2005 20:02:03      TROJAN
#           spr 7018
#       
#       Revision 25 (DELIVERED)
#         Created:  06-SEP-2005 18:08:36      TROJAN
#           spr 7017
#       
#       Revision 24 (DELIVERED)
#         Created:  28-JUL-2005 18:10:49      TROJAN
#           spr 6951
#       
#       Revision 23 (APPROVED)
#         Created:  06-JUL-2005 18:16:43      TROJAN
#           spr 6548
#       
#       Revision 22 (DELIVERED)
#         Created:  07-MAY-2005 11:40:35      OBERFIEL
#           Added Item Header Block
#       
#       Revision 21 (DELIVERED)
#         Created:  11-MAR-2005 14:37:37      TROJAN
#           spr 6716
#       
#       Revision 20 (UNDER WORK)
#         Created:  11-MAR-2005 14:35:48      TROJAN
#           spr 6716
#       
#       Revision 19 (DELIVERED)
#         Created:  04-MAR-2005 15:06:19      TROJAN
#           spr 6698
#       
#       Revision 18 (DELIVERED)
#         Created:  14-FEB-2005 21:17:09      TROJAN
#           spr 6652
#       
#       Revision 17 (APPROVED)
#         Created:  19-JAN-2005 15:40:53      TROJAN
#           spr 6571
#       
#       Revision 16 (APPROVED)
#         Created:  30-SEP-2004 20:22:11      TROJAN
#           stdr 873
#       
#       Revision 15 (APPROVED)
#         Created:  19-AUG-2004 21:03:13      OBERFIEL
#           code change
#       
#       Revision 14 (APPROVED)
#         Created:  01-JUL-2004 15:00:05      OBERFIEL
#           Update
#       
#       Revision 13 (DELIVERED)
#         Created:  08-JAN-2004 21:40:38      PCMS
#           Updating for code cleanup
#       
#       Revision 12 (APPROVED)
#         Created:  05-NOV-2003 19:14:25      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 11 (DELIVERED)
#         Created:  28-FEB-2003 12:46:31      TROJAN
#           spr 4751 4756
#       
#       Revision 10 (DELIVERED)
#         Created:  18-NOV-2002 20:15:07      PCMS
#           Fixed problem send TAFs at the end of the transmission
#           window.
#       
#       Revision 9 (DELIVERED)
#         Created:  14-NOV-2002 14:15:44      PCMS
#           Fixed problem sending TAFs near end of transmission period
#       
#       Revision 8 (DELIVERED)
#         Created:  09-JUL-2002 21:07:06      PCMS
#           Fixed missing line break in error message and invalid path
#           when workstation uses automounter.
#       
#       Revision 7 (DELIVERED)
#         Created:  13-MAY-2002 22:30:25      PCMS
#           Fixed placement of dialog boxes.
#       
#       Revision 6 (DELIVERED)
#         Created:  14-NOV-2001 23:47:06      PCMS
#           Updating
#       
#       Revision 5 (DELIVERED)
#         Created:  14-NOV-2001 21:14:52      PCMS
#           Removed option to transmit forecasts in a collective.
#       
#       Revision 4 (DELIVERED)
#         Created:  30-OCT-2001 19:02:34      PCMS
#           Added last transmission status notification to the main
#           GUI.
#       
#       Revision 3 (DELIVERED)
#         Created:  10-OCT-2001 20:10:23      PCMS
#           Wrong path to handlOUP.pl
#       
#       Revision 2 (DELIVERED)
#         Created:  02-OCT-2001 17:48:39      PCMS
#           Updating for gui changes
#       
#       Revision 1 (DELIVERED)
#         Created:  20-AUG-2001 20:37:18      MOELLER
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7400
#       	Action Date:       11-OCT-2008 12:55:41
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: (Proposed OB9) TAF COR should be allowed on first line
#       
#
# this class expects files in the directory xmit/pending of the form:
# xxx-CCCCNNNXXX-TTTTnn-CCCC-yymmddHHMM-BBB-tttttttttt
# 01234567890123456789012345678901234567890-1039476254
# where xxx is a 3-digit forecaster number

import atexit, logging, os, re, select, time
import ConfigParser
import Pyro
import Avn, AvnThread

_Xmit_Prog = os.path.join(os.environ['FXA_HOME'], 'bin', 'handleOUP.pl')
# pattern for files to transmit
_Pat = r'^\d{3}-[A-Z]{7}[A-Z0-9]{2,3}-[A-Z]{4}\d{1,2}-[A-Z]{4}-\d{10}-[_ACR]{2}[_A-Z]-\d+$'
# pattern for TAF forecasts to be verified
_PatStat = r'^\d{3}-[A-Z]{4}TAF[A-Z0-9]{2,3}-[A-Z]{4}\d{1,2}-[A-Z]{4}-\d{10}-[_ACR]{2}[_A-Z]-\d+$'

# Exit codes from handleOUP.pl
_OUPNotStored = 0x07
_OUPNotSent = 0x08
_OUPNotArchived = 0x10

_Logger = logging.getLogger(__name__)

###############################################################################
class XmitServer:
    NumTries = 3    # maximum number of consecutive failures
    TimeStamp = os.path.join('xmit', 'tstamp')
    Config = os.path.join('etc', 'xmit.cfg')

    def __init__(self, host):
        self._host = host
        self.reg = re.compile(_Pat)
        self.regStat = re.compile(_PatStat)
        self.regSplit = re.compile(r'=+[\s\n]*|\n{2,}|\n$')
        self.lasttime = 0.0
        self.badcount = 0
	self.cfg = None

    def __get_config(self):
	if not os.path.isfile(self.Config):
            _Logger.error('File %s does not exist' % self.Config)
            return
	cp = ConfigParser.SafeConfigParser()
	cp.read(self.Config)
        try:
            cfg = {}
	    cfg.update(dict(cp.items('transmission')))
	    cfg.update(dict(cp.items('verification')))
            cfg['fcstid'] = int(cfg['fcstid'])
            cfg['period'] = int(cfg['period'])
            cfg['frequency'] = int(cfg['frequency'])
            cfg['old'] = 3600.0*int(cfg['old'])
            self._cfg = cfg
        except:
            _Logger.exception('Bad file %s' % self.Config)
        
    def __checkStatus(self, f):
        # This method checks for valid file format.
        # If file name does not match the pattern, or file age is more 
        # than 3 hours, the method returns 'bad'.
        # If the requested transmission time is less than the current
        # time, 'send' is returned, otherwise the method returns 'wait'
        if not self.reg.match(f):
            _Logger.error('File %s does not match pattern', f)
            return 'bad'
        mtime = os.path.getmtime(os.path.join('xmit', 'pending', f))
        now = time.time()
        if mtime < now - self._cfg['old']:
            return 'bad'
        try:
            xmittime = int(f.split('-')[-1])
        except ValueError:
            return 'bad'
        # add few seconds to assure file is ready
        if xmittime < now - 5.0:
            return 'send'
        return 'wait'

    def __unlink(self, path):
        try:
            os.unlink(path)
            _Logger.info('Removed %s', path)
        except OSError:
            _Logger.exception('Cannot remove %s', path)

    def __cleanup(self):
        # Removes status and forecast files older than 3 days
        # if same day, return
        now = time.time()
        if now//86400.0 - self.lasttime//86400.0 == 0.0:
            return
        self.lasttime = now
        cutoff = now - 3*86400
        path = os.path.join('xmit', time.strftime('%a'))
        try:
            if os.path.getmtime(path) < now - 5*86400.0:
                self.__unlink(path)
        except OSError:
            pass
        for dir in [os.path.join('xmit', 'pending'), \
            os.path.join('xmit', 'sent'), os.path.join('xmit', 'bad')]:
            for f in os.listdir(dir):
                path = os.path.join(dir, f)
                if os.path.getmtime(path) < cutoff:
                    self.__unlink(path)
    
    def __moveFile(self, f, dest):
        # This method moves file to 'dest' directory
        _Logger.info('Moving %s to %s', f, dest)
        try:
            os.rename(os.path.join('xmit', 'pending', f),
                os.path.join('xmit', dest, f))
        except OSError:
            _Logger.exception('Cannot rename %s', f)
    
    def __sendFile(self, f):
        # This method calls 'handleOUP.pl' with proper arguments
        # through 'system()' command. The return code is written 
        # to the log file and returned to the calling method.
        _Logger.info('sending %s to SBN', f)
        words = f.split('-')
        awips = words[1]
        tstamp = words[4][4:]   # skip yymm
        bbb = words[5][:3]  # no '!'
        filepath = os.path.join(os.environ['TOP_DIR'], 'xmit', 'pending', f)
        if bbb == '___':
            cmd = '%s -r DEF -d %s %s %s' % \
                (_Xmit_Prog, tstamp, awips, filepath)
        else:
            cmd = '%s -w %s -r DEF -d %s %s %s' % \
                (_Xmit_Prog, bbb, tstamp, awips, filepath)
        for n in range(self.NumTries):
            try:
                status = os.system(cmd)
                if os.WIFEXITED(status):
                    status = os.WEXITSTATUS(status)
                elif os.WIFSIGNALED(status):
                    status = os.WTERMSIG(status)
                if status&0x03:     # handleOUP error
                    msg = 'FAIL%03d %s' % (status, f)
                    rc = False
                    break
                elif status&_OUPNotSent: # retry
                    time.sleep(self._cfg['frequency'])
                else:           # sent
                    msg = 'SUCCESS ' + f
                    rc = True
                    break
            except OSError:
                msg = 'Cannot invoke ' + _Xmit_Prog
                rc = False
                break
        else:
            msg = 'FAIL%03d %s' % (status, f)
            rc = False
            
        self.__writeStatus(msg)
        self._publisher.publish(Avn.Bunch(src='XMIT-'+self._host, value=msg))
        if rc:
            msg = []
            if status&_OUPNotArchived:
                msg.append('Not archived')
            if status&_OUPNotStored:
                msg.append('Not stored')
                
            if msg:
                _Logger.error(' '.join([f] + msg))
        else:
            _Logger.error('%s failed for %s. Check %s on px2f for cause' %
                          (os.path.basename(_Xmit_Prog), f,
                           '/data/logs/fxa/%s/handleOUP.log' % time.strftime('%Y%m%d')))
        return rc

    def __writeStatus(self, msg):
        fname = os.path.join('xmit', time.strftime('%a'))
        try:
            file(fname, 'a').write(msg+'\n')
        except (OSError, IOError):
            _Logger.exception('Cannot write to %s', fname)

    def __get_timestamp(self):
        try:
            return Avn.string2time(file(self.TimeStamp).read())
        except:
            _Logger.error('Cannot access %s', self.TimeStamp)
            return 0.0

    def __update_timestamp(self):
        try:
            file(self.TimeStamp, 'w').write(Avn.time2string())
        except:
            _Logger.exception('Cannot update %s', self.TimeStamp)
            raise SystemExit

    def __get_files(self, ftime):
        cutoff = ftime - 21600.0
        def _filter(f):
            if not self.regStat.match(f):
                return False
            tok = f.split('-')
            header_time = Avn.string2time(tok[4])
            return header_time > cutoff
        return filter(_filter, os.listdir(os.path.join('xmit', 'sent')))

    def __make_stats(self):
        now = time.time()
        ftime = self.__get_timestamp()
        if now - ftime < 3600.0*self._cfg['period']:
            return
        def _process_forecast(f):
            ftok = f.split('-')
            yymm = ftok[4][:4]
            text = file(os.path.join('xmit', 'sent', f)).read()
            if not text.startswith('TAF'):
                return None
            text = text[text.find('\n')+1:]
            tafs = filter(None, [x.strip() for x in self.regSplit.split(text)])
            def _makeLine(taf):
                ttok = taf.split()
                return ' '.join(ftok[2:6] + \
                    [ttok[0], yymm+ttok[1], ttok[2], ftok[0]])
            return '\n'.join(map(_makeLine, tafs))
	try:
            report = '\n'.join(map(_process_forecast, self.__get_files(ftime)))
            if not report:
                raise Avn.AvnError, 'No data to process'
            report += '\n'
            tt, cc = self._cfg['wmo'].split()
            path = os.path.join('xmit', 'pending', '%03d-%s-%s-%s-%s-%s-%-10d' \
                % (self._cfg['fcstid'], self._cfg['awips'], tt, cc, \
                Avn.time2string(now), '___', now))
            file(path, 'w').write(report)
            self.__update_timestamp()
        except Avn.AvnError, e:
            _Logger.warning(str(e))
            self.__update_timestamp()
        except:
            _Logger.exception('Failed to create bulletin')

    def __exitfun(self):
        _Logger.exception('Terminating')
    
    def run(self):
        # don't want popup messages from handleOUP
        try:
            del os.environ['DISPLAY']
        except KeyError:
            pass
        self.__get_config()
        try:
            self._publisher = AvnThread.Publisher()
        except Pyro.errors.NamingError:
            _Logger.critical('Cannot connect to Event Server')
            raise SystemExit
        atexit.register(self.__exitfun)
        while 1:
            self.__cleanup()
            self.__make_stats()
            for f in os.listdir(os.path.join('xmit', 'pending')):
                status = self.__checkStatus(f)
                _Logger.debug('Status for %s: %s', f, status)
                if status == 'send':
                    if self.__sendFile(f):
                        self.__moveFile(f, 'sent')
                    else:
                        self.__moveFile(f, 'bad')
                elif status == 'wait':
                    continue
                else:
                    self.__moveFile(f, 'bad')
            msg = Avn.Bunch(src='XMIT-'+self._host, value='ALIVE')
            self._publisher.publish(msg)
            try:
                select.select([], [], [], self._cfg['frequency'])
            except select.error, e:
                errnumber, strerr = er
                if errnumber != errno.EINTR:
                    raise
