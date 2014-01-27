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
#       MonitorP.py
#       GFS1-NHD:A7810.0000-SCRIPT;1.12
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.12 (DELIVERED)
#         Created:  09-OCT-2009 14:43:25      OBERFIEL
#           Only cat items are sorted by severity
#       
#       Revision 1.11 (DELIVERED)
#         Created:  17-APR-2009 12:11:39      OBERFIEL
#           Added code to handle AvnUnknwnPcp events.
#       
#       Revision 1.10 (DELIVERED)
#         Created:  04-MAR-2008 11:08:49      OBERFIEL
#           Removed comma that causes side-effect of creating a tuple
#           on output.
#       
#       Revision 1.9 (DELIVERED)
#         Created:  29-NOV-2007 10:13:40      OBERFIEL
#           Removed CR characters.
#       
#       Revision 1.8 (REVIEW)
#         Created:  19-NOV-2007 11:51:12      GILMOREDM
#           Backed out previous changes to WxMetar class
#       
#       Revision 1.7 (DELIVERED)
#         Created:  30-JUN-2006 13:53:06      TROJAN
#           spr 7191: Fixed messages for rules that do not use messages
#           from configuration file. 
#           Removed duplicate messages
#       
#       Revision 1.6 (DELIVERED)
#         Created:  30-JUN-2006 13:35:38      TROJAN
#           spr 7192: Fixed messages for rules that do not use messages
#           from 
#           configuration file. Removed duplicate messages
#       
#       Revision 1.5 (DELIVERED)
#         Created:  21-APR-2006 11:29:24      TROJAN
#           spr 7124: added exception catching code ro compare()
#       
#       Revision 1.4 (DELIVERED)
#         Created:  20-APR-2006 15:50:46      TROJAN
#           created method compare() to catch exceptions thrown in
#           derived classes
#       
#       Revision 1.3 (DELIVERED)
#         Created:  23-JAN-2006 08:23:16      TROJAN
#           stdr 956
#       
#       Revision 1.2 (DELIVERED)
#         Created:  07-MAY-2005 11:36:16      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.1 (DELIVERED)
#         Created:  01-JUL-2004 14:42:42      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7430
#       	Action Date:       21-OCT-2009 08:03:43
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Incorrect file permission on ISH files
#       
#
import logging, os, ConfigParser, sets
import Avn

_Logger = logging.getLogger(Avn.CATEGORY)

def _guess(arg):
    '''Tries to guess and convert arg, passed as a string'''
    if type(arg) != type(''):
        return
    try:
        return int(arg)
    except ValueError:
        try:
            return float(arg)
        except ValueError:
            return arg

##############################################################################
# common code for monitoring
##############################################################################
def addRules(items, rules, delta):
    result = {}
    lasttime = max(delta)
    up=Avn.Bunch(msg='Unknown Precipitation Reported',severity=2)

    for t, r in zip(delta, rules):
        if r.type not in result and t != -1: # add item
            result[r.type] = []
            
        if t == -1:
            if r.type in result: # missing data
                del result[r.type]
            continue
        elif t == 0:            # fine
            continue
        
        if r.unique:            # check rules already on stack
            ok = True
            for rs in result[r.type]:
                if r.__class__ == rs.rule.__class__:
                    if r.severity > rs.rule.severity:
                        result[r.type].remove(rs)
                    else:
                        ok = False
                        break
            if ok:
                if t > 0:
                    result[r.type].append(Avn.Bunch(rule=r, time=t))
                else:
                    result[r.type].append(Avn.Bunch(rule=up, time=lasttime))
        else:
            if t > 0:
                result[r.type].append(Avn.Bunch(rule=r, time=t))
            else:
                result[r.type].append(Avn.Bunch(rule=up, time=lasttime))
                                      
    return result

def addMessages(items, status):
    result = {}
    for i in items:
        if i in status:
            if len(status[i]) == 0:
                result[i] = Avn.Bunch(severity=0, msg='OK', time=None)
            else:
                #
                # CAC messages to be sorted by severity
                if i == 'cat':
                    adict = {}
                    #
                    # Typical DSU method follows
                    cacmsgs=[(r.rule.severity,r.rule.msg) for r in status[i]]
                    cacmsgs.sort()
                    #
                    # Remove duplicate messages of lower severity. Unlikely
                    # but can't definitively rule it out
                    #
                    for severity,msg in cacmsgs:
                        adict[msg] = severity
                    #
                    uniqcacmsgs=[(s,m) for m,s in adict.items()]
                    uniqcacmsgs.sort()
                    uniqcacmsgs.reverse()
                    msg = '\n'.join(['%d-%s' % (s,m) for s,m in uniqcacmsgs])
                    _Logger.debug(msg)
                    msg = '\n'.join([m for s,m in uniqcacmsgs])
                else:
                    msg = '\n'.join(sets.Set([r.rule.msg for r in status[i]]))
                #
                # Get maximum severity for this item.
                s = max([r.rule.severity for r in status[i]])
                # the earliest time the error occurs
                result[i] = Avn.Bunch(severity=s,msg=msg,time=status[i][0].time)
        else:
            result[i] = Avn.Bunch(severity=1, msg='Missing data', time=None)
    return result

def applyRules(rules, t, tafdata, other):
    if tafdata is None or other is None:
        return [-1]*len(rules)
    result = [0]*len(rules)
    for n, rule in enumerate(rules):
        try:
            if rule.method(tafdata, other):
                result[n] = t
            else:
                result[n] = 0
        except Avn.AvnMissing:
            result[n] = -1
        except Avn.AvnUnknwnPcp:
            result[n] = -2
    return result

def getActiveRules(id_, namespace, source):
    rules = []
    # file containing rule definitions
    fname = Avn.getTafPath(id_, source + '.cfg')
    cp = ConfigParser.SafeConfigParser()
    cp.read(fname)
    try:
        activeRules = cp.get('rules', 'active')
    except Exception :
        raise Avn.AvnError('No active rules in "%s"' % fname)

    active = ['rule_'+x.strip() for x in cp.get('rules', 'active').split(',')]
    if activeRules:
        while 'rule_' in active:
            active.remove('rule_')
    if not activeRules or len(activeRules) == 0 :
        raise Avn.AvnError('No active rules in "%s"' % fname)

    for s in active:
        try:
            name = cp.get(s, 'method')
            rule = namespace[name]()
        except (ConfigParser.NoOptionError, ConfigParser.NoSectionError), ex:
            _Logger.error(' %s - in file %s' % (str(ex), fname))
            continue
        except KeyError:
            _Logger.error('Invalid method: %s, for rule: %s - in file %s' % (name, s, fname))
            continue
        rule.initialize(**dict(cp.items(s)))
        rules.append(rule)
    return rules

###############################################################################
class Rule(object):
    def __init__(self):
        self.msg = ''
        self.msgfromfile = False

    def initialize(self, **args):
        for key in ['type', 'severity', 'msg', 'unique']:
            if key in args:
                if key == 'unique':
                    setattr(self, key, bool(args[key]))
                elif key == 'severity':
                    setattr(self, key, int(args[key]))
                else:
                    setattr(self, key, args[key])
                del args[key]
        if args:
            for k, v in args.items():
                if ',' in v:
                    args[k] = filter(None, [x.strip() for x in v.split(',')])
                else:
                    args[k] = _guess(v)
            self.args = args
        else:
            self.args = {}
        if self.msg:
            self.msgfromfile = True

    def setmsg(self, msg, *args):
        # don't overwrite messages from configuration file
        if self.msgfromfile:
            return
        if args:
            self.msg = msg % args
        else:
            self.msg = msg

    def method(self, taf, other):
        pass

###############################################################################
class Monitor(object):
    Source = ''
    Namespace = None
    def __init__(self, info, args):
        self.info = info
        self.args = args
        self.rules = getActiveRules(info['ident'], self.Namespace, self.Source)
        for r in self.rules:
            r.sitedata = self.info

    def setNIL(self):
        result = Avn.Bunch(severity=1, msg='NIL TAF')
        return dict.fromkeys(self.args['items'], result)

    def setMissing(self, msg):
        result = Avn.Bunch(severity=1, msg=msg)
        return dict.fromkeys(self.args['items'], result)

    def compare(self, taf):
        if not self.rules:
            return {}
        try:
            return self.__compare(taf)
        except Exception, e:
            _Logger.exception(str(e))
            result = Avn.Bunch(severity=1, msg='Program bug - please report')
            return {'status': dict.fromkeys(self.args['items'], result)}

    def __compare(self, taf):
        # implemented in the derived class
        return {}
