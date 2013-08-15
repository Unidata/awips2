#!/bin/sh
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
#       cvt3.py
#       GFS1-NHD:A8055.0000-SCRIPT;53
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 53 (DELIVERED)
#         Created:  22-OCT-2009 00:55:22      OBERFIEL
#           Implementing CAC -- what a pain!
#       
#       Revision 52 (DELIVERED)
#         Created:  15-OCT-2009 01:12:27      OBERFIEL
#           Adjusted tempolevel downward.
#       
#       Revision 51 (DELIVERED)
#         Created:  28-SEP-2009 10:04:54      OBERFIEL
#           ASB wants messages from AirportOpsThresh suppressed.
#       
#       Revision 50 (DELIVERED)
#         Created:  28-SEP-2009 08:12:19      OBERFIEL
#           TEMPO grace period set to 30 minutes. tempolevel set to 3.
#           CAC rules to be configured for use.
#       
#       Revision 49 (DELIVERED)
#         Created:  28-AUG-2009 10:13:48      OBERFIEL
#           Fixed typo when configuring xmit.cfg for operational use.
#       
#       Revision 48 (DELIVERED)
#         Created:  31-JUL-2009 15:39:04      OBERFIEL
#           Added new forecaster resource, checkPendingQueue.
#           By default, forecasters will be notified when attempting to
#           edit TAF(s) that come
#           from the transmission server 'pending' queue.  This is the
#           cause of many unnecessary
#           trouble tickets.
#       
#       Revision 47 (DELIVERED)
#         Created:  17-JUL-2009 13:46:27      OBERFIEL
#           Added new resource for TAF Editor/Viewer.
#       
#       Revision 46 (DELIVERED)
#         Created:  23-JUN-2009 08:58:30      OBERFIEL
#           Corrected directory path to NAM BUFR profiles (affects the
#           NAM DMO tab in Taf Viewer/Editor)
#       
#       Revision 45 (REVIEW)
#         Created:  29-APR-2009 10:18:22      OBERFIEL
#           "Added collective option to the TAF product configuration
#           file"
#       
#       Revision 44 (REVIEW)
#         Created:  21-APR-2009 10:32:27      OBERFIEL
#           Numerous changes to configuration files for OB9.2
#       
#       Revision 43 (DELIVERED)
#         Created:  19-NOV-2008 09:39:06      OBERFIEL
#           Customized for OB9
#       
#       Revision 42 (DELIVERED)
#         Created:  12-SEP-2008 14:03:50      OBERFIEL
#           Added code to change the WxPlot GUI to be consistent with
#           TAF Editor tabs.
#       
#       Revision 41 (DELIVERED)
#         Created:  22-AUG-2008 22:26:35      OBERFIEL
#           Removed raise statement. Added a bunch of little fixes to
#           tie-up loose threads.
#       
#       Revision 40 (REVIEW)
#         Created:  18-AUG-2008 14:47:05      GILMOREDM
#           removed test code
#       
#       Revision 39 (REVIEW)
#         Created:  18-AUG-2008 14:44:21      GILMOREDM
#           added code to create QC section if needed
#       
#       Revision 38 (DELIVERED)
#         Created:  01-AUG-2008 15:44:44      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 37 (DELIVERED)
#         Created:  23-JUL-2008 08:57:58      OBERFIEL
#           Cleaned up dangling file descriptors; imported tables
#           module.
#       
#       Revision 36 (DELIVERED)
#         Created:  28-JUN-2008 14:09:37      OBERFIEL
#           Fixed argument list to do_thresholds() routine
#       
#       Revision 35 (REVIEW)
#         Created:  27-JUN-2008 13:29:27      OBERFIEL
#           Added function to update threshold files and provide easy
#           way to implement 30-h TAFs.
#       
#       Revision 34 (DELIVERED)
#         Created:  06-JUN-2008 14:02:44      OBERFIEL
#           Updates to scripts
#       
#       Revision 33 (REVIEW)
#         Created:  05-JUN-2008 15:45:14      GILMOREDM
#           Actually added the changes this time.
#       
#       Revision 32 (REVIEW)
#         Created:  05-JUN-2008 15:03:30      GILMOREDM
#           updated Threshold extraction
#       
#       Revision 31 (REVIEW)
#         Created:  02-JUN-2008 15:27:50      GILMOREDM
#           updated to include code for versioning the threshold files
#       
#       Revision 30 (INITIALIZE)
#         Created:  02-JUN-2008 10:19:55      OBERFIEL
#           Initial changes for OB9
#       
#       Revision 29 (DELIVERED)
#         Created:  17-MAR-2008 14:46:56      OBERFIEL
#           Explictly disable TWEB Editor if not already done.
#       
#       Revision 28 (INITIALIZE)
#         Created:  17-MAR-2008 13:39:09      OBERFIEL
#           Fixed typo with VsbyCatDelta (not VisCatDelta).  Also
#           minimum TEMPO grace period is 30 minutes.
#       
#       Revision 27 (DELIVERED)
#         Created:  06-MAR-2008 11:27:25      OBERFIEL
#           Fixed double-spacing in forecaster's resource file.
#       
#       Revision 26 (DELIVERED)
#         Created:  19-FEB-2008 13:26:08      OBERFIEL
#           Updated Globals.Colors list to reflect user's changes.
#       
#       Revision 25 (REVIEW)
#         Created:  06-FEB-2008 09:02:58      GILMOREDM
#           Added code to update gui.cfg file with the tempograceperiod
#           parameter
#       
#       Revision 24 (DELIVERED)
#         Created:  16-NOV-2007 13:46:53      OBERFIEL
#           Added exception handling for extraction of threshold files.
#       
#       Revision 23 (INITIALIZE)
#         Created:  16-NOV-2007 12:52:25      OBERFIEL
#           Repaired damage and removed carriage returns
#       
#       Revision 22 (INITIALIZE)
#         Created:  16-NOV-2007 10:36:43      GILMOREDM
#           changed tarball_dir to 'etc'
#       
#       Revision 21 (INITIALIZE)
#         Created:  16-NOV-2007 10:24:44      GILMOREDM
#           added exception handling to the probability threshold
#           extraction process
#       
#       Revision 20 (INITIALIZE)
#         Created:  16-NOV-2007 10:16:25      GILMOREDM
#           added code to extract probability threshold files from the
#           complete tarball
#       
#       Revision 19 (DELIVERED)
#         Created:  02-NOV-2007 09:16:33      OBERFIEL
#           Added new attributes to TAF info.cfg, server.cfg and
#           gui.cfg files.
#       
#       Revision 18 (DELIVERED)
#         Created:  29-OCT-2007 17:00:45      OBERFIEL
#           Code to properly update the forecaster's X resource file
#           and to add the acars attribute in the TAFs' info.cfg file
#       
#       Revision 17 (DELIVERED)
#         Created:  30-AUG-2007 11:43:26      OBERFIEL
#           Fixed VFT product produced by transmission server; Fixed
#           cvt3.py not to touch cig/vsby thresholds
#           also added new package to preserve ordering and comments in
#           configuration files.
#       
#       Revision 16 (DELIVERED)
#         Created:  28-AUG-2007 08:59:56      OBERFIEL
#           Removed alteration to baseline ceiling and visibility
#           thresholds
#       
#       Revision 15 (DELIVERED)
#         Created:  20-JUN-2007 10:46:08      OBERFIEL
#           Fixed logic error regarding triggerTemplate files.
#           Protected Alaska WFOs from changes to ceiling and
#           visibility thresholds.
#       
#       Revision 14 (DELIVERED)
#         Created:  04-MAY-2007 15:52:14      OBERFIEL
#           Changes to reflect OB8.2/AvnFPS3.5 values and clean up in
#           installation staging scripts. cvt3.py
#           updated to obsolete fields and add new one, tafduration.
#           AvnParser and DataRequestServ code cleaned up 
#           to remove references to avnmos, xtfs attributes and removed
#           obsolete modules.
#       
#       Revision 13 (INITIALIZE)
#         Created:  18-APR-2007 12:36:26      SOLSON
#           Removed CR characters from previous rev of this item.
#       
#       Revision 12 (DELIVERED)
#         Created:  06-DEC-2006 14:03:28      OBERFIEL
#           Update forecaster and their X resource files
#       
#       Revision 11 (DELIVERED)
#         Created:  07-JUL-2006 10:25:56      OBERFIEL
#           Updates to script and cvt3
#       
#       Revision 10 (DELIVERED)
#         Created:  04-APR-2006 08:45:37      OBERFIEL
#           Fixed error for METAR PIL
#       
#       Revision 9 (APPROVED)
#         Created:  29-MAR-2006 11:11:54      OBERFIEL
#           Updates and minor fixes needed since deployment to ATAN
#           sites
#       
#       Revision 8 (DELIVERED)
#         Created:  16-FEB-2006 09:19:18      TROJAN
#           A different script with same name - spr7089
#       
#       Revision 7 (DELIVERED)
#         Created:  26-JUL-2005 15:52:03      TROJAN
#           fix to previous spr
#       
#       Revision 6 (APPROVED)
#         Created:  10-JUL-2005 16:51:41      TROJAN
#           spr 6910
#       
#       Revision 5 (DELIVERED)
#         Created:  07-MAY-2005 11:42:29      OBERFIEL
#           Added Item Header Block
#       
#       Revision 4 (DELIVERED)
#         Created:  18-APR-2005 18:12:19      TROJAN
#           stdr 917
#       
#       Revision 3 (DELIVERED)
#         Created:  09-MAR-2005 18:30:14      TROJAN
#           ngmmos id made 3 letter long
#       
#       Revision 2 (DELIVERED)
#         Created:  08-NOV-2004 19:01:08      OBERFIEL
#           Changes to support LLWS
#       
#       Revision 1 (APPROVED)
#         Created:  30-SEP-2004 18:03:26      TROJAN
#           tool to convert 2.0 configuration to a new format
#
#    Purpose:
#       Python script to convert configuration files from one build to the newly
#       installed version
#
#    Run Instruction:
#       /awips/adapt/avnfps/bin/avnstart.sh cvt3
#
import os, re, sets, sys
from configobj import ConfigObj

TopDir = os.environ.get('TOP_DIR','/awips/adapt/avnfps')

###############################################################################
def cvtInfoCfg(id_):
    """Remove references to NGM and eta models"""
    
    fname = os.path.join('etc', 'tafs', id_, 'info.cfg')
    if not os.path.isfile(fname):
        # print 'File %s does not exist' % fname
        return

    print 'Updating info.cfg for', id_
    co = ConfigObj(fname)
    changed=False
    try:
        try:
            del co['sites']['ngmmos']
            changed=True
        except KeyError:
            pass
        
        try:
            co['sites']['nammos']=co['sites']['etamos']
            del co['sites']['etamos']
            changed=True
        except KeyError:
            pass
        
        try:
            co['sites']['nam']=co['sites']['eta']
            del co['sites']['eta']
            changed=True
        except KeyError:
            pass

        if changed:
            try:
                fp = file(fname, 'w')
            except IOError:
                raise
            
            co.write(fp)
            fp.close()

    except Exception, e:
	print 'cvtInfoCfg failed on %s\n%s' % (id_, e)
        pass

def cvtFcstResourceFile(f,okx,fcstr):
    """Add new resources"""
    resDict={}
    keylist=[]
    fname = os.path.join('etc', 'app-resources', f)
    _fh = open(fname,'r')
    #
    # Peserve the ordering in file
    for lne in _fh:
        try:
            key,value=lne.split(':',1)
            resDict[key]=value
            keylist.append(key)
        except ValueError:
            pass
        
    _fh.close()
    
    if resDict.has_key('*showRoutine') and resDict.has_key('*alwaySaveToDB')\
           and resDict.has_key('*checkPendingQueue'):
        print 'Done with %s' % fname
        return
    #
    # New resources to be added.
    if not resDict.has_key('*alwaySaveToDB'):
        resDict['*alwaySaveToDB']=okx
        try:
            keylist.insert(keylist.index('*autosave'),'*alwaySaveToDB')
        except ValueError:
            keylist.append('*alwaySaveToDB')            

    if not resDict.has_key('*showRoutine'):
        resDict['*showRoutine']='1'
        try:
            keylist.insert(keylist.index('*showProbs'),'*showRoutine')
        except ValueError:
            keylist.append('*showRoutine')            

    if not resDict.has_key('*checkPendingQueue'):
        resDict['*checkPendingQueue']='1'
        try:
            keylist.insert(keylist.index('*confirmSend'),'*checkPendingQueue')
        except ValueError:
            keylist.append('*checkPendingQueue')            
    #
    # Remove the collective resource which isn't really a forecaster option
    # but a TAF product attribute.  It can be overridden, of course.
    #
    try:
        keylist.remove('*collective')
    except ValueError:
        pass
    #
    # Destructive write
    _fh = open(fname,'w')
    _fh.write('!! AvnFPS X Resource File for %s\n' % fcstr )
    
    for key in keylist:
        _fh.write('%s:\t%s\n' % (key,resDict[key].strip()))
        
    _fh.close()
    print 'Done with %s' % fname

def updateServerCfg(fname):
    """Remove obsolete guidance"""
    if not os.path.isfile(fname):
        print 'File %s does not exist' % fname
        return
    try:
        changed = False
        co = ConfigObj(fname)
        #
        # Remove obsolete guidance
        try:
            del co['dis_guid']['ngmmos']
            changed = True
        except KeyError:
            pass
        
        try:
            del co['dis_guid']['avnmos']
            changed = True
        except KeyError:
            pass
        
        try:
            co['dis_guid']['nammos']=co['dis_guid']['etamos']
            del co['dis_guid']['etamos']
            changed = True
        except KeyError:
            pass

        try:
            del co['dis_guid']['eta']
            co['dis_guid']['nam'] = '/data/fxa/point/model/ETA/netcdf'
            changed = True
        except KeyError:
            pass

        try:
            co['dis_guid']['fcstlen']=42
            changed = True
        except KeyError:
            pass

        try:
            co['dis_rltg']['source'] = '/data/fxa/point/gfslamp/netcdf'
            co['dis_rltg']['module'] = 'LAMPLtgThread'
            changed = True
        except KeyError:
            pass

        if changed:
            try:
                fp = file(fname, 'w')
                co.write(fp)
            finally:
                fp.close()
                
        print 'Done with', fname
    except Exception, e:
        print 'updateServerCfg failed\n%s' %  e

def updateGUICfg(fname):

    if not os.path.isfile(fname):
        print 'File %s does not exist' % fname
        return

    try:
        co = ConfigObj(fname)
        #
        try:
            co['viewers']['tags'].remove('ngmmos')
            del co['viewer_ngmmos']
        except ValueError:
            pass

        try:
            co['viewer_nammos']['model']='nammos'
            co['viewer_nammos']['label']='NAM MOS'
        except KeyError:
            pass

        try:
            co['viewer_gfsmos']['label']='GFS MOS'
        except KeyError:
            pass

        try:
            co['viewer_gfslamp']['label']='GFS LAMP'
        except KeyError:
            pass

        try:
            co['viewer_nam']=co['viewer_etabuf']
            del co['viewer_etabuf']
            co['viewer_nam']['label']='NAM DMO'
            co['viewer_nam']['model']='nam'
            pos = co['viewers']['tags'].index('etabuf')
            co['viewers']['tags'].remove('etabuf')
            co['viewers']['tags'].insert(pos,'nam')            
        except (ValueError,KeyError):
            pass

        co['monitor_metar']['tempograceperiod'] = 1800.0
        co['monitor_metar']['tempolevel'] = 2
        
        try:
            fp = file(fname, 'w')
        except IOError:
            raise
        
        co.write(fp)
        fp.close()
            
        print 'Done with', fname                    
    except Exception, e:
        print 'updateGUICfg failed: %s' %  e
        pass
    
def updateWxPlotCfg(fname):

    if not os.path.isfile(fname):
        print 'File %s does not exist' % fname
        return

    try:
        co = ConfigObj(fname)
        try:
            pos = co['viewers']['tags'].index('etamos')
            co['viewers']['tags'].remove('etamos')
            co['viewers']['tags'].insert(pos,'nammos')
        except (ValueError,KeyError):
            pass
        
        try:
            pos = co['viewers']['tags'].index('eta')
            co['viewers']['tags'].remove('eta')
            co['viewers']['tags'].insert(pos,'nam')
        except ValueError:
            pass

        for key,value in [('viewers','ngmmos'),('selected','ngmmos'),('selected','eta')]:
            try:
                co[key]['tags'].remove(value)
            except ValueError:
                pass

        for value in ['viewer_ngmmos','viewer_etamos','viewer_eta']:
            try:
                del co[value]
            except KeyError:
                pass

        if not co.has_key('viewer_nammos'):
            co['viewer_nammos']={'color':'green',
                                 'model':'nammos',
                                 'module':'MosPlot',
                                 'label':'NAM MOS',
                                 }

        if not co.has_key('viewer_nam'):
            co['viewer_nam']={'color':'brown',
                                 'module':'EtaPlot',
                                 'label':'NAM DMO',
                                 'model':'nam',
                                 }

        try:
            co['viewer_gfslamp']['label']='GFS LAMP'
            changed = True
        except KeyError:
            pass

        try:
            co['viewer_gfsmos']['label']='GFS MOS'
            changed = True
        except KeyError:
            pass

        fp = file(fname, 'w')
        co.write(fp)
        fp.close()
        print 'Done with', fname
            
    except Exception, e:
        print 'updateWxPlotCfg failed:\n%s' %  e
        pass

def updateXmitCfg(fname):
    """xmit.cfg file was missing from AWIPS CM"""
    
    if not os.path.isfile(fname):
        print 'File %s does not exist' % fname
        return

    co = ConfigObj(fname)
    changed=False
    try:
        vftfile=''
        if 'XXXX' in co['verification']['wmo']:
            for x in os.listdir(os.path.join('xmit', 'sent')):
                if 'NXUS98' in x:
                    vftfile = x
                    break
            else:
                return
            
        vftitems = vftfile.split('-')[0:4]
        co['verification']['fcstid'] = vftitems[0]
        co['verification']['awips'] = vftitems[1]
        co['verification']['wmo'] = ' '.join(vftitems[2:])
                                              
        try:
            fp = file(fname, 'w')
        except IOError:
            raise
        
        co.write(fp)
        fp.close()
            
        print 'Done with', fname
        
    except Exception, e:
        print 'updateXmitCfg failed:\n%s' %  str(e)
        pass
        
def cvtMtrsCfg(id_):
    """Do much of the leg-work for implementing CAC"""
    fname = os.path.join('etc', 'tafs', id_, 'mtrs.cfg')
    if not os.path.isfile(fname):
        return
    #
    # Configure AvnFPS for CAC
    NewRules = [{'method':'CAC_AirportOpsThresh',
                 'msgfromfile':'True',
                 'type':'cat',
                 'unique':'True',
                 'remarks':'Y',
                 'msg':'Below Airfield Minimums',
                 'severity':'6',
                 'cig':'200',
                 'vsby':'0.5'},
                {'method':'CAC_AirportOpsThresh',
                 'msgfromfile':'True',
                 'type':'cat',
                 'unique':'True',
                 'remarks':'Y',
                 'msg':'Airfield Not Available as an Alternate',
                 'severity':'3',
                 'cig':'600',
                 'vsby':'2'},
                {'method':'FuelAlternate',
                 'msgfromfile':'False',
                 'type':'cat',
                 'unique':'True',
                 'remarks':'Y',
                 'msg':'',
                 'severity':'3',
                 'cig':'2000',
                 'vsby':'3'},
                {'method':'CAC_FltCatDelta',
                 'msgfromfile':'False',
                 'type':'cat',
                 'unique':'True',
                 'remarks':'Y',
                 'msg':'',
                 'severity':'3'}]
    
    NewMethods = ['CAC_AirportOpsThresh','CAC_AirportOpsThresh',
                  'FuelAlternate','CAC_FltCatDelta']
    try:
        co = ConfigObj(fname)
        ActiveRules = ['rule_%s' % x for x in co['rules']['active']]
        once = 0
        #
        # If the rule is already present, remove from the 'add list'
        for rule in ActiveRules:
            try:
                pos = NewMethods.index(co[rule]['method'])
                name = NewMethods.pop(pos)
                if name == 'CAC_AirportOpsThresh' and not once:
                    if float(co[rule]['cig'])*float(co[rule]['vsby']) > 500.0:
                        once = pos = 1
                NewRules.pop(pos)
            except (ValueError,KeyError):
                pass
            except IndexError:
                break
        #
        # If there are some CAC rules missing, rescan for rules
        # under old names
        #
        OldMethods = []
        for name in NewMethods:
            if name.startswith('CAC_'):
                OldMethods.append(name[4:])
            else:
                OldMethods.append(name)
                
        for rule in ActiveRules:
            try:
                pos = OldMethods.index(co[rule]['method'])
                co[rule]['method'] = NewMethods.pop(pos)
                NewRules.pop(pos)
                OldMethods.pop(pos)
            except (ValueError,KeyError):
                pass
            except IndexError:
                break
        #
        # Having searched for CAC rules under the new and old
        # method names and were not found in current configuration,
        # so add them now.
        #
        NewRulesAdded = len(NewRules)        
        if NewRulesAdded > 0:
            print 'Updating mtrs.cfg for %s' % id_
            nextRule = max([int(x) for x in co['rules']['active']])+1
            newsequence = xrange(nextRule,nextRule+len(NewRules))
            NewLabels = ['rule_%d' % x for x in newsequence]
        
            co['rules']['active'] += [str(x) for x in newsequence]
            for keyvalue,attributes in zip(NewLabels,NewRules):
                co[keyvalue] = attributes
        try:
            fp = file(fname, 'w')
        except IOError:
            raise
        
        co.write(fp)
        fp.close()
        #
        # Count the number of active 'cat' rules, rough check to make
        # sure there aren't too many.
        #
        co = ConfigObj(fname)
        CACRules = 0
        for rule in ['rule_%s' % x for x in co['rules']['active']]:
            if co[rule]['type'] == 'cat':
                CACRules += 1
                
        if CACRules < 3 or CACRules > 4:
            print '-------- !!! Attention Aviation Focal Point !!! --------'
            print 'You have %d active CAC rules for location %s.' % (CACRules,id_)
            if CACRules < 3:
                print 'This may be too few.'
            else:
                print 'This may be too many.'
            print 'Please review all METAR monitoring rules for %s using' % id_
            print 'the Monitoring Rule Editor under AvnFPS Configuration'
            print 'GUI to be sure they are correct.'
            print '-------- !!! Attention Aviation Focal Point !!! --------'
            
        print 'Done with %s' % id_
        
    except Exception, e:
        print 'cvtMtrCfg failed on %s\n%s' % (id_, e)
        pass

def cvtTAFPrdct(product):
    """Add collective attribute to file"""
    fname = os.path.join('etc', 'tafs', product)
    if not os.path.isfile(fname):
        return
    
    co = ConfigObj(fname)
    if co['sites'].has_key('collective'):
        return
        
    co['sites']['collective'] = ''
    pils = []
    for id_ in co['sites']['idents']:
        fname = os.path.join('etc', 'tafs', id_, 'info.cfg')
        if not os.path.isfile(fname):
            continue
            
        ci = ConfigObj(fname)
        pils.append(ci['headers']['afos'])
        if len(pils) == 2 and pils[0] != pils[1]:
            break
    #
    # For those locations with one TAF -- that's not a collective
    unique = sets.Set(pils)
    if len(unique) == 1 and len(co['sites']['idents']) > 1:
        co['sites']['collective'] = unique.pop()
            
    fname = os.path.join('etc', 'tafs', product)
    try:
        fp = file(fname, 'w')
        co.write(fp)
        fp.close()
    except IOError, e:
        print 'cvtTAFPrdct failed on %s\n%s' % (product, str(e))
        pass
        
def rmvOldExtensions(dir):
    def rmvOldFiles(ignored,dir,namelist):
        for name in namelist:
            if name.endswith('.old'):
                try:
                    os.remove(os.path.join(dir,name))
                except OSError:
                    pass
                
    os.path.walk(dir,rmvOldFiles,None)

def main():

    os.chdir(TopDir)
    sys.path = sys.path[1:]
    sys.path.extend([os.path.join(TopDir, 'py')])
    
    import AvnParser
    
    fdict = AvnParser.getForecasters()
    fcstrs = dict([(str(fdict[k]['id']),k) for k in fdict])
    okx='0'
    if os.uname()[1].find('-okx') > -1:
        okx='1'

    print 'Processing files in etc'
    rmvOldExtensions(os.path.join('etc'))
    #
    for _file in [x for x in os.listdir(os.path.join('etc','app-resources'))
                  if re.match('X\.\d+$',x)]:
        cvtFcstResourceFile(_file,okx,fcstrs.get(_file.split('.')[1],
                                                 'Unknown Forecaster'))        
    updateGUICfg(os.path.join('etc','gui.cfg'))
    updateWxPlotCfg(os.path.join('etc','wxplot.cfg'))
    updateServerCfg(os.path.join('etc','server.cfg'))
    updateXmitCfg(os.path.join('etc','xmit.cfg'))
#
    sites = [x for x in os.listdir(os.path.join('etc','tafs')) if re.match('[PNKT][A-Z0-9]{3}$',x)]
    for site in sites:
	cvtInfoCfg(site)
#        
    sites.insert(0,'XXXX')
    for site in sites:
        cvtMtrsCfg(site)

    for product in [x for x in os.listdir(os.path.join('etc','tafs')) if x.endswith('.cfg')]:
        cvtTAFPrdct(product)
    
    if okx == '1':
        print """Attention OKX ITO! Attention OKX AFP! Attention OKX ITO! Attention OKX AFP!

  You no longer have to alter TafEditDialog.__saveWorkFile() function
  to store the TAF workfile in the text database.  It is now
  implemented as a forecaster resource setting '*alwaySaveToDB'.  For
  all OKX forecasters, this new resource has been turned on as a
  convenience.
  """      
        print """Attention OKX ITO! Attention OKX AFP! Attention OKX ITO! Attention OKX AFP!"""

##############################################################################
if __name__ == '__main__':
    main()
