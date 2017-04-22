#
#    Name:
#       fixClimateFiles.py
#       GFS1-NHD:A10636.0000-SCRIPT;2
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 2 (DELIVERED)
#         Created:  20-AUG-2009 13:12:14      OBERFIEL
#           Added purpose section to documentation block.
#       
#       Revision 1 (REVIEW)
#         Created:  20-AUG-2009 13:02:53      OBERFIEL
#           Based off Dan's fixhdf.py script.  Modified to beautify
#           output and more robust error handling.
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7428
#       	Action Date:       31-AUG-2009 16:40:04
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: tpo indicator not monitoring properly
#       
#
#    Notes:
#       Originally written by Dan Gilmore.
#
#    Purpose:
#       Utility to remove synthetic observations inserted into HDF5 archives by OB9
#       software.  These bogus reports produce excessive amounts of VLIFR conditions
#       especially when ceilings are examined.
#
#       This problem has been fixed within the Climate Update Tool for OB9.2.
#       Subsequent climate updates with OB9.2 software should not be a problem.
#
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Jun 14, 2016    5693          rferrel        Initial creation
#    Sep 15, 2016    5693          rferrel        Fix but in creating new info rows;
#                                                 and get reject codes from ClimateDataUpdate.

import sys
sys.argv = [__name__]

import os, shutil, stat, tables, time
import ClimateDataUpdate

TopDir = os.environ.get('TOP_DIR', '/awips2/edex/data/share/aviation')
sys.path = sys.path[1:]
sys.path.extend([os.path.join(TopDir, dir) for dir in ['sitepy', 'py']])

import ClimateDataFilePrep

def _tee(log,msg):
    if log != sys.stdout:
        log.write(msg)
        log.flush()
    sys.stdout.write(msg)
    sys.stdout.flush()
    
# def fixClimateFiles(queue, stnPickle, sites='default for sites', climateDir='default for it'):
def fixClimateFiles(queue, stnPickle, sites=None, climateDir=None):
    TopDir = climateDir
    sys.argv.extend(sites)
    _fixClimateFiles()
    
#     _tee(sys.stdout, "FixClimateFiles.fixClimateFiles sites: " + sites + ', climateDir: ' + climateDir)
    
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print 'Usage: %s/bin/avnstart.sh fixClimateFiles id1 [id2 id3 ...]' % TopDir
        sys.exit(0)
    _fixClimateFiles()

def _fixClimateFiles():
    os.chdir(TopDir)
    log_file = os.path.join(TopDir,'tmp','hdf_postproc_log.txt')
    try:
        log = open(log_file,'w')
    except IOError, e:
        log = sys.stdout
        _tee(log,'Unable to open log file %s because %s\n' % (log_file,str(e)))

    #
    # For each station listed on the command line.
    for stn in [x.upper() for x in sys.argv[1:]]:
        stn_Name = '%s.hd5' % (stn)
        _tee(log,'\n[%s] Started removal of bogus obs for %s\n' % (time.ctime(), stn))
        old_file = os.path.join(TopDir, stn_Name)
        new_file = os.path.join(TopDir, 'tmp', stn_Name)
        
        try:
            fh = tables.openFile(old_file,'r')
        except IOError, e:
            _tee(log,'%s\n' % str(e))
            continue
        
        try:
            ClimateDataFilePrep.PrepFiles(stn,new_file)
        except IOError, e:
            _tee(log,'%s\n' % str(e))
            continue

        os.chmod(new_file,stat.S_IRUSR|stat.S_IWUSR|stat.S_IWGRP|stat.S_IRGRP)
        try:
            fh2 = tables.openFile(new_file,'a')
        except IOError, e:
            _tee(log,'%s\n' % str(e))
            continue

        oldobs,oldinfo=fh.root.obs,  fh.root.info
        newobs,newinfo=fh2.root.obs,fh2.root.info

        for row in oldinfo.iterrows(step=1):
            newrow = newinfo.row
            for col in newinfo.colnames:
                try:
                    newrow[col] = row[col]
                except Exception, e:
                    _tee(log,'%s\n' % str(e))
            newrow.append()

        counter = 0
        old_time = 0L

        for row in oldobs.iterrows(step=1):
            if old_time == 0L:
                old_time = row['date_time']
            if row['type'] not in ClimateDataUpdate.reject_codes:
                if row['type'] == 'SY-MT' and row['date_time'] <= old_time:
                    continue
                newrow = newobs.row
                for col in newobs.colnames:
                    try:
                        newrow[col] = row[col]
                    except Exception, e:
                        _tee(log,'%s\n' % str(e))
                        
                newrow.append()
                counter += 1    
                if counter % 500 == 0:
                    _tee(log,'.')
                if counter % 40001 == 0:
                    _tee(log,'\n')
                    
            old_time = row['date_time'] + (15*60)

        _tee(log,'\n[%s] Finished processing for %s\n' % (time.ctime(), stn))
        fh2.root.obs.flush()
        fh.close()
        fh2.close()
        
        try:
            shutil.copy(new_file,old_file)
        except IOError,e:
            _tee(log,'\nUnable to copy %s\nbecause: %s\n' %(new_file,str(e)))

    if log != sys.stdout:
        try:
            log.flush()
            log.close()
        except IOError:
            pass
        
