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
#       WindRose.py
#       GFS1-NHD:A9008.0000-SCRIPT;31
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 31 (DELIVERED)
#         Created:  10-AUG-2009 07:43:51      GILMOREDM
#           Now gracefully handles the situation where a user selects
#           'Save for Google Earth' and then clicks 'Cancel' on the
#           save image dialog. This was producing an error message.
#       
#       Revision 30 (DELIVERED)
#         Created:  03-AUG-2009 10:11:27      OBERFIEL
#           Better computation of lat/lon box for Google Earth to
#           preserve aspect ratio, 
#           what appears as circles in the GUI stays as circles in GE.
#           Also size of image is
#           configurable -- defaults as 10km.  Minor enhancements for
#           saving filenames.
#       
#       Revision 29 (REVIEW)
#         Created:  31-JUL-2009 15:09:57      GILMOREDM
#           Added final functionality to create KML files
#       
#       Revision 28 (REVIEW)
#         Created:  31-JUL-2009 15:02:49      GILMOREDM
#           Added functionality to create KML files for Ground Overlay
#           in Google Earth
#       
#       Revision 27 (DELIVERED)
#         Created:  26-FEB-2008 14:20:54      OBERFIEL
#           imported re module in CigVisDist; Fix output and help
#           documentation in WindRose
#       
#       Revision 26 (DELIVERED)
#         Created:  29-NOV-2007 10:13:40      OBERFIEL
#           Removed CR characters.
#       
#       Revision 25 (REVIEW)
#         Created:  28-NOV-2007 10:01:01      GILMOREDM
#           Corrected issue that produced a misleading month name
#           display.
#       
#       Revision 24 (DELIVERED)
#         Created:  30-OCT-2007 14:18:17      OBERFIEL
#           Removed CR characters.
#       
#       Revision 23 (REVIEW)
#         Created:  26-OCT-2007 10:25:56      GILMOREDM
#           Changed so that a user can select a range of months and
#           hours
#       
#       Revision 22 (DELIVERED)
#         Created:  18-MAY-2007 11:10:59      OBERFIEL
#           Sync'd up code between 3.4 and 3.5 worksets since snapshot
#       
#       Revision 21 (DELIVERED)
#         Created:  25-APR-2007 13:04:23      OBERFIEL
#           Updated staging script to fix weird timestamp, remove CR
#           characters.  Fixed final bug in computing scale for Wind
#           Rose.
#       
#       Revision 20 (DELIVERED)
#         Created:  24-APR-2007 20:48:33      OBERFIEL
#           Fixed so that when scale is NaN that drawing area graphic
#           is preserved and error message is issued.
#       
#       Revision 19 (DELIVERED)
#         Created:  18-APR-2007 14:11:33      OBERFIEL
#           Updated help documentation to reflect the new 'Auto-Update'
#           feature.
#       
#       Revision 18 (DELIVERED)
#         Created:  11-APR-2007 13:51:26      OBERFIEL
#           Updated logic to catch additional exceptions and report
#           them to user.  Canvas should only
#           be cleared when all the data are validated and just before
#           plotting the new data.  Corrected misspelling
#           of criteria.  Thanks Amanda.
#       
#       Revision 17 (DELIVERED)
#         Created:  21-MAR-2007 09:54:49      OBERFIEL
#           For AvnClimate.py: Updated the Help text; MetarMonitorP.py:
#           fix potential permission problem;
#           WindRose.py: Added Auto Update feature, by default always
#           on.
#       
#       Revision 16 (REVIEW)
#         Created:  20-MAR-2007 13:36:54      OBERFIEL
#           Updated text input fields and printer dialogs to be
#           consistent.  Frame tag removed.
#       
#       Revision 15 (DELIVERED)
#         Created:  05-FEB-2007 08:36:51      OBERFIEL
#           Fixed misspelling in help documentation and corrected
#           validators.
#       
#       Revision 14 (BUILD_RELEASE)
#         Created:  04-JAN-2007 12:55:01      OBERFIEL
#           Update print dialog and put a nice feature in.
#       
#       Revision 13 (DELIVERED)
#         Created:  28-DEC-2006 11:14:05      OBERFIEL
#           Added message when NaN detected.  Redo validation critera
#           on EntryField for print dialog.
#       
#       Revision 12 (DELIVERED)
#         Created:  11-AUG-2006 10:28:07      OBERFIEL
#           Corrected index to dictionary
#       
#       Revision 11 (DELIVERED)
#         Created:  23-JUN-2006 13:49:36      OBERFIEL
#           Updated title of graphic
#       
#       Revision 10 (DELIVERED)
#         Created:  30-MAY-2006 15:10:40      TROJAN
#           spr 7144: added auto-update feature, number of years in
#           database
#       
#       Revision 9 (DELIVERED)
#         Created:  19-MAY-2006 08:49:22      TROJAN
#           spr 7144: added Num Hours counter
#       
#       Revision 8 (DELIVERED)
#         Created:  02-MAY-2006 09:25:36      TROJAN
#           SPR 7132: fixed wids array index
#       
#       Revision 7 (DELIVERED)
#         Created:  02-MAY-2006 08:32:41      TROJAN
#           SPR 7137: fixed wids array index
#       
#       Revision 6 (DELIVERED)
#         Created:  24-MAR-2006 17:45:45      TROJAN
#           spr 7106 Pmw code does not forward python exceptions to
#           AvnFPS _Logger
#       
#       Revision 5 (DELIVERED)
#         Created:  24-MAR-2006 09:48:24      TROJAN
#           spr 7103: redirect all error messages to a log file
#       
#       Revision 4 (DELIVERED)
#         Created:  16-FEB-2006 14:43:52      TROJAN
#           removed unnecessary imports and variables
#       
#       Revision 3 (APPROVED)
#         Created:  15-FEB-2006 14:34:48      TROJAN
#           fixes transient dialog behavior - spr 7091
#       
#       Revision 2 (APPROVED)
#         Created:  13-FEB-2006 10:37:29      TROJAN
#           data retrieval in separate thread, changed GUI layout
#       
#       Revision 1 (APPROVED)
#         Created:  30-JAN-2006 07:54:16      TROJAN
#           stdr 945
#
#    Change Document History:
#       1:
#           Change Document:   GFS1-NHD_SPR_7417
#           Action Date:       28-AUG-2009 10:01:43
#           Relationship Type: In Response to
#           Status:           APPROVED
#           Title:             AvnFPS: TUG code does not handle transition from warm to cold seasons
#       
#
import logging, math, os, time
import Avn, ClimLib, numpy, ClimateProcessLogger

import sys
sys.argv = [__name__]

_Help = {
    'title': 'AvnFPS - Wind Rose Display Help', \
    'content': """
This application displays wind rose for selected month and hour,
or range of hours. 

Time selection
    Month - selects month.
    Num Months - select number of months of data to display
    Hour - selects hour. 
    Num Hours - selects number of hours of data to display

Flight Cat
    This option menu restricts the search to flight category
    conditions at or below the selected value. "All" means no
    restrictions.

If Auto Redraw is selected, changing month, hour, or number of hours
fields will cause the wind rose to be redrawn for any valid value in
these fields.

Use the"Draw" button to display wind rose after selecting new site,
or flight category.

The displayed image can be printed or stored in a graphic file.
Use the options under the "File" menu for that purpose.

"""
}

_Logger = logging.getLogger(ClimateProcessLogger.CLIMATE_CATEGORY)
###############################################################################
# private functions used by WindRose
def _periods(t, delta):
    '''Splits event timespan into hourly intervals'''
    h0, s0 = divmod(t%86400, 3600.0)
    h1, s1 = divmod((t+delta)%86400, 3600.0)
    if h1 == h0:            # consecutive reports within the same hour
        return [(h0, (s1-s0)/3600.0)]
    elif (h1-h0)%24 == 1:   # next hour
        if s1 == 0.0:
            return [(h0, 1.0-s0/3600.0)]
        else:
            return [(h0, 1.0-s0/3600.0), (h1, s1/3600.0)]
    elif (h1-h0)%24 == 2:   # 2 hours, but next one may be on the top
        if s1 <= 61.0:      # allow 1 min late
            return [(h0, 1.0-s0/3600.0), ((h1-1)%24, 1.0)]
        else:
            return []
    else:
        return []
    
###############################################################################
class WindRose():
    '''Displays wind rose for a site, given month and range of hours'''
    AppName = """AvnFPS - Wind Rose"""
    _d = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    Days = [sum(_d[:n]) for n in range(1, len(_d)+1)]
    Month = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', \
        'Oct', 'Nov', 'Dec']
    
    def get_wind_config(self, fname):
        # configure wind plot parameters
        # set defaults
        self.fx = 4
        self.set_wind_dirs(36)
        self.winds = [{'color': 'white'}, {'color': 'brown'}, \
            {'color': 'blue', 'val_kt': 5}, {'color': 'green', 'val_kt': 12}, \
            {'color': 'red', 'val_kt': 20}, {'color': 'purple'}]
        for wind in self.winds:
            if 'val_kt' in wind:
                wind['val_mps'] = ClimLib.us2hd_wind_speed(wind['val_kt'])
        # read configuration file
        if not os.path.isfile(fname):
            _Logger.error('Config file %s does not exist' % fname)
            raise Avn.AvnError('Config file %s does not exist' % fname)
        import ConfigParser
        cp = ConfigParser.SafeConfigParser()
        cp.read(fname)
        self.num_winds = cp.getint('wind', 'num_spd')
        self.set_wind_dirs(cp.getint('wind', 'num_dir'))
        self.winds = []
        for tag in ['calm', 'vrb'] + [str(x+1) for x in range(self.num_winds)]:
            section = 'wind_'+tag
            color = cp.get(section, 'color')
            if cp.has_option(section, 'value'):
                v_kt = cp.getint(section, 'value')
                v_mps = ClimLib.us2hd_wind_speed(v_kt)
                self.winds.append({'val_kt': v_kt, 'val_mps': v_mps, 'color': color})
            else:
                self.winds.append({'color': color})

    def set_wind_dirs(self, num):
        # configure number of wind directions
        self.num_dirs = num
        self.vrb_dir = self.num_dirs
        self.calm_dir = self.num_dirs+1

    def get_data(self, month, end_month, flight_cat, id_, fname, configFile, queue):
        self.get_wind_config(configFile)
        try:
            if not os.path.isfile(fname):
                _Logger.error('File %s does not exist' % fname)
                raise Avn.AvnError('File %s does not exist' % fname)
            import tables
            fh = tables.openFile(fname, 'r')
            try:
                import warnings
                # following function - table = fh.getNode('/obs') throws extraneous warnings
                # so going to filter warnings on this function
                warnings.simplefilter("ignore")
                table = fh.getNode('/obs')
                warnings.simplefilter("default")
                if table is None:
                    raise ValueError('Bad data in %s' % fname)
                self.wind_stats(table, month, end_month, flight_cat, queue)
            except Exception, e:
                print e
            finally:
                fh.close()
        except Exception, e:
            msg = 'Cannot retrieve data for %s: %s' % (id_, e)
            _Logger.error(msg)

    def wind_stats(self, table, month, end_month, flight_cat, queue):
        yday1 = (self.Days[month-1]+1)%365
        yday2 = (self.Days[end_month-1])%366
    
        if flight_cat < 3:
            cig_cat = ClimLib.FlightCats[flight_cat]['cig']
            vis_cat = ClimLib.FlightCats[flight_cat]['vis']
        else:
            cig_cat = vis_cat = None

        def _get(row):
            d = {}
            for key in ['date_time', 'wind_dir', 'wind_spd', 'wdir_type']:
                d[key] = row[key]
            return row['date_time'], d
    
        def _filter(row):
            if row['wind_spd'] == table.attrs.wind_spd['fill'] \
                and row['wdir_type'] != 'C':
                return False
            if row['wind_dir'] == table.attrs.wind_dir['fill'] \
                and row['wind_spd'] != 0.0 \
                and row['wdir_type'] not in ['C', 'V']:
                return False
            if cig_cat and vis_cat:
                if row['vis'] > vis_cat and row['cig'] > cig_cat:
                    return False
            return True

        def _process(dt, data):
            if data['wdir_type'] == 'C' or data['wind_spd'] == 0:
                dd_bin = self.calm_dir
            elif data['wdir_type'] == 'V':
                dd_bin = self.vrb_dir
            else:
                dd_bin = int((data['wind_dir']*self.num_dirs)/360.0+0.4)
                if dd_bin == self.num_dirs:
                    dd_bin = 0
            if dd_bin != self.calm_dir:
                for n, wind in enumerate(self.winds[2:-1]):
                    if data['wind_spd'] < wind['val_mps']:
                        ff_bin = n+1
                        break
                else:
                    ff_bin = len(self.winds)-2
            else:
                ff_bin = 0
            periods = _periods(data['date_time'], dt)
            if not periods:
                print time.ctime(data['date_time']), dt
            try:
                for h, delta in periods:
                    stats[int(h)][dd_bin][ff_bin] += delta
            except Exception, e:
                print e
        
        if yday1 < yday2:
            tmp = [_get(row) for row in \
                   table.where('(yday1<=yday) & (yday<=yday2)') \
                   if _filter(row)]
        else:
            tmp = [_get(row) for row in table.where('(yday1<=yday) & (yday<=366)') \
                   if _filter(row)] + \
                   [_get(row) for row in table.where('(0<=yday) & (yday<=yday2)') \
                    if _filter(row)]
        tmp.sort()
        first_year = time.gmtime(tmp[0][0]).tm_year
        last_year = time.gmtime(tmp[-1][0]).tm_year
        stats = numpy.zeros((24, 38, self.num_winds+1), numpy.float32)
        for dt, data in [(y[0]-x[0], x[1]) for x, y in Avn.window(tmp) \
            if y[0]-x[0] < 4200.0]:
            _process(dt, data)
        queue.put([first_year, last_year])
        for hour in range(24):
            for wind_dir in range(self.num_dirs+2):
                for wind_spd in range(self.num_winds+1):
                    sendObj = [hour, wind_dir, wind_spd, float(stats[hour][wind_dir][wind_spd])]
                    queue.put(sendObj)
        queue.put("done")
    
    
    
    
    
    