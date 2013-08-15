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
#       LLWSThread.py
#       GFS1-NHD:A8111.0000-SCRIPT;1.30 
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.30 (DELIVERED)
#         Created:  21-APR-2009 09:40:45      OBERFIEL
#           Removed superfluous INFO messages.
#
#       Revision 1.29 (DELIVERED)
#         Created:  16-APR-2008 13:06:23      GILMOREDM
#           Initialize wind shear variables with missing values.
#           Missing wind shear strings will now appear as WS999/99999KT
#       
#       Revision 1.28 (DELIVERED)
#         Created:  19-NOV-2007 20:31:27      OBERFIEL
#           Removed carriage return characters in files
#       
#       Revision 1.27 (DELIVERED)
#         Created:  31-OCT-2007 14:56:38      GILMOREDM
#           Added code to handle Acars profiles
#       
#       Revision 1.26 (DELIVERED)
#         Created:  20-JUN-2007 10:16:10      OBERFIEL
#           Data cutoff values are documented to be in meters, not
#           feet.
#       
#       Revision 1.25 (DELIVERED)
#         Created:  21-FEB-2006 14:12:23      OBERFIEL
#           Updated logic in _genQCArray
#       
#       Revision 1.24 (APPROVED)
#         Created:  31-JAN-2006 18:12:21      TROJAN
#           spr 7081
#       
#       Revision 1.23 (APPROVED)
#         Created:  29-JAN-2006 13:00:03      TROJAN
#           spr 7083
#       
#       Revision 1.22 (APPROVED)
#         Created:  01-DEC-2005 16:41:49      OBERFIEL
#           Enhancement to read profiler files in LDAD directory
#       
#       Revision 1.21 (REVIEW)
#         Created:  30-NOV-2005 11:42:44      OBERFIEL
#           Updated code based on review
#       
#       Revision 1.20 (DELIVERED)
#         Created:  06-SEP-2005 20:17:40      TROJAN
#           spr 7014
#       
#       Revision 1.19 (DELIVERED)
#         Created:  06-SEP-2005 19:09:58      TROJAN
#           spr 7009
#       
#       Revision 1.18 (APPROVED)
#         Created:  06-SEP-2005 18:24:40      OBERFIEL
#           LLWSThread now uses uvQualityCode information to screen out
#           bad data
#       
#       Revision 1.17 (DELIVERED)
#         Created:  29-AUG-2005 13:42:01      OBERFIEL
#           Fixed LLWSThread to account for elevation of radar in wind
#           data
#       
#       Revision 1.16 (DELIVERED)
#         Created:  06-JUL-2005 18:16:39      TROJAN
#           spr 6548
#       
#       Revision 1.15 (DELIVERED)
#         Created:  07-JUN-2005 18:01:02      OBERFIEL
#           Fixed code to survive OB5.1 to OB6 transistion
#       
#       Revision 1.14 (DELIVERED)
#         Created:  01-JUN-2005 17:41:13      OBERFIEL
#           Bug fixes since RHE3 initial snapshot
#       
#       Revision 1.13 (DELIVERED)
#         Created:  07-MAY-2005 11:34:48      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.12 (DELIVERED)
#         Created:  18-APR-2005 17:31:53      OBERFIEL
#           Changes to support gamin
#       
#       Revision 1.11 (DELIVERED)
#         Created:  25-MAR-2005 18:45:50      OBERFIEL
#           Fixed bug with profiler data
#       
#       Revision 1.10 (DELIVERED)
#         Created:  15-MAR-2005 18:40:09      OBERFIEL
#           Speeded up Text Queue processing
#       
#       Revision 1.9 (DELIVERED)
#         Created:  11-MAR-2005 16:05:05      TROJAN
#           spr 6717
#       
#       Revision 1.8 (APPROVED)
#         Created:  10-MAR-2005 16:51:37      OBERFIEL
#           Fixed reporting height of shear layer. Its always the top.
#       
#       Revision 1.7 (DELIVERED)
#         Created:  15-FEB-2005 13:47:37      TROJAN
#           spr 6650
#       
#       Revision 1.6 (APPROVED)
#         Created:  07-FEB-2005 15:15:01      OBERFIEL
#           Added code changes that GT recommended
#       
#       Revision 1.5 (APPROVED)
#         Created:  04-FEB-2005 16:55:54      OBERFIEL
#           Recoded logic as GT specified. Tinkered with calculation of
#           WS group
#       
#       Revision 1.4 (APPROVED)
#         Created:  22-DEC-2004 15:29:39      OBERFIEL
#           Enforced strict definition of LLWS and re-wrote code
#       
#       Revision 1.3 (APPROVED)
#         Created:  01-DEC-2004 14:29:39      OBERFIEL
#           Fixed a few minor bugs
#       
#       Revision 1.2 (APPROVED)
#         Created:  08-NOV-2004 19:02:00      OBERFIEL
#           Changes to support LLWS
#       
#       Revision 1.1 (UNDER WORK)
#         Created:  02-NOV-2004 16:46:37      OBERFIEL
#           date and time created 11/02/04 16:46:26 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7418
#       	Action Date:       06-OCT-2009 09:42:01
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS:  AvnFPS regression based lightning forecast to use LAMP
#       
#**
#* 
#* 
#* <pre>
#* SOFTWARE HISTORY
#* Date         Ticket#     Engineer    Description
#* ------------ ----------  ----------- --------------------------
#*                                      Initial creation.
#* Mar 25, 2013 1735        rferrel     __initializeLLWSDictsLists now reads cfg data only for 
#*                                      desired site instead of all sites. So it is O(n) instead of O(n**2)
##  
#
import logging, os, Queue, re, time, math, sys
import Avn, AvnParser, LLWSData, MetarData

_Logger = logging.getLogger(Avn.CATEGORY)

_MPS2KNTS = 1.944             # 1.944 m/s == 1 knot
_KNTS2MPS = 0.514             # 1 m/s == 0.514 knots
_MAXLLWSLVL = 625             # 2050 feet == 625 meters
_MINLLWSLVL = 60.9            # 200 feet == 60.9 meters
_HFT2MTRS = 30.48             # 100 feet == 30.48 meters
_MTRS2FT = 3.281              # 1 meter == 3.281 feet
_HR2SECS = 3600               # 1 hour == 3600 seconds

_FileNamePattern = re.compile(r'[0-9]{8}_[0-9]{4}')

class InValid(AttributeError):
   """Exception when multiple pieces of data are incompatible to use together"""
   
##############################################################################
class Server(object):
   """Processes LLWS files and updates a dictionary"""
   __TimeOut = 10.0

   def __init__(self, info):     
      self.profilerList = []
      self.radarList = []
      self.metarList = []
      self.acarsList = []

      self.siteVWPsDict = {}
      self.acarsDict = {}
      self.sfcObsDict = {}
      self.radarElevationsDict = {}
      self.vwpObsDict = {}
      
      # Find out which radars and profiler sites need to be monitored
      self.__initializeLLWSDictsLists(info)

   def __initializeLLWSDictsLists(self,info):
      """
      Examine configuration file and setup dictionaries and lists
      """
      pList = []
      rList = []
      aList = []

      m = info['ident']
      if m is not None:
         siteDict = AvnParser.getTafSiteCfg(m)
         try:
            radars = siteDict['sites']['radars']
            radar_cutoff = siteDict['thresholds']['radar_cutoff']
         except KeyError:
            radars, radar_cutoff = [], []

         try:    
            profilers = siteDict['sites']['profilers']
            profiler_cutoff = siteDict['thresholds']['profiler_cutoff']
         except KeyError:
            profilers, profiler_cutoff = [], []

         try:
            acars = siteDict['sites']['acars']
         except KeyError:
            acars = []

         if len(profilers) > 0 or len(radars) > 0 or len(acars) > 0 :
             #
             # This TAF site needs to be monitored
             self.metarList.append(m)
             self.siteVWPsDict[m] = [radars,profilers,radar_cutoff,profiler_cutoff]
             self.acarsDict[m] = [acars]
             #
             pList.extend(profilers)
             rList.extend(radars)
             aList.extend(acars)
      #
      # Find all unique radars and profilers to monitor
      self.profilerList = dict.fromkeys(pList).keys()
      self.radarList = dict.fromkeys(rList).keys()
      self.acarsList = dict.fromkeys(aList).keys()
      
      
   def __getLatestFile(self, datapath):
      """
      Given directory, find all files that match YYYYMMDD_HHMM pattern and
      return most recent one
      """
      global _FileNamePattern
      try:
         files = [os.path.join(datapath,x) for x in os.listdir(datapath) \
                  if _FileNamePattern.match(x)]
         if files:
            files.sort()
            return files[-1]
         else:
            return ' '
         
      except OSError,e:
         _Logger.error(str(e))
         return ' '

   def __getRadarElevation(self,radar_icao):
      """
      Create a dictionary of Radar IDs and pedestal elevations
      """
      if not self.radarElevationsDict:
         path = Avn.PATH_MGR.getStaticFile(os.path.join(Avn.ConfigDir, 'WSR88Ds.dat')).getPath()
         try:
            for line in file(path):
               icao,name,elevation = line.split(',')
               self.radarElevationsDict[icao] = (name,elevation.strip())
         except IOError, e:
            _Logger.error(str(e))

      return self.radarElevationsDict.get(radar_icao, ['NoRadar','0'])[1]

   def __getRadarTimeStamp(self,fname):
      """
      From the filename, obtain the time it was created
      """
      #
      # Get the timestamp from the filename
      try:
         y=int(fname[-13:-9])
         m=int(fname[-9:-7])
         d=int(fname[-7:-5])
         h=int(fname[-4:-2])
         minute=int(fname[-2:])
         return int(time.mktime((y,m,d,h,minute,0, 0, 0, 0)))
      except TypeError:
         return -1

   def __readRadarData(self,rname, vwpList):
      """
      Read radar VWP data to get the wind and height information
      """
      #
      # Initialization . . .
      U = []
      V = []
      H = []
      last_spd = 0
      
      old_timestamp = self.vwpObsDict.get(rname,[0])[0]
      timestamp = time.mktime(time.gmtime(vwpList.get(0).getTime() / 1000)) - time.timezone
      if time.time() - timestamp > 1.5 * _HR2SECS:
         _Logger.info('Radar data for %s is out of date', rname)
         return [timestamp, H, U, V]
      #if timestamp <= old_timestamp:
      #   raise InValid
      
      rele = self.__getRadarElevation(rname)

      try:         
          sz = vwpList.size()
          for i in range(sz):
              line = vwpList.get(i)
                              
              #hgtS, uS, vS, ignored, spd = line.split()[1:6]
              hgtS = line.getHeight()
              uS = line.getWindU()
              vS = line.getWindV()
              try:
                  h = int(hgtS)*_HFT2MTRS - float(rele)
                  u = float(uS)
                  v = float(vS)
              except ValueError, e:
                    continue
              #
              # Duplicate wind height, pick the maximum
              # njensen commented out cause it doesn't actually work right              
              #if h in H:
              #    if spd > last_spd:
              #        loc = H.index(h)
              #        U[loc] = u
              #        V[loc] = v
              #else:
              H.append(h)
              U.append(u)
              V.append(v)

              #last_spd = spd 
              _Logger.debug( 'R\t%s\t%4.1f\t%03d\t%4.1f' %
                          ( rname, h*_MTRS2FT, int(270. - math.degrees(math.atan2(v,u)))%361,
                            math.hypot(v,u)*_MPS2KNTS))

              # If we go above _MAXLLWSLVL meters (2050 ft), that's enough
              if int(h) > _MAXLLWSLVL:
                  break

      except IOError, e:
         _Logger.error(str(e))
         
      return [timestamp, H, U, V]

   def __readProfilerData(self,data,pname):
      """
      Read profiler VWP file to get the wind and height information
      """
      #
      # Initialization . . .
      timestamp = 0
      U = []
      V = []
      H = []
      
      timestamp = data['validTime'] / 1000
      #
      # If the data is old, return early.
      if time.time() - timestamp > 1.5 * _HR2SECS:
         _Logger.info('Profiler data for %s is out of date', pname)
         return [timestamp, H, U, V]

      lvls = data['numProfLvls']
      uC = data.getNumberAllLevels('uComponent')[:lvls]
      vC = data.getNumberAllLevels('vComponent')[:lvls]
      hC = data.getNumberAllLevels('height')[:lvls]
      QC = data.getNumberAllLevels('uvQualityCode')[:lvls]
      
      for n in range(lvls):
         u = uC[n]
         v = vC[n]
         h = hC[n]
         qc = QC[n]
         if qc == 0:
            H.append(h)
            U.append(u)
            V.append(v)
            _Logger.debug( 'P\t%s\t%4.1f\t%03d\t%4.1f' %
                           ( pname, h*_MTRS2FT, int(270. - math.degrees(math.atan2(v,u)))%361,
                             math.hypot(v,u)*_MPS2KNTS))

         # If we go above 624 meters (2050 ft), that's enough
         if h > _MAXLLWSLVL:
            break

         elif qc and h <= _MAXLLWSLVL:
            _Logger.debug( 'X\t%s\t%4.1f\t%03d\t%4.1f\t%d' %
                           ( pname, h*_MTRS2FT, int(270. - math.degrees(math.atan2(v,u)))%361,
                             math.hypot(v,u)*_MPS2KNTS, qc))

      return [timestamp, H, U, V]

   def __readAcarsData(self,acarsRec, acarsId):
      timestamp = acarsRec.getTimeObs().getTimeInMillis() / 1000
      
      H = []
      U = []
      V = []
      #
      # If the data is old, return early.
      if time.time() - timestamp > 1.5 * _HR2SECS:
         _Logger.info('ACARS Sounding data for %s is out of date', acarsId)
         return [timestamp, H, U, V]
     
      levels = acarsRec.getLevels()
      # TODO: make sure the levels are sorted in ascending order
      levelList = []
      #for level in levels:
      itr = levels.iterator()
      while itr.hasNext():
          level = itr.next()
          alt = level.getFlightLevel()
          spd = level.getWindSpeed()
          dir = level.getWindDirection()
          if not alt or not spd or not dir:
              continue
          level = {'alt': alt.intValue(), 'spd': spd.doubleValue(), 'dir': dir.doubleValue()}
          levelList.append(level)
          
      levelList.sort(lambda x, y: x['alt']-y['alt'])
      delta = levelList[0]['alt']
      
      for level in levelList:
          alt = level['alt']
          spd = level['spd']
          dir = level['dir']
          if spd > 100 or dir > 360.0:
              continue
          
          hgt = alt - delta
          
          if hgt < 10.0:
              continue
          
          spd = -spd
          H.append(hgt)
          U.append(spd*math.sin(math.radians(dir)))
          V.append(spd*math.cos(math.radians(dir)))
      
          if hgt > _MAXLLWSLVL:
              break
      
      return [timestamp, H, U, V]
 

   def __genQCArray(self,_fh,loc,uname,vname):
      """Create QC array based on the two wind component QC checks"""
      
      try:
         var = _fh.variables[uname]
         uQC = var.getValue()[loc]
         var = _fh.variables[vname]
         vQC = var.getValue()[loc]
            
         length = len(uQC)
         QC = [1] * length
         
         for i in xrange(length):
            if str(uQC[i]) in 'CSV' and str(vQC[i]) in 'CSV':
               QC[i] = 0
               
      except KeyError:
         raise InValid
      
      return QC
         
   def __getMetarWind(self,ident):
      """
      As metars arrive obtain the wind information
      """      
      U = V = 0
      obsTime = -1
      
      try:
         #data = self.DRC.getMetars(ident,1,req_time-_HR2SECS,1)
         data = MetarData.retrieve([ident])
      except AttributeError:
         data = None
         _Logger.info('%s METAR is not available', ident)

      if data:
         dcd = data[0].dcd
         if 'wind' in dcd:
            try:
               obsTime = int(dcd['itime']['value'])
               wdir = math.radians(float(dcd['wind']['dd']))
               wspd = float(dcd['wind']['ff'])*_KNTS2MPS
               U = -wspd * math.sin( wdir )
               V = -wspd * math.cos( wdir )
            except KeyError, e:
               _Logger.error(str(e))
            except ValueError:
               pass
            
      if obsTime == -1:
         raise InValid

      return [obsTime,U,V]

   def __getVWPCutoffHeight(self,TafID,VwpID):
      """Determine minimum height of valid data for VWP source"""
      
      cutoff = 0
      
      if VwpID in self.siteVWPsDict[TafID][0]:
         pos = self.siteVWPsDict[TafID][0].index(VwpID)
         cutoff = self.siteVWPsDict[TafID][2][pos]
         
      elif VwpID in self.siteVWPsDict[TafID][1]:
         pos = self.siteVWPsDict[TafID][1].index(VwpID)
         cutoff = self.siteVWPsDict[TafID][3][pos]
      
      return cutoff

   def genShear(self,TafID,VwpID):
      """Calculate shear and find maximum value"""
      #
      try:
          sfcTimeStamp, uSfc, vSfc = self.sfcObsDict[TafID]
          vwpTimeStamp, vwpH, vwpU, vwpV = self.vwpObsDict[VwpID]
      except KeyError:
          #TODO remove this print statement
          print 'LLWSThread.genShear KeyError TafID (%s), VwpID (%s)' % (TafID, VwpID)
          raise InValid
      
      if len(vwpH) == 0 or len(vwpU) == 0 or len(vwpV) == 0:
          raise InValid
      
      if abs( vwpTimeStamp - sfcTimeStamp ) > 1.5 * _HR2SECS:
          raise InValid
      #
      maxS = 0.0
      maxSHeight = maxSDir = 999
      maxSWind = 99
      #
      # Copy arrays
      H = vwpH[:]
      U = vwpU[:]
      V = vwpV[:]
      #
      # Insert surface data
      H.insert(0,10)
      U.insert(0,uSfc)
      V.insert(0,vSfc)
      #
      # Get cutoff height for this source
      try:
         cutoff = self.__getVWPCutoffHeight(TafID,VwpID)
      except KeyError:
         cutoff = 0
      
      lenH = len(H)-1
      for i in [H.index(x) for x in H if x >= cutoff]:
         j = i+1
                  
         while j <= lenH:
            dH = H[j]-H[i]
            #
            # Shear thickness critera must be met.
            if dH < _MINLLWSLVL:
               j += 1
               continue

            dU = U[j]-U[i]
            dV = V[j]-V[i]

            s = math.hypot(dU,dV)/dH
            
            if s > maxS:
               maxS = s
               maxSHeight = int(H[j]*_MTRS2FT*0.01)
               maxSWind = int(math.hypot(V[j],U[j])*_MPS2KNTS+0.5)
               rv = (270. - math.degrees(math.atan2(V[j],U[j])))%361.

               maxSDir = int((rv/10.0)+0.5)*10

            break

      if maxSWind == 0:
         maxSDir = 0

      if maxSWind > 0 and maxSDir == 0:
         maxSDir = 360
         
      return { 'time':float(max(sfcTimeStamp,vwpTimeStamp)),'value': maxS,\
               'str': 'WS%03d/%03d%02dKT' % (maxSHeight, maxSDir, maxSWind) }

   def __writeMetarOutput(self,TAFId):
      """
      Output of max shear goes to a file and notify clients of the update
      """
      update = 0
      fileDict = LLWSData.readLLWS(TAFId)
      #
      # Get the latest SFC data and list of VWP sources
      radars,profilers = self.siteVWPsDict[TAFId][:2]

      for s in radars + profilers:
         if s not in self.vwpObsDict:
            continue

         try:
            fileDict[s] = self.__genShear(TAFId,s)
            update = 1
            
         except InValid:
            pass

      if update:        
         LLWSData.writeLLWS(TAFId,fileDict)
      #
      # Send a message to potential clients
      self.outqueue.put(Avn.Bunch(src=self.name, ident=TAFId))

   def __writeAcarsOutput(self,TafId,acarsId):
      """
      Output of max shear goes to a file and notify clients of the update
      """
      if TafId not in self.sfcObsDict:
          return
      #
      # Read TafId's file contents
      fileDict = LLWSData.readLLWS(TafId)

      try:
          fileDict[acarsId] = self.__genShear(TafId,acarsId)
          LLWSData.writeLLWS(TafId,fileDict)
	 # 
	 # Send a message to potential clients
          self.outqueue.put(Avn.Bunch(src=self.name, ident=TafId))
      
      except InValid:
          pass

   def __writeVWPOutput(self,TAFId,vwpID):
      """
      Output of max shear goes to a file and notify clients of the update
      """
            
      if TAFId not in self.sfcObsDict:
         return
      #
      # Read TAFId's file contents
      fileDict = LLWSData.readLLWS(TAFId)

      try:
         fileDict[vwpID] = self.__genShear(TAFId,vwpID)
         LLWSData.writeLLWS(TAFId,fileDict)
         #
         # Send a message to potential clients
         self.outqueue.put(Avn.Bunch(src=self.name, ident=TAFId))
         
      except InValid:
         pass

   def processRadarData(self, radar_icao, vwpList):
      """
      Process the newly arrived radar data
      """
      try:         
         self.vwpObsDict[radar_icao] = self.__readRadarData(radar_icao, vwpList)
         #
         # Which TAF sites are affected
#         for tafSite in self.siteVWPsDict:
#            if radar_icao in self.siteVWPsDict[tafSite][0]:
#               self.__writeVWPOutput(tafSite,radar_icao)
      except:
         _Logger.info("Error reading radar data")

   def processAcarsData(self,acarsId,acarsRec):
      """+
      Process newly arrived Acars Data
      """
      try:
          self.vwpObsDict[acarsId] = self.__readAcarsData(acarsRec, acarsId)
          #self.__writeAcarsOutput('K' + acarsId, acarsId)
      except:
          _Logger.info("Error reading acars data")
         
      return True
      
   def processProfilerData(self,ident):
      """
      Process the newly arrived profiler data
      """      
      import RefTimePointDataRetrieve, NoDataException
      PARAMETERS = ["profilerId", "validTime", "numProfLvls", "height",
                    "uComponent", "vComponent", "uvQualityCode"]
      site = AvnParser.getTafSiteCfg(ident)
      profilerList = site['sites']['profilers']
      if len(profilerList) > 0:
         for profilerName in profilerList:
            try :
                pdc = RefTimePointDataRetrieve.retrieve('profiler', None, PARAMETERS,
                                                 keyId='validTime', constraint={'profilerId':profilerName},
                                                 maxSize=1)
            except NoDataException.NoDataException:
                _Logger.info("Error reading profiler " + profilerName)
                profilerList.remove(profilerName)
                continue
            validTimes = pdc.keys()
            validTimes.sort(reverse=True)
            data = pdc[validTimes[0]]
            try:
               self.vwpObsDict[profilerName] = self.__readProfilerData(data,profilerName)        
            except InValid:
               _Logger.info("Error reading profiler data")
         return profilerList
      else:
         return []

   def __processVWP(self):
      while True:
         try:
            code, fname, directory = self.FAMQueue.get_nowait( ) 
            if code == 0:        # end thread
               _Logger.info('Got exit request')
               self.TextQueue.abort()
               self.TextQueue.join()
               raise SystemExit

            if code in [gamin.GAMChanged, gamin.GAMCreated]:
               if 'profiler' in directory:
                  self.__processProfilerData(os.path.join(directory, fname))
               elif 'VWP' in directory:
                  self.__processRadarData(os.path.join(directory, fname))
	       elif 'acars' in directory:
		  self.__processAcarsData(os.path.join(directory, fname))

         except Queue.Empty:
            break

   def __processMetars(self):
      while True:
         try:
            ignored, package = self._incomingTextQueue.get_nowait()
            #
            # If there's a product of interest
            if package.msg.src == 'mtrs' and \
                   package.msg.ident in self.metarList:
               try:
                  self.sfcObsDict[package.msg.ident] = self.__getMetarWind(package.msg.ident,package.time)
                  self.__writeMetarOutput(package.msg.ident)
                  
               except InValid:
                  pass

         except Queue.Empty:
            try:
               self.TextQueue.subscribe()
            except Exception, e:
               _Logger.exception('Cannot connect to event server: %s', str(e))
            break
    
   def processMetarData(self, siteID):
       try:
           self.sfcObsDict[siteID] = self.__getMetarWind(siteID)
       except InValid:
           pass
        

   def __processLatestData(self):
      """
      Initially process all files found in gamin monitored directories
      """
      req_time = time.time()    
      for m in self.metarList:
         #
         # Get latest observation
         try:
            self.sfcObsDict[m] = self.__getMetarWind(m,req_time)
            
         except InValid:
            pass

      for f in [self.__getLatestFile(d) for d in self.radarDirs]:
         if 'VWP' in f:
            self.__processRadarData(f)
         
      if self.profilerList:
         for p in self.profilerDirs.split(','):
            self.__processProfilerData(self.__getLatestFile(p))

      if self.acarsList:
	 for a in self.acarsDirs.split(','):
	    self.__processAcarsData(self.__getLatestFile(a))


   def paths(self):
      """
      Public method so that AvnDIS parent can properly set up directories to monitor
      """
      paths = self.radarDirs
      if self.profilerList:
         for p in self.profilerDirs.split(','):
            paths.append(p)
      
      if self.acarsList:
	 for a in self.acarsDirs.split(','):
	    paths.append(a)

      return paths

   def run(self):
      #
      try:    
         self.__processLatestData()         
         while 1:
            self.__processVWP()
            self.__processMetars()
            time.sleep(self.__TimeOut)

      except SystemExit:
         raise
      except Exception:
         _Logger.error('Unexpected error')

