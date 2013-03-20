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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Analysis.py
# Class for Analysis of grid data producing summary statistics.
#
# Author: hansen
# ----------------------------------------------------------------------------
import string, types

from com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID

class Analysis:
   def __init__(self, histoSampler):
      self._histoSampler = histoSampler
      if type(histoSampler) is types.DictionaryType:
         try:
            sampler = histoSampler[histoSampler.keys()[0]]
         except IndexError:
            sampler = None
      else:
         sampler = histoSampler
      self.__parmID = sampler.getParmID()
      if self.__parmID.equals(ParmID()):
         self.__parmID = None

   def getParmID(self, parmNameAndLevel, databaseID):
       index = string.find(parmNameAndLevel, "_")
       if index == -1:
           name = parmNameAndLevel
           level = "SFC"
       else:
           name = parmNameAndLevel[0:index]
           level = parmNameAndLevel[index+1:]
       return ParmID(name, databaseID, level)

   def createStats(self, analysisDef, referenceID, timeRange, component):
       " Create the Statistic dictionary for the forecast "
       statDict = {}

       sampler = self._histoSampler
       for analysis in analysisDef:
          if len(analysis) == 2:
             parmNameAndLevel, method = analysis
             args = None
          else:
             parmNameAndLevel, method, args = analysis
          dbID = self.__parmID.getDbId()
          parmID = self.getParmID(parmNameAndLevel, dbID)
          if type(self._histoSampler) is types.DictionaryType:
             sampler = self._histoSampler[parmNameAndLevel]
          if sampler is not None:             
             parmHisto = sampler.getParmHisto(parmID, referenceID, timeRange.toJavaObj())
          statName = parmNameAndLevel + "__" + method.__name__
          if parmHisto.getSampleLen() == 0 or sampler is None:
             stats = None
          else:
             if args is not None:
                stats = method(parmHisto, timeRange, component, args)
                if len(args) > 0 and type(args[0]) is types.IntType:
                   statName = statName + "_" + `args[0]`
             else:
                stats = method(parmHisto, timeRange, component)
          if parmNameAndLevel not in statDict.keys():
             statDict[parmNameAndLevel] = stats
          statDict[statName] = stats

       return statDict

if __name__ == '__main__':
    print "Cannot run stand-alone"
