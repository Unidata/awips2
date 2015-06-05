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

import copy

#
# Transforms a list of Java WarningRecords into a Python list of dictionaries
# that GFE expects for decoded VTEC strings (i.e. the active table)
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/19/08                      njensen        Initial Creation.
#    04/28/2015      #4027         randerso       Expunged Calendar from ActiveTableRecord
#    
# 
#

import VTECPartners
mappedPils = getattr(VTECPartners, "VTEC_MAPPED_PILS", {})

def transformActiveTableToPython(table):
    if hasattr(table,'size'):
        size = table.size()
        jconv = True
    else:
        size = len(table)
        jconv = False
        
    actTable = []
    for i in range(size):
        if jconv:
            atr = table.get(i)
        else:                 
            atr = table[i]               
        #construct the active table entries, without the geography
        template = {}
        template['vtecstr'] = atr.getVtecstr()
        template['etn'] = int(atr.getEtn())
        template['sig'] = atr.getSig()
        template['phen'] = atr.getPhen()
        if atr.getSegText():
            template['segText'] = atr.getSegText()
        if atr.getOverviewText():
            template['overviewText'] = atr.getOverviewText()
        template['phensig'] = atr.getPhensig()
        template['act'] = atr.getAct()
        template['seg'] = atr.getSeg()                
        template['startTime'] = atr.getStartTime().getTime() / 1000
        template['endTime'] = atr.getEndTime().getTime() / 1000
        template['ufn'] = atr.isUfn()
        template['officeid'] = atr.getOfficeid()
        template['purgeTime'] = atr.getPurgeTime().getTime() / 1000
        template['issueTime'] = atr.getIssueTime().getTime() / 1000
        template['state'] = "Decoded"
        template['xxxid'] = atr.getXxxid()
        
        template['pil'] = remapPil(template['phen'], template['sig'], atr.getPil())
        template['productClass'] = atr.getProductClass()
                        
        template['id'] = atr.getUgcZone()     
        
        template['rawMessage'] = atr.getRawmessage()           
        actTable.append(template)
        
    return actTable

def remapPil(phen, sig, pil):
        # remaps the product pil for certain phen/sig/pils.  The VTECDecoder
        # needs to relate hazards through all states from the same pil. Some
        # short-fused hazards issue in one pil and followup/cancel in
        # another pil.
        key = (phen, sig, pil)
        rPil = mappedPils.get(key,  pil)
        return rPil

def transfomActiveTableToThrift(table, mode='PRACTICE'):
    from dynamicserialize.dstypes.com.raytheon.uf.common.activetable.PracticeActiveTableRecord \
         import PracticeActiveTableRecord
    from dynamicserialize.dstypes.com.raytheon.uf.common.activetable.OperationalActiveTableRecord \
         import OperationalActiveTableRecord
    # TODO: Eliminate use of Calendar
    from dynamicserialize.dstypes.java.util import Date
    tableList = []

    if mode.upper()=='PRACTICE':
        recordCtor = PracticeActiveTableRecord
    else:
        recordCtor = OperationalActiveTableRecord
        
    for template in table:        
        atr = recordCtor()             
        atr.setVtecstr(template['vtecstr'])
        atr.setEtn(str(template['etn']))
        atr.setSig(template['sig'])
        atr.setPhen(template['phen'])
        if template.has_key('segText'):
            atr.setSegText(template['segText'])
        if template.has_key('overviewText'):
            atr.setOverviewText(template['overviewText'])
        atr.setPhensig(template['phensig'])
        atr.setAct(template['act'])
        atr.setSeg(template['seg'])
                               
        start = Date(template['startTime'] * 1000)        
        atr.setStartTime(start)
        end = Date(template['endTime'] * 1000)        
        atr.setEndTime(end)
        purge = Date(template['purgeTime'] * 1000)        
        atr.setPurgeTime(purge)
        issue = Date(template['issueTime'] * 1000)        
        atr.setIssueTime(issue)        
                
        atr.setUfn(template['ufn'])                
        atr.setOfficeid(template['officeid'])        
        # template['state'] = "Decoded"
        atr.setXxxid(template['xxxid'])
        atr.setPil(template['pil'])        
        atr.setProductClass(template['productClass'])                    
        atr.setUgcZone(template['id']) 
        atr.setRawmessage(template['rawMessage'])  
        
        tableList.append(atr)                                                 
        
    return tableList



                                  
