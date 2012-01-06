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
###

from datetime import datetime

def execute(timeObs, injuries, fatalities, remarks):
    "Calculate Sampling String."
    sampleStrings = list()
    for num in range(len(injuries)):
        timeVal = datetime.utcfromtimestamp(timeObs[num]/1000).strftime("%H%MZ");
        injuriesVal = injuries[num]
        fatalitiesVal = fatalities[num]
        remarksVal = remarks[num]
        if (injuriesVal > 0 or fatalitiesVal > 0):
            if (injuriesVal <= 0):
                casualtiesStr = ' *** ' + str(fatalitiesVal) + ' FATAL *** '
            elif (fatalitiesVal <= 0):
                casualtiesStr = ' *** ' + str(injuriesVal) + ' INJ *** '
            else:
               casualtiesStr = ' *** ' + str(fatalitiesVal) + ' FATAL, ' + str(injuriesVal) + ' INJ *** '
        else:
            casualtiesStr = ' '
        sampleStrings.append(timeVal + casualtiesStr + remarksVal)
    return sampleStrings

    