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
import numpy
from numpy import equal
from numpy import where

def execute1(precipTypeStation,POP_hour_bestCatStation):
    tmp = where(equal(POP_hour_bestCatStation, 1), precipTypeStation, -9999)
    tmp[tmp == 0] = 0
    tmp[tmp == 1] = 71
    tmp[tmp == 2] = 89
    tmp[tmp == 3] = 79
    return tmp.astype('float32')

def execute2(CRAIN, CFRZR, CICEP, CSNOW):
    tmp = 1.0*CRAIN+2.0*CFRZR+4.0*CICEP+8.0*CSNOW
    # convert numeric value into symbol
    tmp[tmp <= 0] = 0
    tmp[tmp > 9] = 0
    tmp[tmp == 1] = 79
    tmp[tmp == 2] = 89
    tmp[tmp == 3] = 71
    tmp[tmp == 4] = 77
    tmp[tmp == 5] = 80
    tmp[tmp == 6] = 67
    tmp[tmp == 7] = 69
    tmp[tmp == 8] = 47
    tmp[tmp == 9] = 183
    return tmp

def execute3(CRAIN6hr, CFRZR6hr, CICEP6hr, CSNOW6hr):
    return execute2(CRAIN6hr, CFRZR6hr, CICEP6hr, CSNOW6hr)

def execute4(CRAIN12hr, CFRZR12hr, CICEP12hr, CSNOW12hr):
    return execute2(CCRAIN12hr, CFRZR12hr, CICEP12hr, CSNOW12hr)

def execute5(CPOZP3hr, CPOFP3hr):
    # probablilities are represented between 0 and 1
    tmp= 2.0*CPOZP3hr+8.0*CPOFP3hr
    tmp[tmp <= 0] = 0
    tmp[tmp > 15] = 0
    tmp[tmp == 1] = 79
    tmp[tmp == 2] = 71
    tmp[tmp == 3] = 71
    tmp[tmp == 4] = 80
    tmp[tmp == 5] = 183
    tmp[tmp == 6] = 185
    tmp[tmp == 7] = 185
    tmp[tmp == 8] = 89
    tmp[tmp == 9] = 47
    tmp[tmp == 10] = 186
    tmp[tmp == 11] = 186
    tmp[tmp == 12] = 184
    tmp[tmp == 13] = 188
    tmp[tmp == 14] = 187
    tmp[tmp == 15] = 187
    return tmp
