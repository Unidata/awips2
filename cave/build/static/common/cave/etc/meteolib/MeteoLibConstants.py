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
import math

KELVIN_ZERO = 273.15
FLAG = 1e37
FLG = 99998.0
TOP_FLG = 99999.0
INVALID = -9999.0
MISSING = -9998.0
SURFACE_EMISS = 0.95
STEFMAN_BOLTZMANN_CONSTANT = 8.12e-11
LOW_CLOUD_COEFF=0.80
MID_CLOUD_COEFF=0.67
HIGH_CLOUD_COEFF=0.22
GRAVITY = 980.0

DEG2RAD = math.pi / 180
TWOPI = math.pi * 2
YRLEN = 365.2563
ECC = 0.0167330
SDECMX = 0.40927
VEDAY = 79.25
CN = 1.0
S = 1.94
DRYLAPS = 0.009767
GE=9.8