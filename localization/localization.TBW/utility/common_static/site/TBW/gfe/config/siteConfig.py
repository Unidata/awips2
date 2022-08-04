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
#THIS FILE GENERATED AUTOMATICALLY DURING GFE INSTALL **

import os, socket


GFESUITE_HOME = os.environ['EDEX_HOME']+"/../GFESuite"
GFESUITE_SERVER = "localhost"
GFESUITE_PORT   = '98000000'
GFESUITE_SITEID = 'TBW'
GFESUITE_PRDDIR = GFESUITE_HOME+"/products"
GFESUITE_MHSID  = 'TBW' 
GFESUITE_LOGDIR = GFESUITE_HOME+"/logs/"+GFESUITE_SITEID
