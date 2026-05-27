/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.mpe.ui.colors;

import java.util.List;

import com.raytheon.viz.hydrocommon.colorscalemgr.ColorManager;
import com.raytheon.viz.hydrocommon.util.MPEColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

public class MPEColorManager extends ColorManager {

    private static final String applicationName = "hmapmpe";

    public MPEColorManager() {
        List<NamedColorUseSet> tmp = MPEColors.build_mpe_colors();
        populateColorUseSets(tmp);
        populateDefaultColorUseSets(tmp);
    }

    @Override
    public String getApplicationName() {
        return applicationName;
    }

}
