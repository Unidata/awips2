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
package com.raytheon.viz.gfe.preferences;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PreferenceConstants;
import com.raytheon.viz.gfe.PythonPreferenceStore;

/**
 * Encapsulates access to the Time Scale Displayed Periods preferences
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 24, 2009      #2212 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TimeScaleDisplayedPeriodsPreference {

    private static String[] timeScaleDisplayedPeriods;

    private static PythonPreferenceStore preferenceStore = Activator
            .getDefault().getPreferenceStore();

    // No instantiation
    private TimeScaleDisplayedPeriodsPreference() {

    }

    public static String getPreferenceKey() {
        return PreferenceConstants.GFE_TIME_SCALE_DISPLAY_PERIODS;
    }

    public synchronized static String[] getTimeScaleDisplayedPeriods() {
        if (timeScaleDisplayedPeriods == null) {

            timeScaleDisplayedPeriods = preferenceStore
                    .getStringArray(getPreferenceKey());

            if (timeScaleDisplayedPeriods == null) {
                timeScaleDisplayedPeriods = new String[0];
            }

        }
        return timeScaleDisplayedPeriods;
    }

    public static void removeTimeScaleDisplayedPeriod(String period) {
        List<String> periods = new ArrayList<String>();
        periods.addAll(Arrays.asList(getTimeScaleDisplayedPeriods()));

        if (periods.remove(period)) {
            setTimeScaleDisplayedPeriods(periods.toArray(new String[periods
                    .size()]));
        }
    }

    public synchronized static void setTimeScaleDisplayedPeriods(
            String[] timeScaleDisplayPeriods) {
        TimeScaleDisplayedPeriodsPreference.timeScaleDisplayedPeriods = timeScaleDisplayPeriods;

        preferenceStore.setValue(getPreferenceKey(), timeScaleDisplayPeriods);
    }
}
