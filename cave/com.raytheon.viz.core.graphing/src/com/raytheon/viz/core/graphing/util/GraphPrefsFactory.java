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
package com.raytheon.viz.core.graphing.util;

import java.util.ArrayList;

import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.core.style.graph.AxisScale;
import com.raytheon.viz.core.style.graph.GraphPreferences;

/**
 * Loads GraphPreferences from graph style rules
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2007            njensen     Initial creation	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GraphPrefsFactory {

    public static GraphPreferences buildPreferences(String parameter,
            SingleLevel level) throws VizStyleException {
        GraphPreferences preferences = getPreferences(parameter, level);
        if (preferences == null) {
            preferences = new GraphPreferences();
        }

        if (preferences.getAxisScale() == null) {
            preferences.setAxisScale(new AxisScale());
        }
        if (preferences.getAxisScale().getBaseVal() == null) {
            // base value defaults to 0
            preferences.getAxisScale().setBaseVal(0.0);
        }

        if (preferences.getAxisScale().getMinimumRange() == null) {
            if (preferences.getAxisScale().getScaleType() == AxisScale.Type.LOG) {
                // defaults to -10 for log
                preferences.getAxisScale().setMinimumRange(-10.0);
            } else {
                // defaults to 10 for linear
                preferences.getAxisScale().setMinimumRange(10.0);
            }
        }

        return preferences;
    }

    public static GraphPreferences buildSimplePreferences(boolean log,
            double min, double max) {
        GraphPreferences preferences = new GraphPreferences();
        AxisScale scale = new AxisScale();

        scale.setExactMaxValue(true);
        scale.setExactMinValue(true);
        scale.setMaxValue(max);
        scale.setMinValue(min);
        scale.setBaseVal(0.0);

        if (log) {
            scale.setScaleType(AxisScale.Type.LOG);
            scale.setMinimumRange(-10.0);
        } else {
            scale.setScaleType(AxisScale.Type.LINEAR);
            scale.setMinimumRange(10.0);
        }
        preferences.setAxisScale(scale);

        return preferences;
    }

    private static GraphPreferences getPreferences(String parameter,
            SingleLevel level) throws VizStyleException {
        GraphPreferences prefs = null;
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(level);
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(parameter);
        match.setParameterName(paramList);
        StyleRule sr = StyleManager.getInstance().getStyleRule(
                StyleManager.StyleType.GRAPH, match);
        if (sr != null) {
            prefs = (GraphPreferences) sr.getPreferences();
        }
        return prefs;
    }

}
