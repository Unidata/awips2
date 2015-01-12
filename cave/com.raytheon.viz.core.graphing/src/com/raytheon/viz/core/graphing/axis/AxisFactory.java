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

package com.raytheon.viz.core.graphing.axis;

import java.text.SimpleDateFormat;

/**
 * Factory methods for generating axes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 4, 2007             njensen     Initial creation
 * Jul 28, 2014 3429       mapeters    Removed unused methods, variables, 
 *                                     and imports
 * 
 * </pre>
 * 
 * @author njensen
 */
public class AxisFactory {

    protected static final SimpleDateFormat TIME_FORMAT = new SimpleDateFormat(
            "HH:mm'Z'");

    protected static final SimpleDateFormat DAY_OF_WEEK_FORMAT = new SimpleDateFormat(
            "EEE");

    protected static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(
            "ddMMMyy");

    protected static final SimpleDateFormat REF_TIME_FORMAT = new SimpleDateFormat(
            "dd.HH");

    protected static final SimpleDateFormat FCST_TIME_FORMAT = new SimpleDateFormat(
            "HH'Z' EEE");

    private AxisFactory() {

    }

    /**
     * Builds a linear number axis
     * 
     * @param orientation
     *            the orientation of the axis
     * @param min
     *            the min value of the axis
     * @param max
     *            the max value of the axis
     * @param units
     *            the units of the axis
     * @return the initialized number axis
     */
    public static NumberAxis buildNumberAxis(IAxis.Orientation orientation,
            double min, double max, String units) {
        NumberAxis axis = new NumberAxis(orientation, units);
        axis.setRange(min, max);

        return axis;
    }

}
