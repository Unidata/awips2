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
package com.raytheon.uf.common.dataplugin.grid.units;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.UnitFormat;

/**
 * Provide alias for common units used in grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GridUnits {

    public static boolean register() {
        UnitFormat.getInstance().alias(SI.METER, "gpm");
        UnitFormat.getUCUMInstance().alias(SI.METER, "gpm");
        UnitFormat.getInstance().alias(SI.MILLI(NonSI.BAR), "mb");
        UnitFormat.getUCUMInstance().alias(SI.MILLI(NonSI.BAR), "mb");
        UnitFormat.getInstance().alias(SI.CELSIUS, "C");
        UnitFormat.getUCUMInstance().alias(SI.CELSIUS, "C");
        UnitFormat.getInstance().alias(NonSI.FAHRENHEIT, "F");
        UnitFormat.getUCUMInstance().alias(NonSI.FAHRENHEIT, "F");
        UnitFormat.getInstance().alias(NonSI.DEGREE_ANGLE, "deg");
        UnitFormat.getUCUMInstance().alias(NonSI.DEGREE_ANGLE, "deg");
        UnitFormat.getInstance().alias(NonSI.DEGREE_ANGLE, "Degree");
        UnitFormat.getUCUMInstance().alias(NonSI.DEGREE_ANGLE, "Degree");
        UnitFormat.getInstance().alias(NonSI.KNOT, "kt");
        UnitFormat.getUCUMInstance().alias(NonSI.KNOT, "kt");
        UnitFormat.getInstance().alias(SI.SECOND, "sec");
        UnitFormat.getUCUMInstance().alias(SI.SECOND, "sec");
        UnitFormat.getInstance().alias(SI.METER, "meters");
        UnitFormat.getUCUMInstance().alias(SI.METER, "meters");
        return true;
    }

}
