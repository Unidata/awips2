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
package com.raytheon.viz.pointdata.units;

import javax.measure.quantity.Length;
import javax.measure.quantity.Force;
import javax.measure.quantity.Velocity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
/**
 * Contains references to units used by point data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2009     2338    jsanchez   Initial creation.
 * 
 * </pre>
 * 
 * @author jsanchez
 */
public class PlotUnits {
    
    private PlotUnits() {

    }

    public static final Unit<Velocity> KNOTS = NonSI.KNOT;
    
    public static final Unit<Length> HUNDRED_FEET = SI.HECTO(NonSI.FOOT);
    
    public static final Unit<Length> KILOMILE = SI.KILO(NonSI.MILE);

    public static void register() {
        UnitFormat.getUCUMInstance().label(PlotUnits.KNOTS, "kts");
        
        UnitFormat.getUCUMInstance().label(PlotUnits.HUNDRED_FEET, "hft");
        
        UnitFormat.getUCUMInstance().label(PlotUnits.KILOMILE, "kmi");

    }
}
