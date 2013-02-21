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
package com.raytheon.uf.common.dataplugin.radar.units;

import javax.measure.unit.BaseUnit;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.image.units.PiecewisePixel;


/**
 * RadarUnits: Defines a set of radar unit conversions
 * 
 * Sample Usage (Convert velocity 2x resolution pixel to m/s):
 * 
 * <PRE>
 * Unit&lt;?&gt; u1 = RadarUnits.VELOCITY_PIXEL_2X;
 * 
 * Unit&lt;?&gt; u2 = SI.METER_PER_SECOND;
 * 
 * UnitConverter uConv = u1.getConverterTo(u2);
 * 
 * double inMs = uConv.convert(32.0);
 * 
 * </PRE>
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jul 26, 2007             chammack    Initial Creation.
 *    Oct 09, 2007 465         randerso    Updated to better support level 3
 *   
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class RadarUnits {

    public static final BaseUnit<Reflectivity> DBZ = new BaseUnit<Reflectivity>(
            "dBZ");

    public static final Unit<Reflectivity> REFLECTIVITY_PIXEL = new PiecewisePixel<Reflectivity>(
            RadarUnits.DBZ, new double[] { 16, 33, 240, 255 }, new double[] {
                    -32.5, -20.0, 75.0, 94.5 });

    public static boolean register() {
        UnitFormat.getUCUMInstance().label(RadarUnits.DBZ, "dBZ");
        UnitFormat.getUCUMInstance().label(SI.KILO(NonSI.FOOT), "kft");
        UnitFormat.getUCUMInstance().label(NonSI.KNOT, "kts");
        return true;
    }

}
