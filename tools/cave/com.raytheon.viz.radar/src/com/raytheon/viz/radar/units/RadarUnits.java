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
package com.raytheon.viz.radar.units;

import java.text.ParseException;
import java.text.ParsePosition;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Velocity;
import javax.measure.unit.BaseUnit;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.viz.core.units.IUnitRegistrar;
import com.raytheon.viz.core.units.PiecewisePixel;

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
public class RadarUnits implements IUnitRegistrar {

    public RadarUnits() {

    }

    public static final BaseUnit<Reflectivity> DBZ = new BaseUnit<Reflectivity>(
            "dBZ");

    public static final Unit<Reflectivity> REFLECTIVITY_PIXEL = new PiecewisePixel<Reflectivity>(
            RadarUnits.DBZ, new double[] { 16, 33, 240, 255 }, new double[] {
                    -32.5, -20.0, 75.0, 94.5 });

    public static final Unit<Velocity> VELOCITY_PIXEL;
    static {
        UnitConverter ktsToMps = NonSI.KNOT
                .getConverterTo(SI.METERS_PER_SECOND);
        double[] knots = { -999, -247.0, -100.0, 100.0, 245.0 };
        double[] mps = new double[knots.length];

        for (int i = 0; i < mps.length; i++) {
            mps[i] = ktsToMps.convert(knots[i]);
        }

        VELOCITY_PIXEL = new PiecewisePixel<Velocity>(NonSI.KNOT, new double[] {
                1, 2, 26, 232, 255 }, mps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.units.IUnitRegistrar#register()
     */
    @Override
    public void register() {
        UnitFormat.getUCUMInstance().label(RadarUnits.DBZ, "dBZ");
        UnitFormat.getUCUMInstance().label(SI.KILO(NonSI.FOOT), "kft");
        UnitFormat.getUCUMInstance().label(NonSI.KNOT, "kts");
    }

    public static void main(String[] args) throws ParseException {
        Unit<Reflectivity> rp = RadarUnits.REFLECTIVITY_PIXEL;

        System.out.println();
        int[] testPix = { 0, 8, 16, 20, 33, 50, 120, 240, 250, 255, 300 };
        UnitConverter toDBZ = rp.getConverterTo(RadarUnits.DBZ);
        for (int p : testPix) {
            System.out.println("pixel: " + p + " = " + toDBZ.convert(p)
                    + RadarUnits.DBZ.toString());
        }

        System.out.println();
        double[] testDBZ = { -33, -32.5, -30, -20, -10, 0, 10, 20, 30, 40, 50,
                60, 75, 80, 94.5, 100 };
        UnitConverter toPixel = RadarUnits.DBZ.getConverterTo(rp);
        for (double r : testDBZ) {
            System.out.println(r + RadarUnits.DBZ.toString() + " = pixel: "
                    + (int) toPixel.convert(r));
        }

        Unit<?> imageUnit = RadarUnits.VELOCITY_PIXEL;
        Unit<?> dataUnit = UnitFormat.getUCUMInstance().parseProductUnit("m/s",
                new ParsePosition(0));
        dataUnit = dataUnit.plus(-64.5).times(0.5);

        UnitConverter dataToImage = dataUnit.getConverterTo(imageUnit);

        System.out.println();
        testPix = new int[] { 0, 1, 2, 26, 232, 255 };
        for (int p : testPix) {
            System.out.println("data: " + p + " image: "
                    + dataToImage.convert(p));
        }
    }
}
