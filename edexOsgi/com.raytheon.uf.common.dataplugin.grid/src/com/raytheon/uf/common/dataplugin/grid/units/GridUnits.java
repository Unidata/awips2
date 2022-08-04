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

import javax.measure.Unit;
import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Force;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Time;

import com.raytheon.uf.common.units.CustomUnits;

import si.uom.NonSI;
import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.format.SimpleUnitFormat;
import tec.uom.se.unit.BaseUnit;
import tec.uom.se.unit.MetricPrefix;
import tec.uom.se.unit.Units;

/**
 * Provide alias for common units used in grid.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 20, 2009           mschenke  Initial creation
 * Sep 22, 2015  4498     nabowle   Add PSU.
 * May 05, 2016  5451     jschmid   Add alias for Celsius
 * Apr 15, 2019  7596     lsingh    Added units that removed during JSR-363
 *                                  upgrade
 * Jun 24, 2021  8570     randerso  Added BIT unit definition
 * Jan 12, 2022  8739     randerso  Added additional units used by LDADDecoder
 * Mar 14, 2022  8814     randerso  Added symbol for RANKINE
 *
 * </pre>
 *
 * @author mschenke
 */

public class GridUnits {
    /**
     * Holds the standard gravity constant: 9.80665 m/sÂ² exact.
     */
    private static final int STANDARD_GRAVITY_DIVIDEND = 980_665;

    private static final int STANDARD_GRAVITY_DIVISOR = 100_000;

    /**
     * A unit of force equal to <code>9.80665 N</code> (standard name
     * <code>kgf</code>).
     */
    protected static final Unit<Force> KILOGRAM_FORCE = Units.NEWTON
            .multiply(STANDARD_GRAVITY_DIVIDEND)
            .divide(STANDARD_GRAVITY_DIVISOR);

    /**
     * A unit of pressure equal to the average pressure of the Earth's
     * atmosphere at sea level (standard name <code>atm</code>).
     */
    protected static final Unit<Pressure> ATMOSPHERE = Units.PASCAL
            .multiply(101_325);

    /**
     * A unit of time equal to one twelfth of a year (standard name
     * <code>month</code>).
     */
    public static final Unit<Time> MONTH = Units.YEAR.divide(12);

    public static final BaseUnit<Dimensionless> PSU = new BaseUnit<>("PSU");

    public static boolean register() {
        final SimpleUnitFormat DEFAULT_FORMAT = SimpleUnitFormat
                .getInstance(SimpleUnitFormat.Flavor.Default);

        // UCUM format
        final SimpleUnitFormat ASCII_FORMAT = SimpleUnitFormat
                .getInstance(SimpleUnitFormat.Flavor.ASCII);

        DEFAULT_FORMAT.alias(SI.METRE, "gpm");
        ASCII_FORMAT.alias(SI.METRE, "gpm");
        DEFAULT_FORMAT.alias(MetricPrefix.MILLI(NonSI.BAR), "mb");
        ASCII_FORMAT.alias(MetricPrefix.MILLI(NonSI.BAR), "mb");
        DEFAULT_FORMAT.alias(SI.CELSIUS, "C");
        ASCII_FORMAT.alias(SI.CELSIUS, "C");
        DEFAULT_FORMAT.alias(SI.CELSIUS, "Celsius");
        ASCII_FORMAT.alias(SI.CELSIUS, "Celsius");
        DEFAULT_FORMAT.alias(USCustomary.FAHRENHEIT, "F");
        ASCII_FORMAT.alias(USCustomary.FAHRENHEIT, "F");
        DEFAULT_FORMAT.alias(USCustomary.FAHRENHEIT, "degree_fahrenheit");
        ASCII_FORMAT.alias(USCustomary.FAHRENHEIT, "degree_fahrenheit");
        DEFAULT_FORMAT.alias(USCustomary.RANKINE, "°R");
        ASCII_FORMAT.alias(USCustomary.RANKINE, "degree_rankine");
        DEFAULT_FORMAT.alias(NonSI.DEGREE_ANGLE, "deg");
        ASCII_FORMAT.alias(NonSI.DEGREE_ANGLE, "deg");
        DEFAULT_FORMAT.alias(NonSI.DEGREE_ANGLE, "Degree");
        ASCII_FORMAT.alias(NonSI.DEGREE_ANGLE, "Degree");
        DEFAULT_FORMAT.alias(NonSI.DEGREE_ANGLE, "degree_angle");
        ASCII_FORMAT.alias(NonSI.DEGREE_ANGLE, "degree_angle");
        DEFAULT_FORMAT.alias(NonSI.DEGREE_ANGLE, "°");
        DEFAULT_FORMAT.alias(USCustomary.KNOT, "kt");
        ASCII_FORMAT.alias(USCustomary.KNOT, "kt");
        DEFAULT_FORMAT.alias(SI.SECOND, "sec");
        ASCII_FORMAT.alias(SI.SECOND, "sec");
        DEFAULT_FORMAT.alias(SI.METRE, "meters");
        ASCII_FORMAT.alias(SI.METRE, "meters");
        DEFAULT_FORMAT.alias(NonSI.BAR, "bar");
        ASCII_FORMAT.alias(NonSI.BAR, "bar");
        DEFAULT_FORMAT.alias(NonSI.INCH_OF_MERCURY, "inHg");
        ASCII_FORMAT.alias(NonSI.INCH_OF_MERCURY, "inHg");
        DEFAULT_FORMAT.alias(NonSI.MILLIMETRE_OF_MERCURY, "mmHg");
        ASCII_FORMAT.alias(NonSI.MILLIMETRE_OF_MERCURY, "mmHg");
        DEFAULT_FORMAT.alias(KILOGRAM_FORCE, "kgf");
        ASCII_FORMAT.alias(KILOGRAM_FORCE, "kgf");
        DEFAULT_FORMAT.alias(ATMOSPHERE, "atm");
        ASCII_FORMAT.alias(ATMOSPHERE, "atm");
        DEFAULT_FORMAT.alias(USCustomary.RANKINE, "degree_rankine");
        ASCII_FORMAT.alias(USCustomary.RANKINE, "degree_rankine");
        DEFAULT_FORMAT.alias(MONTH, "month");
        ASCII_FORMAT.alias(MONTH, "month");

        DEFAULT_FORMAT.label(CustomUnits.DECIBEL, "dB");
        ASCII_FORMAT.label(CustomUnits.DECIBEL, "dB");
        DEFAULT_FORMAT.label(CustomUnits.BIT, CustomUnits.BIT.getSymbol());
        ASCII_FORMAT.label(CustomUnits.BIT, CustomUnits.BIT.getSymbol());
        DEFAULT_FORMAT.label(PSU, PSU.getSymbol());
        ASCII_FORMAT.label(PSU, PSU.getSymbol());

        return true;
    }

}
