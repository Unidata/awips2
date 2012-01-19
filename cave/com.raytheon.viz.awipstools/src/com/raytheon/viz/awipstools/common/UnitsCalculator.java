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

package com.raytheon.viz.awipstools.common;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

/**
 * UnitsCalculator.
 * 
 * Tool for viewing conversion information for various values.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 08/28/07     433         Eric Babin    Initial Creation.
 * 25Oct07      499         Eric Babin    Build 9 cleanup.
 * </pre>
 * 
 * @author Eric Babin
 * @version 1
 */
public class UnitsCalculator {

    public static final String TEMP = "Temperature";

    public static final String CEL = "Celsius";

    public static final String FAH = "Fahrenheit";

    public static final String KEL = "Kelvin";

    public static final String SPD = "Speed";

    public static final String KNT = "knots";

    public static final String MPS = "mps";

    public static final String MPH = "mph";

    public static final String FPS = "fps";

    public static final String KPH = "kph";

    public static final String DIS = "Distance";

    public static final String METER = "m";

    public static final String KM = "km";

    public static final String SM = "SM";

    public static final String CM = "cm";

    public static final String FT = "ft";

    public static final String IN = "in";

    public static final String NMI = "Nmi";

    public static final String MILE = "mi";

    public static final String TIME = "Time";

    public static final String DAY = "day";

    public static final String HR = "hr";

    public static final String MIN = "min";

    public static final String SEC = "sec";

    public static final String PRESSURE = "Pressure";

    public static final String INHG = "inHg";

    public static final String MB = "mb";

    public static final String HPA = "hPa";

    public static final String PA = "Pa";

    /**
     * Method to calculate the units
     * 
     * @param from
     * @param to
     * @param originalValue
     * @return the converted value.
     */
    public static double convertUnits(String from, String to,
            double originalValue) {

        if (from.equals(UnitsCalculator.CEL)
                || from.equals(UnitsCalculator.FAH)
                || from.equals(UnitsCalculator.KEL)) {
            return convertTemperatureFromTo(from, to, originalValue);
        } else if (from.equals(UnitsCalculator.KNT)
                || from.equals(UnitsCalculator.MPS)
                || from.equals(UnitsCalculator.MPH)
                || from.equals(UnitsCalculator.FPS)
                || from.equals(UnitsCalculator.KPH)) {
            return convertSpeedFromTo(from, to, originalValue);
        } else if (from.equals(UnitsCalculator.METER)
                || from.equals(UnitsCalculator.KM)
                || from.equals(UnitsCalculator.SM)
                || from.equals(UnitsCalculator.CM)
                || from.equals(UnitsCalculator.FT)
                || from.equals(UnitsCalculator.IN)
                || from.equals(UnitsCalculator.NMI)
                || from.equals(UnitsCalculator.MILE)) {
            return convertDistanceFromTo(from, to, originalValue);
        } else if (from.equals(UnitsCalculator.DAY)
                || from.equals(UnitsCalculator.HR)
                || from.equals(UnitsCalculator.MIN)
                || from.equals(UnitsCalculator.SEC)) {
            return convertTimeFromTo(from, to, originalValue);
        } else if (from.equals(UnitsCalculator.INHG)
                || from.equals(UnitsCalculator.MB)
                || from.equals(UnitsCalculator.HPA)
                || from.equals(UnitsCalculator.PA)) {
            return convertPressureFromTo(from, to, originalValue);
        } else {
            return originalValue;
        }

    }

    /**
     * Converts pressure units.
     * 
     * @param from
     * @param to
     * @param originalValue
     * @return theConverted value
     */
    public static double convertPressureFromTo(String from, String to,
            double originalValue) {

        double pressure = originalValue;

        if (from.equalsIgnoreCase(to)) {
            return originalValue;
        }

        if (from.equalsIgnoreCase("inHg")) {
            if (to.equalsIgnoreCase("mb")) {
                return NonSI.INCH_OF_MERCURY
                        .getConverterTo(SI.MILLI(NonSI.BAR)).convert(
                                originalValue);
            } else if (to.equalsIgnoreCase("hPa")) {
                return NonSI.INCH_OF_MERCURY
                        .getConverterTo(SI.HECTO(SI.PASCAL)).convert(
                                originalValue);
            } else if (to.equalsIgnoreCase("Pa")) {
                return NonSI.INCH_OF_MERCURY.getConverterTo(SI.PASCAL).convert(
                        originalValue);
            }
        } else if (from.equalsIgnoreCase("mb")) {
            pressure = SI.MILLI(NonSI.BAR)
                    .getConverterTo(NonSI.INCH_OF_MERCURY).convert(
                            originalValue);
        } else if (from.equalsIgnoreCase("hPa")) {
            pressure = SI.HECTO(SI.PASCAL)
                    .getConverterTo(NonSI.INCH_OF_MERCURY).convert(
                            originalValue);
        } else if (from.equalsIgnoreCase("Pa")) {
            pressure = SI.PASCAL.getConverterTo(NonSI.INCH_OF_MERCURY).convert(
                    originalValue);
        }

        if (to.equalsIgnoreCase("inHg")) {
            return pressure;
        } else {
            return convertPressureFromTo("inHg", to, pressure);
        }

    }

    /**
     * Converts speed units.
     * 
     * @param from
     * @param to
     * @param originalValue
     * @return the converted speed unit.
     */
    public static double convertSpeedFromTo(String from, String to,
            double originalValue) {

        double knots = 0.0d;

        if (from.equalsIgnoreCase(to)) {
            return originalValue;
        }

        if (from.equals("knots")) {
            if (to.equalsIgnoreCase("mps")) {
                return (originalValue * 1852.0) / 3600.0;
            } else if (to.equalsIgnoreCase("mph")) {
                return (originalValue * 1.852) / 1.609344;
            } else if (to.equalsIgnoreCase("fps")) {
                return originalValue / .5924838;
            } else if (to.equalsIgnoreCase("kph")) {
                return originalValue * 1.852;
            }
        } else if (from.equals("mps")) {
            knots = originalValue / 1852 * 3600.0;
        } else if (from.equals("mph")) {
            knots = (originalValue * 1.609344) / 1.852;
        } else if (from.equals("fps")) {
            knots = originalValue * .5924838;
        } else if (from.equals("kph")) {
            knots = originalValue / 1.852;
        }

        if (to.equalsIgnoreCase("knots")) {
            return knots;
        } else {
            return convertSpeedFromTo("knots", to, knots);
        }
    }

    /**
     * Converts temperature units.
     * 
     * @param from
     * @param to
     * @param originalValue
     * @return the converted temperature unit.
     */

    public static double convertTemperatureFromTo(String from, String to,
            double originalValue) {

        if (from.equalsIgnoreCase(to)) {
            return originalValue;
        }

        if (from.equalsIgnoreCase("Celsius")) {
            if (to.equalsIgnoreCase("Fahrenheit")) {
                return SI.CELSIUS.getConverterTo(NonSI.FAHRENHEIT).convert(
                        originalValue);
            } else if (to.equalsIgnoreCase("Kelvin")) {
                return SI.CELSIUS.getConverterTo(SI.KELVIN).convert(
                        originalValue);
            }
        } else if (from.equalsIgnoreCase("Fahrenheit")) {
            if (to.equalsIgnoreCase("Celsius")) {
                return NonSI.FAHRENHEIT.getConverterTo(SI.CELSIUS).convert(
                        originalValue);

            } else if (to.equalsIgnoreCase("Kelvin")) {
                return NonSI.FAHRENHEIT.getConverterTo(SI.KELVIN).convert(
                        originalValue);
            }
        } else if (from.equals("Kelvin")) {
            if (to.equalsIgnoreCase("Celsius")) {
                return SI.KELVIN.getConverterTo(SI.CELSIUS).convert(
                        originalValue);

            } else if (to.equalsIgnoreCase("Fahrenheit")) {
                return SI.KELVIN.getConverterTo(NonSI.FAHRENHEIT).convert(
                        originalValue);

            }
        }

        return originalValue;
    }

    /**
     * Converts time units
     * 
     * @param from
     * @param to
     * @param originalValue
     * @return the converted time unit.
     */
    public static double convertTimeFromTo(String from, String to,
            double originalValue) {

        double day = originalValue;

        if (from.equalsIgnoreCase(to)) {
            return originalValue;
        }

        if (from.equalsIgnoreCase("day")) {
            if (to.equalsIgnoreCase("hr")) {
                return originalValue * 24;
            } else if (to.equalsIgnoreCase("min")) {
                return originalValue * 1440;
            } else if (to.equalsIgnoreCase("sec")) {
                return originalValue * 86400;
            }
        } else if (from.equalsIgnoreCase("hr")) {
            day = originalValue / 24.0;
        } else if (from.equalsIgnoreCase("min")) {
            day = originalValue / 1440;
        } else if (from.equalsIgnoreCase("sec")) {
            day = originalValue / 86400;
        }

        if (to.equalsIgnoreCase("day")) {
            return day;
        } else {
            return convertTimeFromTo("day", to, day);
        }

    }

    /**
     * Converts distance units.
     * 
     * @param from
     * @param to
     * @param originalValue
     * @return the converted distance unit.
     */
    public static double convertDistanceFromTo(String from, String to,
            double originalValue) {

        double meters = originalValue;

        if (from.equalsIgnoreCase(to)) {
            return originalValue;
        }

        if (from.equalsIgnoreCase("m")) {
            if (to.equalsIgnoreCase("km")) {
                return originalValue / 1000.0;
            } else if (to.equalsIgnoreCase("SM")) {
                return SI.METER.getConverterTo(NonSI.MILE).convert(
                        originalValue);
            } else if (to.equalsIgnoreCase("cm")) {
                return originalValue * 100;
            } else if (to.equalsIgnoreCase("ft")) {
                return SI.METER.getConverterTo(NonSI.FOOT).convert(
                        originalValue);
            } else if (to.equalsIgnoreCase("in")) {
                return SI.METER.getConverterTo(NonSI.INCH).convert(
                        originalValue);
            } else if (to.equalsIgnoreCase("Nmi")) {
                return SI.METER.getConverterTo(NonSI.NAUTICAL_MILE).convert(
                        originalValue);
            } else if (to.equalsIgnoreCase("mi")) {
                return SI.METER.getConverterTo(NonSI.MILE).convert(
                        originalValue);
            }
        } else if (from.equalsIgnoreCase("km")) {
            meters = originalValue * 1000;
        } else if (from.equalsIgnoreCase("SM")) {
            meters = NonSI.MILE.getConverterTo(SI.METER).convert(originalValue);
        } else if (from.equalsIgnoreCase("cm")) {
            meters = originalValue / 100;
        } else if (from.equalsIgnoreCase("ft")) {
            meters = NonSI.FOOT.getConverterTo(SI.METER).convert(originalValue);
        } else if (from.equalsIgnoreCase("in")) {
            meters = NonSI.INCH.getConverterTo(SI.METER).convert(originalValue);
        } else if (from.equalsIgnoreCase("Nmi")) {
            meters = NonSI.NAUTICAL_MILE.getConverterTo(SI.METER).convert(
                    originalValue);
        } else if (from.equalsIgnoreCase("mi")) {
            meters = NonSI.MILE.getConverterTo(SI.METER).convert(originalValue);
        }

        if (to.equalsIgnoreCase("m")) {
            return meters;
        } else {
            return convertDistanceFromTo("m", to, meters);
        }

    }

}
