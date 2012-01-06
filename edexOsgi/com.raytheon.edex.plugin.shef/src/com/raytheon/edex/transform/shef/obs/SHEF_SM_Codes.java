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
package com.raytheon.edex.transform.shef.obs;

import java.util.Calendar;
import java.util.IllegalFormatConversionException;
import java.util.List;

import com.raytheon.uf.common.dataplugin.sfcobs.AncPrecip;
import com.raytheon.uf.common.dataplugin.sfcobs.AncTemp;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2010            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public enum SHEF_SM_Codes implements SHEF_Obs_Codes<ObsCommon> {

    // [ 0] Temperature Instantaneous
    TAIRZZZ("%s %3d", "TA|TAIRZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            int temp = -9999;
            Double tt = report.getTemp();
            if ((tt != null) && (tt > -9999)) {
                temp = Math.round(celsiusToFahr(tt.floatValue() - 273.15f));
            }
            if (temp > -9999) {
                buffer.append(String.format(format, name(), temp));
            }
            return buffer;
        }
    },
    // [ 1] Dew Point
    TDIRZZZ("%s %3d", "TD") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            int temp = -9999;
            Double tt = report.getDwpt();
            if ((tt != null) && (tt > -9999)) {
                temp = Math.round(celsiusToFahr(tt.floatValue() - 273.15f));
            }
            if (temp > -9999) {
                buffer.append(String.format(format, name(), temp));
            }
            return buffer;
        }
    },
    // [ 2] Wind Speed
    USIRZZZ("%s %3d", "US") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            Double spd = report.getWindSpeed();
            if (spd >= 0) {
                if (options.isOptWindInKnots()) {
                    spd = mpsToknots(spd);
                } else {
                    spd = mpsToMPH(spd);
                }
                buffer.append(String.format(format, name(), Math.round(spd)));
            }
            return buffer;
        }
    },
    // [ 3] Wind Direction
    UDIRZZZ("%s %3d", "UD") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            Integer dir = report.getWindDirection();
            if ((dir != null) && (dir >= 0) && (dir <= 360)) {
                if (options.isOptWindInHundreds()) {
                    buffer.append(String.format(format, name(), dir));
                } else {
                    buffer.append(String.format(format, name(), (dir + 5) / 10));
                }
            }
            return buffer;
        }
    },
    // [ 4] Wind Gust
    UGIRZZZ("%s %3d", "UG") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            Double spd = report.getWindGust();
            if (spd >= 0) {
                if (options.isOptWindInKnots()) {
                    spd = mpsToknots(spd);
                } else {
                    spd = mpsToMPH(spd);
                }
                buffer.append(String.format(format, name(), Math.round(spd)));
            }
            return buffer;
        }
    },
    // [ 5] Speed/Direction sss.sddd
    UQIRZZZ("%s  %3d.0%03d", "UQ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            Integer dir = report.getWindDirection();
            Double spd = report.getWindSpeed();
            if ((spd != null) && (spd >= 0)) {
                if ((dir != null) && (dir >= 0) && (dir <= 360)) {
                    if (options.isOptWindInKnots()) {
                        spd = mpsToknots(spd);
                    } else {
                        spd = mpsToMPH(spd);
                    }
                    if (options.isOptWindInHundreds()) {
                        buffer.append(String.format(format, name(),
                                Math.round(spd), dir));
                    } else {
                        buffer.append(String.format(format, name(),
                                Math.round(spd), (dir + 5) / 10));
                    }
                }
            }
            return buffer;
        }
    },
    // [ 6] Altimeter
    PAIRZZZ("%s  %7.2f", "PA") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            Integer p = report.getPressureStation();
            if ((p != null) && (p < -9999)) {
                double pr = p.doubleValue() * 0.000295333727D;
                buffer.append(String.format(format, name(), pr));
            }
            return buffer;
        }
    },
    // 7 Precip 6 hour
    PPQRVZZ("%s  %6.2f", "PPQ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncPrecip> precip = report.getAncPrecip();
            if ((precip != null) && (precip.size() > 0)) {
                for (AncPrecip p : precip) {
                    if (HOURS_06.equals(p.getTimePeriod())) {
                        Double d = p.getPrecipAmount();
                        if (d != null) {
                            if (d == 0) {
                                if ((!options.isOptZero6HourPrecip())
                                        && (!options.isOptZeroAuto6HourPrecip())) {
                                    d = -9999.0;
                                }
                            }
                            if (d > -9999) {
                                d = d * 0.03937D;
                                buffer.append(String.format(format, name(), d));
                            }
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 8 Precip 24 hour
    PPDRZZZ("%s %6.2f", "PPD") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncPrecip> precip = report.getAncPrecip();
            if ((precip != null) && (precip.size() > 0)) {
                for (AncPrecip p : precip) {
                    if (HOURS_24.equals(p.getTimePeriod())) {
                        Double d = p.getPrecipAmount();
                        if (options.isOptZero24HourPrecip()
                                || options.isOptZeroAuto24HourPrecip()) {
                            Calendar c = report.getTimeObs();
                            int hr = c.get(Calendar.HOUR_OF_DAY) * 100;
                            hr += c.get(Calendar.MINUTE);
                            // if the time is out of range, null out the value.
                            if ((hr < 1141) && (hr > 1229)) {
                                d = null;
                            }
                            if ((d != null) && (d == -9999)) {
                                d = 0.0;
                            }
                        }
                        if ((d != null) && (d > -9999)) {
                            d = d * 0.03937D;
                            buffer.append(String.format(format, name(), d));
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 9 Temperature Max 24 hour
    TAIRZXZ("%s %3d", "TX") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncTemp> temps = report.getAncTemp();
            if (temps != null) {
                for (AncTemp t : temps) {
                    if (HOURS_24.equals(t.getTimePeriod())) {
                        if (AncTemp.T_MAX_AIR_TEMP.equals(t.getObsType())) {
                            Double tt = t.getValue();
                            if ((tt != null) && (tt > -9999)) {
                                tt = celsiusToFahr(tt - 273.15);
                                buffer.append(String.format(format, name(),
                                        tt.intValue()));
                            }
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 10 Temperature Min 24 hour
    TAIRZNZ("%s %3d", "TN") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncTemp> temps = report.getAncTemp();
            if (temps != null) {
                for (AncTemp t : temps) {
                    if (HOURS_24.equals(t.getTimePeriod())) {
                        if (AncTemp.T_MIN_AIR_TEMP.equals(t.getObsType())) {
                            Double tt = t.getValue();
                            if ((tt != null) && (tt > -9999)) {
                                tt = celsiusToFahr(tt - 273.15);
                                buffer.append(String.format(format, name(),
                                        tt.intValue()));
                            }
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 11 Precip 1 hour
    PPHRZZZ("%s %6.2f", "PPH") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncPrecip> precip = report.getAncPrecip();
            if ((precip != null) && (precip.size() > 0)) {
                for (AncPrecip p : precip) {
                    if (HOURS_01.equals(p.getTimePeriod())) {
                        Double d = p.getPrecipAmount();
                        if ((d != null) && (d > -9999)) {
                            d = d * 0.03937D;
                            buffer.append(String.format(format, name(), d));
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 12 Snow depth on ground
    SDIRZZZ("%s %3d", "SD") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            return buffer;
        }
    },
    // 13 Water Equivalent
    SWIRZZZ("%s  %6.2f", "SW") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            return buffer;
        }
    },
    // 14 Sunshine
    RTIRZZZ("%s %3s", "RT") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            return buffer;
        }
    },
    // 15 Max Temp past 6 hours
    TAIRZRZ("%s %3d", "TAIRZR") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncTemp> temps = report.getAncTemp();
            if (temps != null) {
                for (AncTemp t : temps) {
                    if (HOURS_06.equals(t.getTimePeriod())) {
                        if (AncTemp.T_MAX_AIR_TEMP.equals(t.getObsType())) {
                            Double tt = t.getValue();
                            if ((tt != null) && (tt > -9999)) {
                                tt = celsiusToFahr(tt - 273.15);
                                buffer.append(String.format(format, name(),
                                        tt.intValue()));
                            }
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 16 Min Temp past 6 hours
    TAIRZHZ("%s %3d", "TAIRZH") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncTemp> temps = report.getAncTemp();
            if (temps != null) {
                for (AncTemp t : temps) {
                    if (HOURS_06.equals(t.getTimePeriod())) {
                        if (AncTemp.T_MIN_AIR_TEMP.equals(t.getObsType())) {
                            Double tt = t.getValue();
                            if ((tt != null) && (tt > -9999)) {
                                tt = celsiusToFahr(tt - 273.15);
                                buffer.append(String.format(format, name(),
                                        tt.intValue()));
                            }
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 17 Precip past 3 hours
    PPTRZZZ("%s %6.2f", "PPT") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncPrecip> precip = report.getAncPrecip();
            if ((precip != null) && (precip.size() > 0)) {
                for (AncPrecip p : precip) {
                    if (HOURS_03.equals(p.getTimePeriod())) {
                        Double d = p.getPrecipAmount();
                        if ((d != null) && (d > -9999)) {
                            d = d * 0.03937D;
                            buffer.append(String.format(format, name(), d));
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 18 Max Temp past 12 hours
    TAIRZYZ("%s %3d", "TAIRZY") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncTemp> temps = report.getAncTemp();
            if (temps != null) {
                for (AncTemp t : temps) {
                    if (HOURS_12.equals(t.getTimePeriod())) {
                        if (AncTemp.T_MIN_AIR_TEMP.equals(t.getObsType())) {
                            Double tt = t.getValue();
                            if ((tt != null) && (tt > -9999)) {
                                tt = celsiusToFahr(tt - 273.15);
                                buffer.append(String.format(format, name(),
                                        tt.intValue()));
                            }
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 19 Min Temp past 12 hours
    TAIRZPZ("%s %3d", "TAIRZP") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<AncTemp> temps = report.getAncTemp();
            if (temps != null) {
                for (AncTemp t : temps) {
                    if (HOURS_12.equals(t.getTimePeriod())) {
                        if (AncTemp.T_MAX_AIR_TEMP.equals(t.getObsType())) {
                            Double tt = t.getValue();
                            if ((tt != null) && (tt > -9999)) {
                                tt = celsiusToFahr(tt - 273.15);
                                buffer.append(String.format(format, name(),
                                        tt.intValue()));
                            }
                        }
                    }
                }
            }
            return buffer;
        }
    },
    // 20 PRECIP accumulator
    PCIRZZZ("%s %6.2f", "PC") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            return buffer;
        }
    },
    // 21 Precipitation Type
    PTIRZZZ("%s %3d", "PTIR") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            return buffer;
        }
    },
    // 22 Pressure Sea Level
    PLIRZZZ("%s %7.2f", "PL") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            Integer p = report.getPressureSealevel();
            if ((p != null) && (p < -9999)) {
                double pr = p.doubleValue() * 0.000295333727D;
                buffer.append(String.format(format, name(), pr));
            }
            return buffer;
        }
    },
    // 23 Pressure characteristic
    PDIRZZZ("%s %3d", "PDIR") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            Integer c = report.getPressChangeChar();
            if ((c != null) && (c != -9999)) {
                buffer.append(String.format(format, name(), c));
            }
            return buffer;
        }
    },
    // 24 Pressure tendency
    PEIRZZZ("%s %6.2f", "PE") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            Double pp = report.getPressChange3Hr();
            if ((pp != null) && (pp != -9999)) {
                buffer.append(String.format(format, name(), pp / 100D));
            }
            return buffer;
        }
    },
    // 25 surface visibility
    XVIRZZZ("%s %6.2f", "XV") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
                String format, String reportText, ObsToSHEFOptions options) {
            return buffer;
        }
    };

    private static Integer HOURS_24 = new Integer(86400);

    private static Integer HOURS_18 = new Integer(64800);

    private static Integer HOURS_12 = new Integer(43200);

    private static Integer HOURS_06 = new Integer(21600);

    private static Integer HOURS_03 = new Integer(10800);

    private static Integer HOURS_01 = new Integer(3600);

    private final String outFormat;

    private final String checkCode;

    /**
     * Construct the private instance of this enum with a given format string.
     * 
     * @param format
     *            The format string to be applied to this parameter.
     */
    private SHEF_SM_Codes(String format, String checkValue) {
        outFormat = format;
        checkCode = checkValue;
    }

    /**
     * Format the associated enum parameter using data within the supplied
     * PluginDataObject.
     * 
     * @param buffer
     *            StringBuilder to receive the encoded data.
     * @param value
     *            The PluginDataObject containing the data to be encoded.
     * @return The StringBuilder with the encoded data.
     */
    public StringBuilder format(StringBuilder buffer, ObsCommon pdo,
            String reportText, ObsToSHEFOptions options) {
        final String errFMT = "Error converting %s format[%s]";

        StringBuilder formattedData = null;
        try {
            if (options.checkPE(checkCode)) {
                formattedData = intFormat(buffer, pdo, outFormat, reportText,
                        options);
            }
        } catch (IllegalFormatConversionException e) {
            throw new RuntimeException(String.format(errFMT, name(), outFormat));
        }

        return formattedData;
    }

    /**
     * This default does nothing. Each enum may override this method to provide
     * formatting specific to the parameter being encoded.
     * 
     * @param buffer
     *            StringBuilder to receive the encoded data.
     * @param report
     *            The ObsCommon containing the data to be encoded.
     * @param format
     *            The format string to be applied for encoding.
     * @return The StringBuilder with the encoded data.
     */
    StringBuilder intFormat(StringBuilder buffer, ObsCommon report,
            String format, String reportText, ObsToSHEFOptions options) {
        return buffer;
    }

    /**
     * Convert a temperature in degrees Celsius to degrees Fahrenheit.
     * 
     * @param temperature
     *            Temperature in degrees Celsius.
     * @return Temperature in degrees Fahrenheit.
     */
    public static double celsiusToFahr(double temperature) {
        return ((temperature * 9.0d) / 5.0d) + 32.0d;
    }

    /**
     * Convert a temperature in degrees Celsius to degrees Fahrenheit.
     * 
     * @param temperature
     *            Temperature in degrees Celsius.
     * @return Temperature in degrees Fahrenheit.
     */
    public static float celsiusToFahr(float temperature) {
        return ((temperature * 9.0f) / 5.0f) + 32.0f;
    }

    /**
     * Convert meters per second to nautical miles per hour.
     * 
     * @param speed
     * @return
     */
    private static double mpsToknots(double speed) {
        return speed * 1.94384449D;
    }

    /**
     * Convert meters per second to statue miles per hour.
     * 
     * @param speed
     * @return
     */
    private static double mpsToMPH(double speed) {
        return speed * 2.23693629D;
    }

}
