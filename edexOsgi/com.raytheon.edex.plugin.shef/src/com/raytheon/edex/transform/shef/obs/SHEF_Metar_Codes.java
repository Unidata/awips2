/**
: * This software was developed and / or modified by Raytheon Company,
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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.IllegalFormatConversionException;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.UnitConverter;

import com.raytheon.edex.transform.shef.MetarToShefTransformer;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.dataplugin.obs.metar.util.SkyCover;
import com.raytheon.uf.common.dataplugin.obs.metar.util.WeatherCondition;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import si.uom.NonSI;
import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * Enums that both declare and define the data encoding for various METAR
 * parameters.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 20, 2008       1659 jkorman     Initial creation
 * Mar 27, 2009       2016 jkorman     Modified visibility processing.
 * Jun 01, 2018       6843 mduff       changes to accommodate ObsToSHEFOptions, clean up.
 * Dec  9, 2019       7953 tgurney     Fix spurious error when wind direction is
 *                                     null or VRB
 *
 * </pre>
 *
 * @author jkorman
 */
public enum SHEF_Metar_Codes implements SHEF_Obs_Codes<MetarRecord> {
    // [ 0] Temperature Instantaneous
    TAIRZZZ("%s %3d", "TA|TAIRZZ", "TAIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            long temp = MISSING;
            int tt = report.getTemperature();
            if (tt > MISSING) {
                temp = Math.round(celsiusToFahr(tt));
            }
            float tf = report.getTempFromTenths();
            if (tf > MISSING) {
                temp = Math.round(celsiusToFahr(tf));
            }
            if (temp > MISSING) {
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), temp));
            }
            return buffer;
        }
    },
    // [ 1] Dew Point
    TDIRZZZ("%s %3d", "TD", "TDIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            long temp = MISSING;
            int tt = report.getDewPoint();
            if (tt > MISSING) {
                temp = Math.round(celsiusToFahr(tt));
            }
            float tf = report.getDewPointFromTenths();
            if (tf > MISSING) {
                temp = Math.round(celsiusToFahr(tf));
            }
            if (temp > MISSING) {
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), temp));
            }
            return buffer;
        }
    },
    // 15 Max Temp past 6 hours
    TAIRZRZ("%s %3d", "TAIRZR", "TAIRZRZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            double tf = report.getMaxTemp6Hour();
            if (tf > -100.0f && tf < 100.0f) {
                tf = celsiusToFahr(tf);
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), Math.round(tf)));
            }
            return buffer;
        }
    },
    // 16 Min Temp past 6 hours
    TAIRZHZ("%s %3d", "TAIRZH", "TAIRZHZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            double tf = report.getMinTemp6Hour();
            if (tf > -100.0f && tf < 100.0f) {
                tf = celsiusToFahr(tf);
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), Math.round(tf)));
            }
            return buffer;
        }
    },
    // 18 Max Temp past 12 hours
    TAIRZYZ("%s %3d", "TAIRZY", "TAIRZYZ"),
    // Cannot find a corresponding value in METAR data.
    // ------------
    // 19 Min Temp past 12 hours
    // Cannot find a corresponding value in METAR data.
    TAIRZPZ("%s %3d", "TAIRZP", "TAIRZPZ"),

    // 9 Temperature Max 24 hour
    TAIRZXZ("%s %3.0f", "TX", "TAIRZXZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            double temp = report.getMaxTemp24Hour();
            if (temp > MISSING) {
                temp = celsiusToFahr(temp);
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), temp));
            }
            return buffer;
        }
    },
    // 10 Temperature Min 24 hour
    TAIRZNZ("%s %3.0f", "TN", "TAIRZNZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            double temp = report.getMinTemp24Hour();
            if (temp > MISSING) {
                temp = celsiusToFahr(temp);
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), temp));
            }
            return buffer;
        }
    },
    // [ 2] Wind Speed
    USIRZZZ("%s %3d", "US", "USIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            double spd = report.getWindSpeed();
            if (spd >= 0) {
                if (!options.isOptWindInKnots()) {
                    spd = knotsToMph(spd);
                }
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), Math.round(spd)));
            }
            return buffer;
        }
    },
    // [ 3] Wind Direction
    UDIRZZZ("%s %3d", "UD", "UDIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            Integer dir = MISSING;
            String dirStr = report.getWindDir();
            try {
                dir = Integer.parseInt(dirStr);
                if (dir >= 0 && dir <= 360) {
                    String pedtsep = checkPEDTSEP(name(), options);
                    if (options.isOptWindInHundreds()) {
                        buffer.append(String.format(format, pedtsep, dir));
                    } else {
                        buffer.append(
                                String.format(format, pedtsep, (dir + 5) / 10));
                    }
                }
            } catch (NumberFormatException nfe) {
                if (dirStr != null && !"VRB".equals(dirStr)) {
                    statusHandler.warn(
                            "Failed to parse wind direction '" + dirStr
                                    + "' in METAR:\n" + report.getReport(),
                            nfe);
                }
            }
            return buffer;
        }
    },
    // [ 4] Peak Wind Gust
    UGIRZZZ("%s %3d", "UG", "UGIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            double spd = report.getWindGust();
            if (spd >= 0) {
                if (!options.isOptWindInKnots()) {
                    spd = knotsToMph(spd);
                }
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), Math.round(spd)));
            }
            return buffer;
        }
    },
    // [ 5] Speed/Direction sss.sddd
    UQIRZZZ("%s  %3d.0%03d", "UQ", "UQIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            int spd = report.getWindSpeed();
            if (spd > MISSING) {
                int dir = MISSING;
                try {
                    dir = Integer.parseInt(report.getWindDir());
                    buffer.append(
                            String.format(format, checkPEDTSEP(name(), options),
                                    Math.round(knotsToMph(spd)), dir));
                } catch (NumberFormatException nfe) {
                    statusHandler.warn("Number Format Exception.", nfe);
                }
            }
            return buffer;
        }
    },
    // [ 6] Altimeter
    PAIRZZZ("%s  %5.2f", "PA", "PAIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            float alt = report.getAltimeter(); // inHg
            if (alt > MISSING) {
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), alt));
            }
            return buffer;
        }
    },
    // 11 Precip 1 hour
    PPHRZZZ("%s %5.2f", "PPH", "PPHRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            boolean isMetric = "T"
                    .equals(options.getGeneralProperty("METRIC", "F"));
            Integer durTime = null;
            boolean processObs = false;

            boolean tracePrecip = false;
            float value = report.getPrecip1Hour();
            if (!options.isOptNoTrace()) {
                tracePrecip = value == 0.0f;
            }

            double precip = MISSING;
            if (METAR.equals(report.getReportType())) {
                processObs = true;
                if (value >= 0.0f) {
                    precip = value;
                    if (isMetric) {
                        precip = mmToInches(precip);
                    }
                } else if (options.isOptZeroAuto1HourPrecip()) {
                    if (report.getAutoStationType() != null) {
                        Matcher m = PNO.matcher(reportText);
                        if (m.find()) {
                            precip = Utilities.INDETERMINATE_PRECIP;
                        } else {
                            precip = 0.0f;
                        }
                    }
                }
            } else if ("SPECI".equals(report.getReportType())) {
                processObs = true;
                if (value >= 0.0f) {
                    precip = value;
                    if (isMetric) {
                        precip = mmToInches(precip);
                    }
                }
            }
            if (processObs) {
                Integer pcTol = options.getOptPCT();
                if (pcTol != null) {
                    String id = report.getStationId();

                    Integer pcReset = options.getPCReset(id);
                    // !null means that a reset is defined for this station.
                    if (pcReset == null) {
                        if (options.isOptStripICAO()) {
                            // Try again with 3 character identifier.
                            pcReset = options.getPCReset(id.substring(1));
                        }
                    }
                    // Did we find a pcReset value?
                    if (pcReset != null) {
                        // Get the tolerance value
                        Integer tol = options.getOptPCT();

                        int obMinute = report.getTimeObs().get(Calendar.MINUTE);
                        durTime = (obMinute + 60 - pcReset) % 60;

                        if (durTime <= tol || Math.abs(durTime - 60) <= tol) {
                            durTime = 60;
                        }
                    }
                }
                String pedtsep = name();
                if (durTime != null) {
                    // Set variable duration code.
                    pedtsep = "PPVRZZZ";
                }
                pedtsep = checkPEDTSEP(pedtsep, options);
                if (precip > MISSING) {
                    if (durTime == null) {
                        if (tracePrecip) {
                            buffer.append(String.format("%s  T", pedtsep));
                        } else {
                            if (precip >= 0.0f) {
                                buffer.append(
                                        String.format(format, pedtsep, precip));
                            } else {
                                buffer.append(String.format("%s  M", pedtsep));
                            }
                        }
                    } else {
                        buffer.append(String.format("DVN%02d/" + format,
                                durTime, pedtsep, precip));
                    }
                }
            }
            return buffer;
        }
    },
    // 17 Precip past 3 hours
    PPTRZZZ("%s %5.2f", "PPT", "PPTRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            boolean isMetric = "T"
                    .equals(options.getGeneralProperty("METRIC", "F"));

            float value = report.getPrecip3Hour();
            double precip = MISSING;

            if (METAR.equals(report.getReportType())) {
                // Are we in a 3 hourly window.
                if (Utilities.isIn3HourWindow(report.getTimeObs())) {
                    if (value >= 0.0f) {
                        precip = value;
                        if (isMetric) {
                            precip = mmToInches(precip);
                        }
                    }
                }
            }

            String pedtsep = checkPEDTSEP(name(), options);
            if (precip > MISSING) {
                if (value < Utilities.INDETERMINATE_PRECIP) {
                    if (precip > 0.0f) {
                        buffer.append(String.format(format, pedtsep, precip));
                    } else if (precip == Utilities.INDETERMINATE_PRECIP) {
                        buffer.append(String.format("%s M", pedtsep, precip));
                    }
                } else {
                    if (precip > 0.0f) {
                        buffer.append(String.format(format, pedtsep, precip));
                    } else if (precip < 0.0f) {
                        buffer.append(String.format("%s M", pedtsep, precip));
                    } else {
                        buffer.append(String.format("%s  T", pedtsep, precip));
                    }
                }
            }
            return buffer;
        }
    },
    // 7 Precip 6 hour
    PPQRZZZ("%s  %5.2f", "PPQ", "PPQRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            boolean isMetric = "T"
                    .equals(options.getGeneralProperty("METRIC", "F"));
            float value = report.getPrecip6Hour();
            double precip = MISSING;
            if (METAR.equals(report.getReportType())) {
                if (value >= 0.0f) {
                    if (Utilities.isIn6HourWindow_2(report.getTimeObs())) {
                        precip = value;
                        if (isMetric) {
                            precip = mmToInches(precip);
                        }
                    }
                } else {
                    if (Utilities.isIn6HourWindow_1(report.getTimeObs())) {
                        // -pall6
                        if (options.isOptZero6HourPrecip()) {
                            if (report.getAutoStationType() != null) {
                                Matcher m = PNO.matcher(reportText);
                                if (m.find()) {
                                    precip = Utilities.INDETERMINATE_PRECIP;
                                } else {
                                    precip = 0.0f;
                                }
                            } else {
                                // not auto
                                if (Utilities.isIndeterminate3_6HourPrecip(
                                        reportText)) {
                                    precip = Utilities.INDETERMINATE_PRECIP;
                                } else {
                                    precip = 0.0f;
                                }
                            }
                        } else if (options.isOptZeroAuto6HourPrecip()) {
                            if (Utilities
                                    .isIndeterminate3_6HourPrecip(reportText)) {
                                precip = Utilities.INDETERMINATE_PRECIP;
                            } else if (report.getAutoStationType() != null) {
                                Matcher m = PNO.matcher(reportText);
                                if (m.find()) {
                                    precip = Utilities.INDETERMINATE_PRECIP;
                                } else {
                                    precip = 0.0f;
                                }
                            }

                        }
                    }
                }
            }
            if (Utilities.isIndeterminate3_6HourPrecip(reportText)) {
                precip = Utilities.INDETERMINATE_PRECIP;
            }

            String pedtsep = checkPEDTSEP(name(), options);
            if (precip > MISSING) {
                if (value < Utilities.INDETERMINATE_PRECIP) {
                    if (precip >= 0.0f) {
                        buffer.append(String.format(format, pedtsep, precip));
                    } else if (precip == Utilities.INDETERMINATE_PRECIP) {
                        buffer.append(String.format("%s M", pedtsep));
                    }
                } else {
                    if (precip > 0.0f) {
                        buffer.append(String.format(format, pedtsep, precip));
                    } else if (precip == 0.0f) {
                        buffer.append(String.format("%s  T", pedtsep));
                    } else {
                        buffer.append(String.format("%s M", pedtsep));
                    }
                }
            }
            return buffer;
        }
    },
    // 8 Precip 24 hour
    PPDRZZZ("%s %5.2f", "PPD", "PPDRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {

            boolean isMetric = "T"
                    .equals(options.getGeneralProperty("METRIC", "F"));

            Calendar c = report.getTimeObs();
            int hrmnTime = c.get(Calendar.HOUR_OF_DAY) * 100;
            hrmnTime += c.get(Calendar.MINUTE);

            double value = report.getPrecip24Hour();
            double precip = MISSING;

            if (METAR.equals(report.getReportType())) {
                if (value > MISSING) {
                    if (options.isOptDecodePrecipAt12Z()) {
                        if (hrmnTime > P24_MIN && hrmnTime < P24_MAX) {
                            precip = value;
                            if (isMetric) {
                                precip = mmToInches(precip);
                            }
                        }
                    } else {
                        precip = value;
                        if (isMetric) {
                            precip = mmToInches(precip);
                        }
                    }
                } else {
                    if (hrmnTime > P24_MIN && hrmnTime < P24_MAX) {
                        if (options.isOptZero24HourPrecip()) {
                            precip = 0.0f;
                        } else if (options.isOptZeroAuto24HourPrecip()) {
                            String autoType = report.getAutoStationType();
                            if (autoType != null) {
                                Matcher m = PNO.matcher(reportText);
                                if (m.find()) {
                                    precip = Utilities.INDETERMINATE_PRECIP;
                                } else {
                                    precip = 0.0f;
                                }
                            }
                        }
                    }
                }
                if (Utilities.isIndeterminate24HourPrecip(reportText)) {
                    precip = Utilities.INDETERMINATE_PRECIP;
                }
            }
            String pedtsep = checkPEDTSEP(name(), options);

            if (precip > MISSING) {
                if (value == MISSING) {
                    if (precip >= 0) {
                        buffer.append(String.format(format, pedtsep, precip));
                    } else if (precip > MISSING) {
                        buffer.append(String.format("%s M", pedtsep));
                    }
                } else {
                    if (precip < 0) {
                        buffer.append(String.format("%s M", pedtsep));
                    } else if (precip > 0) {
                        buffer.append(String.format(format, pedtsep, precip));
                    } else {
                        buffer.append(String.format("%s  T", pedtsep));
                    }
                }
            }
            return buffer;
        }
    },
    // 12 Snow depth on ground
    SDIRZZZ("%s %3d", "SD", "SDIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            if (options.checkPE("SD")) {
                String rmks = reportText;
                Pattern p = Pattern.compile(" 4/\\d{3}");
                Matcher m = p.matcher(rmks);
                if (m.find()) {
                    String s = rmks.substring(m.start() + 3, m.end());
                    try {
                        int depth = Integer.parseInt(s);
                        buffer.append(String.format(format,
                                checkPEDTSEP(name(), options), depth));
                    } catch (NumberFormatException nfe) {
                        // Nothing
                    }
                }
            }
            return buffer;
        }
    },
    // 13 Water Equivalent
    SWIRZZZ("%s  %5.2f", "SW", "SWIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            String rmks = reportText;
            Pattern p = Pattern.compile(" 933\\d{3}");
            Matcher m = p.matcher(rmks);
            if (m.find()) {

                String s = rmks.substring(m.start() + 4, m.end());
                try {
                    int depth = Integer.parseInt(s);
                    buffer.append(String.format(format,
                            checkPEDTSEP(name(), options), depth / 10.0));
                } catch (NumberFormatException nfe) {
                    // Nothing
                }
            }
            return buffer;
        }
    },
    // 14 Sunshine
    RTIRZZZ("%s %3s", "RT", "RTIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            if (options.checkPE("RT")) {
                String rmks = reportText;
                Pattern p = Pattern.compile(" 98\\d{3}");
                Matcher m = p.matcher(rmks);
                if (m.find()) {
                    buffer.append(
                            String.format(format, checkPEDTSEP(name(), options),
                                    rmks.substring(m.start() + 3, m.end())));
                }
            }
            return buffer;
        }
    },
    // ------------
    // 20 PRECIP accumulator
    // Cannot find a corresponding value in METAR data.
    PCIRZZZ("%s %3d", "PC", "PCIRZZZ"),
    // ------------
    // 21 Precipitation Type
    PTIRZZZ("%s %s", "PTIR", "PTIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            List<WeatherCondition> wxData = report.getWeatherCondition();

            List<WeatherCondition> wx = new ArrayList<>();
            for (WeatherCondition w : wxData) {
                WeatherCondition[] ww = WeatherCondition.splitWeather(w);
                for (WeatherCondition www : ww) {
                    wx.add(www);
                }
            }

            StringBuilder precipCode = new StringBuilder("---");

            for (WeatherCondition ww : wx) {
                String precip = ww.getPrecipitation();
                boolean isFreezing = "FZ".equals(ww.getDescriptor());

                // NWSM 10-944 OCTOBER 28, 2005 Table 17
                // 0 Ice prism
                // 1 Rain
                // 2 Freezing rain
                // 3 Drizzle
                // 4 Freezing drizzle
                // 5 Snow
                // 6 Snow pellets
                // 7 Snow grains
                // 8 Ice pellets
                // 9 Hail

                if ("RA".equals(precip)) {
                    precipCode.setCharAt(0, isFreezing ? '2' : '1');
                } else if ("DZ".equals(precip)) {
                    precipCode.setCharAt(1, isFreezing ? '3' : '4');
                } else if ("SN".equals(precip)) {
                    precipCode.setCharAt(2, '5');
                } else if ("SP".equals(precip)) {
                    precipCode.setCharAt(2, '6');
                } else if ("SG".equals(precip)) {
                    precipCode.setCharAt(2, '7');
                } else if ("IP".equals(precip)) {
                    precipCode.setCharAt(2, '8');
                } else if ("GR".equals(precip)) {
                    precipCode.setCharAt(2, '9');
                } else if ("GS".equals(precip)) {
                    precipCode.setCharAt(2, '9');
                } else if ("IC".equals(precip)) {
                    precipCode.setCharAt(2, '0');
                }
            } // for
            boolean written = false;
            for (int i = 0; i < precipCode.length(); i++) {
                if (precipCode.charAt(i) != '-') {
                    if (written) {
                        buffer.append("/");
                    }
                    buffer.append(
                            String.format(format, checkPEDTSEP(name(), options),
                                    precipCode.substring(i, i + 1)));
                    written = true;
                }
            }
            return buffer;
        }
    },
    // 22 Pressure Sea Level
    PLIRZZZ("%s %7.2f", "PL", "PLIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            float slp = report.getSeaLevelPress(); // Pa
            if (slp > MISSING) {
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), slp));
            }
            return buffer;
        }
    },
    // 23 Pressure tendency
    PDIRZZZ("%s %7.2f", "PDIR", "PDIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            float tendency = report.getPressChange3Hour();
            if (tendency > MISSING) {
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), tendency / 10D));
            }
            return buffer;
        }
    },
    // 24 Pressure characteristic
    PEIRZZZ("%s %s.00", "PE", "PEIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            String character = report.getPressChangeChar();
            if (character != null && character.length() == 1) {
                if ("012345678".indexOf(character) > -1) {
                    buffer.append(String.format(format,
                            checkPEDTSEP(name(), options), character));
                }
            }
            return buffer;
        }
    },
    // 25 surface visibility
    XVIRZZZ("%s %7.2f", "XV", "XVIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            double vis = report.getVisibility();
            if (vis >= 0) {
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), vis));
            }
            return buffer;
        }
    },
    // 26 river stage
    // Cannot find a corresponding value in METAR data.
    HGIRZZZ("%s %3d", "HG", "HGIRZZZ"),
    // if(options.checkPE("HG")) {
    // }
    // ------------
    // 27 current weather code
    XWIRZZZ("%s %3d", "XW", "XWIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            if (options.checkPE("XW")) {
                int code = Integer.MIN_VALUE;
                for (WeatherCondition wx : report.getWeatherCondition()) {
                    int n = MetarSynopticCode.getSynopticCode(wx);
                    if (n >= 0) {
                        code = Math.max(code, n);
                    }
                }
                if (code >= 0) {
                    buffer.append(String.format(format,
                            checkPEDTSEP(name(), options), code));
                }
            }
            return buffer;
        }
    },
    // 28 cloud sky cover tenths
    XCIRZZZ("%s %3d", "XC", "XCIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            Set<SkyCover> coverages = report.getSkyCoverage();

            if (coverages != null && !coverages.isEmpty()) {
                int tenths = 0;
                for (SkyCover coverage : coverages) {
                    String type = coverage.getType();

                    if ("FEW".equals(type)) {
                        tenths = 1;
                    } else if ("SCT".equals(type)) {
                        tenths = 3;
                    } else if ("BKN".equals(type)) {
                        tenths = 6;
                    } else if ("OVC".equals(type)) {
                        tenths = 10;
                    } else if ("VV".equals(type)) {
                        tenths = 10;
                    }
                    // If we get to 10, no need to go further.
                    if (tenths == 10) {
                        break;
                    }
                }
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), tenths));
            }
            return buffer;
        }
    },
    // 29 snow depth past 6 hours
    SFQRZZZ("%s  %4.1f", "SFQ", "SFQRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            String rmks = reportText;
            Pattern p = Pattern.compile(" 931\\d{3}");
            Matcher m = p.matcher(rmks);
            if (m.find()) {

                String s = rmks.substring(m.start() + 4, m.end());
                try {
                    int depth = Integer.parseInt(s);
                    buffer.append(String.format(format,
                            checkPEDTSEP(name(), options), depth / 10.0));
                } catch (NumberFormatException nfe) {
                    // Nothing
                }
            }
            return buffer;
        }
    },
    // 30 peak wind speed past hour
    UPIRZZZ("%s %3d", "UP", "UPIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            int spd = report.getPkWndSpd();
            if (spd >= 0) {
                buffer.append(
                        String.format(format, checkPEDTSEP(name(), options),
                                Math.round(knotsToMph(spd))));
            }
            return buffer;
        }
    },
    // 31 peak wind dir past hour
    URIRZZZ("%s %3d", "UR", "URIRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            int dir = report.getPkWndDir();
            if (dir >= 0 && dir <= 360) {
                buffer.append(String.format(format,
                        checkPEDTSEP(name(), options), dir / 10));
            }
            return buffer;
        }
    },
    // 32 snow depth past 24 hours
    SFDRZZZ("%s  %4.1f", "SFD", "SFDRZZZ") {
        @Override
        StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
                String format, String reportText, ObsToSHEFOptions options) {
            String rmks = reportText;
            Pattern p = Pattern.compile(" 24/931\\d{3}");
            Matcher m = p.matcher(rmks);
            if (m.find()) {

                String s = rmks.substring(m.start() + 7, m.end());
                try {
                    int depth = Integer.parseInt(s);
                    buffer.append(String.format(format,
                            checkPEDTSEP(name(), options), depth / 10.0));
                } catch (NumberFormatException nfe) {
                    // Nothing
                }
            }
            return buffer;
        }
    };

    private static final String METAR = MetarToShefTransformer.METAR;

    private static final int MISSING = ShefConstants.SHEF_MISSING;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SHEF_Metar_Codes.class);

    private static final Pattern PNO = Pattern.compile("\\s+PNO");

    private static final int P24_MIN = 1140;

    private static final int P24_MAX = 1230;

    private final String outFormat;

    private final String checkCode;

    private final String peCode;

    /**
     * Construct the private instance of this enum with a given format string.
     *
     * @param format
     *            The format string to be applied to this parameter.
     */
    private SHEF_Metar_Codes(String format, String checkValue, String pe) {
        outFormat = format;
        checkCode = checkValue;
        peCode = pe;
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
    @Override
    public StringBuilder format(StringBuilder buffer, MetarRecord pdo,
            String reportText, ObsToSHEFOptions options) {
        final String errFMT = "Error converting %s format[%s]";

        StringBuilder formattedData = null;
        try {
            if (options.checkPE(checkCode)) {
                formattedData = intFormat(buffer, pdo, outFormat, reportText,
                        options);
            }
        } catch (IllegalFormatConversionException e) {
            throw new RuntimeException(String.format(errFMT, name(), outFormat),
                    e);
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
     *            The MetarRecord containing the data to be encoded.
     * @param format
     *            The format string to be applied for encoding.
     * @return The StringBuilder with the encoded data.
     */
    StringBuilder intFormat(StringBuilder buffer, MetarRecord report,
            String format, String reportText, ObsToSHEFOptions options) {
        return buffer;
    }

    private static final String checkPEDTSEP(String pedtsep,
            ObsToSHEFOptions options) {

        StringBuilder sb = null;

        // 0123456789
        // PEDTSEP
        if (pedtsep != null) {
            sb = new StringBuilder(pedtsep);
            if (options.isOptASOS_TS()) {
                sb.setCharAt(3, 'R');
                if (Utilities.OPT_TRUE.equals(options.getGeneralProperty(
                        Utilities.IS_ASOS, Utilities.OPT_FALSE))) {
                    sb.setCharAt(4, 'O');
                } else {
                    sb.setCharAt(4, 'V');
                }
            }
            if (options.isOptTypeSrcV()) {
                sb.setCharAt(4, 'V');
            }
        }
        return sb != null ? sb.toString() : null;
    }

    public static double celsiusToFahr(double temperature) {
        UnitConverter cvt = SI.CELSIUS.getConverterTo(USCustomary.FAHRENHEIT);
        return cvt.convert(temperature);
    }

    /**
     * Convert wind speeds in knots to statue miles per hour.
     *
     * @param speed
     *            Wind speed in knots.
     * @return Wind speed in statue miles per hour.
     */
    private static double knotsToMph(double speed) {
        UnitConverter cvt = USCustomary.KNOT.getConverterTo(USCustomary.MILE_PER_HOUR);
        return cvt.convert(speed);
    }

    private static double mmToInches(double precip) {
        UnitConverter cvt = MetricPrefix.MILLI(SI.METRE).getConverterTo(USCustomary.INCH);
        return cvt.convert(precip);
    }
}
