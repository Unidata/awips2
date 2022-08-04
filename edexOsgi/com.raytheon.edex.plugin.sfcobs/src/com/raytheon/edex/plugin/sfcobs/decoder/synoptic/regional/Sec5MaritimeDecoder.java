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
package com.raytheon.edex.plugin.sfcobs.decoder.synoptic.regional;

import java.util.Arrays;
import java.util.Calendar;
import java.util.regex.Pattern;

import javax.measure.UnitConverter;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.ReportParser;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SynopticSec5Decoder;
import com.raytheon.uf.common.dataplugin.sfcobs.InterWinds;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * Decode synoptic section 5 regional data Maritime data.</BR>
 * 
 * <pre>
 * CMAN 
 *    555 11022 22023 31150 419026
 *    61219 199020 197018 165020 161021 159021 154019
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010            391 jkorman     Initial coding.
 * Sep 18, 2014 #3627      mapeters    Convert units using {@link UnitConverter},
 *                                     updated deprecated {@link TimeTools} usage.\
 * Sep 30, 2014 #3629      mapeters    Replaced {@link AbstractSfcObsDecoder#matchElement()}
 *                                     calls, added Pattern constants.
 * Apr 20, 2016 DR18361    MPorricelli Added decoding of 1-minute peak wind and lowest
 *                                     pressure data
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class Sec5MaritimeDecoder extends SynopticSec5Decoder {
    
    private static final Pattern WIND_SPEED_10_M = Pattern
            .compile("11[/0-9]{3}");
    
    private static final Pattern WIND_SPEED_20_M = Pattern
            .compile("22[/0-9]{3}");
    
    private static final Pattern PEAK_WIND_3 = Pattern.compile("3[/0-9]{4}");
    
    private static final Pattern CMAN_PEAK_WIND_4 = Pattern.compile("4[/0-9]{5}");
    
    private static final Pattern PEAK_WIND_4 = Pattern.compile("4[/0-9]{4}");
    
    private static final Pattern LOWEST_PRESS_PREV_HOUR = Pattern.compile("5[/0-9]{4}");

    private static final Pattern CONT_WIND_SPEED = Pattern.compile("6[/0-9]{4}");
    
    private static final Pattern CONT_WIND_SPEED_INTERNAL = Pattern
            .compile("[/0-9]{6}");
    
    private static final Pattern TIDE = Pattern.compile("TIDE[/0-9]{4}");

    private static final Pattern LOW_PRESS_TIME = Pattern.compile("7[/0-9]{4}");

    private static final Pattern  CMAN_PEAK_WIND_1_MIN = Pattern.compile("8[/0-9]{5}");

    private static final Pattern PEAK_WIND_1_MIN = Pattern.compile("8[/0-9]{4}");

    private static final Pattern PEAK_WIND_1_MIN_TIME = Pattern.compile("9[/0-9]{4}");

    private static final UnitConverter knotsToMSec = USCustomary.KNOT
            .getConverterTo(SI.METRE_PER_SECOND);

    private static final UnitConverter daPaToPa = MetricPrefix.DEKA(SI.PASCAL)
            .getConverterTo(SI.PASCAL);

    private static final int NUM_WINDS = 6;

    private Double wind10mSpeed = null;

    private Double wind20mSpeed = null;

    private Integer pkWindTimeHour = null;

    private Integer pkWindTimeMinute = null;

    private Integer pkWindDirection = null;

    private Double pkWindSpeed = null;

    private Integer pkWindTimeHourOneMin = null;

    private Integer pkWindTimeMinuteOneMin = null;

    private Integer pkWindDirectionOneMin = null;

    private Double pkWindSpeedOneMin = null;

    private Boolean pkWindSpeedOneMinInKnots = false;

    private Double lowestPressure = null;

    private Integer lowestPressureTimeHour = null;

    private Integer lowestPressureTimeMinute = null;

    private Integer windTimeHour = null;

    private Integer windTimeMinute = null;

    private Double[] windSpeeds = null;

    private Integer[] windDirs = null;

    // private Double tideHeight = null;

    /**
     * Set up decoder for Maritime section 5 data.
     * 
     * @param parent
     *            The decoder parent of the observation.
     */
    public Sec5MaritimeDecoder(AbstractSynopticDecoder parent) {
        super(parent);
    }

    /**
     * Decode this section data.
     * 
     * @param reportParser
     *            Parser containing elements of the observation to be decoded.
     * @throws DecoderException
     *             Thrown when an relevant error has occured.
     */
    public void decode(ReportParser reportParser) throws DecoderException {
        // final Integer speed99 = new Integer(99);
        init();

        if (reportParser == null) {
            // nothing to do.
            return;
        }
        if (reportParser.positionTo(ISynoptic.SEC_5_LEAD_STRING)) {
            String element = null;

            while (true) {
                // if we run out of data, exit.
                if (reportParser.next()) {
                    if ((element = reportParser.getElement()) == null) {
                        break;
                    }
                } else {
                    break;
                }
                if (doGroup(1) && WIND_SPEED_10_M.matcher(element).find()) {
                    // extrapolated 10 meter wind speed
                    Integer temp = AbstractSfcObsDecoder.getInt(element, 2, 5);
                    if ((temp != null) && (temp >= 0)) {
                        wind10mSpeed = temp.doubleValue() / 10.0;
                    }
                    closeGroup(1);
                } else if (doGroup(2)
                        && WIND_SPEED_20_M.matcher(element).find()) {
                    // extrapolated 20 meter wind speed
                    Integer temp = AbstractSfcObsDecoder.getInt(element, 2, 5);
                    if ((temp != null) && (temp >= 0)) {
                        wind20mSpeed = temp.doubleValue() / 10.0;
                    }
                    closeGroup(2);
                } else if (doGroup(3) && PEAK_WIND_3.matcher(element).find()) {
                    // peak wind data time
                    pkWindTimeHour = AbstractSfcObsDecoder
                            .getInt(element, 1, 3);
                    pkWindTimeMinute = AbstractSfcObsDecoder.getInt(element, 3,
                            5);
                    closeGroup(3);
                } else if (doGroup(4)
                        && CMAN_PEAK_WIND_4.matcher(element).find()) {
                    // peak wind data time from CMAN
                    pkWindDirection = AbstractSfcObsDecoder.getInt(element, 1,
                            3);
                    if ((pkWindDirection != null) && (pkWindDirection >= 0)) {
                        pkWindDirection *= 10;
                    }
                    Integer spd = AbstractSfcObsDecoder.getInt(element, 3, 6);
                    if ((spd != null) && (spd >= 0)) {
                        pkWindSpeed = spd.doubleValue();
                    }
                    closeGroup(4);
                } else if (doGroup(4) && PEAK_WIND_4.matcher(element).find()) {
                    // peak wind data time
                    pkWindDirection = AbstractSfcObsDecoder.getInt(element, 1,
                            3);
                    if ((pkWindDirection != null) && (pkWindDirection >= 0)) {
                        pkWindDirection *= 10;
                    }
                    Integer spd = AbstractSfcObsDecoder.getInt(element, 3, 5);
                    if ((spd != null) && (spd >= 0)) {
                        pkWindSpeed = spd.doubleValue();
                    }
                    closeGroup(4);
                } else if (doGroup(5)
                        && LOWEST_PRESS_PREV_HOUR.matcher(element).find()) {
                    // 5plplplpl   plplplpl is the lowest pressure during the previous hour, in tenths of hPa.
                    // This group shall only be reported when the pressure is less than 1009 hPa. Otherwise,
                    // the group will be missing. When pressure exceeds 1000 hPa, the leading "1" will be dropped.
                    // For example, 1004.2 hPa will be coded as 50042.
                    Integer press = AbstractSfcObsDecoder.getInt(element, 1, 5);
                    if (press < 1000) {
                        lowestPressure = daPaToPa.convert((double)press + 10000);
                    } else {
                        lowestPressure = daPaToPa.convert((double)press);
                    }
                    closeGroup(5);
                } else if (doGroup(6)
                        && CONT_WIND_SPEED.matcher(element).find()) {
                    // Continuous wind speed data
                    windTimeHour = AbstractSfcObsDecoder.getInt(element, 1, 3);
                    windTimeMinute = AbstractSfcObsDecoder
                            .getInt(element, 3, 5);

                    windSpeeds = new Double[NUM_WINDS];
                    Arrays.fill(windSpeeds, new Double(-9999.0));
                    windDirs = new Integer[NUM_WINDS];
                    Arrays.fill(windDirs, new Integer(-9999));

                    for (int i = 0; i < NUM_WINDS; i++) {
                        if (reportParser.next()) {
                            if ((element = reportParser.getElement()) == null) {
                                break;
                            }
                        } else {
                            break;
                        }
                        if (CONT_WIND_SPEED_INTERNAL.matcher(element).find()) {
                            Integer wd = AbstractSfcObsDecoder.getInt(element,
                                    0, 3);
                            Integer ws = AbstractSfcObsDecoder.getInt(element,
                                    3, 6);
                            if ((wd != null) && (wd >= 0)) {
                                windDirs[i] = wd;
                            }
                            if ((ws != null) && (ws >= 0)) {
                                windSpeeds[i] = ws.doubleValue() / 10.0;
                            }
                        }
                    }
                    closeGroup(6);
                } else if (doGroup(7) && LOW_PRESS_TIME.matcher(element).find()) {
                    // 7hhmm   hhmm is the hour and minute (UTC) of the lowest pressure during the previous hour
                    lowestPressureTimeHour = AbstractSfcObsDecoder
                            .getInt(element, 1, 3);
                    lowestPressureTimeMinute = AbstractSfcObsDecoder.getInt(element, 3,
                            5);
                    closeGroup(7);
                } else if (doGroup(8)
                        && CMAN_PEAK_WIND_1_MIN.matcher(element).find()) {
                    // 8ddff or 8ddfff     dd is true direction, in tens of degrees, from which the highest
                    // peak one minute wind during the previous hour was blowing. For fixed buoys, ff is the
                    // associated speed, in units indicated by iSubw. For C-MAN reports, fff is the associated
                    // speed in knots.
                    pkWindDirectionOneMin = AbstractSfcObsDecoder.getInt(element, 1, 3);
                    if ((pkWindDirectionOneMin != null) && (pkWindDirectionOneMin >= 0)) {
                        pkWindDirectionOneMin *= 10;
                    }
                    Integer spd = AbstractSfcObsDecoder.getInt(element, 3, 6);
                    if ((spd != null) && (spd >= 0)) {
                        pkWindSpeedOneMin = spd.doubleValue();
                    }
                    pkWindSpeedOneMinInKnots = true;
                    closeGroup(8);
                } else if (doGroup(8) && PEAK_WIND_1_MIN.matcher(element).find()) {
                    // 8ddff or 8ddfff     dd is true direction, in tens of degrees, from which the highest
                    // peak one minute wind during the previous hour was blowing. For fixed buoys, ff is the
                    // associated speed, in units indicated by iSubw.
                    pkWindDirectionOneMin = AbstractSfcObsDecoder.getInt(element, 1, 3);
                    if ((pkWindDirectionOneMin != null) && (pkWindDirectionOneMin >= 0)) {
                        pkWindDirectionOneMin *= 10;
                    }
                    Integer spd = AbstractSfcObsDecoder.getInt(element, 3, 5);
                    if ((spd != null) && (spd >= 0)) {
                        pkWindSpeedOneMin = spd.doubleValue();
                    }
                    closeGroup(8);
                } else if (doGroup(9) && PEAK_WIND_1_MIN_TIME.matcher(element).find()) {
                    // 9hhmm   hhmm is the hour and minute (UTC) of the highest peak one-minute wind
                    // during the previous hour
                    pkWindTimeHourOneMin = AbstractSfcObsDecoder
                                .getInt(element, 1, 3);
                    pkWindTimeMinuteOneMin = AbstractSfcObsDecoder.getInt(element, 3,
                                5);
                    closeGroup(9);
                } else if (TIDE.matcher(element).find()) {
                    // Tidal data in feet.

                }
            }
        }
    }

    /**
     * Reset all values prior to decode.
     */
    private void init() {
        wind10mSpeed = -9999.0;
        wind20mSpeed = -9999.0;
        pkWindTimeHour = null;
        pkWindTimeMinute = null;
        pkWindDirection = -9999;
        pkWindSpeed = -9999.0;
        pkWindTimeHourOneMin = null;
        pkWindTimeMinuteOneMin = null;
        pkWindDirectionOneMin = -9999;
        pkWindSpeedOneMin = -9999.0;
        windSpeeds = null;
        windDirs = null;
        pkWindSpeedOneMinInKnots = false;
    }

    /**
     * Populate an ObsCommon object with the decoded data from this section.
     * 
     * @param receiver
     *            An ObsCommon to receive the decoded data.
     * @return The populated receiver object.
     */
    public ObsCommon getDecodedData(ObsCommon receiver) {

        int iSubW = decoderParent.getISubw();
        boolean inKnots = (iSubW == 3) || (iSubW == 4);

        double wSpeed = 0;
        if (receiver != null) {
            if (wind10mSpeed > -9999.0) {
                wSpeed = wind10mSpeed;
                if (inKnots) {
                    wSpeed = knotsToMSec.convert(wSpeed);
                }
                receiver.setWind10mSpeed(wSpeed);
            }
            if (wind20mSpeed > -9999.0) {
                wSpeed = wind20mSpeed;
                if (inKnots) {
                    wSpeed = knotsToMSec.convert(wSpeed);
                }
                receiver.setWind20mSpeed(wSpeed);
            }
            // Peak wind data
            if ((pkWindTimeHour != null) && (pkWindTimeMinute != null)) {
                Calendar obsT = receiver.getTimeObs();

                Integer year = getParent().getHeader().getYear();
                if (year == -1) {
                    year = null;
                }
                Integer month = getParent().getHeader().getMonth();
                if (month == -1) {
                    month = null;
                }

                Calendar newT = AbstractSynopticDecoder.calculateObsDateTime(
                        obsT, obsT.get(Calendar.DAY_OF_MONTH), pkWindTimeHour,
                        year, month);
                newT.set(Calendar.MINUTE, pkWindTimeMinute);
                // Now do we have valid speed and direction
                if (pkWindDirection != null) {
                    receiver.setPeakWindDir(pkWindDirection);
                }
                wSpeed = pkWindSpeed;
                if (wSpeed > -9999) {
                    if (inKnots) {
                        wSpeed = knotsToMSec.convert(wSpeed);
                    }
                }
                receiver.setPeakWindSpeed(wSpeed);
                receiver.setPeakWindTime(newT.getTimeInMillis());
            }
            // One-minute peak wind data
            if ((pkWindTimeHourOneMin != null) && (pkWindTimeMinuteOneMin != null)) {
                Calendar obsT = receiver.getTimeObs();

                Integer year = getParent().getHeader().getYear();
                if (year == -1) {
                    year = null;
                }
                Integer month = getParent().getHeader().getMonth();
                if (month == -1) {
                    month = null;
                }

                Calendar newT = AbstractSynopticDecoder.calculateObsDateTime(
                        obsT, obsT.get(Calendar.DAY_OF_MONTH), pkWindTimeHourOneMin,
                        year, month);
                newT.set(Calendar.MINUTE, pkWindTimeMinuteOneMin);
                // Now do we have valid speed and direction
                if (pkWindDirectionOneMin != null) {
                    receiver.setPeakWindDirOneMin(pkWindDirectionOneMin);
                }
                wSpeed = pkWindSpeedOneMin;
                if (wSpeed > -9999) {
                    if (inKnots || pkWindSpeedOneMinInKnots) {
                        wSpeed = knotsToMSec.convert(wSpeed);
                    }
                }
                receiver.setPeakWindSpeedOneMin(wSpeed);
                receiver.setPeakWindTimeOneMin(newT.getTimeInMillis());
            }
            // 10 minutes observations.
            if ((windSpeeds != null) && (windDirs != null)) {
                // Do we need to override the observation time
                if ((windTimeHour != null) && (windTimeMinute != null)) {
                    Calendar obsT = receiver.getTimeObs();
                    Integer year = getParent().getHeader().getYear();
                    if (year == -1) {
                        year = null;
                    }
                    Integer month = getParent().getHeader().getMonth();
                    if (month == -1) {
                        month = null;
                    }

                    Calendar newT = AbstractSynopticDecoder
                            .calculateObsDateTime(obsT,
                                    obsT.get(Calendar.DAY_OF_MONTH),
                                    windTimeHour, year, month);
                    newT.set(Calendar.MINUTE, windTimeMinute);

                    for (int i = NUM_WINDS - 1; i >= 0; i--) {
                        InterWinds wind = null;
                        if (windSpeeds[i] != null) {
                            wind = new InterWinds();
                            wSpeed = windSpeeds[i];
                            if (wSpeed >= 0) {
                                if (inKnots) {
                                    wSpeed = knotsToMSec.convert(wSpeed);
                                }
                            }
                            wind.setWindSpeed(wSpeed);
                        }
                        if (windDirs[i] != null) {
                            if (wind == null) {
                                wind = new InterWinds();
                            }
                            wind.setWindDir(windDirs[i].doubleValue());
                        }
                        // Now set the time/type if there was data.
                        if (wind != null) {
                            wind.setObsTime(TimeUtil.newCalendar(newT));
                            receiver.addInterWind(wind);
                        }
                        // adjust the time back ten minutes
                        newT.add(Calendar.MINUTE, -10);
                    } // for
                }
            }
            // Lowest pressure data
            if ((lowestPressureTimeHour != null) && (lowestPressureTimeMinute != null)) {
                Calendar obsT = receiver.getTimeObs();

                Integer year = getParent().getHeader().getYear();
                if (year == -1) {
                    year = null;
                }
                Integer month = getParent().getHeader().getMonth();
                if (month == -1) {
                    month = null;
                }

                Calendar newT = AbstractSynopticDecoder.calculateObsDateTime(
                        obsT, obsT.get(Calendar.DAY_OF_MONTH), lowestPressureTimeHour,
                        year, month);
                newT.set(Calendar.MINUTE, lowestPressureTimeMinute);
                if (lowestPressure != null) {
                    receiver.setLowestPressure(lowestPressure.intValue());
                }
                receiver.setLowestPressureTime(newT.getTimeInMillis());
            }
        }
        return receiver;
    }

}
