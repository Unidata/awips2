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

import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.getInt;
import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.matchElement;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_5_LEAD;

import java.util.Arrays;
import java.util.Calendar;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SynopticSec5Decoder;
import com.raytheon.uf.common.dataplugin.sfcobs.InterWinds;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.ReportParser;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

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
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class Sec5MaritimeDecoder extends SynopticSec5Decoder {
    private static final int NUM_WINDS = 6;

    private Double wind10mSpeed = null;

    private Double wind20mSpeed = null;

    private Integer pkWindTimeHour = null;

    private Integer pkWindTimeMinute = null;

    private Integer pkWindDirection = null;

    private Double pkWindSpeed = null;

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
        if (reportParser.positionTo(SEC_5_LEAD)) {
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
                if (doGroup(1) && matchElement(element, "11[/0-9]{3}")) {
                    // extrapolated 10 meter wind speed
                    Integer temp = getInt(element, 2, 5);
                    if ((temp != null) && (temp >= 0)) {
                        wind10mSpeed = temp.doubleValue() / 10.0;
                    }
                    closeGroup(1);
                } else if (doGroup(2) && matchElement(element, "22[/0-9]{3}")) {
                    // extrapolated 20 meter wind speed
                    Integer temp = getInt(element, 2, 5);
                    if ((temp != null) && (temp >= 0)) {
                        wind20mSpeed = temp.doubleValue() / 10.0;
                    }
                    closeGroup(2);
                } else if (doGroup(3) && matchElement(element, "3[/0-9]{4}")) {
                    // peak wind data time
                    pkWindTimeHour = getInt(element, 1, 3);
                    pkWindTimeMinute = getInt(element, 3, 5);
                    closeGroup(3);
                } else if (doGroup(4) && matchElement(element, "4[/0-9]{5}")) {
                    // peak wind data time from CMAN
                    pkWindDirection = getInt(element, 1, 3);
                    if ((pkWindDirection != null) && (pkWindDirection >= 0)) {
                        pkWindDirection *= 10;
                    }
                    Integer spd = getInt(element, 3, 6);
                    if ((spd != null) && (spd >= 0)) {
                        pkWindSpeed = spd.doubleValue();
                    }
                    closeGroup(4);
                } else if (doGroup(4) && matchElement(element, "4[/0-9]{4}")) {
                    // peak wind data time
                    pkWindDirection = getInt(element, 1, 3);
                    if ((pkWindDirection != null) && (pkWindDirection >= 0)) {
                        pkWindDirection *= 10;
                    }
                    Integer spd = getInt(element, 3, 5);
                    if ((spd != null) && (spd >= 0)) {
                        pkWindSpeed = spd.doubleValue();
                    }
                    closeGroup(4);
                } else if (doGroup(6) && matchElement(element, "6[/0-9]{4}")) {
                    // Continuous wind speed data
                    windTimeHour = getInt(element, 1, 3);
                    windTimeMinute = getInt(element, 3, 5);

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
                        if (matchElement(element, "[/0-9]{6}")) {
                            Integer wd = getInt(element, 0, 3);
                            Integer ws = getInt(element, 3, 6);
                            if ((wd != null) && (wd >= 0)) {
                                windDirs[i] = wd;
                            }
                            if ((ws != null) && (ws >= 0)) {
                                windSpeeds[i] = ws.doubleValue() / 10.0;
                            }
                        }
                    }
                    closeGroup(6);
                } else if (matchElement(element, "TIDE[/0-9]{4}")) {
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
        windSpeeds = null;
        windDirs = null;
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
                    wSpeed = DecoderTools.knotsToMSec(wSpeed);
                }
                receiver.setWind10mSpeed(wSpeed);
            }
            if (wind20mSpeed > -9999.0) {
                wSpeed = wind20mSpeed;
                if (inKnots) {
                    wSpeed = DecoderTools.knotsToMSec(wSpeed);
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
                        wSpeed = DecoderTools.knotsToMSec(wSpeed);
                    }
                }
                receiver.setPeakWindSpeed(wSpeed);
                receiver.setPeakWindTime(newT.getTimeInMillis());
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
                                    wSpeed = DecoderTools.knotsToMSec(wSpeed);
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
                            wind.setObsTime(TimeTools.copy(newT));
                            receiver.addInterWind(wind);
                        }
                        // adjust the time back ten minutes
                        newT.add(Calendar.MINUTE, -10);
                    } // for
                }
            }

        }
        return receiver;
    }

}
