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
package com.raytheon.edex.plugin.sfcobs.decoder.synoptic;

import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.checkRange;
import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.getInt;
import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.matchElement;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_2_LEAD;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_3_LEAD;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_4_LEAD;
import static com.raytheon.edex.plugin.sfcobs.decoder.synoptic.ISynoptic.SEC_5_LEAD;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.VAL_ERROR;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.VAL_MISSING;

import java.util.Calendar;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.sfcobs.AncPrecip;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.core.DataItem;
import com.raytheon.uf.edex.decodertools.core.ReportParser;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Decode synoptic section 1 data. This section has a single group which
 * describes a cloud layer at a level below that of the observing station.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071010            391 jkorman     Initial coding.
 * 20071109            391 jkorman     Added guard for short data.
 * 20080123            757 jkorman     Ensure that the group 9 obs time has
 *                                     the correct day of month.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SynopticSec1Decoder extends AbstractSectionDecoder {
    // The report observation hour - from 9 group
    private Integer obsTimeHour = null;

    // The report observation minute - from 9 group
    private Integer obsTimeMinute = null;

    // private Integer iSubR = null;

    // private Integer iSubX = null;

    private Integer visibility = null;

    private Integer windDirection = null;

    private Double windSpeed = null;

    private DataItem airTemp = null;

    private DataItem dewTemp = null;

    private DataItem relHumid = null;

    private DataItem stationPressure = null;

    private DataItem seaLevelPressure = null;

    private DataItem pressureChange = null;

    private DataItem changeCharacter = null;

    private DataItem precip = null;

    private Integer totalCloud = null;

    private Integer presentWeather = null;

    private Integer pastWeather1 = null;

    private Integer pastWeather2 = null;

    private Integer loCloudHeight = null;

    private Integer lowCloudAmount = null;

    private Integer lowCloudType = null;

    private Integer midCloudType = null;

    private Integer hiCloudType = null;

    /**
     * 
     * @param parent
     */
    public SynopticSec1Decoder(AbstractSynopticDecoder parent) {
        super(parent);
    }

    /**
     * Decode this section data.
     * 
     * @param reportParser
     *            Parser containing elements of the observation to be decoded.
     * @throws DecoderException
     *             Thrown when an relevant error has occurred.
     */
    public void decode(ReportParser reportParser) throws DecoderException {
        final Integer speed99 = new Integer(99);
        String element = null;

        boolean irix = false;
        boolean winds = false;

        if (reportParser == null) {
            // nothing to do.
            return;
        }

        resetGroups();
        // We don't use group "0" in section 1
        closeGroup(0);
        while (true) {
            // if we run out of data, exit.
            if (reportParser.next()) {
                if ((element = reportParser.getElement()) == null) {
                    break;
                }
            } else {
                break;
            }
            if (!irix && !winds
                    && matchElement(element, ISynoptic.SEC_1_IRIXHVV)) {
                // iSubR = getInt(element, 0, 1);
                getInt(element, 0, 1);

                // iSubX = getInt(element, 1, 2);
                getInt(element, 1, 2);

                loCloudHeight = getInt(element, 2, 3);
                visibility = getInt(element, 3, 5);
                irix = true;
                continue;
            } else if (!winds && matchElement(element, ISynoptic.SEC_1_NDDFF)) {
                totalCloud = getInt(element, 0, 1);
                Integer temp = getInt(element, 1, 3);
                if ((temp != null) && (temp >= 0)) {
                    windDirection = temp * 10;
                }
                temp = getInt(element, 3, 5);
                if ((temp != null) && (temp >= 0)) {
                    windSpeed = temp.doubleValue();
                }
                if (speed99.equals(windSpeed)) {
                    if (!reportParser.next()) {
                        winds = true;
                        continue;
                    }
                    element = reportParser.getElement();
                    if ("00".equals(element.substring(0, 2))) {
                        windSpeed = getInt(element, 2, 5).doubleValue();
                        winds = true;
                    } else {
                        // TODO : Need to indicate an error here!
                    }
                } else {
                    winds = true;
                }
                continue;
            } else if (winds && matchElement(element, SEC_2_LEAD)) {
                break;
            } else if (SEC_3_LEAD.equals(element)) {
                break;
            } else if (SEC_4_LEAD.equals(element)) {
                break;
            } else if (SEC_5_LEAD.equals(element)) {
                break;
            } else if ("80000".equals(element)) {
                break;
            }

            if (element.length() < 5) {
                // for now skip over possible ill-formed data.
                continue;
            }
            if ("1".equals(element.substring(0, 1)) && doGroup(1)) {
                airTemp = SynopticGroups.decodeTemperature(element, 1);
                closeGroup(1);
                winds = true;
            } else if ("2".equals(element.substring(0, 1)) && doGroup(2)) {
                if ((dewTemp = SynopticGroups.decodeTemperature(element, 1)) == null) {
                    // no dew point, so check if relative humidity.
                    relHumid = SynopticGroups
                            .decodeRelativeHumidity(element, 1);
                }
                winds = true;
            } else if ("3".equals(element.substring(0, 1)) && doGroup(3)) {
                stationPressure = SynopticGroups.decodeStationPressure(element,
                        1);
                closeGroup(3);
                winds = true;
            } else if ("4".equals(element.substring(0, 1)) && doGroup(4)) {
                // Sea level pressure | geo-potential pressure height.
                // 1 1000
                // 2 925
                // 5 500
                // 7 700
                // 8 850
                seaLevelPressure = SynopticGroups.decodeSeaLevelPressure(
                        element, 1);
                closeGroup(4);
                winds = true;
            } else if ("5".equals(element.substring(0, 1)) && doGroup(5)) {
                Integer val = getInt(element, 1, 2);
                changeCharacter = new DataItem("int", "changeCharacter", 1);
                changeCharacter.setDataValue(val.doubleValue());
                changeCharacter.setDataPeriod(3 * 3600);
                pressureChange = SynopticGroups
                        .decodePressureChange(element, 1);
                closeGroup(5);
                winds = true;
            } else if ("6".equals(element.substring(0, 1)) && doGroup(6)) {
                precip = SynopticGroups.decodePrecip(element, 1);
                closeGroup(6);
                winds = true;
            } else if ("7".equals(element.substring(0, 1)) && doGroup(7)) {
                // TODO :
                presentWeather = getInt(element, 1, 3);
                pastWeather1 = getInt(element, 3, 4);
                pastWeather2 = getInt(element, 4, 5);

                closeGroup(7);
                winds = true;
            } else if ("8".equals(element.substring(0, 1)) && doGroup(8)) {
                if (!winds && matchElement(element, "8[/0-9]{4}")) {

                    if ((lowCloudAmount = getInt(element, 1, 2)) < 0) {
                        lowCloudAmount = null;
                    }
                    if ((lowCloudType = getInt(element, 2, 3)) < 0) {
                        lowCloudType = null;
                    }
                    if ((midCloudType = getInt(element, 3, 4)) < 0) {
                        midCloudType = null;
                    }
                    if ((hiCloudType = getInt(element, 4, 5)) < 0) {
                        hiCloudType = null;
                    }
                    closeGroup(8);
                }
                winds = true;
            } else if ("9".equals(element.substring(0, 1)) && doGroup(9)) {
                obsTimeHour = getInt(element, 1, 3);
                obsTimeMinute = getInt(element, 3, 5);
                break;
            }
        } // while()
    }

    /**
     * Populate an ObsCommon object with the decoded data from this section.
     * 
     * @param receiver
     *            An ObsCommon to receive the decoded data.
     * @return The populated receiver object.
     */
    public ObsCommon getDecodedData(ObsCommon receiver) {
        final Double[] ignore = { VAL_ERROR.doubleValue(),
                VAL_MISSING.doubleValue() };

        // Check to see if we need to convert knots to mps.
        int iSubW = decoderParent.getISubw();
        double conversion = 1.0;
        if ((iSubW == 3) || (iSubW == 4)) {
            conversion = 0.5145;
        }

        if ((visibility != null) && (visibility >= 0)) {

            Integer v = mapHorizontalVis(visibility);
            if ((v != null) && (v >= 0)) {
                receiver.setHorzVisibility(v);
            }
        }

        receiver.setTemp(DataItem.getValue(airTemp, ignore));
        receiver.setDwpt(DataItem.getValue(dewTemp, ignore));
        receiver.setHumidity(DataItem.getValue(relHumid, ignore));

        if ((windDirection != null) && (windSpeed != null)) {
            receiver.setWindDirection(windDirection);
            receiver.setWindSpeed(windSpeed * conversion);
        }

        if (seaLevelPressure != null) {
            Double d = DataItem.getValue(seaLevelPressure, ignore);
            if (d != null) {
                receiver.setPressureSealevel(d.intValue());
            }
        }
        if (stationPressure != null) {
            Double d = DataItem.getValue(stationPressure, ignore);
            if (d != null) {
                receiver.setPressureStation(d.intValue());
            }
        }
        if ((totalCloud != null) && (totalCloud >= 0)) {
            receiver.setTotalCloudCover(totalCloud);
        }
        if ((presentWeather != null) && (presentWeather >= 0)) {
            receiver.setWx_present(presentWeather);
        }
        if ((pastWeather1 != null) && (pastWeather1 >= 0)) {
            receiver.setWx_past_1(pastWeather1);
        }
        if ((pastWeather2 != null) && (pastWeather2 >= 0)) {
            receiver.setWx_past_2(pastWeather2);
        }

        if ((pressureChange != null) && (changeCharacter != null)) {
            receiver.setPressChange3Hr(DataItem
                    .getValue(pressureChange, ignore));
            Double v = DataItem.getValue(changeCharacter, ignore);
            if (v != null) {
                receiver.setPressChangeChar(v.intValue());
            } else {
                receiver.setPressChangeChar(null);
            }
        }
        if (precip != null) {
            Double v = precip.getDataValue();
            Integer p = precip.getDataPeriod();
            if ((v != null) && (v > 0) && (p != null)) {
                AncPrecip precip = new AncPrecip();
                precip.setPrecipAmount(v);
                precip.setTimePeriod(p);
                precip.setObsType(AncPrecip.PRECIP_TOTAL);
                receiver.addPrecip(precip);
            }
        }
        getCloudData(receiver);

        // Do we need to override the observation time
        if ((obsTimeHour != null) && (obsTimeMinute != null)) {
            if (checkRange(0, obsTimeHour, 23)
                    && checkRange(0, obsTimeMinute, 59)) {
                Calendar obsT = receiver.getTimeObs();

                Integer month = getParent().getHeader().getMonth();
                Integer year = getParent().getHeader().getYear();
                if (month == -1) {
                    month = null;
                }
                if (year == -1) {
                    year = null;
                }

                Calendar newT = AbstractSynopticDecoder.calculateObsDateTime(
                        obsT, obsT.get(Calendar.DAY_OF_MONTH), obsTimeHour,
                        year, month);
                newT.set(Calendar.MINUTE, obsTimeMinute);

                // Check if the observation time pushed us forward a day
                long delta = newT.getTimeInMillis() - obsT.getTimeInMillis();
                // Allow up to the reference hour + 30 minutes.
                if (delta > 30 * 60 * 1000) {
                    TimeTools.rollByDays(newT, -1);
                }

                receiver.setTimeObs(newT);
                DataTime dataTime = new DataTime(newT);
                receiver.setDataTime(dataTime);
            }
        }
        return receiver;
    }

    /**
     * Populate a Set collection with cloud data. If the collection passed in is
     * null then a new collection is created.
     * 
     */
    public void getCloudData(ObsCommon receiver) {
        if ((loCloudHeight != null) && (totalCloud != null)) {
            if (loCloudHeight >= 0) {
                receiver.setCloudBaseHeight(loCloudHeight);
            }
        }
        if ((lowCloudType != null) && (lowCloudType >= 0)) {
            receiver.setLowCloudType(lowCloudType);
        }

        if ((midCloudType != null) && (midCloudType >= 0)) {
            receiver.setMidCloudType(midCloudType);
        }

        if ((hiCloudType != null) && (hiCloudType >= 0)) {
            receiver.setHighCloudType(hiCloudType);
        }
    }

    /**
     * Map a synoptic visibility code figure (00..99) to an actual visibility in
     * meters.
     * 
     * @param vis
     *            The synoptic code figure.
     * @return The horizontal visibility.
     */
    private Integer mapHorizontalVis(Integer vis) {
        final int[] hiValues = { 35000, 40000, 45000, 50000, 55000, 60000,
                65000, 70000, 70000, 0, 50, 200, 500, 1000, 2000, 4000, 10000,
                20000, 50000, };
        final int firstHiValue = 81;
        final int loInvalidCode = 51;
        final int hiInvalidCode = 55;

        Integer visibility = null;
        if ((vis != null) && (vis >= 0)) {
            if (vis < loInvalidCode) {
                visibility = vis * 100;
            } else if (vis > hiInvalidCode) {
                if (vis < 81) {
                    visibility = (vis - 50) * 1000;
                } else {
                    visibility = hiValues[vis - firstHiValue];
                }
            }
        }
        return visibility;
    }
}
