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

import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.getInt;
import static com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder.matchElement;

import com.raytheon.uf.edex.decodertools.core.DataItem;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Various methods for decoding specific common data from the synoptic
 * observation.
 * 
 * <pre>
 *   Parameter          Section     group
 *   Relative Humidity  1             2  
 *   Temperature        1             1
 *   Max Temp           3             1
 *   Min Temp           3             2
 *   Dewpoint           1             2
 *   Sea surface Temp   2             0           
 *   Wetbulb            2             8
 *   Station Pressure   1             3
 *   Sea Level Pressure 1             4
 *   Pressure Change    1             5
 *   Precipitation      1             6
 *   Precipitation      5             2    Block 72 regional
 *   Precipitation      3             7    24 hour     
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070925            391 jkorman     Initial Coding.
 * 20071109            391 jkorman     Factored out time constants.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SynopticGroups {

    private static final int PREFIX_START = 0;

    private static final String RH_PREFIX = "29";

    // Precip observation period in hours indexed by tsubr in group 6.
    // TODO : This needs to be external.
    private static int[] precipHours = { -1, 6, 12, 18, 24, 1, 2, 3, 9, 15 };

    /**
     * Decode the relative humidity group.
     * 
     * @param groupData
     *            Data group to be decoded.
     * @param lookingForSect
     *            The Section that contains the data.
     * @return
     */
    public static DataItem decodeRelativeHumidity(String groupData,
            int lookingForSect) {
        DataItem decodedItem = null;

        if (matchElement(groupData, "2\\d{4}")) {
            Integer val = Integer.parseInt(groupData.substring(2, 5));
            if ((val != null) && (val >= 0)) {
                if (lookingForSect == 1) {
                    if (RH_PREFIX.equals(groupData.substring(PREFIX_START,
                            RH_PREFIX.length()))) {
                        decodedItem = new DataItem("Percent", "relHum",
                                lookingForSect);
                        decodedItem.setDataValue(val.doubleValue());
                    }
                }
            }
        }

        return decodedItem;
    }

    /**
     * Decode the temperature data in various synoptic groups.
     * 
     * @param groupData
     *            Data group to be decoded.
     * @param lookingForSect
     *            The Section that contains the data.
     * @return
     */
    public static DataItem decodeTemperature(String groupData,
            int lookingForSect) {
        // Most data is coded to 0.1 degrees.
        double defaultTempScale = 10;

        DataItem decodedItem = null;

        if ((groupData != null) && (groupData.length() == 5)) {
            String s = groupData.substring(2, 5);
            if ("///".equals(s)) {
                return decodedItem;
            }
            Integer val = getInt(groupData, 2, 5);
            if ((val != null) && (val >= 0)) {
                String dataItemName = null;
                int sign = 0;
                char sn = groupData.charAt(1);
                switch (groupData.charAt(PREFIX_START)) {
                case '0': {
                    if (lookingForSect == 2) {
                        if ((sign = getSign(sn)) != 0) {
                            decodedItem = new DataItem("Kelvin", "seaSfcTemp",
                                    lookingForSect);
                            decodedItem.setDataValue(DecoderTools
                                    .celsiusToKelvin(val, sign,
                                            defaultTempScale));
                        }
                    }
                    break;
                }
                case '1': {
                    if (lookingForSect == 1) {
                        dataItemName = "airTemp";
                    } else if (lookingForSect == 3) {
                        dataItemName = "maxTemp";
                    }

                    if ((dataItemName != null) && ((sign = getSign(sn)) != 0)) {
                        decodedItem = new DataItem("Kelvin", dataItemName,
                                lookingForSect);
                        decodedItem.setDataValue(DecoderTools.celsiusToKelvin(
                                val, sign, defaultTempScale));
                    }
                    break;
                }
                case '2': {
                    if (lookingForSect == 1) {
                        dataItemName = "dewTemp";
                    } else if (lookingForSect == 2) {
                        dataItemName = "seaTemp";
                    } else if (lookingForSect == 3) {
                        dataItemName = "minTemp";
                    }

                    if ((dataItemName != null) && ((sign = getSign(sn)) != 0)) {
                        decodedItem = new DataItem("Kelvin", dataItemName,
                                lookingForSect);
                        decodedItem.setDataValue(DecoderTools.celsiusToKelvin(
                                val, sign, defaultTempScale));
                    }
                    break;
                }
                case '8': {
                    if (lookingForSect == 2) {
                        if ((sign = getSign(sn)) != 0) {
                            decodedItem = new DataItem("Kelvin", "wetBulbTemp",
                                    lookingForSect);
                            decodedItem.setDataValue(DecoderTools
                                    .celsiusToKelvin(val, sign,
                                            defaultTempScale));
                        }
                    }
                    break;
                }
                    // No default specified!
                } // end.switch
            }
        }
        return decodedItem;
    }

    /**
     * Decode the station pressure group data.
     * 
     * @param groupData
     *            Data group to be decoded.
     * @param lookingForSect
     *            The Section that contains the data.
     * @return
     */
    public static DataItem decodeStationPressure(String groupData,
            int lookingForSect) {
        DataItem decodedItem = null;

        if ((groupData != null) && (groupData.length() == 5)) {
            if ((groupData.charAt(0) == '3') && (lookingForSect == 1)) {

                Integer val = getInt(groupData, 1, 5);
                if ((val != null) && (val >= 0)) {
                    decodedItem = new DataItem("Pascals", "stationPressure",
                            lookingForSect);
                    // RULE : If the value is between 0 and 100, assume the
                    // value
                    // is above 1000 hPa i.e. 0132 --> 1013.2 hPa --> 101320 Pa
                    if (val < 1000) {
                        decodedItem
                                .setDataValue(DecoderTools.hPaToPascals(val) + 100000);
                    } else {
                        decodedItem
                                .setDataValue(DecoderTools.hPaToPascals(val));
                    }
                }
            }
        }

        return decodedItem;
    }

    /**
     * Decode the synoptic station pressure value in section 1.
     * 
     * @param groupData
     *            Data group to be decoded.
     * @param lookingForSect
     *            The Section that contains the data.
     * @return
     */
    public static DataItem decodeSeaLevelPressure(String groupData,
            int lookingForSect) {
        DataItem decodedItem = null;

        if ((groupData != null) && (groupData.length() == 5)) {
            if ((groupData.charAt(0) == '4') && (lookingForSect == 1)) {

                Integer val = getInt(groupData, 1, 5);
                if ((val != null) && (val >= 0)) {
                    decodedItem = new DataItem("Pascals", "seaLevelPressure",
                            lookingForSect);
                    // RULE : If the value is between 0 and 100, assume the
                    // value
                    // is above 1000 hPa i.e. 0132 --> 1013.2 hPa --> 101320 Pa
                    if (val < 1000) {
                        decodedItem
                                .setDataValue(DecoderTools.hPaToPascals(val) + 100000);
                    } else {
                        decodedItem
                                .setDataValue(DecoderTools.hPaToPascals(val));
                    }
                }
            }
        }

        return decodedItem;
    }

    /**
     * Decode the synoptic station pressure value in section 1.
     * 
     * @param groupData
     *            Data group to be decoded.
     * @param lookingForSect
     *            The Section that contains the data.
     * @return
     */
    public static DataItem decodePressureChange(String groupData,
            int lookingForSect) {
        DataItem decodedItem = null;

        if ((groupData != null) && (groupData.length() == 5)) {
            if ((groupData.charAt(0) == '5') && (lookingForSect == 1)) {

                Integer val = getInt(groupData, 2, 5);
                if ((val != null) && (val >= 0)) {
                    decodedItem = new DataItem("Pascals", "3HRChange",
                            lookingForSect);
                    decodedItem.setDataValue(DecoderTools.hPaToPascals(val));
                    decodedItem.setDataPeriod(3 * TimeTools.SECONDS_HOUR);
                }
            }
        }

        return decodedItem;
    }

    /**
     * Decode various precipitation data from synoptic observations.
     * 
     * @param groupData
     *            Data group to be decoded.
     * @param lookingForSect
     *            The Section that contains the data.
     * @return
     */
    public static DataItem decodePrecip(String groupData, int lookingForSect) {
        DataItem decodedItem = null;

        if ((groupData != null) && (groupData.length() == 5)) {
            if ((groupData.charAt(0) == '6')) {
                if ((lookingForSect == 1) || (lookingForSect == 3)) {
                    Integer val = getInt(groupData, 1, 4);
                    if (val != null) {
                        decodedItem = new DataItem("millimeters", "precip",
                                lookingForSect);
                        if ((val >= 0) && (val < 990)) {
                            decodedItem.setDataValue(val.doubleValue());
                        } else {
                            double value = (val.doubleValue() - 990.0) / 10;
                            decodedItem.setDataValue(value);
                        }

                        val = getInt(groupData, 4, 5);
                        if ((val != null) && (val >= 0)) {
                            decodedItem.setDataPeriod(precipHours[val]
                                    * TimeTools.SECONDS_HOUR);
                        }
                    }
                }
            } else if ((groupData.charAt(0) == '7') && (lookingForSect == 3)) {

                Integer val = getInt(groupData, 1, 5);
                if ((val != null) && (val >= 0)) {
                    decodedItem = new DataItem("millimeters", "precip",
                            lookingForSect);
                    if (val > 9998) {
                        val = 0;
                    }
                    decodedItem.setDataValue(val.doubleValue() / 10.0);
                    decodedItem.setDataPeriod(TimeTools.SECONDS_DAY);
                }
            } else if ((groupData.charAt(0) == '2') && (lookingForSect == 5)) {
                Integer val = getInt(groupData, 1, 5);
                if ((val != null) && (val >= 0)) {
                    decodedItem = new DataItem("millimeters", "cityPrecip",
                            lookingForSect);
                    if (!IDecoderConstants.VAL_MISSING.equals(val)) {
                        // convert 1/100ths of an inch to millimeters.
                        decodedItem
                                .setDataValue(val.doubleValue() / 100 * 25.4);
                    } else {
                        decodedItem.setDataValue(val.doubleValue());
                    }
                    decodedItem.setDataPeriod(TimeTools.SECONDS_DAY);
                }
            }
        }

        return decodedItem;
    }

    /**
     * Get the value of the sign character. 0 = positive, 1 = negative;
     * 
     * @param sign
     *            The sign character to check.
     * @return The sign value. 1 if positive, -1 if negative. A value of zero is
     *         returned if the sign character is invalid.
     */
    public static int getSign(char sign) {
        int signVal = 0;
        if (sign == '0') {
            signVal = 1;
        } else if (sign == '1') {
            signVal = -1;
        }
        return signVal;
    }

    /**
     * 
     * This is a direct port from the NCEP synoptic decoders.
     * 
     * @param di
     * @param wmoRegion
     * @param obsHour
     * @return
     */
    public static DataItem calcMinMaxPeriod(DataItem di,
            final Integer wmoRegion, final Integer obsHour) {

        final Integer hour00 = new Integer(0);
        final Integer hour23 = new Integer(23);

        // TODO : These table values need to be moved to an external
        // configuration.
        final int[] hourTbl = { 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3,
                3, 4, 4, 4, 4, 4, 4, 1, };

        final int[][] minTbl = { { 99, 12, 99, 18, 24, 99, 12, },
                { 12, 12, 99, 24, 99, 12, 99, },
                { 99, 12, 12, 12, 99, 99, 12, },
                { 99, 12, 99, 24, 99, 99, 99, }, };

        final int[][] maxTbl = { { 99, 12, 12, 12, 99, 99, 12, },
                { 99, 12, 99, 24, 99, 99, 99, },
                { 99, 12, 99, 24, 24, 99, 12, },
                { 12, 12, 99, 12, 99, 12, 99, }, };

        int regionIdx = wmoRegion;
        if ((regionIdx > 0) && (regionIdx < 8)) {
            regionIdx--;
            Integer period = null;
            if ((hour00.compareTo(obsHour) >= 0)
                    && (hour23.compareTo(obsHour) <= 0)) {
                if ("minTemp".equals(di.getDataName())) {
                    int p = minTbl[regionIdx][hourTbl[obsHour]];
                    if (p != 99) {
                        period = new Integer(p * TimeTools.SECONDS_HOUR);
                    }
                } else if ("maxTemp".equals(di.getDataName())) {
                    int p = maxTbl[regionIdx][hourTbl[obsHour]];
                    if (p != 99) {
                        period = new Integer(p * TimeTools.SECONDS_HOUR);
                    }
                }
            }
            if (period != null) {
                di.setDataPeriod(period);
            }
        }
        return di;
    }
}
