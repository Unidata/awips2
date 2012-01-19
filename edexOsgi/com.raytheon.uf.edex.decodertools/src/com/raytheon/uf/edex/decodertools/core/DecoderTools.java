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
package com.raytheon.uf.edex.decodertools.core;

import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.ASCII_CR;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.ASCII_LF;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.ASCII_SP;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.VAL_ERROR;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.VAL_MISSING;
import static com.raytheon.uf.edex.decodertools.core.IDecoderConstants.WMO_HEADER;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Various tools to locate and manipulate parts of a weather message.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27 July 2007        411 jkorman     Initial Development
 * 31 July 2007        411 jkorman     Modified WMO_HEADER regex to accept trailing
 *                                     data on line.
 * 10 Aug 2007         379 jkorman     Added stripMessage, extracted from
 *                                     BinLightningDecoder for general use.
 * 14 Aug 2007         379 jkorman     Modified stripMessage and isolateWMOHeader
 *                                     because of problems with byte array/string
 *                                     encoding/decoding errors.                                      
 * 20070912            379 jkorman     Code review cleanup.
 * 20071003            391 jkorman     Factored out isEqual method from decoders.
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class DecoderTools {
    //
    public static final double CEL_TO_KELVIN_OFFSET = 273.15;

    public static final String NO_DATA_MSG = "NODATA:";

    public static final String INGEST_FILE_NAME = "ingestfilename";

    /**
     * Aligns input data to the starting position of the WMO header. In the
     * event that the header could not be found, the original data is returned.
     * All leading data up to the beginning of the WMO header is removed.
     * 
     * @param messageData
     *            The input data to search.
     * @return The message data with leading data removed.
     */
    public static byte[] isolateWMOHeader(byte[] messageData) {
        byte[] retMessage = null;

        String s = new String(messageData);

        Pattern p = Pattern.compile(WMO_HEADER);

        Matcher m = p.matcher(s);
        if (m.find()) {
            int dlen = s.length() - m.start();
            if (dlen > 0) {
                retMessage = new byte[dlen];
                System.arraycopy(messageData, m.start(), retMessage, 0, dlen);
            } else {
                retMessage = messageData;
            }
        } else {
            retMessage = messageData;
        }
        return retMessage;
    }

    /**
     * Attempt to find the start of the data. If the pattern is found all data
     * from the end of the pattern to the end of the data is returned. This
     * removes all leading data to the end of the WMO header.
     * 
     * @param data
     *            Target data that may contain a search pattern.
     * @return The extracted data. Return a null reference if no match is found.
     */
    public static byte[] stripWMOHeader(byte[] data, String pattern) {
        Pattern p = Pattern.compile(pattern);

        String s = new String(data);

        byte[] retMessage = null;
        Matcher m = p.matcher(s);
        if (m.find()) {
            int dlen = s.length() - m.end();
            if (dlen > 0) {
                retMessage = new byte[dlen];
                System.arraycopy(data, m.end(), retMessage, 0, dlen);
            } else {
                retMessage = data;
            }
        }

        return retMessage;
    }

    /**
     * Collapse all carriage control into a single carriage control, collapse
     * runs of spaces into a single space.
     * 
     * @param messageData
     * @return
     */
    public static byte[] cleanData(byte[] messageData) {
        byte[] retMessage = null;
        if (messageData != null) {
            retMessage = new byte[messageData.length];
            byte lastChar = 0;
            int retMsgIdx = 0;
            for (int i = 0; i < messageData.length; i++) {
                switch (messageData[i]) {
                case ASCII_LF:
                case ASCII_CR: {
                    if (lastChar == ASCII_CR) {
                        continue;
                    }
                    retMessage[retMsgIdx++] = ASCII_CR;
                    lastChar = ASCII_CR;
                    break;
                }
                case ASCII_SP: {
                    if (lastChar == ASCII_SP) {
                        continue;
                    }
                    retMessage[retMsgIdx++] = ASCII_SP;
                    lastChar = ASCII_SP;
                    break;
                }
                default: {
                    retMessage[retMsgIdx++] = messageData[i];
                    lastChar = messageData[i];
                    break;
                }
                }
            }
            // when we get here, retMsgIdx is equal to the cleaned message
            // length.
            byte[] t = new byte[retMsgIdx];
            if (t.length > 0) {
                System.arraycopy(retMessage, 0, t, 0, retMsgIdx);
                retMessage = t;
            }
        }
        return retMessage;
    }

    /**
     * Are two objects equal to each other.
     * 
     * @param first
     *            First object to compare.
     * @param second
     *            Second object to compare.
     * @return Are the objects equal.
     */
    public static boolean isEqual(Object first, Object second) {
        // are both pointing to the same object or both null?
        boolean retValue = (first == second);

        if (!retValue) {
            if (first == null) {
                // we can do this because only one of the operands is null!
                retValue = isEqual(second, first);
            } else {
                retValue = first.equals(second);
            }
        }
        return retValue;
    }

    /**
     * Convert decoded Celsius to Kelvin. For example a value of 102 with a sign
     * of -1, and scale of 10 gives a value of 10.2 converted to Kelvin as
     * 283.33.
     * 
     * @param value
     *            The raw decoded value.
     * @param sign
     *            The sign of the data. 1 or -1.
     * @param scale
     *            A scale value that will be used when value must be scaled.
     * @return Calculated Kelvin temperature.
     */
    public static double celsiusToKelvin(int value, int sign, double scale) {
        return ((value * sign) / scale) + CEL_TO_KELVIN_OFFSET;
    }

    /**
     * Convert decoded Celsius to Kelvin. For example a value of 102 with a sign
     * of -1, and scale of 10 gives a value of 10.2 converted to Kelvin as
     * 283.33.
     * 
     * @param value
     *            The raw decoded value.
     * @param sign
     *            The sign of the data. 1 or -1.
     * @param scale
     *            A scale value that will be used when value must be scaled.
     * @return Calculated Kelvin temperature.
     */
    public static double celsiusToKelvin(double value, int sign, double scale) {
        return ((value * sign) / scale) + CEL_TO_KELVIN_OFFSET;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Double celsiusToFahrenheit(Double value) {
        Double temp = null;
        if (value != null) {
            temp = (9.0 / 5.0 * value) + 32.0;
        }
        return temp;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Double fahrenheitToCelsius(Double value) {
        Double temp = null;
        if (value != null) {
            temp = (value - 32.0) * 5.0 / 9.0;
        }
        return temp;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Double celsiusToKelvin(Double value) {
        Double temp = null;
        if (value != null) {
            temp = value + CEL_TO_KELVIN_OFFSET;
        }
        return temp;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Double kelvinToCelsius(Double value) {
        Double temp = null;
        if (value != null) {
            temp = value - CEL_TO_KELVIN_OFFSET;
        }
        return temp;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Double inToPa(Double value) {
        Double temp = null;
        if (value != null) {
            temp = value * 3386.0;
        }
        return temp;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Double paToIn(Double value) {
        Double temp = null;
        if (value != null) {
            temp = value / 3386.0;
        }
        return temp;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Double hPaToPa(Double value) {
        Double temp = null;
        if (value != null) {
            temp = value * 100.0;
        }
        return temp;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Double paTohPa(Double value) {
        Double temp = null;
        if (value != null) {
            temp = value / 100.0;
        }
        return temp;
    }

    /**
     * Convert a wind speed in knots to meters per second.
     * 
     * @param value
     *            A value in knots.
     * @return Speed converted to meters per second.
     */
    public static Double ktsToMSec(Double value) {
        Double speed = null;
        if (value != null) {
            speed = value * 0.514444D;
        }
        return speed;
    }

    /**
     * Convert a wind speed in knots to meters per second.
     * 
     * @param value
     *            A value in knots.
     * @return Speed converted to meters per second.
     */
    public static double knotsToMSec(double value) {
        return value * 0.514444D;
    }

    /**
     * Convert a pressure in hectoPascals to Pascals.
     * 
     * @param value
     * @return
     */
    public static double hPaToPascals(int value) {
        return value * 10.0D;
    }

    /**
     * Find the position of a given target byte array in a data array.
     * 
     * @param target
     * @param data
     * @return
     */
    public static int indexOf(int startAt, byte[] target, byte[] data) {
        int dataPos = -1;
        // If any of the following are true, nothing to find.
        if ((target == null) || (data == null)) {
            return dataPos;
        }
        if (startAt >= data.length) {
            return dataPos;
        }
        if (target.length > data.length - startAt) {
            return dataPos;
        }

        int tagPos = 0;
        int foundPos = -1;
        for (int i = startAt; i < data.length; i++) {
            if (data[i] == target[tagPos]) {
                // Potential found location
                foundPos = i;
                for (int ii = i; ii < data.length; ii++) {
                    if (data[ii] == target[tagPos]) {
                        tagPos++;
                        if (tagPos == target.length) {
                            break;
                        }
                    } else {
                        tagPos = 0;
                        foundPos = -1;
                        break;
                    }
                }
                if (foundPos >= 0) {
                    dataPos = foundPos;
                    break;
                }
            } else {
                tagPos = 0;
                foundPos = -1;
            }
        }
        return dataPos;
    }

    /**
     * Convert a defined substring within an element to an integer value. If the
     * substring equals a string of slashes a value of VAL_MISSING is returned.
     * If a number conversion error occurs a value of VAL_ERROR is returned.
     * 
     * @param element
     *            The text element to decode.
     * @param start
     *            The start position within the text element.
     * @param stop
     *            The stop position within the text element.
     * @return
     */
    public static final Integer getInt(String element, int start, int stop) {
        Integer retValue = null;

        if (element != null) {
            if ((start >= 0) && (stop <= element.length()) && (start < stop)) {
                // If the first character of the substring is a slash, then
                // assume that we are dealing with missing data.
                if (element.charAt(start) == '/') {
                    retValue = VAL_MISSING;
                    // Make sure all the specified data are slashes.
                    for (int i = start; i < stop; i++) {
                        // If not, then its an error.
                        if (element.charAt(i) != '/') {
                            retValue = VAL_ERROR;
                            break;
                        }
                    }
                } else {
                    try {
                        retValue = Integer.parseInt(element.substring(start,
                                stop));
                    } catch (NumberFormatException nfe) {
                        retValue = VAL_ERROR;
                    }
                }
            }
        }

        return retValue;
    }

    /**
     * Create a Coordinate instance with a given latitude, longitude.
     * 
     * @param latitude
     *            The coordinate latitude.
     * @param longitude
     *            The coordinate longitude.
     * @return The created Coordinate instance.
     */
    public static Coordinate createCoordinate(double latitude, double longitude) {
        return new Coordinate(longitude, latitude);
    }

    /**
     * Helper method so we don't have to remember Coordinate x and y.
     * 
     * @param coord
     * @return The Coordinate longitude.
     */
    public static double getCoordinateLongitude(Coordinate coord) {
        return coord.x;
    }

    /**
     * Helper method so we don't have to remember Coordinate x and y.
     * 
     * @param coord
     * @return The Coordinate latitude.
     */
    public static double getCoordinateLatitude(Coordinate coord) {
        return coord.y;
    }

    /**
     * 
     * @param obsText
     * @param wmoHeader
     * @return
     */
    public static String normalizeObs(String obsText, String wmoHeader) {
        String retText = null;

        if ((obsText != null) && (wmoHeader != null)) {
            StringBuilder product = new StringBuilder(wmoHeader);
            product.append("\n");
            char lastChar = '\n';
            for (int i = 0; i < obsText.length(); i++) {

                char c = obsText.charAt(i);
                if ('\r' == c) {
                    c = '\n';
                }
                if (('\n' == lastChar) && (lastChar == c)) {
                    // nothing
                } else {
                    product.append(c);
                    lastChar = c;
                }
            }
            retText = product.toString();
        }
        return retText;
    }

}
