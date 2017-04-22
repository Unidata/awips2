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
package com.raytheon.uf.edex.plugin.mpe.gather.dhr;

import java.util.Calendar;

import com.mchange.lang.ShortUtils;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * POJO representation of a DHR Radar Header.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2016 4625       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class DHRHeader {

    private static final class DHRHeaderIndices {
        public static final int LAT_INDEX1 = 10;

        public static final int LAT_INDEX2 = 11;

        public static final int LON_INDEX1 = 12;

        public static final int LON_INDEX2 = 13;

        public static final int PRO_ID_INDEX = 15;

        public static final int VOLCOVPAT_INDEX = 17;

        public static final int OPERMODE_INDEX = 18;

        public static final int BIAS_INDEX = 29;

        public static final int DBZMIN_INDEX = 30;

        public static final int DBZINC_INDEX = 31;

        public static final int DBZCNT_INDEX = 32;

        public static final int END_DATE_INDEX = 47;

        public static final int END_TIME_INDEX = 48;

        public static final int SAMPLE_SIZE_INDEX = 49;

        public static final int RADIAL_NUM_INDEX = 74;
    }

    public static final int HEADER_LENGTH = 75;

    /*
     * The product code specified in the header should be this value provided
     * that a valid DHR Radar file has been read.
     */
    private static final int EXPECTED_PRODUCT_CODE = 32;

    private static final float DBZ_DIVIDEND = 10.0f;

    private static final float BIAS_DIVIDEND = 100.0f;

    /*
     * The raw values that are read directly from the DHR Radar file header.
     */
    private final short[] rawHeader;

    private int productCode;

    private Calendar productDateTime;

    private int radialNumber;

    /*
     * This latitude and longitude is used within an hrap grid which is why it
     * does not appear to be the expected type.
     */
    private int latitude;

    private int longitude;

    /*
     * TODO: provide additional documentation for these fields. Would be more
     * beneficial to do so on the decode side because at that point, their use
     * is actually seen.
     */
    private short volcovpat;

    private short opermode;

    private float dbzmin;

    private float dbzinc;

    private float dbzcnt;

    private float bias;

    private short sampleSize;

    public DHRHeader(final short[] rawHeader) throws InvalidDHRException {
        if (rawHeader == null) {
            throw new IllegalArgumentException(
                    "Required argument 'rawHeader' cannot be NULL.");
        }
        if (rawHeader.length != HEADER_LENGTH) {
            throw new IllegalArgumentException(
                    "Required argument 'rawHeader' is not the correct length. Expected = "
                            + HEADER_LENGTH + "; received = "
                            + rawHeader.length + ".");
        }
        this.rawHeader = rawHeader;

        verifyProductCode();
        calculateProductDateTime();
        calculateProductLatLon();
        loadRemainingFields();
    }

    private void verifyProductCode() throws InvalidDHRException {
        productCode = rawHeader[DHRHeaderIndices.PRO_ID_INDEX];
        if (productCode == EXPECTED_PRODUCT_CODE) {
            return;
        }
        throw new InvalidDHRException(
                "Header contained an unexpected product code: " + productCode
                        + ". Was expecting a product code of: "
                        + EXPECTED_PRODUCT_CODE + ". Invalid DHR product?");
    }

    /**
     * Determines the date/time associated with the product. Within the product,
     * the alternate Julian date/time format used throughout most of MPE is
     * used. With this format, the Julian date is recorded as the number of days
     * that have passed since the Epoch and the Julian time is recorded as the
     * number of minutes that have passed since midnight.
     * 
     * @throws InvalidDHRException
     */
    private void calculateProductDateTime() {
        final int days = rawHeader[DHRHeaderIndices.END_DATE_INDEX];
        final int minutes = rawHeader[DHRHeaderIndices.END_TIME_INDEX];

        /*
         * Start with a Calendar set to the Epoch.
         */
        final Calendar epochCalendar = TimeUtil.newEpochCalendar();
        /*
         * Add the number of days that have passed since the epoch.
         */
        epochCalendar.add(Calendar.DATE, days);
        /*
         * Add the number of minutes that have passed since midnight.
         */
        epochCalendar.add(Calendar.MINUTE, minutes);
        productDateTime = epochCalendar;
    }

    /**
     * Determines the hrap-compatible latitude/longitude associated with the
     * product. TODO: apply additional clarification when writing the actual
     * decoder.
     * 
     * @throws InvalidDHRException
     */
    private void calculateProductLatLon() throws InvalidDHRException {
        latitude = getCoordinateValue(DHRHeaderIndices.LAT_INDEX1,
                DHRHeaderIndices.LAT_INDEX2);
        longitude = getCoordinateValue(DHRHeaderIndices.LON_INDEX1,
                DHRHeaderIndices.LON_INDEX2);
    }

    /**
     * Loads the remaining header fields that can either be directly assigned or
     * just involve simple casting and/or mathematical division.
     */
    private void loadRemainingFields() {
        radialNumber = rawHeader[DHRHeaderIndices.RADIAL_NUM_INDEX];
        volcovpat = rawHeader[DHRHeaderIndices.VOLCOVPAT_INDEX];
        opermode = rawHeader[DHRHeaderIndices.OPERMODE_INDEX];
        dbzmin = (float) rawHeader[DHRHeaderIndices.DBZMIN_INDEX]
                / DBZ_DIVIDEND;
        dbzinc = (float) rawHeader[DHRHeaderIndices.DBZINC_INDEX]
                / DBZ_DIVIDEND;
        dbzcnt = (float) rawHeader[DHRHeaderIndices.DBZCNT_INDEX];
        bias = (float) rawHeader[DHRHeaderIndices.BIAS_INDEX] / BIAS_DIVIDEND;
        sampleSize = rawHeader[DHRHeaderIndices.SAMPLE_SIZE_INDEX];
    }

    /**
     * Combines the two values retrievable at the specified indices to form a
     * hrap-compatible coordinate value. Ensuring that the values that are used
     * are unsigned is essential.
     * 
     * @param index1
     *            the specified first index
     * @param index2
     *            the specified second index
     * @return the generated combination
     */
    private int getCoordinateValue(final int index1, final int index2) {
        /*
         * TODO: ShortUtils will not be required in Java 8. This capability will
         * be available on Short, itself.
         */
        long tmpLong = (long) ShortUtils.toUnsigned(rawHeader[index1]) << 16;
        return (int) (tmpLong + (long) ShortUtils.toUnsigned(rawHeader[index2]));
    }

    public int getProductCode() {
        return productCode;
    }

    public Calendar getProductDateTime() {
        return productDateTime;
    }

    public int getRadialNumber() {
        return radialNumber;
    }

    public int getLatitude() {
        return latitude;
    }

    public int getLongitude() {
        return longitude;
    }

    public short getVolcovpat() {
        return volcovpat;
    }

    public short getOpermode() {
        return opermode;
    }

    public float getDbzmin() {
        return dbzmin;
    }

    public float getDbzinc() {
        return dbzinc;
    }

    public float getDbzcnt() {
        return dbzcnt;
    }

    public float getBias() {
        return bias;
    }

    public short getSampleSize() {
        return sampleSize;
    }
}