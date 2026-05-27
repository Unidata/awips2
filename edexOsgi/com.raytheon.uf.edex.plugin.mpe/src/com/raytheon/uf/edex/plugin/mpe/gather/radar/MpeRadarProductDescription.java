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
package com.raytheon.uf.edex.plugin.mpe.gather.radar;

import java.nio.ByteBuffer;
import java.util.Calendar;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Product description block for a radar product.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */

public class MpeRadarProductDescription {
    public static final int SHORT_SIZE = 51;

    public static final int BYTE_SIZE = SHORT_SIZE * 2;

    private static final class DescriptionIndices {
        public static final int LAT_INDEX1 = 1;

        public static final int LAT_INDEX2 = 2;

        public static final int LON_INDEX1 = 3;

        public static final int LON_INDEX2 = 4;

        public static final int PRO_ID_INDEX = 6;

        public static final int OPERMODE_INDEX = 7;

        public static final int VOLCOVPAT_INDEX = 8;

        public static final int BIAS_INDEX = 20;

        public static final int DATA_LVL_CNT_INDEX = 23;

        public static final int END_DATE_INDEX = 38;

        public static final int END_TIME_INDEX = 39;

        public static final int SAMPLE_SIZE_INDEX = 40;
    }

    short[] data;

    private int latitude;

    private int longitude;

    private Calendar productDateTime;

    private short operationalMode;

    private short volumeCoveragePattern;

    private short meanFieldBias;

    private short sampleSize;

    private short productCode;

    private short dataLevelCount;

    private short jDate;

    private short jTime;

    public MpeRadarProductDescription(ByteBuffer buf)
            throws InvalidMpeRadarException {
        super();
        MpeRadarDecodeUtils.checkFileRemaining(buf, "Product Description", BYTE_SIZE);

        // read file as shorts
        data = new short[SHORT_SIZE];
        buf.asShortBuffer().get(data);
        parseData(data);

        // position byte buffer after this product description
        buf.position(buf.position() + BYTE_SIZE);
    }

    /**
     * @param data
     */
    protected void parseData(short[] data) {
        if (data[0] != -1) {
            throw new IllegalArgumentException("Data is incorrectly formed.");
        }
        latitude = getCoordinateValue(data[DescriptionIndices.LAT_INDEX1],
                data[DescriptionIndices.LAT_INDEX2]);
        longitude = getCoordinateValue(data[DescriptionIndices.LON_INDEX1],
                data[DescriptionIndices.LON_INDEX2]);
        productCode = data[DescriptionIndices.PRO_ID_INDEX];
        operationalMode = data[DescriptionIndices.OPERMODE_INDEX];
        volumeCoveragePattern = data[DescriptionIndices.VOLCOVPAT_INDEX];
        meanFieldBias = data[DescriptionIndices.BIAS_INDEX];
        dataLevelCount = data[DescriptionIndices.DATA_LVL_CNT_INDEX];
        jDate = data[DescriptionIndices.END_DATE_INDEX];
        jTime = data[DescriptionIndices.END_TIME_INDEX];
        productDateTime = calculateDateTime(jDate, jTime);
        sampleSize = data[DescriptionIndices.SAMPLE_SIZE_INDEX];
    }

    /**
     * Calculate the data time.
     *
     * @param days
     * @param minutes
     * @return
     */
    protected Calendar calculateDateTime(short days, short minutes) {
        // Start with a GMT Calendar set to the Epoch.
        final Calendar epochCalendar = TimeUtil.newEpochCalendar();
        epochCalendar.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        // Add the number of days that have passed since the epoch, minus 1.
        epochCalendar.add(Calendar.DATE, days - 1);
        // Add the number of minutes that have passed since midnight.
        epochCalendar.add(Calendar.MINUTE, minutes);
        return epochCalendar;
    }

    /**
     * Combines the two values to form a hrap-compatible coordinate value.
     * Ensuring that the values that are used are unsigned is essential.
     *
     * @param mshw
     *            the most significant half-word value
     * @param lshw
     *            the least significant half-word value
     * @return the generated combination
     */
    protected int getCoordinateValue(final short mshw, final short lshw) {
        long tmpLong = Short.toUnsignedLong(mshw) << 16;
        return (int) (tmpLong + Short.toUnsignedLong(lshw));
    }

    /**
     * @return the latitude
     */
    public int getLatitude() {
        return latitude;
    }

    /**
     * @return the longitude
     */
    public int getLongitude() {
        return longitude;
    }

    /**
     * @return the productDateTime
     */
    public Calendar getProductDateTime() {
        return productDateTime;
    }

    /**
     * @return the operationalMode
     */
    public short getOperationalMode() {
        return operationalMode;
    }

    /**
     * @return the volumeCoveragePattern
     */
    public short getVolumeCoveragePattern() {
        return volumeCoveragePattern;
    }

    /**
     * @return the mean field bias
     */
    public short getMeanFieldBias() {
        return meanFieldBias;
    }

    /**
     * @return the sample size
     */
    public short getSampleSize() {
        return sampleSize;
    }

    /**
     * @return the productCode
     */
    public short getProductCode() {
        return productCode;
    }

    /**
     * @return the dataLevelCount
     */
    public short getDataLevelCount() {
        return dataLevelCount;
    }

    /**
     * @return the jDate
     */
    public short getjDate() {
        return jDate;
    }

    /**
     * @return the jTime
     */
    public short getjTime() {
        return jTime;
    }
}
