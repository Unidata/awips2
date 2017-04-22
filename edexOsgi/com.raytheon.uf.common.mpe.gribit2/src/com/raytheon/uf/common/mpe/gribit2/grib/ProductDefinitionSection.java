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
package com.raytheon.uf.common.mpe.gribit2.grib;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;

import com.raytheon.uf.common.mpe.gribit2.XmrgToGribConstants;

/**
 * POJO representation of a Grib Product Definition Section (PDS). Based on:
 * /rary.ohd.pproc.gribit/TEXT/engrib.f and
 * /rary.ohd.pproc.gribit/TEXT/w3fi68.f.
 * 
 * TODO: in the future, it is possible that this POJO may not be specific to
 * xmrg -> grib and it may be applicable to all grib decoding in general.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2016 4619       bkowal      Initial creation
 * Aug 18, 2016 4619       bkowal      Implemented section write.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class ProductDefinitionSection {

    private static final int INCLUDE_GDS_VALUE = 128;

    private static final int INCLUDE_BMS_VALUE = 64;

    private static final int INCLUDE_GDS_AND_BMS_VALUE = INCLUDE_GDS_VALUE
            + INCLUDE_BMS_VALUE;

    private static final int TIME_RANGE_IND_10 = 10;

    /**
     * number of bytes in PDS
     */
    private int numberBytes = XmrgToGribConstants.PDS_BYTE_COUNT;

    /**
     * parameter table version number (NWS field - Oct 98)
     */
    private int paramTableVersionNum;

    /**
     * identification of originating center (Table 0)
     */
    private int originatingCenter;

    /**
     * model identification (Table A)
     */
    private int modelId;

    /**
     * grid identification (Table B)
     */
    private int grid;

    /**
     * include GDS section (0-no, 1-yes) (Table 1)
     */
    private Boolean includeGDS = Boolean.TRUE;

    /**
     * include BMS section (0-no, 1-yes) (also Table 1)
     */
    private Boolean includeBMS = Boolean.TRUE;

    /**
     * indicator of parameters and units (Table 2)
     */
    private int paramUnitIndicator;

    /**
     * indicator of type of level (Table 3) 1 - surface
     */
    private int typeLevelIndicator = 1;

    /**
     * value 1 of level (0 for level 1)
     */
    private int levelValue1 = 0;

    /**
     * value 2 of level (0 for level 1)
     */
    private int levelValue2 = 0;

    /**
     * The representation of year is specific to GRIB. Ex: 1998 => grib year 98;
     * 1999 => grib year 99; 2000 => grib year 100; 2001 => grib year 1; 2002 =>
     * grib year 2.
     */
    private int year;

    private int month;

    private int day;

    private int hour;

    private int minute;

    /**
     * fcst time unit (Table 4)
     */
    private int forecastTimeUnit;

    /**
     * p1 period of time (number of time units)
     */
    private int periodOfTimeP1;

    /**
     * p2 period of time (number of time units)
     */
    private int periodOfTimeP2;

    /**
     * time range indicator (Table 5)
     */
    private int timeRangerIndicator;

    /**
     * number included in average when average or accumulated value indicated in
     * Table 5
     */
    private int includedNumber = 0;

    /**
     * number missing from averages
     */
    private int avgMissingNumber;

    /**
     * century of initial reference (20, change to 21 on Jan 1, 2001, etc.)
     */
    private int century;

    /**
     * identification of sub-center Table C for originating center = 9 RFCs 150
     * to 162
     */
    private int subCenter;

    /**
     * check value of grib_set_subcenter_0 token if set to ON, then set id(24)
     * to 0
     */
    private int gribSetSubCenter;

    /**
     * decimal scale factor (power of 10)--number of decimal positions
     */
    private int decimalScaleFactor;

    /**
     * Note: id(25), id(26), id(27) have all been set to 0. So, 0-padding may be
     * necessary when this POJO is written out.
     */

    /**
     * Reads the contents of the product definition section from the specified
     * {@link ByteBuffer} and populates fields in this instance of the
     * {@link ProductDefinitionSection}.
     * 
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     */
    public void readSection(final ByteBuffer byteBuffer) {
        final byte[] lengthBytes = new byte[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        byteBuffer.get(lengthBytes);
        final short[] unsignedLengthBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(lengthBytes);
        numberBytes = XmrgGribPropertyUtils.convertToSize(unsignedLengthBytes);

        // TODO: make sure remaining length is positive.
        final byte[] sectionBytes = new byte[numberBytes
                - XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        byteBuffer.get(sectionBytes);
        final short[] unsignedSectionBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(sectionBytes);

        /*
         * fourth byte is: the parameter table version
         */
        paramTableVersionNum = (int) unsignedSectionBytes[0];

        /*
         * fifth byte is: identification of the originating center.
         */
        originatingCenter = (int) unsignedSectionBytes[1];

        /*
         * sixth byte is: model identification
         */
        modelId = (int) unsignedSectionBytes[2];

        /*
         * seventh byte is: grid identification
         */
        grid = (int) unsignedSectionBytes[3];
        if (grid == XmrgToGribConstants.PDS_NGRID) {
            grid = XmrgToGribConstants.DEFAULT_NGRID;
        }

        /*
         * eighth byte is: a combination of the include GDS and include BMS
         * flags
         */
        final int includeBMSOrGDSValue = (int) unsignedSectionBytes[4];
        includeGDS = (includeBMSOrGDSValue == INCLUDE_GDS_VALUE)
                || (includeBMSOrGDSValue == INCLUDE_GDS_AND_BMS_VALUE);
        includeBMS = (includeBMSOrGDSValue == INCLUDE_BMS_VALUE)
                || (includeBMSOrGDSValue == INCLUDE_GDS_AND_BMS_VALUE);

        /*
         * ninth byte is: indicator of parameters and units
         */
        paramUnitIndicator = (int) unsignedSectionBytes[5];

        /*
         * tenth byte is: indicator of type of level
         */
        typeLevelIndicator = (int) unsignedSectionBytes[6];

        /*
         * eleventh and twelfth bytes are: the level 1 and level 2 values. Note:
         * the xmrg to grib conversion, as implemented, hard codes them to 0
         * during generation.
         */
        levelValue1 = (int) unsignedSectionBytes[7];
        levelValue2 = (int) unsignedSectionBytes[8];

        /*
         * thirteenth byte is: the "grib year". This is not the same as a
         * Calendar year.
         */
        year = (int) unsignedSectionBytes[9];

        /*
         * fourteenth byte is: the month.
         */
        month = (int) unsignedSectionBytes[10];

        /*
         * fifteenth byte is: the day
         */
        day = (int) unsignedSectionBytes[11];

        /*
         * sixteenth byte is: the hour
         */
        hour = (int) unsignedSectionBytes[12];

        /*
         * seventeenth byte is: the minute
         */
        minute = (int) unsignedSectionBytes[13];

        /*
         * eighteenth byte is: the forecast time unit
         */
        forecastTimeUnit = (int) unsignedSectionBytes[14];

        /*
         * twenty-first byte (needed before 19 and 20 can be determined) is:
         * time range indicator
         */
        timeRangerIndicator = (int) unsignedSectionBytes[17];
        if (timeRangerIndicator == TIME_RANGE_IND_10) {
            periodOfTimeP1 = (unsignedSectionBytes[15] * XmrgGribPropertyUtils.DIVISOR_6)
                    + unsignedSectionBytes[16];
        } else {
            periodOfTimeP1 = (int) unsignedSectionBytes[15];
            periodOfTimeP2 = (int) unsignedSectionBytes[16];
        }

        /*
         * twenty-second and twenty-third bytes are: number included in average
         */
        includedNumber = (unsignedSectionBytes[18] * XmrgGribPropertyUtils.DIVISOR_6)
                + unsignedSectionBytes[19];

        /*
         * twenty-fourth byte is: number missing from averages
         */
        avgMissingNumber = (int) unsignedSectionBytes[20];

        /*
         * twenty-fifth byte is: the century
         */
        century = (int) unsignedSectionBytes[21];

        /*
         * twenty-sixth byte is: identification of the sub-center
         */
        subCenter = (int) unsignedSectionBytes[22];

        /*
         * twenty-seventh and twenty-eight bytes are: decimal scale factor.
         */
        final int[] unpackedValue = new int[1];
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(26), 16, 0, 1, true);
        decimalScaleFactor = unpackedValue[0];

        /*
         * Any bytes over 28 are not applicable to grib files produced by xmrg
         * because the standard PDS size for xmrg -> grib is 28 bytes.
         */
    }

    /**
     * Writes the contents of the Product Definition Section to the specified
     * {@link ByteBuffer}.
     * 
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     */
    public void writeSection(final OutputStream os) throws IOException {
        short[] packedValues = new short[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        final int[] toPack = new int[1];

        try {
            /*
             * the length.
             */
            toPack[0] = numberBytes;
            XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            /*
             * fourth byte is: the parameter table version
             */
            os.write(paramTableVersionNum);

            /*
             * fifth byte is: identification of the originating center.
             */
            os.write(originatingCenter);

            /*
             * sixth byte is: model identification
             */
            os.write(modelId);

            /*
             * seventh byte is: grid identification
             */
            if (grid == XmrgToGribConstants.DEFAULT_NGRID) {
                os.write(XmrgToGribConstants.PDS_NGRID);
            } else {
                os.write(grid);
            }

            /*
             * eighth byte is: a combination of the include GDS and include BMS
             * flags
             */
            if (Boolean.TRUE.equals(includeBMS)
                    && Boolean.TRUE.equals(includeGDS)) {
                os.write(INCLUDE_GDS_AND_BMS_VALUE);
            } else if (Boolean.TRUE.equals(includeBMS)
                    || Boolean.TRUE.equals(includeGDS)) {
                if (Boolean.TRUE.equals(includeBMS)) {
                    os.write(INCLUDE_BMS_VALUE);
                } else {
                    os.write(INCLUDE_GDS_VALUE);
                }
            } else {
                /*
                 * Neither the GDS or BMS has been included.
                 */
                os.write(0);
            }

            /*
             * ninth byte is: indicator of parameters and units
             */
            os.write(paramUnitIndicator);

            /*
             * tenth byte is: indicator of type of level
             */
            os.write(typeLevelIndicator);

            /*
             * eleventh and twelfth bytes are: the level 1 and level 2 values.
             * Note: the xmrg to grib conversion, as implemented, hard codes
             * them to 0 during generation.
             */
            os.write(levelValue1);
            os.write(levelValue2);

            /*
             * thirteenth byte is: the "grib year". This is not the same as a
             * Calendar year.
             */
            os.write(year);

            /*
             * fourteenth byte is: the month.
             */
            os.write(month);

            /*
             * fifteenth byte is: the day
             */
            os.write(day);

            /*
             * sixteenth byte is: the hour
             */
            os.write(hour);

            /*
             * seventeenth byte is: the minute
             */
            os.write(minute);

            /*
             * eighteenth byte is: the forecast time unit
             */
            os.write(forecastTimeUnit);

            /*
             * twenty-first byte (needed before 19 and 20 can be determined) is:
             * time range indicator
             */
            if (timeRangerIndicator == TIME_RANGE_IND_10) {
                toPack[0] = periodOfTimeP1;
                packedValues = new short[2];
                XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 16);
                XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);
            } else {
                os.write(periodOfTimeP1);
                os.write(periodOfTimeP2);
            }
            os.write(timeRangerIndicator);

            /*
             * twenty-second and twenty-third bytes are: number included in
             * average
             */
            packedValues = new short[2];
            toPack[0] = includedNumber;
            XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 16);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            /*
             * twenty-fourth byte is: number missing from averages
             */
            os.write(avgMissingNumber);

            /*
             * twenty-fifth byte is: the century
             */
            os.write(century);

            /*
             * twenty-sixth byte is: identification of the sub-center
             */
            os.write(subCenter);

            /*
             * twenty-seventh and twenty-eight bytes are: decimal scale factor.
             */
            toPack[0] = decimalScaleFactor;
            XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 16);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);
        } catch (IOException e) {
            throw new IOException(
                    "Failed to write the product definition section.", e);
        }
    }

    public int getNumberBytes() {
        return numberBytes;
    }

    public void setNumberBytes(int numberBytes) {
        this.numberBytes = numberBytes;
    }

    public int getParamTableVersionNum() {
        return paramTableVersionNum;
    }

    public void setParamTableVersionNum(int paramTableVersionNum) {
        this.paramTableVersionNum = paramTableVersionNum;
    }

    public int getOriginatingCenter() {
        return originatingCenter;
    }

    public void setOriginatingCenter(int originatingCenter) {
        this.originatingCenter = originatingCenter;
    }

    public int getModelId() {
        return modelId;
    }

    public void setModelId(int modelId) {
        this.modelId = modelId;
    }

    public int getGrid() {
        return grid;
    }

    public void setGrid(int grid) {
        this.grid = grid;
    }

    public Boolean getIncludeGDS() {
        return includeGDS;
    }

    public void setIncludeGDS(Boolean includeGDS) {
        this.includeGDS = includeGDS;
    }

    public Boolean getIncludeBMS() {
        return includeBMS;
    }

    public void setIncludeBMS(Boolean includeBMS) {
        this.includeBMS = includeBMS;
    }

    public int getParamUnitIndicator() {
        return paramUnitIndicator;
    }

    public void setParamUnitIndicator(int paramUnitIndicator) {
        this.paramUnitIndicator = paramUnitIndicator;
    }

    public int getTypeLevelIndicator() {
        return typeLevelIndicator;
    }

    public void setTypeLevelIndicator(int typeLevelIndicator) {
        this.typeLevelIndicator = typeLevelIndicator;
    }

    public int getLevelValue1() {
        return levelValue1;
    }

    public void setLevelValue1(int levelValue1) {
        this.levelValue1 = levelValue1;
    }

    public int getLevelValue2() {
        return levelValue2;
    }

    public void setLevelValue2(int levelValue2) {
        this.levelValue2 = levelValue2;
    }

    public int getYear() {
        return year;
    }

    public void setYear(int year) {
        this.year = year;
    }

    public int getMonth() {
        return month;
    }

    public void setMonth(int month) {
        this.month = month;
    }

    public int getDay() {
        return day;
    }

    public void setDay(int day) {
        this.day = day;
    }

    public int getHour() {
        return hour;
    }

    public void setHour(int hour) {
        this.hour = hour;
    }

    public int getMinute() {
        return minute;
    }

    public void setMinute(int minute) {
        this.minute = minute;
    }

    public int getForecastTimeUnit() {
        return forecastTimeUnit;
    }

    public void setForecastTimeUnit(int forecastTimeUnit) {
        this.forecastTimeUnit = forecastTimeUnit;
    }

    public int getPeriodOfTimeP1() {
        return periodOfTimeP1;
    }

    public void setPeriodOfTimeP1(int periodOfTimeP1) {
        this.periodOfTimeP1 = periodOfTimeP1;
    }

    public int getPeriodOfTimeP2() {
        return periodOfTimeP2;
    }

    public void setPeriodOfTimeP2(int periodOfTimeP2) {
        this.periodOfTimeP2 = periodOfTimeP2;
    }

    public int getTimeRangerIndicator() {
        return timeRangerIndicator;
    }

    public void setTimeRangerIndicator(int timeRangerIndicator) {
        this.timeRangerIndicator = timeRangerIndicator;
    }

    public int getIncludedNumber() {
        return includedNumber;
    }

    public void setIncludedNumber(int includedNumber) {
        this.includedNumber = includedNumber;
    }

    public int getAvgMissingNumber() {
        return avgMissingNumber;
    }

    public void setAvgMissingNumber(int avgMissingNumber) {
        this.avgMissingNumber = avgMissingNumber;
    }

    public int getCentury() {
        return century;
    }

    public void setCentury(int century) {
        this.century = century;
    }

    public int getSubCenter() {
        return subCenter;
    }

    public void setSubCenter(int subCenter) {
        this.subCenter = subCenter;
    }

    public int getGribSetSubCenter() {
        return gribSetSubCenter;
    }

    public void setGribSetSubCenter(int gribSetSubCenter) {
        this.gribSetSubCenter = gribSetSubCenter;
    }

    public int getDecimalScaleFactor() {
        return decimalScaleFactor;
    }

    public void setDecimalScaleFactor(int decimalScaleFactor) {
        this.decimalScaleFactor = decimalScaleFactor;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("ProductDefinitionSection [");
        sb.append("numberBytes=").append(numberBytes);
        sb.append(", paramTableVersionNum=").append(paramTableVersionNum);
        sb.append(", originatingCenter=").append(originatingCenter);
        sb.append(", modelId=").append(modelId);
        sb.append(", grid=").append(grid);
        sb.append(", includeGDS=").append(includeGDS);
        sb.append(", includeBMS=").append(includeBMS);
        sb.append(", paramUnitIndicator=").append(paramUnitIndicator);
        sb.append(", typeLevelIndicator=").append(typeLevelIndicator);
        sb.append(", levelValue1=").append(levelValue1);
        sb.append(", levelValue2=").append(levelValue2);
        sb.append(", year=").append(year);
        sb.append(", month=").append(month);
        sb.append(", day=").append(day);
        sb.append(", hour=").append(hour);
        sb.append(", minute=").append(minute);
        sb.append(", forecastTimeUnit=").append(forecastTimeUnit);
        sb.append(", periodOfTimeP1=").append(periodOfTimeP1);
        sb.append(", periodOfTimeP2=").append(periodOfTimeP2);
        sb.append(", includedNumber=").append(includedNumber);
        sb.append(", avgMissingNumber=").append(avgMissingNumber);
        sb.append(", century=").append(century);
        sb.append(", subCenter=").append(subCenter);
        sb.append(", gribSetSubCenter=").append(gribSetSubCenter);
        sb.append(", decimalScaleFactor=").append(decimalScaleFactor);
        sb.append("]");
        return sb.toString();
    }
}