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
package com.raytheon.uf.edex.plugin.mpe.gather.dsp;

import java.nio.ByteBuffer;
import java.util.Calendar;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.InvalidMpeRadarException;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarProductDescription;

/**
 * Reads and represents the Product Description block for a DSP Radar File.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588       nabowle     Initial creation
 * Jul 19, 2018 5588       mapeters    Fix parsing of maxPrecipValue
 *
 * </pre>
 *
 * @author nabowle
 */

public class DSPProductDescription extends MpeRadarProductDescription {

    private static final class DSPDescriptionIndices {

        public static final int START_DATE_INDEX = 17;

        public static final int START_TIME_INDEX = 18;

        public static final int MIN_DATA_LVL_INDEX = 21;

        public static final int DATA_LVL_SCALE_INDEX = 22;

        public static final int MAX_PRECIP_INDEX = 37;
    }

    private Calendar startDateTime;

    private short dataLevelScaleFactor;

    private short minDataLevel;

    private float maxPrecipValue;

    private short julianBeginDate;

    private short julianBeginTime;

    public DSPProductDescription(ByteBuffer buf)
            throws InvalidMpeRadarException {
        super(buf);
    }

    @Override
    protected void parseData(short[] data) {
        super.parseData(data);
        julianBeginDate = data[DSPDescriptionIndices.START_DATE_INDEX];
        julianBeginTime = data[DSPDescriptionIndices.START_TIME_INDEX];
        startDateTime = calculateDateTime(julianBeginDate,
                (short) (julianBeginTime / TimeUtil.SECONDS_PER_MINUTE));
        minDataLevel = data[DSPDescriptionIndices.MIN_DATA_LVL_INDEX];
        dataLevelScaleFactor = data[DSPDescriptionIndices.DATA_LVL_SCALE_INDEX];
        maxPrecipValue = data[DSPDescriptionIndices.MAX_PRECIP_INDEX];
    }

    /**
     * @return the startDateTime
     */
    public Calendar getStartDateTime() {
        return startDateTime;
    }

    /**
     * @return the dataLevelScaleFactor
     */
    public short getDataLevelScaleFactor() {
        return dataLevelScaleFactor;
    }

    /**
     * @return the minDataLevel
     */
    public short getMinDataLevel() {
        return minDataLevel;
    }

    /**
     * @return the maxPrecipValue
     */
    public float getMaxPrecipValue() {
        return maxPrecipValue;
    }

    /**
     * @return the j_beg_date
     */
    public short getJulianBeginDate() {
        return julianBeginDate;
    }

    /**
     * @return the j_beg_time
     */
    public short getJulianBeginTime() {
        return julianBeginTime;
    }
}
