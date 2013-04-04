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
package com.raytheon.uf.common.stats.util;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.raytheon.uf.common.stats.ProcessEvent;
import com.raytheon.uf.common.stats.util.UnitUtils.TimeConversion;
import com.raytheon.uf.common.stats.util.UnitUtils.UnitTypes;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.units.DataSizeUnit;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2013   1357      mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class UnitUtilsTest {

    private static final String EVENT_TYPE = ProcessEvent.class.getName();

    private static final String DATA_TYPE = "processingTime";

    @Test
    public void testConvertBytesToBytes() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(DataSizeUnit.BYTE.getUnit());
        uu.setDisplayUnit(DataSizeUnit.BYTE.getUnit());

        double value = uu.convertDataSizeValue(DataSizeUnit.BYTE, 100);

        assertEquals(100, value, 0);
    }

    @Test
    public void testConvertBytesToKb() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(DataSizeUnit.BYTE.getUnit());
        uu.setDisplayUnit(DataSizeUnit.KB.getUnit());

        double value = uu.convertDataSizeValue(DataSizeUnit.BYTE, 1024);

        assertEquals(1, value, 0);
    }

    @Test
    public void testConvertBytesToMb() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(DataSizeUnit.BYTE.getUnit());
        uu.setDisplayUnit(DataSizeUnit.MB.getUnit());

        double value = uu.convertDataSizeValue(DataSizeUnit.BYTE, 1048576);

        assertEquals(1, value, 0);
    }

    @Test
    public void testConvertValueFromMsToSecond() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(TimeConversion.MS.getDataUnit());
        uu.setDisplayUnit(TimeConversion.Second.getDataUnit());

        double value = uu.convertTimeValue(TimeConversion.MS,
                TimeUtil.MILLIS_PER_SECOND * 3);

        assertEquals(3, value, 0);
    }

    @Test
    public void testConvertValueFromMsToMinute() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(TimeConversion.MS.getDataUnit());
        uu.setDisplayUnit(TimeConversion.Minute.getDataUnit());

        double value = uu.convertTimeValue(TimeConversion.MS,
                TimeUtil.MILLIS_PER_MINUTE * 2);

        assertEquals(2, value, 0);
    }

    @Test
    public void testConvertValueFromMsToHours() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(TimeConversion.MS.getDataUnit());
        uu.setDisplayUnit(TimeConversion.Hour.getDataUnit());

        double value = uu.convertTimeValue(TimeConversion.MS,
                TimeUtil.MILLIS_PER_HOUR);

        assertEquals(1, value, 0);
    }

    @Test
    public void testConvertValueFromHoursToMs() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(TimeConversion.Hour.getDataUnit());
        uu.setDisplayUnit(TimeConversion.MS.getDataUnit());

        double value = uu.convertTimeValue(TimeConversion.Hour, 1);

        assertEquals(TimeUtil.MILLIS_PER_HOUR, value, 0);
    }

    @Test
    public void testVerifyDisplayUnitIsTime() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(TimeConversion.Hour.getDataUnit());
        uu.setDisplayUnit(TimeConversion.MS.getDataUnit());

        UnitTypes type = uu.getUnitType();

        assertEquals(UnitTypes.TIME, type);
    }

    @Test
    public void testVerifyDisplayUnitIsSize() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(DataSizeUnit.KB.getUnit());
        uu.setDisplayUnit(DataSizeUnit.MB.getUnit());

        UnitTypes type = uu.getUnitType();

        assertEquals(UnitTypes.DATA_SIZE, type);
    }

    @Test
    public void testConvertValueToSec() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(TimeConversion.MS.getDataUnit());
        uu.setDisplayUnit(TimeConversion.Second.getDataUnit());

        double value = uu.convertValue(TimeUtil.MILLIS_PER_SECOND * 3);

        assertEquals(3, value, 0);
    }

    @Test
    public void testConvertValueToMinute() {
        UnitUtils uu = new UnitUtils(EVENT_TYPE, DATA_TYPE);
        uu.setUnitType(TimeConversion.MS.getDataUnit());
        uu.setDisplayUnit(TimeConversion.Minute.getDataUnit());

        double value = uu.convertValue(TimeUtil.MILLIS_PER_MINUTE * 3);

        assertEquals(3, value, 0);
    }

}
