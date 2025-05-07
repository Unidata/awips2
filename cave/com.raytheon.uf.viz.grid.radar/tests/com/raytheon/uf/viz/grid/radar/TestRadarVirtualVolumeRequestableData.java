/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.grid.radar;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Unit tests for {@link RadarVirtualVolumeRequestableData}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 05, 2024 2037092    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
class TestRadarVirtualVolumeRequestableData {

    private static final String RR = "RR";

    private static final String RR_VIRT = "RRvirt";

    @Mock
    private GridCoverage space;

    private final DataTime baseTime = new DataTime("2024-01-01_12:00:00.0");

    private final DataTime virtualTime = new DataTime("2024-01-01_12:06:00.0");

    private RadarRequestableData baseData;

    private RadarVirtualVolumeRequestableData virtualData;

    private RadarVirtualVolumeRequestableData aliasData;

    private MockedStatic<RadarRequestableData> dataMockedStatic;

    @BeforeEach
    public void setupBeforeEach() throws Exception {
        dataMockedStatic = TestRadarRequestableData
                .getMockedStatic("radar-koax", "dBZ", 2.4, space);

        RadarRecord radarSource = new RadarRecord();
        radarSource.setDataTime(baseTime);

        baseData = new RadarRequestableData(radarSource, RR);
        virtualData = new RadarVirtualVolumeRequestableData(virtualTime,
                baseData);
        aliasData = new RadarVirtualVolumeRequestableData(null, baseData);
    }

    @AfterEach
    public void tearDownAfterEach() {
        dataMockedStatic.close();
    }

    @Test
    void testBuildGridRecord1() throws VizException {
        /*
         * Virtual time passed in -> virtual time used and virtual param abbrev
         * used
         */
        GridRecord record = RadarVirtualVolumeRequestableData
                .buildGridRecord(virtualTime, baseData);

        assertEquals(virtualTime, record.getDataTime());
        assertEquals(RR_VIRT, record.getParameter().getAbbreviation());
    }

    @Test
    void testBuildGridRecord2() throws VizException {
        /*
         * Null time passed in -> base time used, but virtual param abbrev still
         * used
         */
        GridRecord record = RadarVirtualVolumeRequestableData
                .buildGridRecord(null, baseData);

        assertEquals(baseTime, record.getDataTime());
        assertEquals(RR_VIRT, record.getParameter().getAbbreviation());
    }

    @Test
    void testGetDataTime1() {
        DataTime virtualTimeOutput = virtualData.getDataTime();

        assertEquals(virtualTime, virtualTimeOutput);
    }

    @Test
    void testGetDataTime2() {
        DataTime aliasTimeOutput = aliasData.getDataTime();

        assertEquals(baseTime, aliasTimeOutput);
    }

    @Test
    void testGetTimeAndSpace1() {
        TimeAndSpace expected = new RadarVirtualTimeAndSpace(virtualTime, space,
                baseTime);

        TimeAndSpace actual = virtualData.getTimeAndSpace();

        assertEquals(expected, actual);
    }

    @Test
    void testGetTimeAndSpace2() {
        TimeAndSpace expected = new TimeAndSpace(baseTime, space);

        TimeAndSpace actual = aliasData.getTimeAndSpace();

        assertEquals(expected, actual);
    }

    @Test
    void testGetParameter() {
        String actual = aliasData.getParameter();

        assertEquals(RR_VIRT, actual);
    }

    @Test
    void testGetGridRecordParameter() {
        String actual = aliasData.getGridSource().getParameter()
                .getAbbreviation();

        assertEquals(RR_VIRT, actual);
    }
}