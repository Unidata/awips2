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
import static org.mockito.Mockito.mock;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.time.DataTime;

/**
 * Unit tests for {@link RadarCubeRequestableData}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2024 2037624    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarCubeRequestableData {

    private static final DataTime dt1200 = new DataTime(
            "2024-01-01_12:06:00.0");

    private static final DataTime dt1206 = new DataTime(
            "2024-01-01_12:06:00.0");

    private static final IGridGeometryProvider space = mock(
            IGridGeometryProvider.class);

    static Stream<Arguments> provideParamsForGetTimeAndSpace() {
        TestRequestableData d0_5 = new TestRequestableData(
                new TimeAndSpace(dt1206, space), 0.5);
        TestRequestableData d1_5 = new TestRequestableData(
                new TimeAndSpace(dt1206, space), 1.5);
        TestRequestableData d1_5virt = new TestRequestableData(
                new RadarVirtualTimeAndSpace(dt1206, space, dt1200), 1.5);
        TestRequestableData d2_5virt = new TestRequestableData(
                new RadarVirtualTimeAndSpace(dt1206, space, dt1200), 2.5);

        return Stream.of(
                // No virtual data -> normal time/space
                Arguments.of(dt1206, List.of(d0_5, d1_5),
                        new TimeAndSpace(dt1206, space)),
                /*
                 * Virtual data above 1.5 tilt -> virtual time/space with
                 * level=1.5
                 */
                Arguments.of(dt1206, List.of(d0_5, d1_5, d2_5virt),
                        new RadarVirtualDerivedTimeAndSpace(dt1206, space,
                                dt1200, 1.5)),
                /*
                 * Virtual data above 0.5 tilt -> virtual time/space with
                 * level=0.5
                 */
                Arguments.of(dt1206, List.of(d0_5, d1_5virt, d2_5virt),
                        new RadarVirtualDerivedTimeAndSpace(dt1206, space,
                                dt1200, 0.5)));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetTimeAndSpace")
    void testGetTimeAndSpace(DataTime dt,
            List<AbstractRequestableData> paramList, TimeAndSpace expectedTas) {
        AbstractRequestableData dataToCopy = new TestRequestableData(
                new TimeAndSpace(dt, space), 0.5);
        RadarCubeRequestableData data = new RadarCubeRequestableData(
                dataToCopy);
        for (AbstractRequestableData paramData : paramList) {
            data.addParam(paramData);
        }

        TimeAndSpace tas = data.getTimeAndSpace();

        assertEquals(expectedTas, tas);
    }

    private static Level getTiltLevel(double tilt) {
        // Mostly copied from RadarDataAccessFactory
        MasterLevel masterLevel = new MasterLevel("TILT");
        masterLevel.setUnitString("°");
        masterLevel.setType("INC");
        masterLevel.setDescription("Tilt angle of a radar scan.");
        Level level = new Level();
        level.setMasterLevel(masterLevel);
        level.setLevelonevalue(tilt);
        return level;
    }

    private static class TestRequestableData extends AbstractRequestableData {

        private final TimeAndSpace tas;

        private TestRequestableData(TimeAndSpace tas, double tilt) {
            this.tas = tas;
            this.dataTime = tas.getTime();
            this.space = tas.getSpace();
            this.level = getTiltLevel(tilt);
        }

        @Override
        public TimeAndSpace getTimeAndSpace() {
            return tas;
        }

        @Override
        public Object getDataValue(Object arg) throws DataCubeException {
            return null;
        }
    }
}
