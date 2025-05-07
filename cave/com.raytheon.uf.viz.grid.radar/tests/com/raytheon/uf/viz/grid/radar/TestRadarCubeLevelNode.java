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

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.derivparam.tree.AbstractCubeLevelNode;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.CubeLevel;
import com.raytheon.uf.common.time.DataTime;

/**
 * Unit tests for {@link RadarCubeLevelNode}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2024 2037624    mapeters    Initial creation
 * Oct 14, 2024 2037939    mapeters    Test getAvailability() and helper methods
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
class TestRadarCubeLevelNode {

    private static final DataTime dt_1154 = new DataTime(
            "2024-01-01_11:54:00.0");

    private static final DataTime dt_1200 = new DataTime(
            "2024-01-01_12:00:00.0");

    private static final IGridGeometryProvider space1 = mock(
            IGridGeometryProvider.class);

    private static final IGridGeometryProvider space2 = mock(
            IGridGeometryProvider.class);

    private static final TimeAndSpace tas_1154_space1 = new TimeAndSpace(
            dt_1154, space1);

    private static final TimeAndSpace tas_1200_space1 = new TimeAndSpace(
            dt_1200, space1);

    private static final RadarVirtualTimeAndSpace vtas_1200_space1 = new RadarVirtualTimeAndSpace(
            dt_1200, space1, dt_1154);

    private static final Level level0_5 = new Level("0.5TILT");

    private static final Level level1_5 = new Level("1.5TILT");

    private static final AbstractRequestableNode paramNode0_5 = new ConcreteRequestableNode(
            level0_5);

    private static final AbstractRequestableNode paramNode1_5 = new ConcreteRequestableNode(
            level1_5);

    private static final Pair<TimeAndSpace, Level> tas1200_0_5 = ImmutablePair
            .of(tas_1200_space1, level0_5);

    private static final Pair<TimeAndSpace, Level> tas1200_1_5 = ImmutablePair
            .of(tas_1200_space1, level1_5);

    private static final Pair<TimeAndSpace, Level> vtas1200_1_5 = ImmutablePair
            .of(vtas_1200_space1, level1_5);

    private static final Pair<TimeAndSpace, Level> tas1154_0_5 = ImmutablePair
            .of(tas_1154_space1, level0_5);

    private static final Pair<TimeAndSpace, Level> tas1154_1_5 = ImmutablePair
            .of(tas_1154_space1, level1_5);

    private static final RadarVirtualDerivedTimeAndSpace vdtas1200_0_5 = new RadarVirtualDerivedTimeAndSpace(
            dt_1200, space1, dt_1154, 0.5);

    @Mock
    private AbstractRequestableData data1;

    @Mock
    private AbstractRequestableData data2;

    @Mock
    private AbstractRequestableData data3;

    @Test
    void testGetParamData() {
        /*
         * Setup param map with a normal time/space entry that matches, along
         * with a virtual time/space entry that matches. Verify that the result
         * is the combination of their data lists, and that non-matching
         * time/space entries are ignored.
         */
        RadarCubeLevelNode node = new RadarCubeLevelNode(
                mock(AbstractCubeLevelNode.class));
        // Linked map so we know the data order to use in assertEquals below
        Map<TimeAndSpace, List<AbstractRequestableData>> paramMap = new LinkedHashMap<>();
        // Normal and virtual time/space that match
        paramMap.put(tas_1200_space1, List.of(data1));
        paramMap.put(vtas_1200_space1, List.of(data2));
        // Non-matching entries
        paramMap.put(tas_1154_space1, List.of(data3));
        paramMap.put(new TimeAndSpace(dt_1200, space2), List.of(data3));

        List<AbstractRequestableData> data = node.getParamData(paramMap,
                tas_1200_space1);

        assertEquals(data, List.of(data1, data2));
    }

    static Stream<Arguments> provideParamsForGetAvailability() {
        Map<AbstractRequestableNode, Set<TimeAndSpace>> dependencyAvail = Map
                .of(paramNode0_5, Set.of(tas_1154_space1, tas_1200_space1),
                        paramNode1_5, Set.of(tas_1154_space1));
        Map<AbstractRequestableNode, Set<TimeAndSpace>> dependencyAvailWithVirt = Map
                .of(paramNode0_5, Set.of(tas_1154_space1, tas_1200_space1),
                        paramNode1_5,
                        Set.of(tas_1154_space1, vtas_1200_space1));

        return Stream.of(
                /*
                 * 2+ levels for 11:54 so it's available, only 1 for 12:00 so
                 * it's not available
                 */
                Arguments.of(dependencyAvail, Set.of(tas_1154_space1)),
                /*
                 * 2+ normal levels for 11:54 so it's available, 1 normal (0.5)
                 * and 1 virtual (1.5) for 12:00 so it has a virtual derived
                 * availability indicating that 0.5 is the current scan tilt
                 */
                Arguments.of(dependencyAvailWithVirt,
                        Set.of(tas_1154_space1, vdtas1200_0_5)));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetAvailability")
    void testGetAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> dependencyAvailability,
            Set<TimeAndSpace> expectedAvail) throws DataCubeException {
        AbstractRequestableNode pressureNode = mock(
                AbstractRequestableNode.class);
        CubeLevel<AbstractRequestableNode, AbstractRequestableNode> cubeLevel0_5 = new CubeLevel<>(
                pressureNode, paramNode0_5);
        CubeLevel<AbstractRequestableNode, AbstractRequestableNode> cubeLevel1_5 = new CubeLevel<>(
                pressureNode, paramNode1_5);

        RadarCubeLevelNode node = new RadarCubeLevelNode(
                List.of(cubeLevel0_5, cubeLevel1_5), "radar-koax");

        Set<TimeAndSpace> avail = node.getAvailability(dependencyAvailability);

        assertEquals(expectedAvail, avail);
    }

    static Stream<Arguments> provideParamsForGetMultiLevelAvailability() {
        return Stream.of(
                // Only one time/space -> no multi-level availability
                Arguments.of(Set.of(tas1200_0_5, tas1154_0_5), Set.of()),
                /*
                 * 2+ levels for 12:00 but only one for 11:54 -> only 12:00 is
                 * returned
                 */
                Arguments.of(Set.of(tas1200_0_5, tas1200_1_5, tas1154_0_5),
                        Set.of(tas_1200_space1)),
                /*
                 * One normal level and one virtual level for 12:00 -> normal
                 * and virtual time/space are both returned
                 */
                Arguments.of(Set.of(tas1200_0_5, vtas1200_1_5, tas1154_0_5),
                        Set.of(tas_1200_space1, vtas_1200_space1)),
                /*
                 * One normal level and one virtual level for 12:00, and two
                 * normal levels for 11:54 -> normal and virtual time/space are
                 * returned for 12:00, and normal for 11:54
                 */
                Arguments.of(Set.of(tas1200_0_5, vtas1200_1_5, tas1154_0_5, tas1154_1_5),
                        Set.of(tas_1200_space1, vtas_1200_space1,
                                tas_1154_space1)));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetMultiLevelAvailability")
    void testGetMultiLevelAvailability(
            Set<Pair<TimeAndSpace, Level>> paramAvailability,
            Set<TimeAndSpace> expectedAvail) {
        Set<TimeAndSpace> actualAvail = RadarCubeLevelNode
                .getMultiLevelAvailability(paramAvailability);

        assertEquals(expectedAvail, actualAvail);
    }

    static Stream<Arguments> provideParamsForMergeVirtualAvailability() {
        return Stream.of(
                // No virtual time/space, set unchanged
                Arguments.of(Set.of(tas_1200_space1), Set.of(tas1200_0_5, tas1200_1_5),
                        null),
                /*
                 * Normal 0.5 time/space and virtual 1.5 time/space are merged
                 * into a RadarVirtualDerivedTimeAndSpace with current tilt of
                 * 0.5
                 */
                Arguments.of(Set.of(tas_1200_space1, vtas_1200_space1),
                        Set.of(tas1200_0_5, vtas1200_1_5), vdtas1200_0_5),
                /*
                 * Same as above but there are also some 11:54 time/space
                 * entries that shouldn't change the result
                 */
                Arguments.of(
                        Set.of(tas_1200_space1, vtas_1200_space1,
                                tas_1154_space1),
                        Set.of(tas1200_0_5, vtas1200_1_5, tas1154_0_5, tas1154_1_5),
                        vdtas1200_0_5));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForMergeVirtualAvailability")
    void testGetMergedVirtualAvailability(
            Set<TimeAndSpace> multiLevelAvailability,
            Set<Pair<TimeAndSpace, Level>> paramAvailability,
            RadarVirtualDerivedTimeAndSpace expectedVirtualDerivedAvail) {
        RadarVirtualDerivedTimeAndSpace actualVirtualDerivedAvail = RadarCubeLevelNode
                .getMergedVirtualAvailability(multiLevelAvailability,
                        paramAvailability);

        assertEquals(expectedVirtualDerivedAvail, actualVirtualDerivedAvail);
    }

    static Stream<Arguments> provideParamsForGetParamAvailability() {
        Map<AbstractRequestableNode, Set<TimeAndSpace>> dependencyAvail = Map
                .of(paramNode0_5, Set.of(tas_1154_space1, tas_1200_space1),
                        paramNode1_5, Set.of(tas_1154_space1));
        Map<AbstractRequestableNode, Set<TimeAndSpace>> dependencyAvailWithVirt = Map
                .of(paramNode0_5, Set.of(tas_1154_space1, tas_1200_space1),
                        paramNode1_5,
                        Set.of(tas_1154_space1, vtas_1200_space1));

        return Stream.of(
                Arguments.of(dependencyAvail,
                        Set.of(tas1154_0_5, tas1200_0_5, tas1154_1_5)),
                Arguments.of(dependencyAvailWithVirt,
                        Set.of(tas1154_0_5, tas1200_0_5, tas1154_1_5, vtas1200_1_5)));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetParamAvailability")
    void testGetParamAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> dependencyAvailability,
            Set<Pair<TimeAndSpace, Level>> expectedParamAvail) {
        AbstractRequestableNode pressureNode = mock(
                AbstractRequestableNode.class);
        CubeLevel<AbstractRequestableNode, AbstractRequestableNode> cubeLevel0_5 = new CubeLevel<>(
                pressureNode, paramNode0_5);
        CubeLevel<AbstractRequestableNode, AbstractRequestableNode> cubeLevel1_5 = new CubeLevel<>(
                pressureNode, paramNode1_5);

        RadarCubeLevelNode node = new RadarCubeLevelNode(
                List.of(cubeLevel0_5, cubeLevel1_5), "radar-koax");

        Set<Pair<TimeAndSpace, Level>> paramAvail = node
                .getParamAvailability(dependencyAvailability);

        assertEquals(expectedParamAvail, paramAvail);
    }

    protected static class ConcreteRequestableNode
            extends AbstractRequestableNode {

        public ConcreteRequestableNode(Level level) {
            super(level);
            setValue(level.toString());
        }

        @Override
        public boolean isConstant() {
            return false;
        }
    }
}
