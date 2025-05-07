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
package com.raytheon.viz.grid.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Unit tests for {@link VerticalInteractionConfigManager}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 9, 2024  2036517    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
class TestVerticalInteractionConfigManager {

    private static final String ML1 = "MasterLevel1";

    private static final String ML2 = "MasterLevel2";

    private static final VerticalInteractionLevelGroup ml1GroupEmpty = buildLevelGroup(
            ML1, "");

    private static final VerticalInteractionLevelGroup ml1GroupBlank = buildLevelGroup(
            ML1, " ");

    private static final VerticalInteractionLevelGroup ml1Group1 = buildLevelGroup(
            ML1, "1");

    private static final VerticalInteractionLevelGroup ml1Group2 = buildLevelGroup(
            ML1, "2");

    private static final VerticalInteractionLevelGroup ml2Group3 = buildLevelGroup(
            ML2, "3");

    private static MockedStatic<PathManagerFactory> pmMockedStatic;

    static Stream<Arguments> provideParamsForGetLevelGroups() {
        return Stream.of(
                // Empty master level groups map -> empty list
                Arguments.of(Map.of(), ML1, List.of()),
                // No entry for master level -> empty list
                Arguments.of(Map.of(ML2, List.of(ml2Group3)), ML1, List.of()),
                // Entry for master level -> that master level's groups
                Arguments.of(Map.of(ML1, List.of(ml1Group1)), ML1,
                        List.of(ml1Group1)));
    }

    @BeforeAll
    static void setupBeforeAll() {
        IPathManager pm = mock(IPathManager.class);
        pmMockedStatic = mockStatic(PathManagerFactory.class);
        pmMockedStatic.when(() -> PathManagerFactory.getPathManager())
                .thenReturn(pm);
    }

    @AfterAll
    static void tearDownAfterAll() {
        pmMockedStatic.close();
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetLevelGroups")
    void testGetLevelGroups(
            Map<String, List<VerticalInteractionLevelGroup>> masterLevelGroups,
            String masterLevel,
            List<VerticalInteractionLevelGroup> expectedGroups) {
        VerticalInteractionConfigManager mgr = VerticalInteractionConfigManager
                .getInstance();
        mgr.masterLevelGroups.clear();
        mgr.masterLevelGroups.putAll(masterLevelGroups);

        List<VerticalInteractionLevelGroup> actualGroups = mgr
                .getLevelGroups(masterLevel);

        assertEquals(expectedGroups, actualGroups);
        // Verify the list is unmodifiable
        assertThrows(UnsupportedOperationException.class,
                () -> actualGroups.clear());
    }

    @Test
    void testGetInstance() {
        VerticalInteractionConfigManager mgr1 = VerticalInteractionConfigManager
                .getInstance();
        VerticalInteractionConfigManager mgr2 = VerticalInteractionConfigManager
                .getInstance();

        assertSame(mgr1, mgr2);
    }

    static Stream<Arguments> provideParamsForLoadConfig() {
        List<VerticalInteractionLevelGroup> groups_ml1_12 = List.of(ml1Group1,
                ml1Group2);
        List<VerticalInteractionLevelGroup> groups_ml1_12BlankEmpty = List
                .of(ml1Group1, ml1Group2, ml1GroupEmpty, ml1GroupBlank);
        List<VerticalInteractionLevelGroup> groups_ml1_12_ml2_3 = List
                .of(ml1Group1, ml1Group2, ml2Group3);
        return Stream.of(
                // Null levels -> empty map
                Arguments.of(null, Map.of()),
                // Levels object without any groups -> empty map
                Arguments.of(buildLevels(List.of()), Map.of()),
                // Single group -> single map entry
                Arguments.of(buildLevels(List.of(ml1Group1)),
                        Map.of(ML1, List.of(ml1Group1))),
                /*
                 * Two groups for same master level -> groups go in same map
                 * entry
                 */
                Arguments.of(buildLevels(groups_ml1_12),
                        Map.of(ML1, groups_ml1_12)),
                // Same as above, but groups with blank/empty levels are ignored
                Arguments.of(buildLevels(groups_ml1_12BlankEmpty),
                        Map.of(ML1, groups_ml1_12)),
                /*
                 * Groups for different master levels -> mapped appropriately by
                 * master level
                 */
                Arguments.of(buildLevels(groups_ml1_12_ml2_3),
                        Map.of(ML1, groups_ml1_12, ML2, List.of(ml2Group3))));

    }

    @ParameterizedTest
    @MethodSource("provideParamsForLoadConfig")
    void testLoadConfig(VerticalInteractionLevels levels,
            Map<String, List<VerticalInteractionLevelGroup>> expectedMap) {
        VerticalInteractionConfigManager mgr = VerticalInteractionConfigManager
                .getInstance();

        mgr.loadConfig(levels);

        assertEquals(expectedMap, mgr.masterLevelGroups);
    }

    private static VerticalInteractionLevelGroup buildLevelGroup(
            String masterLevel, String levels) {
        VerticalInteractionLevelGroup group = new VerticalInteractionLevelGroup();
        group.setMasterLevel(masterLevel);
        group.setLevels(levels);
        return group;
    }

    private static VerticalInteractionLevels buildLevels(
            List<VerticalInteractionLevelGroup> groups) {
        VerticalInteractionLevels levels = new VerticalInteractionLevels();
        levels.setGroups(groups);
        return levels;
    }
}
