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
package com.raytheon.uf.viz.d2d.core.time;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * Unit tests for {@link D2DTimeMatcher}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2025 2038488    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestD2DTimeMatcher {

    private static final String LEVEL_TYPE = "TestType";

    private static final DataTime dt1200_1 = buildTime("2025-01-01_12:00:00.0",
            1d, LEVEL_TYPE);

    private static final DataTime dt1200_2 = buildTime("2025-01-01_12:00:00.0",
            2d, LEVEL_TYPE);

    private static final DataTime dt1200_3 = buildTime("2025-01-01_12:00:00.0",
            3d, LEVEL_TYPE);

    private static final DataTime dt1210_1 = buildTime("2025-01-01_12:10:00.0",
            1d, LEVEL_TYPE);

    private static final DataTime dt1210_2 = buildTime("2025-01-01_12:10:00.0",
            2d, LEVEL_TYPE);

    private static final DataTime dt1210_3 = buildTime("2025-01-01_12:10:00.0",
            3d, LEVEL_TYPE);

    private static final DataTime dt1220_2 = buildTime("2025-01-01_12:20:00.0",
            2d, LEVEL_TYPE);

    private D2DTimeMatcher tm;

    private IDescriptor desc;

    private AbstractVizResource<?, IDescriptor> rsc;

    @SuppressWarnings("unchecked")
    @BeforeEach
    void setupBeforeEach() {
        try (MockedStatic<VizGlobalsManager> globalsMockedStatic = mockStatic(
                VizGlobalsManager.class, RETURNS_DEEP_STUBS);
                MockedStatic<AbstractTimeMatchingConfigurationFactory> tmConfigMockedStatic = mockStatic(
                        AbstractTimeMatchingConfigurationFactory.class)) {
            tm = new D2DTimeMatcher();
        }

        desc = mock(IDescriptor.class);
        rsc = mock(AbstractVizResource.class);
        when(rsc.getDescriptor()).thenReturn(desc);
        ResourceList rl = mock(ResourceList.class);
        when(rl.containsRsc(rsc)).thenReturn(true);
        when(desc.getResourceList()).thenReturn(rl);
    }

    @Test
    void testFilterTimesByFrozenLevel1() {
        // Null frozen level -> unfiltered times
        DataTime[] inputTimes = { dt1200_1, dt1200_2 };

        DataTime[] outputTimes = D2DTimeMatcher
                .filterTimesByFrozenLevel(inputTimes, null);

        assertArrayEquals(new DataTime[] { dt1200_1, dt1200_2 }, outputTimes);
    }

    @Test
    void testFilterTimesByFrozenLevel2() {
        // Frozen level == 1 -> only level 1 time returned
        DataTime[] inputTimes = { dt1200_1, dt1200_2, dt1210_1, dt1210_2 };

        DataTime[] outputTimes = D2DTimeMatcher
                .filterTimesByFrozenLevel(inputTimes, 1d);

        assertArrayEquals(new DataTime[] { dt1200_1, dt1210_1 }, outputTimes);
    }

    @Test
    void testUpdateAndApplyFrozenLevels1() {
        // Empty times -> empty times (not null since we technically succeeded)
        DataTime[] actualTimes = tm.updateAndApplyFrozenLevels(rsc,
                new DataTime[0], false);

        assertArrayEquals(new DataTime[0], actualTimes);
    }

    @Test
    void testUpdateAndApplyFrozenLevels2() {
        // Null times -> empty times (not null since we technically succeeded)
        DataTime[] actualTimes = tm.updateAndApplyFrozenLevels(rsc, null,
                false);

        assertArrayEquals(new DataTime[0], actualTimes);
    }

    @Test
    void testUpdateAndApplyFrozenLevels3() {
        /*
         * Non-basis resource, descriptor time for resource is level 1 -> level
         * is frozen to 1 and times are filtered to level 1
         */
        DataTime[] inputTimes = { dt1200_1, dt1200_2 };
        when(desc.getTimeForResource(rsc)).thenReturn(dt1200_1);

        DataTime[] actualTimes = tm.updateAndApplyFrozenLevels(rsc, inputTimes,
                false);

        DataTime[] expectedTimes = { dt1200_1 };
        assertArrayEquals(expectedTimes, actualTimes);
        assertEquals(1d, tm.frozenLevels.get(LEVEL_TYPE));
    }

    @Test
    void testUpdateAndApplyFrozenLevels4() {
        /*
         * Non-basis resource, no time for resource, and no frozen level -> null
         * to indicate filtering was not successful
         */
        DataTime[] inputTimes = { dt1200_1, dt1200_2 };
        when(desc.getTimeForResource(rsc)).thenReturn(null);

        DataTime[] actualTimes = tm.updateAndApplyFrozenLevels(rsc, inputTimes,
                false);

        assertEquals(null, actualTimes);
    }

    @Test
    void testUpdateAndApplyFrozenLevels5() {
        /*
         * Non-basis resource, no time for resource, but frozen level cached ->
         * filtered to cached level
         */
        DataTime[] inputTimes = { dt1200_1, dt1200_2 };
        when(desc.getTimeForResource(rsc)).thenReturn(null);
        tm.frozenLevels.put(LEVEL_TYPE, 1d);

        DataTime[] actualTimes = tm.updateAndApplyFrozenLevels(rsc, inputTimes,
                false);

        DataTime[] expectedTimes = { dt1200_1 };
        assertArrayEquals(expectedTimes, actualTimes);
        assertEquals(1d, tm.frozenLevels.get(LEVEL_TYPE));
    }

    @Test
    void testUpdateAndApplyFrozenLevels6() {
        // Basis resource -> null cached, un-filtered times are returned
        DataTime[] inputTimes = { dt1200_1, dt1200_2 };

        DataTime[] actualTimes = tm.updateAndApplyFrozenLevels(rsc, inputTimes,
                true);

        DataTime[] expectedTimes = { dt1200_1, dt1200_2 };
        assertArrayEquals(expectedTimes, actualTimes);
        assertTrue(tm.frozenLevels.containsKey(LEVEL_TYPE));
        assertEquals(null, tm.frozenLevels.get(LEVEL_TYPE));
    }

    @Test
    void testUpdateAndApplyFrozenLevelsAfterOverlay1() {
        /*
         * Multiple levels in the initial overlay times -> most common overlay
         * level is frozen and available times are filtered to that level and
         * returned
         */
        DataTime[] inputAvailableTimes = { dt1200_1, dt1200_2, dt1200_3,
                dt1210_1, dt1210_2, dt1210_3, dt1220_2 };
        DataTime[] inputOverlayTimes = { dt1200_2, dt1200_3, dt1210_2 };

        DataTime[] actualAvailableTimes = tm
                .updateAndApplyFrozenLevelsAfterOverlay(rsc,
                        inputAvailableTimes, inputOverlayTimes);

        assertArrayEquals(new DataTime[] { dt1200_2, dt1210_2, dt1220_2 },
                actualAvailableTimes);
        assertEquals(2d, tm.frozenLevels.get(LEVEL_TYPE));
    }

    @Test
    void testUpdateAndApplyFrozenLevelsAfterOverlay2() {
        /*
         * Already only one level in the initial overlay times -> that level is
         * cached and null is returned to indicate that initial overlay times
         * are good as-is
         */
        DataTime[] inputAvailableTimes = { dt1200_1, dt1200_2, dt1200_3,
                dt1210_1, dt1210_2, dt1210_3 };
        DataTime[] inputOverlayTimes = { dt1200_3, dt1210_3 };

        DataTime[] actualAvailableTimes = tm
                .updateAndApplyFrozenLevelsAfterOverlay(rsc,
                        inputAvailableTimes, inputOverlayTimes);

        assertEquals(null, actualAvailableTimes);
        assertEquals(3d, tm.frozenLevels.get(LEVEL_TYPE));
    }

    private static DataTime buildTime(String timeStr, Double levelVal,
            String levelType) {
        DataTime dt = new DataTime(timeStr);
        dt.setLevel(levelVal, levelType);
        return dt;
    }
}
