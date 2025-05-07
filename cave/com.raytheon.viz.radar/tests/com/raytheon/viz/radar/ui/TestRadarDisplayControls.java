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
package com.raytheon.viz.radar.ui;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;

/**
 * Unit tests for {@link RadarDisplayControls}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2024 2037092    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarDisplayControls {

    @ParameterizedTest
    @ValueSource(booleans = { true, false })
    void testIsVirtualVolumeEnabled(boolean enabled) {
        RadarDisplayControls controls = new RadarDisplayControls();
        controls.virtualVolumeEnabled = enabled;

        boolean actualEnabled = controls.isVirtualVolumeEnabled();

        assertEquals(enabled, actualEnabled);
    }

    @ParameterizedTest
    @CsvSource({ "true,true", "true,false", "false,false", "false,true" })
    void testSetVirtualVolumeEnabled(boolean initialEnabled,
            boolean enabledToSet) {
        /*
         * Verify that value is actually set, and that display manager is
         * notified only if the value actually changed
         */
        RadarDisplayControls controls = new RadarDisplayControls();
        controls.virtualVolumeEnabled = initialEnabled;

        RadarDisplayManager displayMgr = mock(RadarDisplayManager.class);
        try (MockedStatic<RadarDisplayManager> mockedStatic = mockStatic(
                RadarDisplayManager.class)) {
            mockedStatic.when(RadarDisplayManager::getInstance)
                    .thenReturn(displayMgr);
            controls.setVirtualVolumeEnabled(enabledToSet);
        }

        assertEquals(enabledToSet, controls.virtualVolumeEnabled);
        if (initialEnabled == enabledToSet) {
            verify(displayMgr, never()).displayConfigUpdated();
        } else {
            verify(displayMgr).displayConfigUpdated();
        }

    }
}
