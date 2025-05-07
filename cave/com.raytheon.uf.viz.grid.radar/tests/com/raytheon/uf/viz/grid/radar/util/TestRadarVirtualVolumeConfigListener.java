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
package com.raytheon.uf.viz.grid.radar.util;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * Unit tests for {@link RadarVirtualVolumeConfigListener}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2024 2037624    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
class TestRadarVirtualVolumeConfigListener {

    private static final String DT_1200 = "2024-01-01_12:00:00.0";

    private static final String DT_1206 = "2024-01-01_12:06:00.0";

    private final DataTime dt1200 = new DataTime(DT_1200);

    private final DataTime dt1206 = new DataTime(DT_1206);

    private DataTime dt1200_1 = buildDt(DT_1200, 1);

    private DataTime dt1200_2 = buildDt(DT_1200, 2);

    private DataTime dt1206_1 = buildDt(DT_1206, 1);

    private DataTime dt1206_2 = buildDt(DT_1206, 2);

    /** Radar display manager with virtual volumes enabled */
    private final RadarDisplayManager displayMgrEnabled = buildRadarDisplayManager(
            true);

    /** Radar display manager with virtual volumes disabled */
    private final RadarDisplayManager displayMgrDisabled = buildRadarDisplayManager(
            false);

    private MockedStatic<RadarDisplayManager> displayManagerMockedStatic;

    private MockedStatic<RadarVirtualVolumeUtil> utilMockedStatic;

    private AbstractVizResource<?, ?> rsc;

    @Mock
    private AbstractRequestableResourceData rscData;

    private RadarVirtualVolumeConfigListener listener;

    @BeforeEach
    void setupBeforeEach() {
        displayManagerMockedStatic = mockStatic(RadarDisplayManager.class);
        displayManagerMockedStatic.when(RadarDisplayManager::getInstance)
                .thenReturn(displayMgrEnabled);
        utilMockedStatic = mockStatic(RadarVirtualVolumeUtil.class);

        rsc = mock(AbstractVizResource.class);
        lenient().doReturn(rscData).when(rsc).getResourceData();
        listener = new RadarVirtualVolumeConfigListener(rsc);
    }

    @AfterEach
    void tearDownAfterEach() {
        displayManagerMockedStatic.close();
        utilMockedStatic.close();
    }

    @Test
    void testUpdateConfig1() {
        /*
         * When virtual volume status hasn't changed, do nothing
         * (RadarDisplayManager was mocked to have virtual volumes enabled in
         * the setup method, and isn't changed here)
         */
        listener.updateConfigInternal();

        verify(rscData, never()).invalidateAvailableTimesCache();
        utilMockedStatic.verify(
                () -> RadarVirtualVolumeUtil.reloadFrames(any(), any()),
                never());
    }

    @Test
    void testUpdateConfig2() {
        /*
         * When virtual volume status changes from enabled to disabled in
         * RadarDisplayManager, verify that the listener's flag is updated
         * accordingly and the resource's latest time is reloaded.
         */
        displayManagerMockedStatic.when(RadarDisplayManager::getInstance)
                .thenReturn(displayMgrDisabled);
        when(rsc.getDataTimes()).thenReturn(new DataTime[] { dt1200, dt1206 });

        listener.updateConfigInternal();

        assertFalse(listener.virtualVolumeEnabled);
        verify(rscData).invalidateAvailableTimesCache();
        utilMockedStatic.verify(
                () -> RadarVirtualVolumeUtil.reloadFrames(Set.of(dt1206), rsc));
    }

    @Test
    void testUpdateConfig3() {
        /*
         * When virtual volume status changes from enabled to disabled in
         * RadarDisplayManager, verify that the listener's flag is updated
         * accordingly and all the resource's spatial frame times for the latest
         * time are reloaded.
         */
        displayManagerMockedStatic.when(RadarDisplayManager::getInstance)
                .thenReturn(displayMgrDisabled);
        when(rsc.getDataTimes()).thenReturn(
                new DataTime[] { dt1200_1, dt1200_2, dt1206_1, dt1206_2 });

        listener.updateConfigInternal();

        assertFalse(listener.virtualVolumeEnabled);
        verify(rscData).invalidateAvailableTimesCache();
        utilMockedStatic.verify(() -> RadarVirtualVolumeUtil
                .reloadFrames(Set.of(dt1206_1, dt1206_2), rsc));
    }

    private RadarDisplayManager buildRadarDisplayManager(
            boolean virtualVolumeEnabled) {
        RadarDisplayManager displayMgr = mock(RadarDisplayManager.class);
        RadarDisplayControls displayControls = mock(RadarDisplayControls.class);
        when(displayControls.isVirtualVolumeEnabled())
                .thenReturn(virtualVolumeEnabled);
        when(displayMgr.getCurrentSettings()).thenReturn(displayControls);
        return displayMgr;
    }

    private static DataTime buildDt(String dtStr, double level) {
        DataTime dt = new DataTime(dtStr);
        dt.setLevel(level, "TYPE");
        return dt;
    }
}
