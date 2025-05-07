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
package com.raytheon.uf.viz.grid.radar.rsc;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.RETURNS_MOCKS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.verification.VerificationMode;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.Capabilities;
import com.raytheon.uf.viz.grid.radar.RadarVirtualDerivedTimeAndSpace;
import com.raytheon.uf.viz.grid.radar.rsc.util.RadarSRMResourceUtils;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeUtil;
import com.raytheon.viz.grid.record.RequestableDataRecord;
import com.raytheon.viz.grid.rsc.GridResourceData;

/**
 * Unit tests for {@link RadarGridResourceData}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2024 2037624    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
class TestRadarGridResourceData {

    private static final DataTime dt1200 = new DataTime(
            "2024-01-01_12:00:00.0");

    private static final DataTime dt1206 = new DataTime(
            "2024-01-01_12:06:00.0");

    private static final DataTime dt1212 = new DataTime(
            "2024-01-01_12:12:00.0");

    private RadarGridResourceData rscData;

    @Mock
    private AbstractGridResource<GridResourceData> rsc;

    @Mock
    private IGridGeometryProvider space;

    @BeforeEach
    void setupBeforeEach() {
        rscData = new RadarGridResourceData();
    }

    static Stream<Arguments> provideParamsForConstructResource() {
        return Stream.of(Arguments.of("RR", false, false),
                Arguments.of("RRvirt", true, false),
                Arguments.of("SRM", false, true),
                Arguments.of("SRMvirt", true, true));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForConstructResource")
    void testConstructResource(String paramAbbrev, boolean virtualVolumeParam,
            boolean srmParam) throws VizException {
        /*
         * Verify that a non-null resource is constructed, and that virtual
         * volume listeners and/or SRM listeners are registered, depending on
         * the parameter.
         */
        rscData.setMetadataMap(
                new HashMap<>(Map.of(GridConstants.PARAMETER_ABBREVIATION,
                        new RequestConstraint(paramAbbrev))));

        try (MockedStatic<RadarVirtualVolumeUtil> vvUtilMockedStatic = mockStatic(
                RadarVirtualVolumeUtil.class);
                MockedStatic<RadarSRMResourceUtils> srmUtilMockedStatic = mockStatic(
                        RadarSRMResourceUtils.class)) {
            AbstractGridResource<GridResourceData> rsc = rscData
                    .constructResource(getLoadProps(), new PluginDataObject[0]);

            assertNotNull(rsc);
            VerificationMode vvNumInvocations = virtualVolumeParam ? times(1)
                    : never();
            vvUtilMockedStatic.verify(
                    () -> RadarVirtualVolumeUtil
                            .registerVirtualVolumeListeners(eq(rsc), any()),
                    vvNumInvocations);
            VerificationMode srmNumInvocations = srmParam ? times(1) : never();
            srmUtilMockedStatic.verify(
                    () -> RadarSRMResourceUtils.registerSRMListeners(rsc),
                    srmNumInvocations);
        }
    }

    static Stream<Arguments> provideParamsForGetExtraLegendText() {
        TimeAndSpace tas = new TimeAndSpace(dt1206,
                mock(IGridGeometryProvider.class));
        RequestableDataRecord normalRecord = mock(RequestableDataRecord.class);
        when(normalRecord.getTimeAndSpace()).thenReturn(tas);

        RadarVirtualDerivedTimeAndSpace virtTas = new RadarVirtualDerivedTimeAndSpace(
                dt1206, mock(IGridGeometryProvider.class), dt1200, 1.5);
        RequestableDataRecord virtualRecord = mock(RequestableDataRecord.class);
        when(virtualRecord.getTimeAndSpace()).thenReturn(virtTas);

        return Stream.of(
                // Non-requestable record -> empty text
                Arguments.of(mock(GridRecord.class), ""),
                // Record with normal time/space -> empty text
                Arguments.of(normalRecord, ""),
                // Record with virtual time/space -> virtual volume text
                Arguments.of(virtualRecord, "(12:00Z above 1.5)"));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetExtraLegendText")
    void testGetExtraLegendText(GridRecord record, String expectedText) {
        String actualText = rscData.getExtraLegendText(record);

        assertEquals(expectedText, actualText);
    }

    @Test
    void testGetPdos() {
        /*
         * Setup resource with a couple times with PDOs, and one without. Verify
         * that the PDOs are combined into a single list, and that the time with
         * a null mapping causes no issues.
         */
        PluginDataObject pdo1 = mock(PluginDataObject.class);
        PluginDataObject pdo2 = mock(PluginDataObject.class);
        PluginDataObject pdo3 = mock(PluginDataObject.class);
        when(rsc.getDataTimes())
                .thenReturn(new DataTime[] { dt1200, dt1206, dt1212 });
        when(rsc.getPluginDataObjects(dt1200)).thenReturn(List.of(pdo1));
        when(rsc.getPluginDataObjects(dt1206)).thenReturn(List.of(pdo2, pdo3));
        when(rsc.getPluginDataObjects(dt1212)).thenReturn(null);

        Collection<PluginDataObject> pdos = rscData.getPdos(rsc);

        assertEquals(List.of(pdo1, pdo2, pdo3), pdos);
    }

    /**
     * Build load properties that returns mocks for all capabilities.
     *
     * @return load properties
     */
    private static LoadProperties getLoadProps() {
        Capabilities capabilities = mock(Capabilities.class);
        when(capabilities.getCapability(any(), any())).then(invocation -> {
            Class<?> capClass = invocation.getArgument(1);
            return mock(capClass, RETURNS_MOCKS);
        });
        LoadProperties loadProps = mock(LoadProperties.class);
        when(loadProps.getCapabilities()).thenReturn(capabilities);
        return loadProps;
    }
}
