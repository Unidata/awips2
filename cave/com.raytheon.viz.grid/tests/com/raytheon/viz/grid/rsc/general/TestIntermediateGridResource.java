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
package com.raytheon.viz.grid.rsc.general;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.RETURNS_MOCKS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.Capabilities;
import com.raytheon.viz.grid.rsc.GridResourceData;

/**
 * Unit tests for {@link IntermediateGridResource}.
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
class TestIntermediateGridResource {

    private static final DataTime dt1200 = new DataTime(
            "2024-01-01_12:00:00.0");

    private static final DataTime dt1206 = new DataTime(
            "2024-01-01_12:06:00.0");

    private TestExtendedIntermediateGridResource rsc;

    @BeforeEach
    void setupBeforeEach() {
        // Build load properties that returns mocks for all capabilities
        Capabilities capabilities = mock(Capabilities.class);
        when(capabilities.getCapability(any(), any())).then(invocation -> {
            Class<?> capClass = invocation.getArgument(1);
            return mock(capClass, RETURNS_MOCKS);
        });
        LoadProperties loadProps = mock(LoadProperties.class);
        when(loadProps.getCapabilities()).thenReturn(capabilities);

        GridResourceData rscData = mock(GridResourceData.class);
        when(rscData.isKeepDataWhileRetrievingUpdate()).thenReturn(true);

        rsc = new TestExtendedIntermediateGridResource(rscData, loadProps);
    }

    static Stream<Arguments> provideParamsForGetPluginDataObjects() {
        List<PluginDataObject> pdos1 = List.of(mock(PluginDataObject.class));
        List<PluginDataObject> pdos2 = List.of(mock(PluginDataObject.class));
        Map<DataTime, List<PluginDataObject>> dt1200toPdos1 = Map.of(dt1200,
                pdos1);
        Map<DataTime, List<PluginDataObject>> dt1200toPdos2 = Map.of(dt1200,
                pdos2);
        return Stream.of(
                // Null time -> null PDOs
                Arguments.of(Map.of(), Map.of(), null, null),
                // Valid time but empty maps -> null PDOs
                Arguments.of(Map.of(), Map.of(), dt1200, null),
                // Valid time but not in maps -> null PDOs
                Arguments.of(dt1200toPdos1, dt1200toPdos2, dt1206, null),
                // Time in pdoMap only -> pdoMap value
                Arguments.of(dt1200toPdos1, Map.of(), dt1200, pdos1),
                // Time in pdoMap and previousPdoMap -> pdoMap value
                Arguments.of(dt1200toPdos1, dt1200toPdos2, dt1200, pdos1),
                // Time in previousPdoMap only -> previousPdoMap value
                Arguments.of(Map.of(), dt1200toPdos2, dt1200, pdos2));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetPluginDataObjects")
    void testGetPluginDataObjects(Map<DataTime, List<PluginDataObject>> pdoMap,
            Map<DataTime, List<PluginDataObject>> previousPdoMap,
            DataTime timeArg, List<PluginDataObject> expectedPdos) {
        rsc.getPdoMap().putAll(pdoMap);
        rsc.previousPdoMap.putAll(previousPdoMap);

        List<PluginDataObject> actualPdos = rsc.getPluginDataObjects(timeArg);

        assertEquals(expectedPdos, actualPdos);
    }

    /**
     * Sub-class of {@link IntermediateGridResource} for testing. This just
     * allows us to access/populate its protected pdoMap field.
     */
    private static class TestExtendedIntermediateGridResource
            extends IntermediateGridResource {

        private TestExtendedIntermediateGridResource(
                GridResourceData resourceData, LoadProperties loadProperties) {
            super(resourceData, loadProperties);
        }

        private Map<DataTime, List<PluginDataObject>> getPdoMap() {
            return pdoMap;
        }
    }
}
