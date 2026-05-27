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
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.Capabilities;

/**
 * Unit tests for {@link GridResource}.
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
class TestGridResource {

    private GridResource<?> rsc;

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

        rsc = new GridResource<>(mock(AbstractResourceData.class), loadProps);
    }

    static Stream<Arguments> provideParamsForGetFirstGridRecord() {
        GridRecord record1 = mock(GridRecord.class);
        GridRecord record2 = mock(GridRecord.class);
        return Stream.of(Arguments.of(null, null),
                Arguments.of(List.of(), null),
                Arguments.of(List.of(record1), record1),
                Arguments.of(List.of(record1, record2), record1));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetFirstGridRecord")
    void testGetFirstGridRecord(List<PluginDataObject> inputPdos,
            GridRecord expectedRecord) {
        GridRecord record = rsc.getFirstGridRecord(inputPdos);

        assertEquals(expectedRecord, record);
    }
}
