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

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.inventory.tree.SourceNode;

/**
 * Unit tests for {@link RadarGridExtension}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2024 2037939    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarGridExtension {

    static Stream<Arguments> provideParamsForResolvePluginSpecifiedField() {
        return Stream.of(
                Arguments.of("radar-koax", "TILT", "TILT",
                        TiltTemporalGridDataLevelNode.class),
                Arguments.of("RAP13", "TILT", "TILT",
                        TiltStaticGridDataLevelNode.class),
                Arguments.of("radar-koax", "TILT", "RR", null),
                Arguments.of("radar-koax", "TILT", "RCP",
                        RcpGridDataLevelNode.class),
                Arguments.of("radar-koax", "FHAG", "TILT", null));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForResolvePluginSpecifiedField")
    void testResolvePluginSpecifiedField(String source, String masterLevelName,
            String param, Class<?> expectedClass) {
        RadarGridExtension extension = new RadarGridExtension();
        SourceNode sourceNode = new SourceNode();
        sourceNode.setValue(source);
        Level level = new Level();
        level.setMasterLevel(new MasterLevel(masterLevelName));
        level.setLevelonevalue(1d);
        DerivParamField field = new DerivParamField();
        field.setParam(param);

        Object actualObj = extension.resolvePluginSpecifiedField(sourceNode,
                level, null, field);

        if (expectedClass == null) {
            assertNull(actualObj);
        } else {
            assertTrue(actualObj.getClass() == expectedClass);
        }
    }
}
