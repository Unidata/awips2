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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import java.text.ParsePosition;
import java.util.stream.Stream;

import javax.measure.Unit;
import javax.xml.bind.JAXBException;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

import tech.units.indriya.format.SimpleUnitFormat;

/**
 * Unit tests for {@link VerticalInteractionLevelGroup}.
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
class TestVerticalInteractionLevelGroup {

    private static final SingleTypeJAXBManager<VerticalInteractionLevelGroup> jaxb = SingleTypeJAXBManager
            .createWithoutException(VerticalInteractionLevelGroup.class);

    private static MockedStatic<LevelFactory> lfMockedStatic;

    @BeforeAll
    static void setupBeforeAll() {
        LevelFactory levelFactory = mock(LevelFactory.class);
        // Mock getLevel method that converts and rounds level values
        when(levelFactory.getLevel(any(), anyDouble(), any()))
                .then(invocation -> {
                    String masterLevelName = invocation.getArgument(0);
                    double value = invocation.getArgument(1);
                    String unitStr = invocation.getArgument(2);

                    String masterLevelUnitStr = getUnit(masterLevelName);
                    if (unitStr != null) {
                        Unit<?> unit = SimpleUnitFormat
                                .getInstance(SimpleUnitFormat.Flavor.ASCII)
                                .parseObject(unitStr, new ParsePosition(0));
                        Unit<?> masterLevelUnit = SimpleUnitFormat
                                .getInstance(SimpleUnitFormat.Flavor.ASCII)
                                .parseObject(masterLevelUnitStr,
                                        new ParsePosition(0));
                        value = unit.getConverterToAny(masterLevelUnit)
                                .convert(value);
                    }
                    // Limit precision to 3 places past the decimal
                    value = ((int) (value * 1000)) / 1000.0;

                    MasterLevel masterLevel = new MasterLevel(masterLevelName);
                    masterLevel.setUnitString(masterLevelUnitStr);
                    Level level = new Level();
                    level.setMasterLevel(masterLevel);
                    level.setLevelonevalue(value);
                    return level;
                });

        lfMockedStatic = mockStatic(LevelFactory.class);
        lfMockedStatic.when(LevelFactory::getInstance).thenReturn(levelFactory);
    }

    @AfterAll
    static void tearDownAfterAll() {
        lfMockedStatic.close();
    }

    static Stream<Arguments> provideParamsForDeserialize() {
        return Stream.of(
                // No attributes -> everything defaults to an empty string
                Arguments.of("<verticalInteractionLevelGroup/>", "", "", ""),
                /*
                 * Group without unit -> values are used as is except that
                 * number of decimal places is normalized
                 */
                Arguments.of(
                        "<verticalInteractionLevelGroup masterLevel=\"FH\" label=\"Test Group (1-10 km)\" levels=\"1000,5000,10000\"/>",
                        "FH", "Test Group (1-10 km)", "1000.0,5000.0,10000.0"),
                /*
                 * Group with unit -> level values are converted and decimal
                 * places normalized
                 */
                Arguments.of(
                        "<verticalInteractionLevelGroup masterLevel=\"FHAG\" label=\"Test Group (1.5-4.5 km)\" unit=\"km\" levels=\"1.5,3,4.5\"/>",
                        "FHAG", "Test Group (1.5-4.5 km)",
                        "1500.0,3000.0,4500.0"));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForDeserialize")
    void testUnmarshalFromXml(String xml, String expectedMasterLevel,
            String expectedLabel, String expectedLevels) throws JAXBException {
        VerticalInteractionLevelGroup group = jaxb.unmarshalFromXml(xml);

        assertEquals(expectedMasterLevel, group.getMasterLevel());
        assertEquals(expectedLabel, group.getLabel());
        assertEquals(expectedLevels, group.getLevels());
        /*
         * Unit should always be null since it's applied during unmarshalling
         * and then cleared
         */
        assertNull(group.getUnit());
    }

    static Stream<Arguments> provideParamsForMatchesLevels() {
        return Stream.of(
                // Exact match -> true
                Arguments.of("1.0,5.0,10.0", "1.0,5.0,10.0", true),
                // Match after thatLevels is normalized -> true
                Arguments.of("1.0,5.0,10.0", "1,5,10", true),
                // Different values -> false
                Arguments.of("1.0,5.0,10.0", "1,5,10,15", false));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForMatchesLevels")
    void testMatchesLevels(String thisLevels, String thatLevels,
            boolean expectedResult) {
        VerticalInteractionLevelGroup group = new VerticalInteractionLevelGroup();
        group.setMasterLevel("FH");
        group.setLevels(thisLevels);

        boolean actualResult = group.matchesLevels(thatLevels);

        assertEquals(expectedResult, actualResult);
    }

    /**
     * Helper method for level factory mock to look up the unit that should be
     * used for some master levels.
     *
     * @param masterLevel
     *            master level
     * @return master level unit
     */
    private static String getUnit(String masterLevel) {
        switch (masterLevel) {
        case "FH":
        case "FHAG":
            return "m";
        }
        throw new RuntimeException("Unsupported master level: " + masterLevel);
    }
}
