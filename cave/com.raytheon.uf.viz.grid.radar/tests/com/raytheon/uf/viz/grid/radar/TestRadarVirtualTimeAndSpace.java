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
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.Mockito.mock;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.time.DataTime;

/**
 * Unit tests for {@link RadarVirtualTimeAndSpace}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2024 2037092    mapeters    Initial creation
 * Jul 24, 2024 2037624    mapeters    Renamed, add matches() tests
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarVirtualTimeAndSpace {

    private static final DataTime dt1200 = new DataTime(
            "2024-01-01_12:00:00.0");

    private static final DataTime dt1206 = new DataTime(
            "2024-01-01_12:06:00.0");

    private static IGridGeometryProvider space1 = mock(
            IGridGeometryProvider.class);

    private static IGridGeometryProvider space2 = mock(
            IGridGeometryProvider.class);

    static Stream<Arguments> provideParamsForMatches() {
        TimeAndSpace tas_1200_space1 = new TimeAndSpace(dt1200, space1);

        return Stream.of(
                // One normal TimeAndSpace, one VirtualTimeAndSpace
                // Same time/space
                Arguments.of(tas_1200_space1,
                        new RadarVirtualTimeAndSpace(dt1200, space1, dt1206),
                        true),
                // Different time
                Arguments.of(tas_1200_space1,
                        new RadarVirtualTimeAndSpace(dt1206, space1, dt1200),
                        false),
                // Different space
                Arguments.of(tas_1200_space1,
                        new RadarVirtualTimeAndSpace(dt1200, space2, dt1206),
                        false));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForMatches")
    void testMatches(TimeAndSpace tas1, TimeAndSpace tas2,
            boolean expectedResult) {
        boolean actualResult = tas1.matches(tas2);
        if (tas2 != null) {
            boolean actualResultReversed = tas2.matches(tas1);

            assertEquals(expectedResult, actualResultReversed);
        }

        assertEquals(expectedResult, actualResult);
    }

    @Test
    void testEquals1() {
        // Same times/space -> equal
        RadarVirtualTimeAndSpace tas1 = new RadarVirtualTimeAndSpace(dt1206,
                space1, dt1200);
        RadarVirtualTimeAndSpace tas2 = new RadarVirtualTimeAndSpace(dt1206,
                space1, dt1200);

        assertEquals(tas1, tas2);
    }

    @Test
    void testEquals2() {
        // Different prev scan times -> not equal
        RadarVirtualTimeAndSpace tas1 = new RadarVirtualTimeAndSpace(dt1206,
                space1, dt1200);
        RadarVirtualTimeAndSpace tas2 = new RadarVirtualTimeAndSpace(dt1206,
                space1, dt1206);

        assertNotEquals(tas1, tas2);
    }

    @Test
    void testHashCode1() {
        // Same times/space -> same hash code
        int hash1 = new RadarVirtualTimeAndSpace(dt1206, space1, dt1200)
                .hashCode();
        int hash2 = new RadarVirtualTimeAndSpace(dt1206, space1, dt1200)
                .hashCode();

        assertEquals(hash1, hash2);
    }

    @Test
    void testHashCode2() {
        // Different prev scan times -> different hash code
        int hash1 = new RadarVirtualTimeAndSpace(dt1206, space1, dt1200)
                .hashCode();
        int hash2 = new RadarVirtualTimeAndSpace(dt1206, space1, dt1206)
                .hashCode();

        assertNotEquals(hash1, hash2);
    }
}
