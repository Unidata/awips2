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
package com.raytheon.uf.viz.xy.crosssection;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Unit tests for {@link CrossSectionFrameData}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2024 2037631    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestCrossSectionFrameData {

    @Test
    void testConstructor() {
        // Pass in null data -> verify that it gets converted to an empty list
        CrossSectionFrameData frameData = new CrossSectionFrameData(null, null);

        assertEquals(List.of(), frameData.data);
    }

    static Stream<Arguments> provideParamsForHasData() {
        return Stream.of(Arguments.of(null, false),
                Arguments.of(List.of(), false),
                Arguments.of(List.of(new float[10]), true));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForHasData")
    void testHasData(List<float[]> data, boolean expectedResult) {
        CrossSectionFrameData frameData = new CrossSectionFrameData(data, null);

        boolean hasData = frameData.hasData();

        assertEquals(expectedResult, hasData);
    }
}
