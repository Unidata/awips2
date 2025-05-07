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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.LineString;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;

/**
 * Unit tests for {@link CrossSectionResourceData}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr  2, 2024 2037091    mapeters    Initial creation
 * May 24, 2024 2037092    mapeters    Add testSetLineInfoFromDescriptor
 *
 * </pre>
 *
 * @author mapeters
 */
public class TestCrossSectionResourceData {

    private static final String ALL_LAT = "AllLAT";

    private static final String DT_STR = "2024-03-25_12:00:00.0";

    private CrossSectionResourceData rscData;

    @BeforeEach
    public void setup() {
        rscData = new CrossSectionResourceData();
        rscData.levelType = ALL_LAT;
        rscData.numLines = 10;
    }

    @Test
    public void testGetAffectedFrameTimes1() {
        // Pass in level-less time -> return that time for each line
        DataTime dtInput = new DataTime(DT_STR);

        Collection<DataTime> dtsExpected = new ArrayList<>();
        for (int i = 0; i < 10; ++i) {
            DataTime dt = new DataTime(DT_STR);
            dt.setLevel((double) i, ALL_LAT);
            dtsExpected.add(dt);
        }

        Collection<DataTime> dtsActual = rscData.getAffectedFrameTimes(dtInput);

        assertEquals(dtsExpected, dtsActual);
    }

    @Test
    public void testGetAffectedFrameTimes2() {
        // Pass in line-level time -> return just that same time
        DataTime dtInput = new DataTime(DT_STR);
        dtInput.setLevel(5d, ALL_LAT);

        DataTime dtExpected = new DataTime(DT_STR);
        dtExpected.setLevel(5d, ALL_LAT);
        Collection<DataTime> dtsExpected = List.of(dtExpected);

        Collection<DataTime> dtsActual = rscData.getAffectedFrameTimes(dtInput);

        assertEquals(dtsExpected, dtsActual);
    }

    public void testSetLineInfoFromDescriptor() {
        CrossSectionDescriptor descriptor = mock(CrossSectionDescriptor.class);
        List<LineString> lines = new ArrayList<>();
        for (int i = 0; i < 15; ++i) {
            lines.add(mock(LineString.class));
        }
        when(descriptor.getLines()).thenReturn(lines);
        when(descriptor.getLineID()).thenReturn("AllLon");

        rscData.setLineInfoFromDescriptor(descriptor);

        assertEquals(rscData.numLines, 15);
        assertEquals(rscData.levelType, "AllLon");
    }
}
