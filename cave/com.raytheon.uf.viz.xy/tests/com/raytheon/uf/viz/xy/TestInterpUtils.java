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
package com.raytheon.uf.viz.xy;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.awt.Rectangle;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 *
 * Unit tests for {@link InterpUtils}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2024  2037698    bines       Initial creation
 *
 * </pre>
 *
 * @author bines
 */

class TestInterpUtils {

    private Rectangle rect;

    private float[] data = { 2f, 1f, 1f, 5f };

    @BeforeEach
    public void setupBeforeEach() throws Exception {
        rect = new Rectangle(0, 0, 2, 2);
    }

    @Test
    void testGetInterpolatedData() {
        // Use nearest neighbor
        // Rectangle is 2x2 so coord is closest to last value
        double x1 = 0.7;
        double y1 = 0.7;
        assertEquals(5,
                InterpUtils.getInterpolatedData(rect, x1, y1, data, true));

        // Don't use nearest neighbor
        assertEquals(3.0500001907348633,
                InterpUtils.getInterpolatedData(rect, x1, y1, data, false));

        // Rectangle is 2x2 so coord is closest to first value
        double x2 = 0.2;
        double y2 = 0.2;
        assertEquals(2,
                InterpUtils.getInterpolatedData(rect, x2, y2, data, true));

        // Don't use nearest neighbor, original function
        assertEquals(1.8000000715255737,
                InterpUtils.getInterpolatedData(rect, x2, y2, data));
    }

}
