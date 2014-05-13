/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.shef;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Test;

import com.raytheon.edex.transform.shef.MetarToShefTransformer;
import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * Tests extracted from MetarToShef.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2012 1185       jkorman     Extracted from mains
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TestMetarToShefTransformer {

    /**
     * Test that the transformer creates an empty iterator when given no input.
     */
    @Test
    public void testMetarToShefInteratorA() {
        PluginDataObject[] pdos = null;
        Iterator<?> it = MetarToShefTransformer.iterate(pdos);
        assertNotNull(it);
        assertFalse(it.hasNext());
        assertNull(it.next());

        pdos = new PluginDataObject[0];
        it = MetarToShefTransformer.iterate(pdos);
        assertNotNull(it);
        assertFalse(it.hasNext());
        assertNull(it.next());
    }

    /**
     * Test that the transformer creates an non-empty iterator for given input.
     */
    @Test
    public void testMetarToShefInteratorB() {
        PluginDataObject p = new PluginDataObject() {

            @Override
            public IDecoderGettable getDecoderGettable() {
                return null;
            }

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.uf.common.dataplugin.PluginDataObject#getPluginName
             * ()
             */
            @Override
            public String getPluginName() {
                return "testMetarToShef";
            }
        };

        PluginDataObject[] pdos = { p, };
        Iterator<?> it = MetarToShefTransformer.iterate(pdos);
        assertNotNull(it);
        assertTrue(it.hasNext());
        assertNotNull(it.next());
    }

    @Test
    public void testFormatAsComment() {

        StringBuilder sb = new StringBuilder(
                "KOMA 251152Z 35007KT 10SM BKN035 BKN250 03/01 A2970 RMK AO2 SLP062"
                        + "\n60000 T00330011 10078 20033 53021 931012 933025 98245 4/005=");

        StringBuilder newobs = new StringBuilder();
        newobs = MetarToShefTransformer.writeObs(newobs, sb.toString());

        assertEquals(
                "\r\r\n:KOMA 251152Z 35007KT 10SM BKN035 BKN250 03/01 A2970 RMK AO2 SLP062"
                        + "\r\r\n:   60000 T00330011 10078 20033 53021 931012 933025 98245 4/005=",
                newobs.toString());

    }

}
