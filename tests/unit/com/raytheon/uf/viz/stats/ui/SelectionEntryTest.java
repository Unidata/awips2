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
package com.raytheon.uf.viz.stats.ui;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * SelectionEntry test class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2013            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SelectionEntryTest {

    @Test
    public void testCreateSelectionEntryClassAndAddKeys() {
        SelectionEntry se = new SelectionEntry();
        se.addPair("Key", "Value");
        se.addPair("Key2", "Value2");
        se.addPair("Key3", "Value3");
        assertTrue("Keys are wrong", se.toString()
                .equals("Value:Value2:Value3"));
    }
}
