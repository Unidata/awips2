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
package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Test {@link BandwidthUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2012 0726       djohnson     Initial creation
 * Nov 09, 2012 1286       djohnson     Add conversion test.
 * Dec 11, 2012 1403       djohnson     No longer valid to run without bandwidth management.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BandwidthUtilTest {

    @Test
    public void testConvertingKilobytesPerSecondToBytesPerSpecifiedMinutes() {
        // 768 (KB / s) = 141 557 760 bytes per (3 minutes)
        assertEquals("Incorrect conversion from KB/s to bytes!", 141557760,
                BandwidthUtil
                        .convertKilobytesPerSecondToBytesPerSpecifiedMinutes(
                                768, 3));
    }
}
