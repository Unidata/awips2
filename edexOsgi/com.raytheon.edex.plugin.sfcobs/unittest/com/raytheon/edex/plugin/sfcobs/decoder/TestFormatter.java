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
package com.raytheon.edex.plugin.sfcobs.decoder;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.TestCase;

public class TestFormatter extends TestCase {

    
    public void testLongPattern() {
        
        String pattern = "[1357]((0\\d{3})|(1(([0-7]\\d{2})|(800))))";
        Pattern p = Pattern.compile(pattern);
        
        Matcher m = p.matcher("10000");
        assertTrue(m.matches());

        m = p.matcher("31800");
        assertTrue(m.matches());

        m = p.matcher("50899");
        assertTrue(m.matches());

        m = p.matcher("70900");
        assertTrue(m.matches());

        m = p.matcher("70999");
        assertTrue(m.matches());

        m = p.matcher("71000");
        assertTrue(m.matches());

        m = p.matcher("71799");
        assertTrue(m.matches());

        int windWavePeriod_hi = 115;
        
        windWavePeriod_hi = (int) Math.round(windWavePeriod_hi / 10.0);
        
        assertEquals(12,windWavePeriod_hi);

    }
}
