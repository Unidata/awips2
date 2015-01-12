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
package com.raytheon.uf.common.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * Test {@link StringUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2012 740        djohnson     Initial creation
 * Aug 20, 2012 0743       djohnson     Disambiguate method call.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class StringUtilTest {

    @Test
    public void testJoinedStringsUseJoinCharacter()
    {
        assertEquals("1,2,3",
                StringUtil.join(new String[] { "1", "2", "3" }, ','));
    }
    
    @Test
    public void testJoinReturnsNullForNullArrayOfStrings() {
        assertNull(StringUtil.join((Object[]) null, ','));
    }

    @Test
    public void testJoinReturnsNullForZeroLengthArray() {
        assertNull(StringUtil.join(new String[0], ','));
    }

}
