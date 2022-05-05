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
package com.raytheon.uf.common.registry.ebxml;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Test;

/**
 * Test {@link IntegerAttribute}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 20, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class IntegerAttributeTest {

    @Test
    public void testGetQueryValueCreatesCommaSeparatedListOfValues() {
        IntegerAttribute attribute = IntegerAttribute.fromIntegers(Arrays
                .asList(1, 2));
        assertEquals("Incorrect query value string generated!", "1,2",
                attribute.getQueryValue());
    }

    @Test
    public void testGetQueryValueCreatesSingleValueString() {
        IntegerAttribute attribute = new IntegerAttribute(1);
        assertEquals("Incorrect query value string generated!", "1",
                attribute.getQueryValue());
    }
}
