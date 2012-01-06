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
package com.raytheon.edex.util;

import junit.framework.Assert;

import org.junit.Test;

import com.raytheon.uf.common.util.FileUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class FileUtilTest {
    private static final String[] unmangled = { "Hour 0-240", "Yes/No",
            "N. America" };

    private static final String[] mangled = { "Hour_CA0_CN240", "Yes_CPNo",
            "N_CO_CAAmerica" };

    /**
     * Test method for
     * {@link com.raytheon.uf.common.util.FileUtil#mangle(java.lang.String)}.
     */
    @Test
    public void testMangle() {
        for (int i = 0; i < unmangled.length; i++) {
            String result = FileUtil.mangle(unmangled[i]);
            Assert.assertEquals(mangled[i], result);
        }
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.util.FileUtil#unmangle(java.lang.String)}.
     */
    @Test
    public void testUnmangle() {
        for (int i = 0; i < mangled.length; i++) {
            String result = FileUtil.unmangle(mangled[i]);
            Assert.assertEquals(unmangled[i], result);
        }
    }

}
