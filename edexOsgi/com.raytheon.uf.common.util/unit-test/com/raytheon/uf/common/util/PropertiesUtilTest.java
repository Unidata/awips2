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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.junit.Test;

/**
 * Test {@link PropertiesUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2012 819        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class PropertiesUtilTest {

    private static final Class<PropertiesUtilTest> CLASS = PropertiesUtilTest.class;

    @Test
    public void testCanReadPropertiesInputStream() throws IOException {
        InputStream is = null;
        try {
            is = CLASS.getResourceAsStream(CLASS.getSimpleName()
                    + ".properties");
            Properties properties = PropertiesUtil.read(is);

            assertFalse(
                    "The properties should not be empty after reading an InputStream!",
                    properties.isEmpty());
        } finally {
            PropertiesUtil.close(is);
        }
    }

    @Test
    public void testCanReadPropertiesFile() throws IOException {
        File fileToRead = File.createTempFile(
                PropertiesUtilTest.class.getSimpleName(), null);

        InputStream is = null;
        try {
            is = CLASS.getResourceAsStream(CLASS.getSimpleName()
                    + ".properties");

            FileUtil.write(is, fileToRead);

            assertTrue("The file to read should exist!", fileToRead.isFile());
        } finally {
            PropertiesUtil.close(is);
        }

        Properties properties = PropertiesUtil.read(fileToRead);
        assertFalse("The properties should not be empty after reading a file!",
                properties.isEmpty());
    }
}
