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

package com.raytheon.edex.services;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;

import org.junit.Test;

import com.raytheon.edex.exception.EdexException;
import com.raytheon.edex.msg.utility.GetUtilityResponse;
import com.raytheon.edex.msg.utility.ListResponseEntry;
import com.raytheon.edex.msg.utility.ListUtilityResponse;
import com.raytheon.edex.util.Checksum;
import com.raytheon.edex.util.FileUtil;
import com.raytheon.edex.utility.LocalizationContext;

/**
 * 
 * Test for Utility Manager
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2007            chammack    Initial Creation.	
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class TestUtilityManager {

    private class ListResponseEntryComparator implements
            Comparator<ListResponseEntry> {

        @Override
        public int compare(ListResponseEntry o1, ListResponseEntry o2) {
            return o1.getFileName().compareTo(o2.getFileName());
        }

    }

    /**
     * Test method for
     * {@link com.raytheon.edex.services.UtilityManager#listFiles(java.lang.String, java.lang.String)}.
     */
    @Test
    public void testListFiles() {

        // Bogus directory test

        ListUtilityResponse response = UtilityManager.listFiles("/bar", null,
                null, true, true);
        assertNotNull(response.getErrorText());

        response = UtilityManager.listFiles("/tmp", null, null, true, true);
        assertNotNull(response.getErrorText());

        // ---------------- Set up testing facade
        FileUtil.deleteDir(new File("/tmp/utilityMgrTest"));

        File f = new File("/tmp/utilityMgrTest/cave_config/site/OAX");
        f.mkdirs();

        File file1 = new File(
                "/tmp/utilityMgrTest/cave_config/site/OAX/abc123.xml");
        Date date1 = null;
        String checksum1 = null;

        writeFile(file1, "a b c d e f g h i j k l m n o p");

        try {
            date1 = new Date(file1.lastModified());
            checksum1 = Checksum.getMD5Checksum(file1);
            writeFile(new File(
                    "/tmp/utilityMgrTest/cave_config/site/OAX/abc123.xml.md5"),
                    checksum1);
        } catch (EdexException e) {
            e.printStackTrace();
            fail("Internal exception");
        }

        File directory = new File(
                "/tmp/utilityMgrTest/cave_config/site/OAX/z/1/2");
        directory.mkdirs();

        File file2 = new File(
                "/tmp/utilityMgrTest/cave_config/site/OAX/z/1/2/config.xml");
        Date date2 = null;
        String checksum2 = null;

        writeFile(file2, "1234567891011121314151617181920");
        try {
            date2 = new Date(file2.lastModified());
            checksum2 = Checksum.getMD5Checksum(file2);
            writeFile(
                    new File(
                            "/tmp/utilityMgrTest/cave_config/site/OAX/z/1/2/config.xml.md5"),
                    checksum2);
        } catch (EdexException e) {
            e.printStackTrace();
            fail("Internal exception");
        }

        // ---------------- End set up testing facade

        LocalizationContext context = new LocalizationContext(
                "cave_config.site.OAX");

        // test recursive, files only
        ListUtilityResponse resp = UtilityManager.listFiles(
                "/tmp/utilityMgrTest", context, null, true, true);
        ListResponseEntry[] entries = resp.getEntries();

        assertEquals(2, entries.length);

        Arrays.sort(entries, new ListResponseEntryComparator());

        assertEquals("./abc123.xml", entries[0].getFileName());
        assertEquals(date1, entries[0].getDate());
        assertEquals(false, entries[0].isDirectory());
        assertEquals(checksum1, entries[0].getChecksum());

        assertEquals("./z/1/2/config.xml", entries[1].getFileName());
        assertEquals(date2, entries[1].getDate());
        assertEquals(false, entries[1].isDirectory());
        assertEquals(checksum2, entries[1].getChecksum());

        // test recursive, files only, with subDirectory
        resp = UtilityManager.listFiles("/tmp/utilityMgrTest", context, "z/1",
                true, true);
        entries = resp.getEntries();

        assertEquals(1, entries.length);

        assertEquals("z/1/2/config.xml", entries[0].getFileName());
        assertEquals(date2, entries[0].getDate());
        assertEquals(false, entries[0].isDirectory());
        assertEquals(checksum2, entries[0].getChecksum());

        // test non-recursive, files only
        resp = UtilityManager.listFiles("/tmp/utilityMgrTest", context, null,
                false, true);
        entries = resp.getEntries();

        assertEquals(1, entries.length);

        assertEquals("./abc123.xml", entries[0].getFileName());
        assertEquals(date1, entries[0].getDate());
        assertEquals(false, entries[0].isDirectory());
        assertEquals(checksum1, entries[0].getChecksum());

        // test recursive, files only, with subDirectory
        resp = UtilityManager.listFiles("/tmp/utilityMgrTest", context, "z/1",
                true, true);
        entries = resp.getEntries();

        assertEquals(1, entries.length);

        assertEquals("z/1/2/config.xml", entries[0].getFileName());
        assertEquals(date2, entries[0].getDate());
        assertEquals(false, entries[0].isDirectory());
        assertEquals(checksum2, entries[0].getChecksum());

        // test recursive, directories included, with subDirectory
        resp = UtilityManager.listFiles("/tmp/utilityMgrTest", context, "z/1",
                true, false);
        entries = resp.getEntries();

        assertEquals(3, entries.length);

        Arrays.sort(entries, new ListResponseEntryComparator());

        assertEquals("z/1", entries[0].getFileName());
        assertEquals(date2, entries[0].getDate());
        assertEquals(true, entries[0].isDirectory());
        assertEquals(null, entries[0].getChecksum());

        assertEquals("z/1/2", entries[1].getFileName());
        assertEquals(date2, entries[1].getDate());
        assertEquals(true, entries[1].isDirectory());
        assertEquals(null, entries[1].getChecksum());

        assertEquals("z/1/2/config.xml", entries[2].getFileName());
        assertEquals(date2, entries[2].getDate());
        assertEquals(false, entries[2].isDirectory());
        assertEquals(checksum2, entries[2].getChecksum());

        // test non-recursive, files only
        resp = UtilityManager.listFiles("/tmp/utilityMgrTest", context, null,
                false, true);
        entries = resp.getEntries();

        assertEquals(1, entries.length);

        assertEquals("./abc123.xml", entries[0].getFileName());
        assertEquals(date1, entries[0].getDate());
        assertEquals(false, entries[0].isDirectory());
        assertEquals(checksum1, entries[0].getChecksum());

        // test non-recursive, including directories
        resp = UtilityManager.listFiles("/tmp/utilityMgrTest", context, null,
                false, false);
        entries = resp.getEntries();

        assertEquals(3, entries.length);

        Arrays.sort(entries, new ListResponseEntryComparator());

        assertEquals(".", entries[0].getFileName());
        assertEquals(date1, entries[0].getDate());
        assertEquals(true, entries[0].isDirectory());
        assertEquals(null, entries[0].getChecksum());

        assertEquals("./abc123.xml", entries[1].getFileName());
        assertEquals(date1, entries[1].getDate());
        assertEquals(false, entries[1].isDirectory());
        assertEquals(checksum1, entries[1].getChecksum());

        assertEquals("./z", entries[2].getFileName());
        assertEquals(date1, entries[2].getDate());
        assertEquals(true, entries[2].isDirectory());
        assertEquals(null, entries[2].getChecksum());

        // test with file
        resp = UtilityManager.listFiles("/tmp/utilityMgrTest", context,
                "abc123.xml", false, false);
        entries = resp.getEntries();

        assertEquals(1, entries.length);

        assertEquals("abc123.xml", entries[0].getFileName());
        assertEquals(date1, entries[0].getDate());
        assertEquals(false, entries[0].isDirectory());
        assertEquals(checksum1, entries[0].getChecksum());

        // clean up
        FileUtil.deleteDir(new File("/tmp/utilityMgrTest"));

    }

    /**
     * Test method for
     * {@link com.raytheon.edex.services.UtilityManager#getFile(String, String, String)}
     */
    @Test
    public void testGetFile() {
        File f = new File("/tmp/utilityMgrTest/cave_config/site/OAX");
        f.mkdirs();

        File file1 = new File(
                "/tmp/utilityMgrTest/cave_config/site/OAX/abc123.xml");
        String checksum1 = null;
        String data1 = "a b c d e f g h i j k l m n o p";

        writeFile(file1, data1);

        try {
            checksum1 = Checksum.getMD5Checksum(file1);
            writeFile(new File(
                    "/tmp/utilityMgrTest/cave_config/site/OAX/abc123.xml.md5"),
                    checksum1);
        } catch (EdexException e) {
            e.printStackTrace();
            fail("Internal exception");
        }

        LocalizationContext context = new LocalizationContext(
                "cave_config.site.OAX");
        GetUtilityResponse resp = UtilityManager.getFile("/tmp/utilityMgrTest",
                context, "abc123.xml");
        assertEquals("abc123.xml", resp.getPathName());
        assertEquals(checksum1, resp.getChecksum());
        assertEquals(data1.getBytes().length, resp.getData().length);
        assertEquals(context, resp.getContext());

        FileUtil.deleteDir(new File("/tmp/utilityMgrTest"));
    }

    /**
     * Test method for
     * {@link com.raytheon.edex.services.UtilityManager#putFile(String, String, String)}
     */
    public void testPutFile() {

    }

    private void writeFile(File file, String msg) {
        try {
            PrintWriter pw = new PrintWriter(file);
            pw.write(msg);
            pw.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            fail("Internal exception");
        }
    }

}
