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
package com.raytheon.uf.common.util.file;

import static com.raytheon.uf.common.util.file.FilenameFilters.reverse;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

import org.junit.Test;

import com.raytheon.uf.common.util.TestUtil;

/**
 * Test {@link FilenameFilters}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2013 1794       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class FilenameFiltersTest {

    private static final File UNUSED_DIR = new File(".");

    @Test
    public void acceptAllAlwaysReturnsTrue() {
        assertTrue(FilenameFilters.ACCEPT_ALL.accept(null, null));
    }

    @Test
    public void acceptNoneAlwaysReturnsFalse() {
        assertFalse(FilenameFilters.ACCEPT_NONE.accept(null, null));
    }

    @Test
    public void reverseWillReturnOppositeOfFilter() {
        assertEquals(!FilenameFilters.ACCEPT_ALL.accept(null, null),
                reverse(FilenameFilters.ACCEPT_ALL).accept(null, null));
        assertEquals(!FilenameFilters.ACCEPT_NONE.accept(null, null),
                reverse(FilenameFilters.ACCEPT_NONE).accept(null, null));
    }

    @Test
    public void byFileExtensionAcceptsFileWithExtension() {
        assertTrue(FilenameFilters.byFileExtension(".xml").accept(UNUSED_DIR,
                "blah.xml"));
    }

    @Test
    public void byFileExtensionDoesNotAcceptFileWithoutExtension() {
        assertFalse(FilenameFilters.byFileExtension(".xml").accept(UNUSED_DIR,
                "blah.txt"));
    }

    @Test
    public void noLinuxHiddenFilesAcceptsNonHidden() {
        assertTrue(FilenameFilters.NO_LINUX_HIDDEN_FILES.accept(UNUSED_DIR,
                "nothidden"));
    }

    @Test
    public void noLinuxHiddenFilesDoesNotAcceptHidden() {
        assertFalse(FilenameFilters.NO_LINUX_HIDDEN_FILES.accept(UNUSED_DIR,
                ".hidden"));
    }

    @Test
    public void byFiltersReturnsTrueIfAllFiltersAccept() {
        FilenameFilter sumFilter = FilenameFilters.byFilters(
                FilenameFilters.NO_LINUX_HIDDEN_FILES,
                FilenameFilters.ACCEPT_ALL);
        assertTrue(sumFilter.accept(UNUSED_DIR, "nothidden"));
    }

    @Test
    public void byFiltersReturnsFalseIfAnyFilterDoesNotAccept() {
        FilenameFilter sumFilter = FilenameFilters.byFilters(
                FilenameFilters.NO_LINUX_HIDDEN_FILES,
                FilenameFilters.ACCEPT_ALL);
        assertFalse(sumFilter.accept(UNUSED_DIR, ".hidden"));
    }

    @Test
    public void acceptFilesReturnsTrueForFile() throws IOException {
        File testDir = TestUtil.setupTestClassDir(FilenameFiltersTest.class);
        File file = new File(testDir, "realFile.txt");
        file.createNewFile();

        assertTrue(FilenameFilters.ACCEPT_FILES.accept(file.getParentFile(),
                file.getName()));
    }

    @Test
    public void acceptFilesReturnsFalseForDirectory() throws IOException {
        File testDir = TestUtil.setupTestClassDir(FilenameFiltersTest.class);
        File file = new File(testDir, "directory");
        file.mkdirs();

        assertFalse(FilenameFilters.ACCEPT_FILES.accept(file.getParentFile(),
                file.getName()));
    }

    @Test
    public void acceptDirectoriesReturnsTrueForDirectory() throws IOException {
        File testDir = TestUtil.setupTestClassDir(FilenameFiltersTest.class);
        File file = new File(testDir, "directory");
        file.mkdirs();

        assertTrue(FilenameFilters.ACCEPT_DIRECTORIES.accept(
                file.getParentFile(), file.getName()));
    }

    @Test
    public void acceptDirectoriesReturnsFalseForFile() throws IOException {
        File testDir = TestUtil.setupTestClassDir(FilenameFiltersTest.class);

        File file = new File(testDir, "realFile.txt");
        file.createNewFile();

        assertFalse(FilenameFilters.ACCEPT_DIRECTORIES.accept(
                file.getParentFile(), file.getName()));
    }
}
