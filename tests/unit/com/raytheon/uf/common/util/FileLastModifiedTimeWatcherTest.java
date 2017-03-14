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

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import java.io.File;
import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

/**
 * Test {@link FileLastModifiedTimeWatcher}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 08, 2013 1645       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class FileLastModifiedTimeWatcherTest {

    private final File testDir = TestUtil
            .setupTestClassDir(FileLastModifiedTimeWatcherTest.class);

    private final File file = new File(testDir, "test.txt");

    private FileLastModifiedTimeWatcher watcher;

    @Before
    public void setUp() throws IOException {
        file.createNewFile();
        watcher = new FileLastModifiedTimeWatcher(file);
    }

    @Test
    public void newerLastModifiedWillReturnTrueForModified() throws IOException {
        increaseFileLastModifiedTime();

        assertThat(watcher.hasBeenModified(), is(true));
    }

    private boolean increaseFileLastModifiedTime() {
        return file.setLastModified(file.lastModified() + 5000L);
    }

    @Test
    public void sameLastModifiedWillReturnFalseForModified() throws IOException {
        assertThat(watcher.hasBeenModified(), is(false));
    }

    @Test
    public void newerLastModifiedTwiceWillReturnTrueForModified()
            throws IOException {
        increaseFileLastModifiedTime();

        watcher.hasBeenModified();

        increaseFileLastModifiedTime();

        assertThat(watcher.hasBeenModified(), is(true));
    }

    @Test
    public void sameLastModifiedAfterNewerWillReturnFalseForModified()
            throws IOException {
        increaseFileLastModifiedTime();

        watcher.hasBeenModified();

        assertThat(watcher.hasBeenModified(), is(false));
    }

}
