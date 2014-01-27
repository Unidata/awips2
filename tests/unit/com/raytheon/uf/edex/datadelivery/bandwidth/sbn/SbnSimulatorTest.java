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
package com.raytheon.uf.edex.datadelivery.bandwidth.sbn;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;

import java.io.File;
import java.io.IOException;

import junit.framework.Assert;

import org.junit.Test;

import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;
import com.raytheon.uf.edex.datadelivery.bandwidth.sbn.SbnSimulator.IFileProcessor;

/**
 * Test {@link SbnSimulator}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2013 1648       djohnson     Initial creation
 * Oct 18, 2013 2267       bgonzale     Updated tests to work with sbnSimulator reading from site specific dirs.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SbnSimulatorTest {

    private final static String SITE = "OAX";
    private final File testDir = TestUtil
            .setupTestClassDir(SbnSimulatorTest.class);

    private final IFileProcessor fileProcessor = mock(IFileProcessor.class);

    private final SbnSimulator simulator = new SbnSimulator(testDir,
            fileProcessor, SITE);

    @Test
    public void processesEachFileInDirectory() throws IOException {
        SbnSimulator simAAA = new SbnSimulator(testDir, fileProcessor, "AAA");
        SbnSimulator simBBB = new SbnSimulator(testDir, fileProcessor, "BBB");

        File fileOneExpectedResult = createTestFileGetExpected(testDir,
                "fileOne.txt", SITE);
        File fileTwoExpectedResult = createTestFileGetExpected(testDir,
                "fileTwo.txt", "BBB");

        simulator.checkForSbnData();
        simAAA.checkForSbnData();
        simBBB.checkForSbnData();

        verify(fileProcessor).processFile(fileOneExpectedResult);
        verify(fileProcessor).processFile(fileTwoExpectedResult);
    }

    @Test
    public void zeroFilesInDirectoryResultsInNoProcessing() throws IOException {
        simulator.checkForSbnData();

        verifyZeroInteractions(fileProcessor);
    }

    @Test
    public void fileInDirectoryProcessedOnlyOnce() throws IOException {
        File fileOneExpectedResult = createTestFileGetExpected(testDir,
                "fileOne.txt", SITE);

        simulator.checkForSbnData();

        verify(fileProcessor, times(1)).processFile(fileOneExpectedResult);
    }

    @Test
    public void fileInDirectoryIsDeleted() throws IOException {
        createTestFileGetExpected(testDir, "fileOne.txt", SITE);

        simulator.checkForSbnData();

        int fileCount = FileUtil.listFiles(testDir,
                FilenameFilters.ACCEPT_FILES, true).size();

        Assert.assertEquals(
                "Found unexpected files in the processing directory.", 0,
                fileCount);
    }

    @Test
    public void errorOnOneFileDoesNotStopTheOthers() throws IOException {
        createTestFileGetExpected(testDir, "fileOne.txt", "AA1");
        createTestFileGetExpected(testDir, "fileTwo.txt", "AA2");

        doThrow(new IOException()).when(fileProcessor).processFile(
                any(File.class));

        simulator.checkForSbnData();

        verify(fileProcessor, times(2)).processFile(any(File.class));
    }

    private File createTestFileGetExpected(File dir, String name, String site)
            throws IOException {
        File file = new File(dir, name);
        File fileExpectedResult = new File(dir, "sbnSimulator/" + site
                + File.separator + name);

        FileUtil.bytes2File(name.getBytes(), file);
        simulator.distributeToSiteDirs();
        return fileExpectedResult;
    }


}
