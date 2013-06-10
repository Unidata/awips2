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

import static com.raytheon.uf.common.util.Matchers.hasNoFiles;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.TestUtil;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SbnSimulatorTest {

    private final File testDir = TestUtil
            .setupTestClassDir(SbnSimulatorTest.class);

    private final IFileProcessor fileProcessor = mock(IFileProcessor.class);

    private final SbnSimulator simulator = new SbnSimulator(testDir,
            fileProcessor);

    @Test
    public void processesEachFileInDirectory() throws IOException {
        File fileOne = new File(testDir, "fileOne.txt");
        File fileTwo = new File(testDir, "fileTwo.txt");

        FileUtil.bytes2File("fileOne".getBytes(), fileOne);
        FileUtil.bytes2File("fileTwo".getBytes(), fileTwo);

        simulator.checkForSbnData();

        verify(fileProcessor).processFile(fileOne);
        verify(fileProcessor).processFile(fileTwo);
    }

    @Test
    public void zeroFilesInDirectoryResultsInNoProcessing() throws IOException {
        simulator.checkForSbnData();

        verifyZeroInteractions(fileProcessor);
    }

    @Test
    public void fileInDirectoryProcessedOnlyOnce() throws IOException {
        File fileOne = new File(testDir, "fileOne.txt");

        FileUtil.bytes2File("fileOne".getBytes(), fileOne);

        simulator.checkForSbnData();

        verify(fileProcessor, times(1)).processFile(fileOne);
    }

    @Test
    public void fileInDirectoryIsDeleted() throws IOException {
        File fileOne = new File(testDir, "fileOne.txt");

        FileUtil.bytes2File("fileOne".getBytes(), fileOne);

        simulator.checkForSbnData();

        assertThat(testDir, hasNoFiles());
    }

    @Test
    public void errorOnOneFileDoesNotStopTheOthers() throws IOException {
        FileUtil.bytes2File("fileOne".getBytes(), new File(testDir,
                "fileOne.txt"));
        FileUtil.bytes2File("fileTwo".getBytes(), new File(testDir,
                "fileTwo.txt"));

        doThrow(new IOException()).when(fileProcessor).processFile(
                any(File.class));

        simulator.checkForSbnData();

        verify(fileProcessor, times(2)).processFile(any(File.class));
    }
}
