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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;

/**
 * Test {@link DeserializeRetrievedDataFromIngest}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Feb 12, 2013 1543       djohnson     Can only test the retrieval response is now not null.
 * Feb 15, 2013 1543       djohnson     Some renames.
 * Mar 05, 2013 1647       djohnson     Pass wmo header strategy to constructor.
 * Mar 19, 2013 1794       djohnson     Read from a queue rather than the file system.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DeserializeRetrievedDataFromIngestTest {
    private final File directory = TestUtil
            .setupTestClassDir(DeserializeRetrievedDataFromIngestTest.class);

    private final ConcurrentLinkedQueue<String> retrievalQueue = new ConcurrentLinkedQueue<String>();

    private final DeserializeRetrievedDataFromIngest service = new DeserializeRetrievedDataFromIngest(
            retrievalQueue);

    @BeforeClass
    public static void classSetUp() {
        PathManagerFactoryTest.initLocalization();
    }

    @Test
    public void deserializesRetrievedDataFromTheQueue()
            throws Exception {

        addRetrievalToQueue();

        final RetrievalResponseXml restored = service.findRetrievals();

        // Just make sure the payload is present
        assertThat(restored.getRetrievalAttributePluginDataObjects().get(0)
                .getRetrievalResponse(), is(notNullValue()));
    }


    @Test
    public void removesFromQueueWhileRetrieving()
            throws Exception {

        addRetrievalToQueue();

        service.findRetrievals();

        assertThat(retrievalQueue, is(empty()));
    }

    @Test
    public void returnsNullWhenNothingInTheQueue() throws Exception {

        final RetrievalResponseXml restored = service.findRetrievals();

        assertNull(restored);
    }

    private void addRetrievalToQueue() throws SerializationException,
            IOException {
        RetrievalResponseXml retrievalPluginDataObjects = RetrievalPluginDataObjectsFixture.INSTANCE
                .get();

        new SerializeRetrievedDataToDirectory(directory,
                new AlwaysSameWmoHeader("SMYG10 LYBM 280000"))
                .processRetrievedPluginDataObjects(retrievalPluginDataObjects);

        final List<File> files = FileUtil.listFiles(directory,
                FilenameFilters.ACCEPT_FILES, false);
        retrievalQueue.add(FileUtil.file2String(files.get(0)));
    }
}
