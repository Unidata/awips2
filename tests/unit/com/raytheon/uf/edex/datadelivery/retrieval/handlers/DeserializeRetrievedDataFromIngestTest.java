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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Matchers;

import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;

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
 * Aug 09, 2013 1822       bgonzale     Added parameters to processRetrievedPluginDataObjects.
 * Oct 01, 2013 2267       bgonzale     Pass request parameter instead of components of request.
 * Nov 04, 2013 2506       bgonzale     Fixed IRetreivalDao mock initialization.
 *                                      Test deserialization of data with leading and trailing 
 *                                      content on the xml.
 * Jan 30, 2014 2686       dhladky      refactor of retrieval.                                     
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DeserializeRetrievedDataFromIngestTest {
    private final File directory = TestUtil
            .setupTestClassDir(DeserializeRetrievedDataFromIngestTest.class);

    private final DeserializeRetrievedDataFromIngest service = new DeserializeRetrievedDataFromIngest();

    private final IRetrievalDao mockDao = mock(IRetrievalDao.class);

    @BeforeClass
    public static void classSetUp() {
        PathManagerFactoryTest.initLocalization();
    }

    @Before
    public void setup() {
        when(mockDao.getById((RetrievalRequestRecordPK) Matchers.anyObject()))
                .thenReturn(
                        RetrievalRequestRecordFixture.INSTANCE.getInstance(0,
                                new Random(0)));
    }

    @Test
    public void deserializesRetrievedDataFromTheQueue()
            throws Exception {

        addRetrievalToQueue();

        final RetrievalResponseXml restored = service.processRequest(null);

        // Just make sure the payload is present
        assertThat(restored.getRetrievalAttributePluginDataObjects().get(0)
                .getRetrievalResponse(), is(notNullValue()));
    }


    @Test
    public void removesFromQueueWhileRetrieving()
            throws Exception {

        addRetrievalToQueue();

        service.processRequest(null);

        //assertThat(retrievalQueue, is(empty()));
    }

    @Test
    public void returnsNullWhenNothingInTheQueue() throws Exception {

        final RetrievalResponseXml restored = service.processRequest(null);

        assertNull(restored);
    }

    @Test
    public void attemptIngestWhenDataHasLeadingAndTrailingContent()
            throws Exception {

        addSimulatedSBNRetrievalToQueue();

        final RetrievalResponseXml restored = service.processRequest(null);

        // check for the payload
        assertThat(restored.getRetrievalAttributePluginDataObjects().get(0)
                .getRetrievalResponse(), is(notNullValue()));
    }

    private void addRetrievalToQueue() throws SerializationException,
            IOException {
        RetrievalResponseXml retrievalPluginDataObjects = RetrievalPluginDataObjectsFixture.INSTANCE
                .get();
        RetrievalRequestRecord request = new RetrievalRequestRecord();
        request.setProvider("");
        request.setPlugin("");
        request.setInsertTime(new Date());

        new SerializeRetrievedDataToDirectory(directory,
                new AlwaysSameWmoHeader("SMYG10 LYBM 280000"), mockDao)
                .processRetrievedPluginDataObjects(retrievalPluginDataObjects);

        final List<File> files = FileUtil.listFiles(directory,
                FilenameFilters.ACCEPT_FILES, false);
        //retrievalQueue.add(FileUtil.file2String(files.get(0)));
    }

    private void addSimulatedSBNRetrievalToQueue()
            throws SerializationException,
            IOException {
        RetrievalResponseXml retrievalPluginDataObjects = RetrievalPluginDataObjectsFixture.INSTANCE
                .get();
        RetrievalRequestRecord request = new RetrievalRequestRecord();
        request.setProvider("");
        request.setPlugin("");
        request.setInsertTime(new Date());

        new SerializeRetrievedDataToDirectory(directory,
                new WmoHeaderWithLeadingAndTrailingContent("SMYG10 LYBM 280000"),
                mockDao)
                .processRetrievedPluginDataObjects(retrievalPluginDataObjects);

        final List<File> files = FileUtil.listFiles(directory,
                FilenameFilters.ACCEPT_FILES, false);
        //retrievalQueue.add(FileUtil.file2String(files.get(0)));
    }

    private static class WmoHeaderWithLeadingAndTrailingContent extends
            AlwaysSameWmoHeader {

        public WmoHeaderWithLeadingAndTrailingContent(String wmoHeader) {
            super(wmoHeader);
        }

        @Override
        public String applyWmoHeader(String dataProvider, String dataFormat,
                String sourceType, Date date, String data) {
            String SBN_noise_prefix = "001\r\r\n747\r\r\n";
            String SBN_noise_suffix = "\n\r\r\n 003";
            String output = super.applyWmoHeader(dataProvider, dataFormat,
                    sourceType,
                    date, data);
            return SBN_noise_prefix + output + SBN_noise_suffix;
        }

    }
}
