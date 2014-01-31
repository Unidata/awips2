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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.event.retrieval.DataRetrievalEvent;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceTypeFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter.TranslationException;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;

/**
 * Test {@link RetrievalTask}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2013 1543       djohnson     Initial creation
 * Feb 07, 2013 1543       djohnson     Add test to simulate SBN retrieval task behavior.
 * Feb 12, 2013 1543       djohnson     Retrieval responses are now sent further down the chain.
 * Feb 15, 2013 1543       djohnson     Class renames.
 * Mar 05, 2013 1647       djohnson     Pass wmo header strategy to constructor.
 * Mar 19, 2013 1794       djohnson     RetrievalTasks integrate at a queue.
 * Apr 29, 2013 1910       djohnson     Unregister from EventBus after each test.
 * Aug 09, 2013 1822       bgonzale     Added parameters to processRetrievedPluginDataObjects.
 * Oct 01, 2013 2267       bgonzale     Pass request parameter instead of components of request.
 * Nov 04, 2013 2506       bgonzale     removed IRetrievalDao parameter.
 * Jan 15, 2014 2678       bgonzale     Added Queue.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { SpringFiles.UNIT_TEST_DB_BEANS_XML,
        SpringFiles.RETRIEVAL_DATADELIVERY_DAOS_XML })
@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)
public class RetrievalTaskTest {
    /**
     * Places the plugin data object into a collection for inspection.
     */
    public class PlaceInCollectionProcessor implements
            IRetrievalPluginDataObjectsProcessor {
        public final List<PluginDataObject> pluginDataObjects = new ArrayList<PluginDataObject>();

        /**
         * {@inheritDoc}
         * 
         * @return RetrievalRequestRecord
         */
        @Override
        public RetrievalRequestRecord processRetrievedPluginDataObjects(
                RetrievalResponseXml retrievalPluginDataObjects)
                throws SerializationException, TranslationException {
            final List<RetrievalResponseWrapper> retrievalAttributePluginDataObjects = retrievalPluginDataObjects
                    .getRetrievalAttributePluginDataObjects();
            final RetrievalRequestRecord requestRecord = dao
                    .getById(retrievalPluginDataObjects.getRequestRecord());
            final Retrieval retrieval = requestRecord.getRetrievalObj();
            final ServiceType serviceType = retrieval.getServiceType();
            final RetrievalAdapter serviceRetrievalAdapter = ServiceTypeFactory
                    .retrieveServiceRetrievalAdapter(serviceType);
            final Iterator<RetrievalAttribute> attributesIter = retrieval
                    .getAttributes().iterator();

            for (RetrievalResponseWrapper pluginDataObjectEntry : retrievalAttributePluginDataObjects) {

                if (!attributesIter.hasNext()) {
                    throw new RuntimeException(
                            "Did not find a RetrievalAttribute to match the retrieval response!");
                }

                // Restore the attribute xml prior to processing the response
                final IRetrievalResponse response = pluginDataObjectEntry
                        .getRetrievalResponse();
                response.setAttribute(attributesIter.next());

                final Map<String, PluginDataObject[]> processed = serviceRetrievalAdapter
                        .processResponse(response);
                for (PluginDataObject[] pdos : processed.values()) {
                    pluginDataObjects.addAll(Arrays.asList(pdos));
                }
            }
            return requestRecord;
        }
    }

    private RetrievalRequestRecord opsnetRetrieval;

    private RetrievalRequestRecord sbnRetrieval;

    @Autowired
    @Qualifier(value = "retrievalDao")
    private IRetrievalDao dao;

    private final ConcurrentLinkedQueue<RetrievalRequestRecordPK> retrievalQueue = new ConcurrentLinkedQueue<RetrievalRequestRecordPK>();

    private final PlaceInCollectionProcessor retrievedDataProcessor = new PlaceInCollectionProcessor();

    private final List<DataRetrievalEvent> eventsReceived = new ArrayList<DataRetrievalEvent>();

    @Before
    public void setUp() throws RegistryHandlerException {
        PathManagerFactoryTest.initLocalization();

        opsnetRetrieval = RetrievalRequestRecordFixture.INSTANCE.get(1);
        sbnRetrieval = RetrievalRequestRecordFixture.INSTANCE.get(2);
        opsnetRetrieval.setNetwork(Network.OPSNET);
        sbnRetrieval.setNetwork(Network.SBN);

        EventBus.register(this);
    }

    @After
    public void tearDown() {
        EventBus.unregister(this);
    }

    @Test
    public void processesRetrievalForItsSpecifiedNetwork()
            throws DataAccessLayerException {

        stageRetrievals();

        runRetrievalTask();

        verifyCorrectStateForRetrieval(opsnetRetrieval, State.COMPLETED);
    }

    @Test
    public void storesPluginDataObjectsForItsSpecifiedNetwork()
            throws DataAccessLayerException, SerializationException {

        stageRetrievals();

        runRetrievalTask();

        assertThat(retrievedDataProcessor.pluginDataObjects,
                hasSize(opsnetRetrieval.getRetrievalObj().getAttributes()
                        .size()));
    }

    @Ignore("dataRetrievalEvent is no longer sent separately from storage, perhaps restore it later?")
    public void dataRetrievalEventIsSentForItsSpecifiedNetwork()
            throws Exception {

        stageRetrievals();

        runRetrievalTask();

        final int numberOfRetrievalAttributes = opsnetRetrieval
                .getRetrievalObj().getAttributes().size();
        assertThat(eventsReceived, hasSize(numberOfRetrievalAttributes));
        // TODO: Is there a way to distinguish between the events sent by the
        // separate retrieval attributes, e.g. to make sure each attribute sent
        // an event and not one attribute sent two?
    }

    // TODO: Add tests for one retrieval failing and another succeeding, make
    // sure correct events are sent and correct number of plugin data objects
    // generated

    @Test
    public void doesNotProcessRetrievalForAnotherNetwork()
            throws DataAccessLayerException {

        stageRetrievals();

        runRetrievalTask();

        verifyCorrectStateForRetrieval(sbnRetrieval, State.PENDING);
    }

    @Test
    public void retrievalTaskCanStoreDataToDirectoryThatAnotherTaskProcesses()
            throws Exception {
        dao.create(RetrievalRequestRecordFixture.INSTANCE.get());

        IRetrievalsFinder retrievalDataFinder = new PerformRetrievalsThenReturnFinder(
                retrievalQueue, dao);

        final ConcurrentLinkedQueue<String> retrievalQueue = new ConcurrentLinkedQueue<String>();
        final File testDirectory = TestUtil
                .setupTestClassDir(RetrievalTaskTest.class);
        IRetrievalPluginDataObjectsProcessor serializeToDirectory = new SerializeRetrievedDataToDirectory(
                testDirectory, new AlwaysSameWmoHeader("SMYG10 LYBM 280000"),
                dao);

        RetrievalTask downloadTask = new RetrievalTask(Network.OPSNET,
                retrievalDataFinder, serializeToDirectory,
                mock(IRetrievalResponseCompleter.class));
        RetrievalTask readDownloadsTask = new RetrievalTask(Network.OPSNET,
                new DeserializeRetrievedDataFromIngest(retrievalQueue),
                retrievedDataProcessor, new RetrievalResponseCompleter(
                        mock(SubscriptionNotifyTask.class), dao));

        downloadTask.run();

        final List<RetrievalRequestRecord> all = dao.getAll();
        for (RetrievalRequestRecord request : all) {
            assertThat(request.getState(), is(State.RUNNING));
        }

        for (File file : FileUtil.listFiles(testDirectory,
                FilenameFilters.ACCEPT_FILES, false)) {
            retrievalQueue.add(FileUtil.file2String(file));
        }

        readDownloadsTask.run();

        final List<RetrievalRequestRecord> allRetrievals = dao.getAll();
        assertThat(allRetrievals, hasSize(1));
        assertThat(retrievedDataProcessor.pluginDataObjects, hasSize(2));

        for (RetrievalRequestRecord request : allRetrievals) {
            assertThat(request.getState(), is(State.COMPLETED));
        }

    }

    /**
     * Stage the retrievals in the database.
     */
    @Transactional
    private void stageRetrievals() {

        dao.create(opsnetRetrieval);
        retrievalQueue.add(opsnetRetrieval.getId());
        dao.create(sbnRetrieval);
        retrievalQueue.add(sbnRetrieval.getId());
    }

    /**
     * Run the actual retrieval task.
     */
    private void runRetrievalTask() {
        // Create required strategies for finding, processing, and completing
        // retrievals
        final IRetrievalsFinder retrievalDataFinder = new PerformRetrievalsThenReturnFinder(
                retrievalQueue, dao);
        final IRetrievalResponseCompleter retrievalCompleter = new RetrievalResponseCompleter(
                mock(SubscriptionNotifyTask.class), dao);

        new RetrievalTask(Network.OPSNET, retrievalDataFinder,
                retrievedDataProcessor, retrievalCompleter).run();
    }

    /**
     * Verify the retrieval record is in the expected state.
     * 
     * @param retrieval
     *            the retrieval
     * @param state
     *            the expected state
     * @throws DataAccessLayerException
     */
    private void verifyCorrectStateForRetrieval(
            RetrievalRequestRecord retrieval, State state)
            throws DataAccessLayerException {
        RetrievalRequestRecord recordInDb = dao
                .getRequests(retrieval.getId().getSubscriptionName())
                .iterator().next();

        assertThat(recordInDb.getState(), is(equalTo(state)));
    }

    /**
     * This method will be invoked by the EventBus when a data retrieval event
     * is sent.
     * 
     * @param event
     *            the event
     */
    @Subscribe
    public void receivedDataDeliveryEvent(DataRetrievalEvent event) {
        eventsReceived.add(event);
    }
}
