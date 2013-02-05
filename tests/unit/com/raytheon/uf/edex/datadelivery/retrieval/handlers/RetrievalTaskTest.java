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
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.event.retrieval.DataRetrievalEvent;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.DatabaseUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;

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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RetrievalTaskTest {
    /**
     * Places the plugin data object into a collection for inspection.
     */
    public static class PlaceInCollectionProcessor implements
            IRetrievalPluginDataObjectsProcessor {
        public final List<PluginDataObject> pluginDataObjects = new ArrayList<PluginDataObject>();

        /**
         * {@inheritDoc}
         */
        @Override
        public void processRetrievedPluginDataObjects(
                RetrievalPluginDataObjects retrievalPluginDataObjects)
                throws Exception {
            final List<RetrievalAttributePluginDataObjects> retrievalAttributePluginDataObjects = retrievalPluginDataObjects
                    .getRetrievalAttributePluginDataObjects();

            for (RetrievalAttributePluginDataObjects pluginDataObjectEntry : retrievalAttributePluginDataObjects) {
                PluginDataObject[] value = pluginDataObjectEntry
                        .getPluginDataObjects();
                pluginDataObjects.addAll(Arrays.asList(value));
            }
        }
    }

    private final RetrievalRequestRecord opsnetRetrieval = RetrievalRequestRecordFixture.INSTANCE
            .get(1);

    private final RetrievalRequestRecord sbnRetrieval = RetrievalRequestRecordFixture.INSTANCE
            .get(2);

    private RetrievalDao dao;

    private final PlaceInCollectionProcessor retrievedDataProcessor = new PlaceInCollectionProcessor();

    private final List<DataRetrievalEvent> eventsReceived = new ArrayList<DataRetrievalEvent>();

    @Before
    public void setUp() throws RegistryHandlerException {
        DatabaseUtil.start();
        PathManagerFactoryTest.initLocalization();

        opsnetRetrieval.setNetwork(Network.OPSNET);
        sbnRetrieval.setNetwork(Network.SBN);

        dao = new RetrievalDao();

        EventBus.register(this);
    }

    @After
    public void tearDown() {
        DatabaseUtil.shutdown();
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

        assertThat(retrievedDataProcessor.pluginDataObjects.size(),
                is(equalTo(opsnetRetrieval.getRetrievalObj().getAttribute()
                        .size())));
    }

    @Test
    public void dataRetrievalEventIsSentForItsSpecifiedNetwork()
            throws Exception {

        stageRetrievals();

        runRetrievalTask();

        assertThat(eventsReceived.size(), is(equalTo(opsnetRetrieval
                .getRetrievalObj().getAttribute().size())));
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

    /**
     * Stage the retrievals in the database.
     */
    private void stageRetrievals() {
        dao.create(opsnetRetrieval);
        dao.create(sbnRetrieval);
    }

    /**
     * Run the actual retrieval task.
     */
    private void runRetrievalTask() {
        // Create required strategies for finding, processing, and completing
        // retrievals
        final IRetrievalPluginDataObjectsFinder retrievalDataFinder = new PerformRetrievalPluginDataObjectsFinder(
                Network.OPSNET);
        final IRetrievalPluginDataObjectsProcessor retrievalPluginDataObjectsProcessor = new NotifyOfPluginDataObjectsDecorator(
                retrievedDataProcessor);
        final IRetrievalResponseCompleter retrievalCompleter = new RetrievalResponseCompleter(
                mock(SubscriptionNotifyTask.class));

        new RetrievalTask(Network.OPSNET, retrievalDataFinder,
                retrievalPluginDataObjectsProcessor, retrievalCompleter).run();
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
