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
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;

/**
 * Test {@link PerformRetrievalsThenReturnFinder}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2013 1543       djohnson     Initial creation
 * Jan 15, 2014 2678       bgonzale     Added Queue.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class PerformRetrievalPluginDataObjectsFinderTest {

    private static final String EXCEPTION_MESSAGE = "thrown on purpose";

    private final ConcurrentLinkedQueue<RetrievalRequestRecordPK> retrievalQueue = new ConcurrentLinkedQueue<RetrievalRequestRecordPK>();

    private static final IRetrievalDao MOCK_DAO = mock(IRetrievalDao.class);

    private final Retrieval retrievalThatThrowsException = new Retrieval() {
        private static final long serialVersionUID = 1109443017002028345L;

        @Override
        public ArrayList<RetrievalAttribute> getAttributes() {
            throw new IllegalStateException(EXCEPTION_MESSAGE);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public ServiceType getServiceType() {
            return ServiceType.OPENDAP;
        }
    };

    private RetrievalRequestRecord retrievalThatDoesNotThrowException;

    @Before
    public void setUp() {
        PathManagerFactoryTest.initLocalization();

        retrievalThatDoesNotThrowException = RetrievalRequestRecordFixture.INSTANCE
                .get();
    }

    @Test
    public void requestRecordSetToFailedStatusWhenExceptionThrown()
            throws SerializationException {

        RetrievalRequestRecord record = new RetrievalRequestRecord();
        try {
            record.setRetrievalObj(retrievalThatThrowsException);
        } catch (NullPointerException npe) {
            // This is expected because we create an anonymous
            // retrievalThatThrowsException
            // instance, and can't dynamically serialize it
        }

        processRetrieval(record);

        assertThat(record.getState(), is(equalTo(State.FAILED)));
    }

    @Test
    public void requestRecordSetToCompletedStatusWhenNoExceptionThrown()
            throws SerializationException {

        processRetrieval(retrievalThatDoesNotThrowException);

        assertThat(retrievalThatDoesNotThrowException.getState(),
                is(equalTo(State.COMPLETED)));
    }

    @Test
    public void requestRecordSetToFailedStatusWhenNoPayloadReturned() {
        IRetrievalResponse retrievalResponse = mock(IRetrievalResponse.class);
        when(retrievalResponse.getPayLoad()).thenReturn(null);

        PerformRetrievalsThenReturnFinder.setCompletionStateFromResponse(
                retrievalThatDoesNotThrowException, retrievalResponse);

        assertThat(retrievalThatDoesNotThrowException.getState(),
                is(equalTo(State.FAILED)));
    }

    private void processRetrieval(RetrievalRequestRecord retrieval) {
        final PerformRetrievalsThenReturnFinder pluginDataObjectsFinder = new PerformRetrievalsThenReturnFinder(
                retrievalQueue, MOCK_DAO);
        pluginDataObjectsFinder.process(retrieval);
    }
}
