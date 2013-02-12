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

import java.util.ArrayList;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;

/**
 * Test {@link PerformRetrievalPluginDataObjectsFinder}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class PerformRetrievalPluginDataObjectsFinderTest {

    private static final String EXCEPTION_MESSAGE = "thrown on purpose";

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

    private final RetrievalRequestRecord retrievalThatDoesNotThrowException = RetrievalRequestRecordFixture.INSTANCE
            .get();

    @Before
    public void setUp() {
        PathManagerFactoryTest.initLocalization();
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

    private void processRetrieval(RetrievalRequestRecord retrieval) {
        final PerformRetrievalPluginDataObjectsFinder pluginDataObjectsFinder = new PerformRetrievalPluginDataObjectsFinder(
                Network.OPSNET);
        pluginDataObjectsFinder.process(retrieval);
    }
}
