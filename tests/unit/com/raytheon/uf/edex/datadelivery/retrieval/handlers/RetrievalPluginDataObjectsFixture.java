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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.AbstractFixture;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;
import com.raytheon.uf.edex.datadelivery.retrieval.opendap.MockOpenDapRetrievalAdapter;

/**
 * Fixture for {@link RetrievalPluginDataObjects} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Feb 12, 2013 1543       djohnson     No longer set plugin data objects themselves, just retrieval attributes.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RetrievalPluginDataObjectsFixture extends
        AbstractFixture<RetrievalPluginDataObjects> {

    public static final RetrievalPluginDataObjectsFixture INSTANCE = new RetrievalPluginDataObjectsFixture();

    /**
     * Prevent construction.
     */
    private RetrievalPluginDataObjectsFixture() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RetrievalPluginDataObjects get(long seedValue) {
        RetrievalRequestRecord requestRecord = RetrievalRequestRecordFixture.INSTANCE
                .get(seedValue);
        List<RetrievalAttributePluginDataObjects> retrievalAttributePluginDataObjects = new ArrayList<RetrievalAttributePluginDataObjects>();
        try {
            for (final RetrievalAttribute attribute : requestRecord
                    .getRetrievalObj().getAttributes()) {
                retrievalAttributePluginDataObjects
                        .add(new RetrievalAttributePluginDataObjects(
                                attribute,
                                new MockOpenDapRetrievalAdapter()
                                        .performRequest(new IRetrievalRequestBuilder() {
                                            @Override
                                            public String processTime(
                                                    Time prtXML) {
                                                return "" + prtXML;
                                            }

                                            @Override
                                            public String processCoverage() {
                                                return "noCoverage";
                                            }

                                            @Override
                                            public String getRequest() {
                                                return "request";
                                            }

                                            @Override
                                            public RetrievalAttribute getAttribute() {
                                                return attribute;
                                            }
                                        })));
            }
        } catch (SerializationException e) {
            throw new RuntimeException(e);
        }

        final RetrievalPluginDataObjects retrievalPluginDataObjects = new RetrievalPluginDataObjects(
                requestRecord, retrievalAttributePluginDataObjects);
        return retrievalPluginDataObjects;
    }
}
