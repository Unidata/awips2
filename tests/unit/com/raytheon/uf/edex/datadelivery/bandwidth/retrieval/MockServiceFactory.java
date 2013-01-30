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
package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.junit.Ignore;

import com.google.common.collect.Maps;
import com.raytheon.uf.common.datadelivery.registry.Collection;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverageFixture;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaDataFixture;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.IExtractMetaData;
import com.raytheon.uf.edex.datadelivery.retrieval.IParseMetaData;
import com.raytheon.uf.edex.datadelivery.retrieval.LinkStore;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalGenerator;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.response.RetrievalResponse;

/**
 * {@link ServiceFactory} that doesn't do much, just create a fake retrieval.
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
@Ignore
public class MockServiceFactory extends RetrievalGenerator implements
        ServiceFactory, IExtractMetaData, IParseMetaData {

    /**
     * Just returns a retrieval.
     */
    public class MockRetrievalAdapter extends RetrievalAdapter implements
            IRetrievalRequestBuilder {

        /**
         * {@inheritDoc}
         */
        @Override
        public IRetrievalRequestBuilder createRequestMessage(
                RetrievalAttribute prxml) {
            return this;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public RetrievalResponse performRequest(IRetrievalRequestBuilder request) {
            return new RetrievalResponse(request.getAttribute());
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Map<String, PluginDataObject[]> processResponse(
                IRetrievalResponse response) throws TranslationException {
            final Map<String, PluginDataObject[]> map = Maps.newHashMap();
            map.put("grid", new PluginDataObject[] { new GridRecord() });

            return map;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String processTime(Time prtXML) {
            return "someTime";
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String processCoverage() {
            return "someCoverage";
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String getRequest() {
            return "someRequest";
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public RetrievalAttribute getAttribute() {
            return new RetrievalAttribute();
        }

    }

    /**
     * @param serviceType
     */
    public MockServiceFactory(ServiceType serviceType) {
        super(serviceType);
    }

    /**
     * @param provider
     */
    public MockServiceFactory(Provider provider) {
        this(provider.getServiceType());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IExtractMetaData getExtractor() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IParseMetaData getParser(Date lastUpdate) {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RetrievalGenerator getRetrievalGenerator() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<DataSetMetaData> parseMetaData(Provider provider,
            LinkStore store, Collection collection, String dataDateFormat) {
        return Arrays
                .<DataSetMetaData> asList(OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                        .get());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void storeMetaData(List<DataSetMetaData> metaDatas, DataSet dataSet) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> extractMetaData() throws Exception {
        return Collections.emptyMap();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean checkLastUpdate(Date lastUpdate) {
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDataDate() throws Exception {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Date getDataDate() {
        return new Date();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setUrl(String url) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Retrieval> buildRetrieval(SubscriptionBundle bundle) {
        final Subscription subscription = bundle.getSubscription();

        Retrieval retrieval = new Retrieval();
        retrieval.setConnection(bundle.getConnection());
        retrieval.setNetwork(subscription.getRoute());
        retrieval.setOwner(subscription.getOwner());
        retrieval.setProviderType(bundle.getProvider().getProviderType()
                .iterator().next());
        retrieval.setServiceType(this.getServiceType());
        retrieval.setSubscriptionName(subscription.getName());
        retrieval.setSubscriptionType(getSubscriptionType(subscription));

        final ArrayList<RetrievalAttribute> attributes = new ArrayList<RetrievalAttribute>();
        attributes.add(getAttribute(bundle));
        attributes.add(getAttribute(bundle));
        retrieval.setAttribute(attributes);

        return Arrays.asList(retrieval);
    }

    /**
     * @param bundle
     * @return
     */
    private RetrievalAttribute getAttribute(SubscriptionBundle bundle) {
        RetrievalAttribute attribute = new RetrievalAttribute();
        attribute.setCoverage(GriddedCoverageFixture.INSTANCE.get());
        attribute.setSubName(bundle.getSubscription().getName());
        attribute.setProvider(bundle.getProvider().getName());
        return attribute;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected RetrievalAdapter getServiceRetrievalAdapter() {
        return new MockRetrievalAdapter();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Subscription removeDuplicates(Subscription sub) {
        return sub;
    }

}
