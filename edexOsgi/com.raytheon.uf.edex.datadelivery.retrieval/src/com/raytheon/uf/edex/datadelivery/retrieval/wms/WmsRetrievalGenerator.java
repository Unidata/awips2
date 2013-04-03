package com.raytheon.uf.edex.datadelivery.retrieval.wms;

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalGenerator;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;

/**
 * 
 * WMS Retrieval Generator.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2012 955        djohnson     Moved to wms specific package.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class WmsRetrievalGenerator extends RetrievalGenerator {

    WmsRetrievalGenerator() {
        super(ServiceType.WMS);
    }

    @Override
    public List<Retrieval> buildRetrieval(SubscriptionBundle bundle) {
        throw new UnsupportedOperationException("Not implemented");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected RetrievalAdapter getServiceRetrievalAdapter() {
        return new WMSRetrievalAdapter();
    }

    @Override
    protected Subscription removeDuplicates(Subscription sub) {
        throw new UnsupportedOperationException("Not implemented");
    }

}
