package com.raytheon.uf.edex.datadelivery.retrieval.wfs;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.PointDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Provider.ProviderType;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalGenerator;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;

/**
 * 
 * WFS Retrieval Generator.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2012 955        djohnson     Moved to wfs specific package.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * May 12, 2013 753        dhladky      Implemented
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class WfsRetrievalGenerator extends RetrievalGenerator {
    
    private static final IUFStatusHandler statusHandler = UFStatus
    .getHandler(WfsRetrievalGenerator.class);
    
    WfsRetrievalGenerator() {
        super(ServiceType.WFS);
    }

    @Override
    public List<Retrieval> buildRetrieval(SubscriptionBundle bundle) {
        
        List<Retrieval> retrievals = Collections.emptyList();
        switch (bundle.getDataType()) {
        case POINT:
            retrievals = getPointRetrievals(bundle);
            break;
        default:
            statusHandler.error("Grid DATA WFS NOT YET IMPLEMENTED");
            throw new IllegalArgumentException("Grid DATA WFS NOT YET IMPLEMENTED");
        }

        return retrievals;
    }
    
    /**
     * create the grid type retrievals
     * 
     * @param bundle
     * @return
     */
    private List<Retrieval> getPointRetrievals(SubscriptionBundle bundle) {

        List<Retrieval> retrievals = new ArrayList<Retrieval>();
        Subscription sub = bundle.getSubscription();

        sub = removeDuplicates(sub);

        if (sub != null) {

            PointTime subTime = (PointTime) sub.getTime();
            String retrievalUrl = getRetrievalUrl(sub);
            sub.setUrl(retrievalUrl);

            if (sub.getUrl() == null) {
                statusHandler
                        .info("Skipping subscription that is unfulfillable with the current metadata.");
                return Collections.emptyList();
            }
            
            // with point data they all have the same data 
            Parameter param = sub.getParameter().get(0);
            Retrieval retrieval = getRetrieval(sub, bundle,
                    param, subTime);
            retrievals.add(retrieval);
        }

        return retrievals;
    }
    
    /**
     * Determines the retrieval URL
     * 
     * @param subscription
     *            the subscription
     * @return the url for retrieval or null if no retrieval should take place
     */
    private static String getRetrievalUrl(Subscription subscription) {
        String url = subscription.getUrl();

        DataSetMetaData result = null;
        try {
            result = DataDeliveryHandlers.getDataSetMetaDataHandler().getById(
                    url);
            if (result == null) {
                throw new RegistryHandlerException(
                        "Unable to find the dataset by id from its unique url!",
                        new NullPointerException("DataSetMetaData"));
            }

            if (satisfiesSubscriptionCriteria(subscription, result)) {
                return url;
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to find the dataset by id from its unique url!", e);
        }

        return null;
    }

    /**
     * Determines whether a subscription can be satisified by the dataset
     * metadata.
     * 
     * @param subscription
     *            the subscription
     * @param dsmd
     *            the dataset metadata
     * @return true if the datasetmetadata will satisfy the subscription
     */
    @VisibleForTesting
    static boolean satisfiesSubscriptionCriteria(Subscription subscription,
            DataSetMetaData dsmd) {
      

        if (dsmd instanceof PointDataSetMetaData) {
            PointDataSetMetaData data = (PointDataSetMetaData) dsmd;
            //TODO determine some check for validity of point data sets
            
            return true;
            
        }

        return false;
    }

    
    /**
     * Get the retrieval
     * 
     * @param sub
     * @param bundle
     * @param param
     * @param level
     * @param Time
     * @return
     */
    private Retrieval getRetrieval(Subscription sub, SubscriptionBundle bundle,
            Parameter param, PointTime time) {

        Retrieval retrieval = new Retrieval();
        retrieval.setSubscriptionName(sub.getName());
        retrieval.setServiceType(getServiceType());
        retrieval.setConnection(bundle.getConnection());
        retrieval.getConnection().setUrl(sub.getUrl());
        retrieval.setOwner(sub.getOwner());
        retrieval.setSubscriptionType(getSubscriptionType(sub));
        retrieval.setNetwork(sub.getRoute());

        // Coverage and type processing
        Coverage cov = sub.getCoverage();
        ProviderType pt = null;
        if (cov instanceof Coverage){
            pt = ProviderType.POINT;
            retrieval.setProviderType(pt);
        } else {
            throw new UnsupportedOperationException(
                    "WFS retrieval does not yet support coverages other than Point. ");
        }

        // Attribute processing
        RetrievalAttribute att = new RetrievalAttribute();
        Parameter lparam = processParameter(param);
        att.setCoverage(cov);
        lparam.setLevels(param.getLevels());
        att.setTime(time);
        att.setParameter(lparam);
        att.setSubName(retrieval.getSubscriptionName());
        att.setPlugin(pt.getPlugin());
        att.setProvider(sub.getProvider());
        retrieval.addAttribute(att);

        return retrieval;
    }



    /**
     * {@inheritDoc}
     */
    @Override
    protected RetrievalAdapter getServiceRetrievalAdapter() {
        return new WfsRetrievalAdapter();
    }

    @Override
    protected Subscription removeDuplicates(Subscription sub) {
        throw new UnsupportedOperationException("Not implemented");
    }

}
