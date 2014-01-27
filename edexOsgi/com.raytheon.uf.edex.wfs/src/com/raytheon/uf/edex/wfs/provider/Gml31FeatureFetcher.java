/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.provider;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_1_1_0.FilterType;
import net.opengis.gml.v_3_1_1.AbstractFeatureType;
import net.opengis.gml.v_3_1_1.FeaturePropertyType;

import org.hibernate.criterion.Criterion;

import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.IWfsProvider.WfsOpType;
import com.raytheon.uf.edex.wfs.filter.v1_1_0.FilterProcessor;
import com.raytheon.uf.edex.wfs.filter.v1_1_0.QueryFilterVisitor;
import com.raytheon.uf.edex.wfs.reg.WfsQueryOptions;
import com.raytheon.uf.edex.wfs.reg.WfsQueryResults;
import com.raytheon.uf.edex.wfs.reg.WfsQueryResults.ResultType;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;

/**
 * Feature fetcher for gml version 3.1.1
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class Gml31FeatureFetcher extends FeatureFetcher {

    public static final String GML_VERSION = "3.1.1";

    /**
     * @param registry
     */
    public Gml31FeatureFetcher(WfsRegistryImpl registry) {
        super(registry);
    }

    /**
     * @param request
     * @param options
     * @return
     * @throws WfsException
     */
    public CountedFeatures<FeaturePropertyType> getFeatures(
            GetFeatureReq request, OgcServiceInfo<WfsOpType> serviceinfo)
            throws WfsException {
        final List<FeaturePropertyType> members = new ArrayList<FeaturePropertyType>();
        FeatureCallback callback = new FeatureCallback() {
            @Override
            public long addResults(WfsQueryResults results) {
                return populateFeatures(members, results);
            }
        };
        WfsQueryOptions options = new WfsQueryOptions(ResultType.JAXB);
        options.setGmlVersion(GML_VERSION);
        long count = getFeatures(request, options, callback);
        return new CountedFeatures<FeaturePropertyType>(members, count);
    }

    /**
     * @param members
     * @param results
     * @return
     */
    @SuppressWarnings("unchecked")
    protected long populateFeatures(List<FeaturePropertyType> members,
            WfsQueryResults results) {
        List<JAXBElement<?>> list = (List<JAXBElement<?>>) results.getResults();
        if (list == null || list.isEmpty()) {
            return 0;
        }
        for (JAXBElement<?> feature : list) {
            FeaturePropertyType propType = new FeaturePropertyType();
            propType.setFeature((JAXBElement<? extends AbstractFeatureType>) feature);
            members.add(propType);
        }
        return list.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.provider.FeatureFetcher#getFromFilter(java.lang
     * .Object, com.raytheon.uf.edex.wfs.provider.VisitorBag)
     */
    protected Criterion getFromFilter(Object filter, VisitorBag bag)
            throws Exception {
        if (filter != null) {
            FilterProcessor proc = new FilterProcessor((FilterType) filter);
            return (Criterion) proc.accept(new QueryFilterVisitor(), bag);
        } else {
            // FIXME
            return null;
        }
    }

}
