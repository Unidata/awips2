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
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_2_0_0.FilterType;
import net.opengis.gml.v_3_2_1.AbstractFeatureType;
import net.opengis.gml.v_3_2_1.FeaturePropertyType;

import org.hibernate.criterion.Criterion;

import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.IWfsProvider.WfsOpType;
import com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Processor;
import com.raytheon.uf.edex.wfs.filter.v2_0_0.QueryFilterVisitor;
import com.raytheon.uf.edex.wfs.reg.WfsQueryOptions;
import com.raytheon.uf.edex.wfs.reg.WfsQueryResults;
import com.raytheon.uf.edex.wfs.reg.WfsQueryResults.ResultType;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;

/**
 * Feature fetcher for GML 3.2.1
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
public class Gml32FeatureFetcher extends FeatureFetcher {

    public static final String GML_VERSION = "3.2.1";

    /**
     * @param registry
     */
    public Gml32FeatureFetcher(WfsRegistryImpl registry) {
        super(registry);
    }

    /**
     * @param request
     * @param options
     * @return
     * @throws WfsException
     */
    public CountedFeatures<FeaturePropertyType> getFeatures(
            GetFeatureReq request, OgcServiceInfo<WfsOpType> serviceinfo,
            boolean gatherMetadata) throws WfsException {
        final List<FeaturePropertyType> members = new ArrayList<FeaturePropertyType>();
        final Date[] holder = new Date[1];
        FeatureCallback callback = new FeatureCallback() {
            @Override
            public long addResults(WfsQueryResults results) {
                Date latest = results.getLatestResult();
                if (holder[0] == null || latest.after(holder[0])) {
                    holder[0] = latest;
                }
                return populateFeatures(members, results);
            }
        };
        WfsQueryOptions options = new WfsQueryOptions(ResultType.JAXB);
        options.setGmlVersion(GML_VERSION);
        options.setGatherMetadata(gatherMetadata);
        long count = getFeatures(request, options, callback);
        CountedFeatures<FeaturePropertyType> rval = new CountedFeatures<FeaturePropertyType>(
                members, count);
        rval.latest = holder[0];
        return rval;
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
            propType.setAbstractFeature((JAXBElement<? extends AbstractFeatureType>) feature);
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
            QueryFilterVisitor visitor = new QueryFilterVisitor();
            Filter2Processor proc = new Filter2Processor((FilterType) filter);
            return (Criterion) proc.accept(visitor, bag);
        } else {
            // FIXME
            return null;
        }
    }
}
