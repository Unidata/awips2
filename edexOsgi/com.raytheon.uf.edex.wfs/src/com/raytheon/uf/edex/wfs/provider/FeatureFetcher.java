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
package com.raytheon.uf.edex.wfs.provider;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBException;

import net.opengis.filter.v_1_1_0.FilterType;

import org.geotools.geometry.jts.JTS;
import org.hibernate.criterion.Criterion;
import org.hibernatespatial.criterion.SpatialRestrictions;
import org.opengis.feature.simple.SimpleFeature;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wfs.IWfsProvider.WfsOpType;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.reg.IWfsSource;
import com.raytheon.uf.edex.wfs.reg.WfsQuery;
import com.raytheon.uf.edex.wfs.reg.WfsQueryOptions;
import com.raytheon.uf.edex.wfs.reg.WfsQueryResults;
import com.raytheon.uf.edex.wfs.reg.WfsQueryResults.ResultType;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.request.FeatureQuery;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;
import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Abstract base for retrieving features from storage
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class FeatureFetcher {

    protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    protected WfsRegistryImpl registry;

    public static class CountedFeatures<T> {
        public List<T> features;

        public long count = 0;

        public Date latest;

        public CountedFeatures(List<T> features) {
            this(features, features.size());
        }

        public CountedFeatures(long count) {
            this(new ArrayList<T>(0), count);
        }

        public CountedFeatures(List<T> features, long count) {
            this.features = features;
            this.count = count;
        }
    }

    protected static abstract class FeatureCallback {
        public abstract long addResults(WfsQueryResults results);
    }

    /**
     * @param registry
     */
    public FeatureFetcher(WfsRegistryImpl registry) {
        this.registry = registry;
    }

    @SuppressWarnings("unchecked")
    public List<List<SimpleFeature>> getSimpleFeatures(GetFeatureReq request,
            OgcServiceInfo<WfsOpType> serviceinfo) throws WfsException {
        List<List<SimpleFeature>> rval = new ArrayList<List<SimpleFeature>>();
        for (FeatureQuery q : request.getQueries()) {
            for (QualifiedName type : q.getTypeNames()) {
                IWfsSource source = registry.getSource(type);
                if (source != null) {
                    String spatial = source.getFeatureSpatialField(type);
                    String vert = source.getFeatureVerticalField(type);
                    String id = source.getFeatureIdField(type);
                    VisitorBag bag = new VisitorBag(
                            source.getFeatureEntity(type), spatial, vert, id);
                    bag.setFieldMap(source.getFieldMap());
                    WfsQuery wfsq;
                    try {
                        wfsq = new WfsQuery(getQuery(q, bag),
                                request.getMaxFeatures(), q.getSortBys(),
                                q.getPropertyNames(), q.getTimeRange());
                    } catch (OgcException e) {
                        throw new WfsException(e);
                    } catch (WfsException e) {
                        throw e;
                    } catch (Exception e) {
                        log.error("Problem parsing wfs query", e);
                        throw new WfsException(Code.INVALID_REQUEST,
                                "Invalid filter");
                    }
                    WfsQueryResults results = source.query(type, wfsq,
                            new WfsQueryOptions(ResultType.SIMPLE));
                    rval.add((List<SimpleFeature>) results.getResults());
                } else {
                    throw new WfsException(Code.INVALID_REQUEST,
                            "Unkown feature type: " + type);
                }
            }
        }
        return rval;
    }

    protected long getFeatures(GetFeatureReq request, WfsQueryOptions options,
            FeatureCallback callback) throws WfsException {
        long count = 0;
        for (FeatureQuery q : request.getQueries()) {
            for (QualifiedName type : q.getTypeNames()) {
                IWfsSource source = registry.getSource(type);
                if (source != null) {
                    String spatial = source.getFeatureSpatialField(type);
                    String vert = source.getFeatureVerticalField(type);
                    String id = source.getFeatureIdField(type);
                    VisitorBag bag = new VisitorBag(
                            source.getFeatureEntity(type), spatial, vert, id);
                    bag.setFieldMap(source.getFieldMap());
                    WfsQuery wfsq;
                    try {
                        wfsq = new WfsQuery(getQuery(q, bag),
                                request.getMaxFeatures(), q.getSortBys(),
                                q.getPropertyNames(), q.getTimeRange());
                    } catch (OgcException e) {
                        throw new WfsException(e);
                    } catch (WfsException e) {
                        throw e;
                    } catch (Exception e) {
                        log.error("Problem parsing wfs query", e);
                        throw new WfsException(Code.INVALID_REQUEST,
                                "Invalid filter");
                    }
                    if (request.getResulttype() == GetFeatureReq.ResultType.hits) {
                        count += source.count(type, wfsq);
                    } else {
                        WfsQueryResults result = source.query(type, wfsq,
                                options);
                        count += callback.addResults(result);
                    }
                } else {
                    throw new WfsException(Code.INVALID_REQUEST,
                            "Unkown feature type: " + type);
                }
            }
        }
        return count;
    }

    /**
     * @param q
     * @param spatial
     * @return
     * @throws Exception
     */
    protected Criterion getQuery(FeatureQuery q, VisitorBag bag)
            throws Exception {
        Criterion rval = null;
        switch (q.getFilterType()) {
        case BBOX:
            OgcBoundingBox bbox = (OgcBoundingBox) q.getFilter();
            rval = getBbox(bbox.getMiny(), bbox.getMinx(), bbox.getMaxy(),
                    bbox.getMaxx(), bag);
            break;
        case FIDS:
            // TODO
            rval = getDefault(bag);
            break;
        case XML:
            FilterType f = parseFilterXml((String) q.getFilter());
            rval = getFromFilter(f, bag);
            break;
        case XMLOBJ:
            rval = getFromFilter(q.getFilter(), bag);
            break;
        default:
            rval = getDefault(bag);
        }
        return rval;
    }

    protected Criterion getDefault(VisitorBag bag) {
        return null;
    }

    protected FilterType parseFilterXml(String xml) throws JAXBException {
        return (FilterType) registry.unmarshal(xml);
    }

    protected abstract Criterion getFromFilter(Object filter, VisitorBag bag)
            throws Exception;

    protected Criterion getBbox(double lowerLat, double lowerLon,
            double upperLat, double upperLon, VisitorBag bag)
            throws WfsException {
		Polygon geom = JTS.toGeometry(new Envelope(lowerLon, upperLon,
				lowerLat, upperLat));
        if (bag.getSpatialField() == null) {
            throw new WfsException(Code.InvalidParameterValue,
                    "Geospatial filter not supported for feature type");
        }
		return SpatialRestrictions.within(bag.getSpatialField(), geom);
	}

}
