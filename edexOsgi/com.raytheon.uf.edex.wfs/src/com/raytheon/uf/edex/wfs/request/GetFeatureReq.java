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
package com.raytheon.uf.edex.wfs.request;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import net.opengis.filter.v_1_1_0.SortByType;
import net.opengis.filter.v_1_1_0.SortOrderType;
import net.opengis.filter.v_1_1_0.SortPropertyType;
import net.opengis.wfs.v_1_1_0.GetFeatureType;
import net.opengis.wfs.v_1_1_0.QueryType;
import net.opengis.wfs.v_1_1_0.ResultTypeType;
import net.opengis.wfs.v_2_0_0.GetPropertyValueType;
import net.opengis.wfs.v_2_0_0.StoredQueryType;

import com.raytheon.uf.edex.ogc.common.http.MimeType;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.querystore.IStoredQueryCallback;
import com.raytheon.uf.edex.wfs.request.FeatureQuery.QFilterType;
import com.raytheon.uf.edex.wfs.request.SortBy.Order;

/**
 * Wrapper for WFS get feature request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GetFeatureReq extends WfsRequest {

    public enum ResultType {
        results, hits
    }

    protected List<FeatureQuery> queries = new LinkedList<FeatureQuery>();

    protected MimeType outputformat = new MimeType(
            "text/xml; subtype=\"gml/3.1.1\"");

    protected ResultType resulttype = ResultType.results;

    // may want to set default
    protected int maxFeatures = Integer.MAX_VALUE;

    /**
     * @param type
     */
    public GetFeatureReq() {
        super(Type.GetFeature);
    }

    public GetFeatureReq(GetFeatureType req) {
        super(Type.GetFeature);
        setRawrequest(req);
        ResultTypeType resultType = req.getResultType();
        if (resultType == ResultTypeType.HITS) {
            setResulttype(ResultType.hits);
        }
        String outputFormat = req.getOutputFormat();
        if (outputFormat != null) {
            setOutputformat(new MimeType(outputFormat));
        }
        BigInteger maxFeatures = req.getMaxFeatures();
        if (maxFeatures != null) {
            setMaxFeatures(maxFeatures.intValue());
        }
        List<QueryType> query = req.getQuery();
        if (query != null) {
            for (QueryType qt : query) {
                addQuery(getQuery(qt));
            }
        }
    }

    /**
     * @param obj
     * @throws WfsException
     */
    public GetFeatureReq(net.opengis.wfs.v_2_0_0.GetFeatureType req,
            IStoredQueryCallback callback) throws WfsException {
        super(Type.GetFeature);
        setRawrequest(req);
        net.opengis.wfs.v_2_0_0.ResultTypeType resultType = req.getResultType();
        String outputFormat = req.getOutputFormat();
        BigInteger maxFeatures = req.getCount();
        List<JAXBElement<?>> querys = req.getAbstractQueryExpression();
        init(resultType, outputFormat, maxFeatures, querys, callback);
    }

    /**
     * @param req
     * @param callback
     * @throws WfsException
     */
    public GetFeatureReq(GetPropertyValueType req, IStoredQueryCallback callback)
            throws WfsException {
        super(Type.GetFeature);
        setRawrequest(req);
        net.opengis.wfs.v_2_0_0.ResultTypeType resultType = req.getResultType();
        String outputFormat = req.getOutputFormat();
        BigInteger maxFeatures = req.getCount();
        List<JAXBElement<?>> querys = new ArrayList<JAXBElement<?>>(1);
        querys.add(req.getAbstractQueryExpression());
        init(resultType, outputFormat, maxFeatures, querys, callback);
    }

    /**
     * Initialize with common WFS 2.0 query parameters
     * 
     * @param resultType
     * @param outputFormat
     * @param maxFeatures
     * @param querys
     * @param callback
     * @throws WfsException
     */
    private void init(net.opengis.wfs.v_2_0_0.ResultTypeType resultType,
            String outputFormat, BigInteger maxFeatures,
            List<JAXBElement<?>> querys, IStoredQueryCallback callback)
            throws WfsException {
        if (resultType == net.opengis.wfs.v_2_0_0.ResultTypeType.HITS) {
            setResulttype(ResultType.hits);
        }
        if (outputFormat != null) {
            setOutputformat(new MimeType(outputFormat));
        }
        if (maxFeatures != null) {
            setMaxFeatures(maxFeatures.intValue());
        }
        if (queries != null) {
            for (JAXBElement<?> e : querys) {
                Object value = e.getValue();
                if (value instanceof net.opengis.wfs.v_2_0_0.QueryType) {
                    addQuery(getQuery((net.opengis.wfs.v_2_0_0.QueryType) value));
                } else if (value instanceof StoredQueryType) {
                    addAllQueries(callback.getQueries((StoredQueryType) value));
                } else {
                    throw new WfsException(Code.OptionNotSupported,
                            "Unsupported query type");
                }
            }
        }
    }

    public void addQueries(List<net.opengis.wfs.v_2_0_0.QueryType> queries) {
        for (net.opengis.wfs.v_2_0_0.QueryType q : queries) {
            addQuery(getQuery(q));
        }
    }

    /**
     * @param qt
     * @return
     */
    protected FeatureQuery getQuery(net.opengis.wfs.v_2_0_0.QueryType qt) {
        return new FeatureQuery(qt);
    }

    /**
     * @param qt
     * @return
     */
    protected FeatureQuery getQuery(QueryType qt) {
        FeatureQuery rval = new FeatureQuery();
        rval.setFilter(qt.getFilter(), QFilterType.XMLOBJ);
        SortByType sortBy = qt.getSortBy();
        if (sortBy != null) {
            for (SortPropertyType prop : sortBy.getSortProperty()) {
                String name = prop.getPropertyName().getContent().get(0)
                        .toString();
                Order o = (prop.getSortOrder() == SortOrderType.DESC ? Order.Descending
                        : Order.Ascending);
                rval.addSortBy(new SortBy(name, o));
            }
        }
        String srsName = qt.getSrsName();
        if (srsName != null) {
            rval.setSrsName(srsName);
        }
        for (QName q : qt.getTypeName()) {
            rval.addTypeName(new QualifiedName(q.getNamespaceURI(), q
                    .getLocalPart(), q.getPrefix()));
        }
        return rval;
    }

    public void addQuery(FeatureQuery query) {
        this.queries.add(query);
    }

    public void addAllQueries(List<FeatureQuery> queries) {
        this.queries.addAll(queries);
    }

    /**
     * @return the queries
     */
    public List<FeatureQuery> getQueries() {
        return queries;
    }

    /**
     * @param queries
     *            the queries to set
     */
    public void setQueries(List<FeatureQuery> queries) {
        this.queries = queries;
    }

    /**
     * @return the outputformat
     */
    public MimeType getOutputformat() {
        return outputformat;
    }

    /**
     * @param outputformat
     *            the outputformat to set
     */
    public void setOutputformat(MimeType outputformat) {
        this.outputformat = outputformat;
    }

    /**
     * @return the resulttype
     */
    public ResultType getResulttype() {
        return resulttype;
    }

    /**
     * @param resulttype
     *            the resulttype to set
     */
    public void setResulttype(ResultType resulttype) {
        this.resulttype = resulttype;
    }

    /**
     * @return the maxFeatures
     */
    public int getMaxFeatures() {
        return maxFeatures;
    }

    /**
     * @param maxFeatures
     *            the maxFeatures to set
     */
    public void setMaxFeatures(int maxFeatures) {
        this.maxFeatures = maxFeatures;
    }

}
