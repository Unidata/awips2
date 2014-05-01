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
package com.raytheon.uf.edex.wfs.v2_0_0;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import net.opengis.filter.v_2_0_0.ComparisonOperatorType;
import net.opengis.filter.v_2_0_0.ComparisonOperatorsType;
import net.opengis.filter.v_2_0_0.ConformanceType;
import net.opengis.filter.v_2_0_0.FilterCapabilities;
import net.opengis.filter.v_2_0_0.GeometryOperandsType;
import net.opengis.filter.v_2_0_0.GeometryOperandsType.GeometryOperand;
import net.opengis.filter.v_2_0_0.IdCapabilitiesType;
import net.opengis.filter.v_2_0_0.LogicalOperators;
import net.opengis.filter.v_2_0_0.ScalarCapabilitiesType;
import net.opengis.filter.v_2_0_0.SpatialCapabilitiesType;
import net.opengis.filter.v_2_0_0.SpatialOperatorType;
import net.opengis.filter.v_2_0_0.SpatialOperatorsType;
import net.opengis.ows.v_1_1_0.AllowedValues;
import net.opengis.ows.v_1_1_0.CodeType;
import net.opengis.ows.v_1_1_0.DCP;
import net.opengis.ows.v_1_1_0.DomainType;
import net.opengis.ows.v_1_1_0.HTTP;
import net.opengis.ows.v_1_1_0.LanguageStringType;
import net.opengis.ows.v_1_1_0.NoValues;
import net.opengis.ows.v_1_1_0.OperationsMetadata;
import net.opengis.ows.v_1_1_0.RequestMethodType;
import net.opengis.ows.v_1_1_0.ServiceIdentification;
import net.opengis.wfs.v_2_0_0.FeatureTypeListType;
import net.opengis.wfs.v_2_0_0.FeatureTypeType;
import net.opengis.wfs.v_2_0_0.WFSCapabilitiesType;

import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.feature.GmlUtils;
import com.raytheon.uf.edex.ogc.common.http.MimeType;
import com.raytheon.uf.edex.wfs.IWfsProvider.WfsOpType;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.request.GetCapReq;

/**
 * Utility class for creating capabilities documents for WFS 2.0
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class Capabilities {

    protected static final String SERV_TYPE = "WFS";

    protected static final String SERV_TITLE = "EDEX WFS";

    protected static final String OWS_NS = OgcNamespace.OWS;

    protected static final String GML_NS = OgcNamespace.GML;

    protected static final String OGC_NS = OgcNamespace.OGC;

    protected static final String WFS_NS = OgcNamespace.WFS;

    protected static final String VERSION = "2.0.0";

    protected static final MimeType GML_MIME = GmlUtils.GML32_TYPE;

    protected String[] gmlObjects = { "AbstractFeatureType", "PointType",
            "LineStringType", "PolygonType", "MultiPointType" };

    protected String[] geometryOperands = { "Envelope", "Point", "LineString",
            "Polygon" };

    protected String[] spatialOperators = { "BBOX", "Equals", "Within",
            "Disjoint", "Contains", "Crosses", "Intersects", "Overlaps",
            "Touches" };

    protected String[] comparisonOperators = { "PropertyIsLessThan",
            "PropertyIsGreaterThan", "PropertyIsLessThanOrEqualTo",
            "PropertyIsGreaterThanOrEqualTo", "PropertyIsEqualTo",
            "PropertyIsNotEqualTo" };

    protected String[] logicOperators = { "And", "Or", "Not" };

    protected FeatureTranslator translator = new FeatureTranslator();

    protected WfsRegistryImpl registry;

    private final WfsOperationsDescriber opDescriber = new WfsOperationsDescriber();

    public Capabilities(WfsRegistryImpl registry) {
        this.registry = registry;
    }

    public WFSCapabilitiesType getCapabilities(GetCapReq request,
            OgcServiceInfo<WfsOpType> serviceinfo) throws WfsException {
        WFSCapabilitiesType cap = new WFSCapabilitiesType();
        cap.setServiceIdentification(getServiceId(serviceinfo));
        cap.setOperationsMetadata(getOpData(serviceinfo));
        cap.setFeatureTypeList(getFeatureTypes(request, serviceinfo));
        cap.setVersion(VERSION);
        cap.setFilterCapabilities(getFilterCap());
        return cap;
    }

    /**
     * @param serviceinfo
     * @return
     */
    protected OperationsMetadata getOpData(OgcServiceInfo<WfsOpType> serviceinfo) {
        return opDescriber.getOpData(serviceinfo);
    }

    /**
     * @param op
     * @return
     */
    protected List<DCP> getDcpList(OgcOperationInfo<WfsOpType> op) {
        List<DCP> rval = new LinkedList<DCP>();
        DCP dcp = new DCP();
        HTTP http = new HTTP();
        List<JAXBElement<RequestMethodType>> value = new LinkedList<JAXBElement<RequestMethodType>>();
        if (op.hasHttpGet()) {
            value.add(getRequestType("Get", op.getHttpGetRes()));
        }
        if (op.hasHttpPost()) {
            value.add(getRequestType("Post", op.getHttpPostRes()));
        }
        http.setGetOrPost(value);
        dcp.setHTTP(http);
        rval.add(dcp);
        return rval;
    }

    protected JAXBElement<RequestMethodType> getRequestType(String name,
            String value) {
        JAXBElement<RequestMethodType> rval = new JAXBElement<RequestMethodType>(
                new QName(OWS_NS, name), RequestMethodType.class,
                new RequestMethodType());
        rval.getValue().setHref(value);
        return rval;
    }

    protected DomainType getAsDomainType(String name, Collection<String> values) {
        DomainType rval = new DomainType();
        rval.setName(name);
        List<Object> toVals = new ArrayList<Object>(values.size());
        for (String val : values) {
            toVals.add(val);
        }
        AllowedValues value = new AllowedValues();
        value.setValueOrRange(toVals);
        rval.setAllowedValues(value);
		return rval;
	}

	/**
	 * @return
	 */
	protected FilterCapabilities getFilterCap() {
		FilterCapabilities rval = new FilterCapabilities();
		rval.setConformance(getConformance());
		rval.setScalarCapabilities(getScalarCapabilities());
		rval.setSpatialCapabilities(getSpatialCapabilities());
		rval.setIdCapabilities(getIdCapabilities());
		return rval;
	}

	/**
     * @return
     */
    protected ConformanceType getConformance() {
        ConformanceType rval = new ConformanceType();
        DomainType domain = new DomainType();
        //TODO, is this the correct name here??
        domain.setName("Filter_Conformance");
        NoValues value = new NoValues();
        domain.setNoValues(value);
        rval.setConstraint(Arrays.asList(domain));
        return rval;
    }
    
    /**
     * @return
     */
    protected IdCapabilitiesType getIdCapabilities() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @return
     */
    protected SpatialCapabilitiesType getSpatialCapabilities() {
        SpatialCapabilitiesType rval = new SpatialCapabilitiesType();
        rval.setGeometryOperands(getGeometryOperands());
        rval.setSpatialOperators(getSpatialOperators());
        return rval;
    }

    /**
     * @return
     */
    protected SpatialOperatorsType getSpatialOperators() {
        SpatialOperatorsType rval = new SpatialOperatorsType();
        List<SpatialOperatorType> ops = new ArrayList<SpatialOperatorType>(
                spatialOperators.length);
        for (String name : spatialOperators) {
            SpatialOperatorType op = new SpatialOperatorType();
            op.setName(name);
            ops.add(op);
        }
        rval.setSpatialOperator(ops);
        return rval;
    }

    /**
     * @return
     */
    protected GeometryOperandsType getGeometryOperands() {
        GeometryOperandsType rval = new GeometryOperandsType();
        List<GeometryOperand> ops = new ArrayList<GeometryOperand>(
                geometryOperands.length);
        for (String op : geometryOperands) {
            QName name = new QName(OgcNamespace.GML, op);
            GeometryOperand gop = new GeometryOperand();
            gop.setName(name);
            ops.add(gop);
        }
        rval.setGeometryOperand(ops);
        return rval;
    }

    /**
     * @return
     */
    protected ScalarCapabilitiesType getScalarCapabilities() {
        ScalarCapabilitiesType rval = new ScalarCapabilitiesType();
        rval.setComparisonOperators(getComparisonOperators());
        rval.setLogicalOperators(GetLogicalOperators());
        return rval;
    }

    /**
     * @return
     */
    protected LogicalOperators GetLogicalOperators() {
        return null;
    }

    /**
     * @return
     */
    protected ComparisonOperatorsType getComparisonOperators() {
        ComparisonOperatorsType rval = new ComparisonOperatorsType();
        List<ComparisonOperatorType> ops = new ArrayList<ComparisonOperatorType>(
                comparisonOperators.length);
        for (String name : comparisonOperators) {
            ComparisonOperatorType op = new ComparisonOperatorType();
            op.setName(name);
            ops.add(op);
        }
        rval.setComparisonOperator(ops);
        return rval;
    }

    /**
     * @param request
     * @param serviceinfo
     * @return
     * @throws WfsException
     */
    protected FeatureTypeListType getFeatureTypes(GetCapReq request,
            OgcServiceInfo<WfsOpType> serviceinfo) throws WfsException {
        FeatureTypeListType rval = new FeatureTypeListType();
        // rval.setOperations(getOperations(serviceinfo));
        rval.setFeatureType(getFeatureTypes(request));
        return rval;
    }

    protected List<FeatureTypeType> getFeatureTypes(GetCapReq request)
            throws WfsException {
        return translator.transform(registry.getFeatures());
    }

    /**
     * @param serviceinfo
     * @return
     */
    protected ServiceIdentification getServiceId(
            OgcServiceInfo<WfsOpType> serviceinfo) {
        ServiceIdentification rval = new ServiceIdentification();
        CodeType ct = new CodeType();
        ct.setValue(SERV_TYPE);
        rval.setServiceType(ct);
        LanguageStringType lst = new LanguageStringType();
        lst.setValue(SERV_TITLE);
        rval.setTitle(Arrays.asList(lst));
        rval.setServiceTypeVersion(Arrays.asList(Wfs2_0_0Provider.version));
		return rval;
	}
}
