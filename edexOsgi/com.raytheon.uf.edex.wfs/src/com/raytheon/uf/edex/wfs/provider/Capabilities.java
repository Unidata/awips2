/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.provider;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import net.opengis.filter.v_1_1_0.ArithmeticOperatorsType;
import net.opengis.filter.v_1_1_0.ComparisonOperatorType;
import net.opengis.filter.v_1_1_0.ComparisonOperatorsType;
import net.opengis.filter.v_1_1_0.FilterCapabilities;
import net.opengis.filter.v_1_1_0.GeometryOperandsType;
import net.opengis.filter.v_1_1_0.IdCapabilitiesType;
import net.opengis.filter.v_1_1_0.LogicalOperators;
import net.opengis.filter.v_1_1_0.ScalarCapabilitiesType;
import net.opengis.filter.v_1_1_0.SpatialCapabilitiesType;
import net.opengis.filter.v_1_1_0.SpatialOperatorNameType;
import net.opengis.filter.v_1_1_0.SpatialOperatorType;
import net.opengis.filter.v_1_1_0.SpatialOperatorsType;
import net.opengis.ows.v_1_0_0.CodeType;
import net.opengis.ows.v_1_0_0.DCP;
import net.opengis.ows.v_1_0_0.DomainType;
import net.opengis.ows.v_1_0_0.HTTP;
import net.opengis.ows.v_1_0_0.Operation;
import net.opengis.ows.v_1_0_0.OperationsMetadata;
import net.opengis.ows.v_1_0_0.RequestMethodType;
import net.opengis.ows.v_1_0_0.ServiceIdentification;
import net.opengis.wfs.v_1_1_0.FeatureTypeListType;
import net.opengis.wfs.v_1_1_0.FeatureTypeType;
import net.opengis.wfs.v_1_1_0.GMLObjectTypeListType;
import net.opengis.wfs.v_1_1_0.GMLObjectTypeType;
import net.opengis.wfs.v_1_1_0.OutputFormatListType;
import net.opengis.wfs.v_1_1_0.WFSCapabilitiesType;

import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsProvider.WfsOpType;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.request.GetCapReq;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class Capabilities {

	protected String SERV_TYPE = "WFS";

	protected String SERV_TITLE = "EDEX WFS";

	protected String OWS_NS = OgcNamespace.OWS;

	protected String GML_NS = OgcNamespace.GML;

	protected String OGC_NS = OgcNamespace.OGC;

	protected String WFS_NS = OgcNamespace.WFS;

	protected String GML_MIME = "text/xml; subtype=gml/3.1.1";

	protected String[] gmlObjects = { "AbstractFeatureType", "PointType",
			"LineStringType", "PolygonType", "MultiPointType" };

	protected String[] geometryOperands = { "Envelope", "Point", "LineString",
			"Polygon" };

	protected SpatialOperatorNameType[] spatialOperators = {
			SpatialOperatorNameType.BBOX, SpatialOperatorNameType.EQUALS };

	protected ComparisonOperatorType[] comparisonOperators = {
			ComparisonOperatorType.LESS_THAN,
			ComparisonOperatorType.GREATER_THAN,
			ComparisonOperatorType.LESS_THAN_EQUAL_TO,
			ComparisonOperatorType.GREATER_THAN_EQUAL_TO,
			ComparisonOperatorType.EQUAL_TO,
			ComparisonOperatorType.NOT_EQUAL_TO };

	protected String[] logicOperators = { "And" };

	protected FeatureTranslator translator = new FeatureTranslator();

	protected WfsRegistryImpl registry;

	public Capabilities(WfsRegistryImpl registry) {
		this.registry = registry;
	}

	public WFSCapabilitiesType getCapabilities(GetCapReq request,
			OgcServiceInfo<WfsOpType> serviceinfo) throws WfsException {
		WFSCapabilitiesType cap = new WFSCapabilitiesType();
		cap.setServiceIdentification(getServiceId(serviceinfo));
		cap.setOperationsMetadata(getOpData(serviceinfo));
		cap.setFeatureTypeList(getFeatureTypes(request, serviceinfo));
		cap.setServesGMLObjectTypeList(getServesGML());
		cap.setSupportsGMLObjectTypeList(getSupportsGML());
		cap.setFilterCapabilities(getFilterCap());
		return cap;
	}

	/**
	 * @param serviceinfo
	 * @return
	 */
	protected OperationsMetadata getOpData(OgcServiceInfo<WfsOpType> serviceinfo) {
		OperationsMetadata rval = new OperationsMetadata();
		List<Operation> operations = new LinkedList<Operation>();
		for (OgcOperationInfo<WfsOpType> op : serviceinfo.getOperations()) {
			Operation to = new Operation();
			to.setName(op.getType().toString());
			to.setDCP(getDcpList(op));
			to.setParameter(getOpParams(op));
			operations.add(to);
		}
		rval.setOperation(operations);
		rval.setParameter(getParams(serviceinfo));
		// may want to do constraints as well
		return rval;
	}

	/**
	 * @param op
	 * @return
	 */
	private List<DomainType> getOpParams(OgcOperationInfo<WfsOpType> op) {
		return Arrays.asList(
				getAsDomainType("AcceptVersions", op.getVersions()),
				getAsDomainType("AcceptFormats", op.getFormats()));
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
		List<String> toVals = new ArrayList<String>(values.size());
		for (String val : values) {
			toVals.add(val);
		}
		rval.setValue(toVals);
		return rval;
	}

	/**
	 * @param serviceinfo
	 * @return
	 */
	protected List<DomainType> getParams(OgcServiceInfo<WfsOpType> serviceinfo) {
		// TODO this info should be passed in from somewhere
		return Arrays.asList(getAsDomainType("srsName",
				Arrays.asList("EPSG:4326")));
	}

	/**
	 * @return
	 */
	protected FilterCapabilities getFilterCap() {
		FilterCapabilities rval = new FilterCapabilities();
		rval.setScalarCapabilities(getScalarCapabilities());
		rval.setSpatialCapabilities(getSpatialCapabilities());
		rval.setIdCapabilities(getIdCapabilities());
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
		for (SpatialOperatorNameType name : spatialOperators) {
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
		List<QName> ops = new ArrayList<QName>(geometryOperands.length);
		for (String op : geometryOperands) {
			QName name = new QName(OgcNamespace.GML, op);
			ops.add(name);
		}
		rval.setGeometryOperand(ops);
		return rval;
	}

	/**
	 * @return
	 */
	protected ScalarCapabilitiesType getScalarCapabilities() {
		ScalarCapabilitiesType rval = new ScalarCapabilitiesType();
		rval.setArithmeticOperators(getArithmeticOperators());
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
		for (ComparisonOperatorType op : comparisonOperators) {
			ops.add(op);
		}
		rval.setComparisonOperator(ops);
		return rval;
	}

	/**
	 * @return
	 */
	protected ArithmeticOperatorsType getArithmeticOperators() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @return
	 */
	protected GMLObjectTypeListType getSupportsGML() {
		GMLObjectTypeListType rval = new GMLObjectTypeListType();
		List<GMLObjectTypeType> gmlObs = new ArrayList<GMLObjectTypeType>(
				gmlObjects.length);
		for (String type : gmlObjects) {
			gmlObs.add(getGMLObjType(type));
		}
		rval.setGMLObjectType(gmlObs);
		return rval;
	}

	protected GMLObjectTypeType getGMLObjType(String name) {
		GMLObjectTypeType rval = new GMLObjectTypeType();
		rval.setName(new QName(GML_NS, name));
		OutputFormatListType outFormats = new OutputFormatListType();
		outFormats.setFormat(Arrays.asList(GML_MIME));
		rval.setOutputFormats(outFormats);
		return rval;
	}

	/**
	 * @return
	 */
	protected GMLObjectTypeListType getServesGML() {
		// TODO Auto-generated method stub
		return null;
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
		rval.setTitle(SERV_TITLE);
		return rval;
	}
}
