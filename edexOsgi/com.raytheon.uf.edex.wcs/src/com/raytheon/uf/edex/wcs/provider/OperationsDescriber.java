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
 * May 9, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.provider;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.ows.v_1_1_0.AllowedValues;
import net.opengis.ows.v_1_1_0.DCP;
import net.opengis.ows.v_1_1_0.DomainType;
import net.opengis.ows.v_1_1_0.HTTP;
import net.opengis.ows.v_1_1_0.ObjectFactory;
import net.opengis.ows.v_1_1_0.Operation;
import net.opengis.ows.v_1_1_0.OperationsMetadata;
import net.opengis.ows.v_1_1_0.RequestMethodType;

import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wcs.provider.OgcWcsProvider.WcsOpType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class OperationsDescriber {

	protected ObjectFactory owsFactory = new ObjectFactory();

	public OperationsMetadata getOpData(OgcServiceInfo<WcsOpType> serviceinfo) {
		OperationsMetadata rval = new OperationsMetadata();
		List<Operation> operations = new LinkedList<Operation>();
		for (OgcOperationInfo<WcsOpType> op : serviceinfo.getOperations()) {
			Operation to = new Operation();
			to.setName(op.getType().toString());
			to.setDCP(getDcpList(op));
			to.setParameter(getOpParams(op));
			operations.add(to);
		}
		rval.setOperation(operations);
		rval.setParameter(getParams(serviceinfo));
		return rval;
	}

	protected List<DomainType> getParams(OgcServiceInfo<WcsOpType> serviceinfo) {
		List<DomainType> rval = new LinkedList<DomainType>();
		// TODO: this info should be passed in from somewhere
		DomainType parameter = new DomainType();
		parameter.setName("srsName");
		List<String> value = new LinkedList<String>();
		value.add("EPSG:4326");
		rval.add(parameter);
		return rval;
	}

	protected List<DCP> getDcpList(OgcOperationInfo<WcsOpType> op) {
		List<DCP> rval = new LinkedList<DCP>();
		DCP dcp = new DCP();
		HTTP http = new HTTP();
		List<JAXBElement<RequestMethodType>> value = new LinkedList<JAXBElement<RequestMethodType>>();
		if (op.hasHttpGet()) {
			RequestMethodType req = getRequestType(op.getHttpGetRes());
			value.add(owsFactory.createHTTPPost(req));
		}
		if (op.hasHttpPost()) {
			RequestMethodType req = getRequestType(op.getHttpPostRes());
			value.add(owsFactory.createHTTPPost(req));
		}
		http.setGetOrPost(value);
		dcp.setHTTP(http);
		rval.add(dcp);
		return rval;
	}

	protected RequestMethodType getRequestType(String value) {
		RequestMethodType rval = owsFactory.createRequestMethodType();
		rval.setHref(value);
		return rval;
	}

	private List<DomainType> getOpParams(OgcOperationInfo<WcsOpType> op) {

		List<DomainType> opParamList = new ArrayList<DomainType>();

		switch (op.getType()) {
		case GetCapabilities:
			opParamList = Arrays.asList(
					getAsDomainType("AcceptVersions", op.getVersions()),
					getAsDomainType("AcceptFormats", op.getFormats()),
					getAsDomainType("service", op.getServices()));
			break;
		case DescribeCoverage:
			opParamList = Arrays.asList(
					getAsDomainType("AcceptVersions", op.getVersions()),
					getAsDomainType("AcceptFormats", op.getFormats()),
					getAsDomainType("service", op.getServices()));// ,
			// getAsDomainType("Identifier", getLayerIdentifierList()));
			break;
		case GetCoverage:
			opParamList = Arrays
					.asList(getAsDomainType("AcceptVersions", op.getVersions()),
							getAsDomainType("AcceptFormats", op.getFormats()),
							getAsDomainType("service", op.getServices()),
							// getAsDomainType("Identifier",
							// getLayerIdentifierList()),
							getAsDomainType("InterpolationType",
									getInterpolationType()),
							getAsDomainType("format", getFormat()));
			break;
		default:

		}

		return opParamList;
	}

	protected List<String> getInterpolationType() {
		List<String> interpolationType = new ArrayList<String>();

		interpolationType.add("nearest");
		interpolationType.add("linear");

		return interpolationType;
	}

	protected List<String> getFormat() {
		List<String> format = new ArrayList<String>();

		format.add("IDataRecord");
		// format.add("image/tiff");
		// format.add("image/jpeg");
		// format.add("image/netcdf");

		return format;
	}

	protected DomainType getAsDomainType(String name, Collection<String> values) {
		DomainType rval = new DomainType();
		rval.setName(name);

		AllowedValues avs = new AllowedValues();

		List<Object> toVals = new LinkedList<Object>();
		for (String val : values) {
			toVals.add(val);
		}

		avs.setValueOrRange(toVals);
		rval.setAllowedValues(avs);

		return rval;
	}
}
