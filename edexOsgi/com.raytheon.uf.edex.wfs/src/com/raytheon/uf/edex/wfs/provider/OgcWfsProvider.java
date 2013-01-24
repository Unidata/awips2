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
 * Apr 22, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.provider;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.JAXBException;

import net.opengis.ows.v_1_0_0.ExceptionReport;
import net.opengis.ows.v_1_0_0.ExceptionType;
import net.opengis.wfs.v_1_1_0.DescribeFeatureTypeType;
import net.opengis.wfs.v_1_1_0.FeatureCollectionType;
import net.opengis.wfs.v_1_1_0.GetCapabilitiesType;
import net.opengis.wfs.v_1_1_0.GetFeatureType;
import net.opengis.wfs.v_1_1_0.ObjectFactory;
import net.opengis.wfs.v_1_1_0.TransactionResponseType;
import net.opengis.wfs.v_1_1_0.TransactionType;
import net.opengis.wfs.v_1_1_0.WFSCapabilitiesType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.opengis.feature.simple.SimpleFeature;
import org.springframework.context.ApplicationContext;

import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.ErrorType;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.WfsProvider;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.reg.WfsSource;
import com.raytheon.uf.edex.wfs.request.DescFeatureTypeReq;
import com.raytheon.uf.edex.wfs.request.GetCapReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq.ResultType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.raytheon.uf.edex.wfs.request.TransReq;
import com.raytheon.uf.edex.wfs.request.WfsRequest;
import com.raytheon.uf.edex.wfs.request.WfsRequest.Type;

public class OgcWfsProvider implements WfsProvider {

	protected String describeFeatureUrl;

	protected Log log = LogFactory.getLog(this.getClass());

	protected WfsRegistryImpl registry;

	protected Capabilities capabilities;

	protected FeatureFetcher features;

	protected Transactor transactor;

	protected ObjectFactory wfsFactory = new ObjectFactory();

	public OgcWfsProvider(WfsRegistryImpl registry) {
		this.capabilities = new Capabilities(registry);
		this.features = new FeatureFetcher(registry);
		this.registry = registry;
		this.transactor = new Transactor();
	}

	protected OgcWfsProvider() {
		// unit tests
	}

	@Override
	public OgcResponse getError(WfsException e, String exceptionFormat) {
		ExceptionType et = new ExceptionType();
		et.setExceptionCode(e.getCode().toString());
		et.setExceptionText(Arrays.asList(e.getMessage()));
		ExceptionReport report = new ExceptionReport();
		report.setException(Arrays.asList(et));
		String rval = "";
		String mimeType = OgcResponse.TEXT_XML_MIME;
		if (exceptionFormat.equalsIgnoreCase(OgcResponse.TEXT_XML_MIME)) {
			try {
				rval = registry.marshal(report);
				mimeType = OgcResponse.TEXT_XML_MIME;
			} catch (JAXBException e1) {
				log.error("Unable to marshal WFS response", e1);
				rval = "<ows:ExceptionReport version=\"1.0.0\"";
				rval += "xsi:schemaLocation=\"http://www.opengis.net/ows\"";
				rval += "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
						+ "xmlns:ows=\"http://www.opengis.net/ows\">";
				rval += "<ows:Exception exceptionCode=\"" + e.getCode() + "\">";
				rval += "<ows:ExceptionText>" + e.getMessage()
						+ "</ows:ExceptionText>";
				rval += "</ows:Exception></ows:ExceptionReport>";
				mimeType = OgcResponse.TEXT_XML_MIME;
			}
		} else if (exceptionFormat.equalsIgnoreCase(OgcResponse.TEXT_HTML_MIME)) {
			rval = "<html xmlns=\"http://www.w3.org/1999/xhtml\">";
			rval += "<br>An error occurred performing the request:<br>";
			rval += "<br>Error Code: " + e.getCode().toString();
			rval += "<br>Message: " + e.getMessage() + "</html>";
			mimeType = OgcResponse.TEXT_HTML_MIME;
		}
		OgcResponse resp = new OgcResponse(rval, mimeType, TYPE.TEXT);
		switch (e.getCode()) {
		case INTERNAL_SERVER_ERROR:
			resp.setError(ErrorType.INT_ERR);
			break;
		default:
			resp.setError(ErrorType.BAD_REQ);
		}
		return resp;
	}

	protected String getXml(InputStream in) throws IOException {
		return new java.util.Scanner(in).useDelimiter("\\A").next();
	}

	@Override
	public WfsRequest getRequest(InputStream in) {
		Object obj;
		WfsRequest rval;
		String xml;
		try {
			xml = getXml(in);
			obj = registry.unmarshal(xml);
		} catch (Exception e) {
			log.error("Unable to decode request", e);
			return getDecodeError(OgcResponse.TEXT_XML_MIME);
		}
		if (obj instanceof GetCapabilitiesType) {
			rval = new GetCapReq((GetCapabilitiesType) obj);
		} else if (obj instanceof GetFeatureType) {
			rval = new GetFeatureReq((GetFeatureType) obj);
		} else if (obj instanceof DescribeFeatureTypeType) {
			rval = new DescFeatureTypeReq((DescribeFeatureTypeType) obj);
		} else if (obj instanceof TransactionType) {
			rval = TransReq.buildTransReq((TransactionType) obj);
			if (rval == null) {
				rval = getDecodeError(OgcResponse.TEXT_XML_MIME);
			}
		} else {
			rval = getDecodeError(OgcResponse.TEXT_XML_MIME);
		}
		if (rval.getType() != Type.ERROR) {
			rval.setRawrequest(obj);
		}
		return rval;
	}

	public WfsRequest getDecodeError(String exceptionFormat) {
		OgcResponse error = getError(new WfsException(Code.INVALID_REQUEST,
				"Unable to decode request"), exceptionFormat);
		WfsRequest rval = new WfsRequest(Type.ERROR);
		rval.setRawrequest(error);
		return rval;
	}

	@Override
	public OgcResponse getCapabilities(GetCapReq request,
			OgcServiceInfo<WfsOpType> serviceinfo) {
		OgcResponse rval;
		try {
			WFSCapabilitiesType cap = capabilities.getCapabilities(request,
					serviceinfo);
			rval = marshalResponse(wfsFactory.createWFSCapabilities(cap));
		} catch (WfsException e) {
			rval = getError(e, request.getExceptionFormat());
		}
		return rval;
	}

	protected OgcResponse marshalResponse(Object jaxbobject) {
		OgcResponse rval;
		try {
			String xml = registry.marshal(jaxbobject);
			rval = new OgcResponse(xml, "text/gml; subtype=gml/3.1.1",
					TYPE.TEXT);
		} catch (JAXBException e) {
			log.error("Unable to marshal WFS response", e);
			// TODO: real error code
			rval = getError(new WfsException(Code.INVALID_REQUEST),
					OgcResponse.TEXT_XML_MIME);
		}
		return rval;
	}

	@Override
	public OgcResponse getFeature(GetFeatureReq request,
			OgcServiceInfo<WfsOpType> serviceinfo) {
		OgcResponse rval;
		FeatureCollectionType featColl;
		String format = request.getOutputformat();
		try {
			if (format.toLowerCase().contains("gml")
					|| format.toLowerCase().contains("xml")) {
				// use JAXB instead of gml simple feature formatter
				featColl = features.getFeatures(request, serviceinfo);
				rval = marshalResponse(wfsFactory
						.createFeatureCollection(featColl));
			} else {
				if (request.getResulttype() == ResultType.hits) {
					throw new WfsException(Code.INVALID_PARAMETER_VALUE,
							"Hits result not supported in format: " + format);
				}
				SimpleFeatureFormatter formatter = getFormatter(format);
				List<List<SimpleFeature>> res = features.getSimpleFeatures(
						request, serviceinfo);
				try {
					rval = formatter.format(res);
				} catch (Exception e) {
					log.error("Problem formatting features", e);
					throw new WfsException(Code.INTERNAL_SERVER_ERROR);
				}
			}
		} catch (WfsException e) {
			rval = getError(e, request.getExceptionFormat());
		}
		return rval;
	}

	protected SimpleFeatureFormatter getFormatter(String format)
			throws WfsException {
		ApplicationContext ctx = EDEXUtil.getSpringContext();
		String[] beans = ctx.getBeanNamesForType(SimpleFeatureFormatter.class);
		for (String bean : beans) {
			SimpleFeatureFormatter sff = (SimpleFeatureFormatter) ctx
					.getBean(bean);
			if (sff.matchesFormat(format)) {
				return sff;
			}
		}
		throw new WfsException(Code.INVALID_PARAMETER_VALUE,
				"Unsupported format: " + format);
	}

	@Override
	public OgcResponse transaction(TransReq request) {
		TransactionResponseType rval = transactor.transaction(request);
		return marshalResponse(wfsFactory.createTransactionResponse(rval));
	}

	@Override
	public OgcResponse describeFeatureType(DescFeatureTypeReq request,
			OgcServiceInfo<WfsOpType> serviceInfo) {
		OgcResponse rval;
		try {
			String xml = descFeatureInternal(request, serviceInfo);
			if (xml == null) {
				throw new WfsException(Code.INVALID_REQUEST,
						"Unknown type name(s)");
			}
			rval = new OgcResponse(xml, "text/xml; subtype=gml/3.1.1",
					TYPE.TEXT);
		} catch (WfsException e) {
			rval = getError(e, request.getExceptionFormat());
		}
		return rval;
	}

	public String descFeatureInternal(DescFeatureTypeReq request,
			OgcServiceInfo<WfsOpType> serviceInfo) throws WfsException {
		List<QualifiedName> typenames = request.getTypenames();
		String xml;
		if (typenames == null || typenames.size() == 0) {
			xml = getAllSchemas(serviceInfo);
		} else if (typenames.size() == 1) {
			xml = getOneSchema(typenames.get(0));
		} else {
			xml = getMergedSchemas(typenames, serviceInfo);
		}
		return xml;
	}

	// the following are desc feature type specific TODO move to separate object

	/**
	 * @param roles
	 * @param username
	 * @param typenames
	 * @return
	 * @throws WfsException
	 */
	protected String getMergedSchemas(List<QualifiedName> typenames,
			OgcServiceInfo<WfsOpType> serviceInfo) throws WfsException {
		int count = 0;
		boolean success = false;
		StringBuilder rval = new StringBuilder("<?xml version=\"1.0\" ?>\n");
		rval.append("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\">\n");
		for (QualifiedName name : typenames) {
			if (registry.getSource(name) == null) {
				continue;
			}
			success = true;
			if (name.getPrefix() == null) {
				name.setPrefix("ns" + count++);
			}
			rval.append("<import namespace=\"");
			rval.append(name.getNamespace());
			rval.append("\" schemaLocation=\"");
			rval.append(getDescUrl(serviceInfo));
			rval.append("&amp;namespace=");
			rval.append(name.getPrefix());
			rval.append(":");
			rval.append(name.getNamespace());
			rval.append("&amp;typeName=");
			rval.append(name.getPrefix());
			rval.append(":");
			rval.append(name.getName());
			rval.append("\"/>\n");
		}
		rval.append("</schema>");
		return success ? rval.toString() : null;
	}

	protected String getDescUrl(OgcServiceInfo<WfsOpType> serviceInfo) {
		if (describeFeatureUrl == null) {
			for (OgcOperationInfo<WfsOpType> opinfo : serviceInfo
					.getOperations()) {
				String res = parse(opinfo.getHttpGetRes());
				if (res != null) {
					describeFeatureUrl = res;
					break;
				}
			}
			if (describeFeatureUrl == null) {
				log.error("Unable to construct describe feature URL");
				describeFeatureUrl = "http://localhost:8085/wfs?request=describefeaturetype";
			}
		}
		return describeFeatureUrl;
	}

	/**
	 * @param httpGetRes
	 * @return
	 */
	private String parse(String target) {
		if (target == null) {
			return null;
		}
		if (target.toLowerCase().contains("describefeaturetype")) {
			return target;
		}
		return null;
	}

	/**
	 * @param roles
	 * @param username
	 * @param qualifiedName
	 * @return
	 * @throws WfsException
	 */
	protected String getOneSchema(QualifiedName feature) throws WfsException {
		WfsSource source = registry.getSource(feature);
		return source == null ? null : source.describeFeatureType(feature);
	}

	/**
	 * @param roles
	 * @param username
	 * @return
	 * @throws WfsException
	 */
	protected String getAllSchemas(OgcServiceInfo<WfsOpType> serviceInfo)
			throws WfsException {
		return getMergedSchemas(getFeatureNames(), serviceInfo);
	}

	public List<QualifiedName> getFeatureNames() {
		// no authorization since the results of this will be authorized
		// individually
		List<WfsFeatureType> features = registry.getFeatures();
		List<QualifiedName> rval = new ArrayList<QualifiedName>(features.size());
		for (WfsFeatureType f : features) {
			rval.add(f.getName());
		}
		return rval;
	}

}
