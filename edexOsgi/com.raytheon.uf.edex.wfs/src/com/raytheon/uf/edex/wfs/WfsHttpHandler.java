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
package com.raytheon.uf.edex.wfs;

import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcPrefix;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.feature.JsonFeatureFormatter;
import com.raytheon.uf.edex.ogc.common.feature.ShpFeatureFormatter;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpHandler;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpRequest;
import com.raytheon.uf.edex.ogc.common.output.OgcResponseOutput;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.WfsProvider.WfsOpType;
import com.raytheon.uf.edex.wfs.request.DescFeatureTypeReq;
import com.raytheon.uf.edex.wfs.request.FeatureQuery;
import com.raytheon.uf.edex.wfs.request.FeatureQuery.QFilterType;
import com.raytheon.uf.edex.wfs.request.GetCapReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq.ResultType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.raytheon.uf.edex.wfs.request.SortBy;
import com.raytheon.uf.edex.wfs.request.SortBy.Order;
import com.raytheon.uf.edex.wfs.request.TransReq;
import com.raytheon.uf.edex.wfs.request.WfsRequest;
import com.raytheon.uf.edex.wfs.request.WfsRequest.Type;

public class WfsHttpHandler implements OgcHttpHandler {

	public static final String REQUEST_HEADER = "request";

	public static final String CAP_PARAM = "getcapabilities";

	public static final String DESC_PARAM = "describefeaturetype";

	public static final String GET_PARAM = "getfeature";

	public static final String OUTFORMAT_HEADER = "outputformat";

	public static final String RESTYPE_HEADER = "resulttype";

	public static final String PROPNAME_HEADER = "propertyname";

	public static final String MAXFEAT_HEADER = "maxfeatures";

	public static final String SRSNAME_HEADER = "srsname";

	public static final String TYPENAME_HEADER = "typename";

	public static final String FEATID_HEADER = "featureid";

	public static final String FILTER_HEADER = "filter";

	public static final String BBOX_HEADER = "bbox";

	public static final String SORTBY_HEADER = "sortby";

	public static final String NS_HEADER = "namespace";

	protected Pattern nspattern = Pattern.compile("xmlns\\((\\S+)=(\\S+)\\)");

	protected WfsProvider provider;

	protected Log log = LogFactory.getLog(this.getClass());

	public WfsHttpHandler(WfsProvider provider) {
		this.provider = provider;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.ogc.common.http.OgcHttpHandler#handle(javax.servlet
	 * .http.HttpServletRequest, javax.servlet.http.HttpServletResponse,
	 * java.util.Map)
	 */
	@Override
	public void handle(OgcHttpRequest req) {
		try {
			handleInternal(req);
		} catch (Exception e) {
			log.error("Unable to handle request", e);
		}
	}

	protected void handleInternal(OgcHttpRequest req)
			throws Exception {
		OgcResponse rval = null;
		Map<String, Object> headers = req.getHeaders();
		HttpServletResponse response = req.getResponse();
		OgcServiceInfo<WfsOpType> serviceInfo = getServiceInfo(req.getRequest());
		WfsRequest request;
		if (req.isPost()) {
			InputStream is = req.getInputStream();
			request = provider.getRequest(is);
		} else {
			request = getRequestFromHeaders(headers);
		}

		rval = validateExceptionFormat(request);
		if (rval != null) {
			sendResponse(response, rval);
		}

		String username = (String) headers.get(USER_HEADER);
		String[] roles = (String[]) headers.get(ROLES_HEADER);
		request.setUsername(username);
		request.setRoles(roles);
		switch (request.getType()) {
		case DescribeFeature:
			rval = provider.describeFeatureType((DescFeatureTypeReq) request,
					serviceInfo);
			break;
		case GetCapabilities:
			rval = provider.getCapabilities((GetCapReq) request, serviceInfo);
			break;
		case GetFeature:
			rval = provider.getFeature((GetFeatureReq) request, serviceInfo);
			break;
		case Transaction:
			rval = provider.transaction((TransReq) request);
			break;
		case ERROR:
			rval = (OgcResponse) request.getRawrequest();
			break;
		}
		sendResponse(response, rval);
	}

	/**
	 * @param request
	 * @return
	 */
	private OgcResponse validateExceptionFormat(WfsRequest request) {
		if (!request.getExceptionFormat().equalsIgnoreCase(
				OgcResponse.TEXT_HTML_MIME)
				&& !request.getExceptionFormat().equalsIgnoreCase(
						OgcResponse.TEXT_XML_MIME)) {
			return provider.getError(new WfsException(
					Code.INVALID_PARAMETER_VALUE,
					"exceptions parameter invalid"), OgcResponse.TEXT_XML_MIME);
		}
		return null;
	}

	protected WfsRequest getRequestFromHeaders(Map<String, Object> headers) {
		WfsRequest rval = null;
		Object obj = headers.get(REQUEST_HEADER);
		if (obj instanceof String) {
			String req = (String) obj;
			if (req.equalsIgnoreCase(CAP_PARAM)) {
				rval = new GetCapReq();
			} else if (req.equalsIgnoreCase(DESC_PARAM)) {
				rval = buildDescFeatureReq(headers);
			} else if (req.equalsIgnoreCase(GET_PARAM)) {
				rval = buildGetFeatureReq(headers);
			}
		}
		String exceptionFormat = getHeader(EXCEP_FORMAT_HEADER, headers);
		if (exceptionFormat == null || exceptionFormat.isEmpty()) {
			exceptionFormat = OgcResponse.TEXT_XML_MIME;
		}
		if (rval == null) {
			OgcResponse error = provider.getError(new WfsException(
					Code.INVALID_REQUEST, "Unable to decode request"),
					exceptionFormat);
			rval = new WfsRequest(Type.ERROR);
			rval.setRawrequest(error);
		}

		return rval;
	}

	protected Map<String, String> getNameSpaceMap(Map<String, Object> headers) {
		Map<String, String> nsmap = new HashMap<String, String>();
		String ns = getHeader(NS_HEADER, headers);
		if (ns != null) {
			for (String s : splitOnComma(ns)) {
				Matcher matcher = nspattern.matcher(s);
				if (matcher.matches()) {
					nsmap.put(matcher.group(1), matcher.group(2));
				}
			}
		} else {
			nsmap.put(OgcPrefix.EDEX, OgcNamespace.EDEX);
		}
		return nsmap;
	}

	protected String[] splitOnComma(String str) {
		final String marker = "λλλ";
		String[] rval = str.replaceAll("\\\\,", marker).split(",");
		for (int i = 0; i < rval.length; ++i) {
			rval[i] = rval[i].replaceAll(marker, ",");
		}
		return rval;
	}

	/**
	 * @param headers
	 * @return
	 */
	protected GetFeatureReq buildGetFeatureReq(Map<String, Object> headers) {
		GetFeatureReq rval = new GetFeatureReq();
		String resType = getHeader(RESTYPE_HEADER, headers);
		if (resType != null) {
			ResultType valueOf = GetFeatureReq.ResultType.valueOf(resType);
			if (valueOf != null) {
				rval.setResulttype(valueOf);
			}
		}
		String max = getHeader(MAXFEAT_HEADER, headers);
		if (max != null) {
			try {
				rval.setMaxFeatures(Integer.parseInt(max));
			} catch (NumberFormatException e) {
				// ignore
			}
		}
		String outputformat = getHeader(OUTFORMAT_HEADER, headers);
		if (outputformat != null) {
			rval.setOutputformat(outputformat);
		}
		Map<String, String> nsmap = getNameSpaceMap(headers);
		String[] bboxes = splitOnParens(BBOX_HEADER, headers);
		String[] filters = splitOnParens(FILTER_HEADER, headers);
		String[] sorts = splitOnParens(SORTBY_HEADER, headers);
		String[] props = splitOnParens(PROPNAME_HEADER, headers);
		String[] srsnames = splitOnParens(SRSNAME_HEADER, headers);
		String[] types = getHeaderArr(TYPENAME_HEADER, headers);
		for (int i = 0; i < types.length; ++i) {
			FeatureQuery fq = new FeatureQuery();
			if (bboxes.length > 0) {
				fq.setFilter(getBoundingBox(bboxes[i]), QFilterType.BBOX);
			} else if (filters.length > 0) {
				fq.setFilter(filters[i], QFilterType.XML);
			}
			fq.setTypeNames(getTypeNames(types[i], nsmap));
			if (i < sorts.length) {
				fq.setSortBys(getSortBys(sorts[i]));
			}
			if (i < props.length) {
				fq.setPropertyNames(Arrays.asList(splitOnComma(props[i])));
			}
			if (i < srsnames.length) {
				fq.setSrsName(srsnames[i]);
			}
			rval.addQuery(fq);
		}
		return rval;
	}

	/**
	 * @param nsmap
	 * @param string
	 * @return
	 */
	protected List<QualifiedName> getTypeNames(String typename,
			Map<String, String> nsmap) {
		String[] parts = typename.split(":", 2);
		List<QualifiedName> rval = new LinkedList<QualifiedName>();
		if (parts.length == 1) {
			// default names to the edex namespace
			rval.add(new QualifiedName(OgcNamespace.EDEX, parts[0], null));
		} else if (parts.length == 2) {
			rval.add(new QualifiedName(nsmap.get(parts[0]), parts[1], parts[0]));
		}
		return rval;
	}

	protected String[] splitOnParens(String name, Map<String, Object> headers) {
		String val = getHeader(name, headers);
		String[] rval;
		if (val != null) {
			val = val.replaceAll("\\s*\\(", "");
			rval = val.split("\\)");
		} else {
			rval = new String[0];
		}
		return rval;
	}

	/**
	 * @param headers
	 * @return
	 */
	protected List<SortBy> getSortBys(String sortby) {
		List<SortBy> rval = new LinkedList<SortBy>();
		String[] sorts = splitOnComma(sortby);
		for (String s : sorts) {
			String[] parts = s.split("\\s+");
			SortBy.Order order = Order.Ascending;
			if (parts.length == 2) {
				if (parts[1].trim().equalsIgnoreCase("D")) {
					order = Order.Descending;
				}
			}
			rval.add(new SortBy(parts[0].trim(), order));
		}
		return rval;
	}

	/**
	 * @param bboxes
	 * @return
	 */
	protected OgcBoundingBox getBoundingBox(String bbox) {
		String[] parts = splitOnComma(bbox);
		OgcBoundingBox rval = null;
		if (parts.length == 4) {
			rval = new OgcBoundingBox();
			try {
				rval.setMinx(Double.parseDouble(parts[0]));
				rval.setMiny(Double.parseDouble(parts[1]));
				rval.setMaxx(Double.parseDouble(parts[2]));
				rval.setMaxy(Double.parseDouble(parts[3]));
			} catch (NumberFormatException e) {
				// ignore
			}
		}// else TODO handle non 2d WGS84
		return rval;
	}

	/**
	 * @param headers
	 * @return
	 */
	protected DescFeatureTypeReq buildDescFeatureReq(Map<String, Object> headers) {
		DescFeatureTypeReq rval = new DescFeatureTypeReq();
		String outputformat = getHeader(OUTFORMAT_HEADER, headers);
		if (outputformat != null) {
			rval.setOutputformat(outputformat);
		}
		String typename = getHeader(TYPENAME_HEADER, headers);
		if (typename != null) {
			Map<String, String> nsmap = getNameSpaceMap(headers);
			rval.setTypenames(getTypeNames(typename, nsmap));
		}
		return rval;
	}

	protected String getHeader(String name, Map<String, Object> headers) {
		Object obj = headers.get(name);
		String rval = null;
		if (obj != null && obj instanceof String) {
			rval = (String) obj;
		}
		return rval;
	}

	protected String[] getHeaderArr(String name, Map<String, Object> headers) {
		String[] rval;
		String value = getHeader(name, headers);
		if (value != null) {
			rval = splitOnComma(value);
		} else {
			rval = new String[0];
		}
		return rval;
	}

	/**
	 * @param httpRequest
	 * @return
	 */
	protected OgcServiceInfo<WfsOpType> getServiceInfo(
			HttpServletRequest request) {
		return getServiceInfo(request.getServerName(), request.getServerPort());
	}

	public OgcServiceInfo<WfsOpType> getServiceInfo(String host, int port) {
		String base = "http://" + host;
		if (port != 80) {
			base += ":" + port;
		}
		base += "/wfs";
		OgcServiceInfo<WfsOpType> rval = new OgcServiceInfo<WfsOpType>(base);
		String getCapGet = base + "?request=" + CAP_PARAM;
		String getFeatureGet = base + "?request=" + GET_PARAM;
		String descFeatureGet = base + "?request=" + DESC_PARAM;
		rval.addOperationInfo(getOp(getCapGet, base, WfsOpType.GetCapabilities));
		rval.addOperationInfo(getOp(descFeatureGet, base,
				WfsOpType.DescribeFeatureType));
		OgcOperationInfo<WfsOpType> getFeat = getOp(getFeatureGet, base, WfsOpType.GetFeature);
		getFeat.addFormat(ShpFeatureFormatter.mimeType);
		getFeat.addFormat(JsonFeatureFormatter.mimeType);
		rval.addOperationInfo(getFeat);
		return rval;
	}

	protected OgcOperationInfo<WfsOpType> getOp(String get, String post,
			WfsOpType type) {
		OgcOperationInfo<WfsOpType> rval = new OgcOperationInfo<WfsOpType>(type);
		rval.setHttpGetRes(get);
		rval.setHttpPostRes(post);
		rval.addVersion("1.1.0");
		rval.addFormat("text/xml");
		return rval;
	}

	protected void sendResponse(HttpServletResponse httpRes,
			OgcResponse response) throws Exception {
		OgcResponseOutput.output(response, httpRes);
	}

	public static void main(String[] args) {
		String foo = "(InWaterA_1M/wkbGeom,InWaterA_1M/tileId) (InWaterA_1M/wkbGeom,InWaterA_1M/tileId)(InWaterA_1M/wkbGeom,InWaterA_1M/tileId)";
		foo = foo.replaceAll("\\s*\\(", "");
		for (String s : foo.split("\\)")) {
			System.out.println("line: " + s);
		}
	}

}