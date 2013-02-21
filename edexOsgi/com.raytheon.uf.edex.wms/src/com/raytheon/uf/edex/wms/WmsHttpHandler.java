/**********************************************************************
 *
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
 **********************************************************************/
package com.raytheon.uf.edex.wms;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.collections.map.CaseInsensitiveMap;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.styling.StyledLayerDescriptor;

import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.feature.JsonFeatureFormatter;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpHandler;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpRequest;
import com.raytheon.uf.edex.ogc.common.output.OgcResponseOutput;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.WmsProvider.WmsOpType;
import com.raytheon.uf.edex.wms.format.GmlFeatureFormatter;
import com.raytheon.uf.edex.wms.format.HtmlFeatureFormatter;
import com.raytheon.uf.edex.wms.sld.SldParser;
import com.raytheon.uf.edex.wms.sld.SldParserRegistry;
import com.vividsolutions.jts.geom.Envelope;

public class WmsHttpHandler implements OgcHttpHandler {

	public static final String REQUEST_HEADER = "request";

	public static final String SLD_HEADER = "sld";

	public static final String SLD_VERSION_HEADER = "sld_version";

	public static final String SLD_BODY_HEADER = "sld_body";

	public static final String VERSION_HEADER = "version";

	public static final String FORMAT_HEADER = "format";

	public static final String UPDATESEQ_HEADER = "updatesequence";

	public static final String LAYERS_HEADER = "layers";

	public static final String STYLES_HEADER = "styles";

	public static final String CRS_HEADER = "crs";

	public static final String BBOX_HEADER = "bbox";

	public static final String WIDTH_HEADER = "width";

	public static final String HEIGHT_HEADER = "height";

	public static final String TRANSPARENT_HEADER = "transparent";

	public static final String BGCOLOR_HEADER = "bgcolor";

	public static final String TIME_HEADER = "time";

	public static final String ELEVATION_HEADER = "elevation";

	public static final String MIME_HEADER = "Content-Type";

	public static final String QLAYERS_HEADER = "query_layers";

	public static final String FCOUNT_HEADER = "feature_count";

	public static final String IFORMAT_HEADER = "info_format";

	public static final String I_HEADER = "i";

	public static final String J_HEADER = "j";

	public static final String LAYER_HEADER = "layer";

	public static final String STYLE_HEADER = "style";

	public static final String RULE_HEADER = "rule";

	public static final String SCALE_HEADER = "scale";

	public static final String FEATURE_TYPE_HEADER = "feature_type";

	protected static final String CAP_PARAM = "getcapabilities";

	protected static final String MAP_PARAM = "getmap";

	protected static final String FEAT_PARAM = "getfeatureinfo";

	protected static final String LEG_PARAM = "getlegendgraphic";

	protected WmsProvider provider;

	protected Log log = LogFactory.getLog(this.getClass());

	protected SldParserRegistry sldRegistry;

	public WmsHttpHandler(WmsProvider provider, SldParserRegistry sldRegistry) {
		this.provider = provider;
		this.sldRegistry = sldRegistry;
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
	public void handle(OgcHttpRequest request) {
		try {
			handleInternal(request);
		} catch (Exception e) {
			log.error("Unable to handle request", e);
		}
	}

	protected void handleInternal(OgcHttpRequest ogcRequest) throws Exception {
		HttpServletResponse response = ogcRequest.getResponse();
		if (ogcRequest.isPost()) {
			InputStream is = ogcRequest.getInputStream();
			sendResponse(response, provider.handlePost(is));
			return;
		}
		Map<String, Object> headers = ogcRequest.getHeaders();
		String exceptionFormat = getString(headers.get(EXCEP_FORMAT_HEADER));
		if (exceptionFormat == null || exceptionFormat.isEmpty()) {
			exceptionFormat = OgcResponse.TEXT_XML_MIME;
		}
		OgcResponse rval;

		rval = validateExceptionFormat(exceptionFormat);
		if (rval != null) {
			sendResponse(response, rval);
		}

		Object obj = headers.get(REQUEST_HEADER);
		if (obj instanceof String) {
			String reqName = (String) obj;
			rval = getResponse(reqName, ogcRequest.getRequest(), headers);
		} else {
			rval = handleError(new WmsException(Code.MissingParameterValue,
					"Missing parameter: " + REQUEST_HEADER), exceptionFormat);
		}
		sendResponse(response, rval);
	}

	/**
	 * @param exceptionFormat
	 * @return
	 */
	private OgcResponse validateExceptionFormat(String exceptionFormat) {
		WmsException ex = validateExceptionFormatAsException(exceptionFormat);
		if (ex != null) {
			return provider.getError(ex, OgcResponse.TEXT_XML_MIME);
		}
		return null;
	}

	private WmsException validateExceptionFormatAsException(
			String exceptionFormat) {
		if (!exceptionFormat.equalsIgnoreCase(OgcResponse.TEXT_HTML_MIME)
				&& !exceptionFormat.equalsIgnoreCase(OgcResponse.TEXT_XML_MIME)
				&& !exceptionFormat
						.equalsIgnoreCase(OgcResponse.APP_VND_OGC_SE_XML)) {
			new WmsException(Code.InvalidParameterValue,
					"exceptions parameter invalid");
		}
		return null;
	}

	protected OgcResponse getResponse(String reqName,
			HttpServletRequest request, Map<String, Object> headers) {
		BaseRequest<WmsOpType> req;
		OgcResponse rval;
		String exceptionFormat = getString(headers.get(EXCEP_FORMAT_HEADER));
		if (exceptionFormat == null || exceptionFormat.isEmpty()) {
			exceptionFormat = OgcResponse.TEXT_XML_MIME;
		}
		try {
			req = getRequest(reqName, request, headers);
			if (req != null) {
				req.setVersion(getString(headers.get(VERSION_HEADER)));
				req.setFormat(getString(headers.get(FORMAT_HEADER)));
				req.setUpdateSequence(getString(headers.get(UPDATESEQ_HEADER)));
				req.setUserName(getString(headers.get(USER_HEADER)));
				req.setRoles(getStringArr(headers.get(ROLES_HEADER)));
				req.setExceptionFormat(exceptionFormat);
				rval = req.execute(provider);
			} else {
				throw new WmsException(Code.OperationNotSupported,
						"No such operation: " + reqName);
			}
		} catch (WmsException e) {
			rval = handleError(e, exceptionFormat);
		}
		return rval;
	}

	protected BaseRequest<WmsOpType> getRequest(String reqName,
			HttpServletRequest request, Map<String, Object> headers)
			throws WmsException {
		BaseRequest<WmsOpType> req = null;
		if (reqName.equalsIgnoreCase(CAP_PARAM)) {
			req = getBaseRequest(request, headers);
		} else if (reqName.equalsIgnoreCase(MAP_PARAM)) {
			req = parseMapRequest(headers);
		} else if (reqName.equalsIgnoreCase(FEAT_PARAM)) {
			req = getFeatureInfoReq(headers);
		} else if (reqName.equalsIgnoreCase(LEG_PARAM)) {
			req = getLegendGraphicReq(headers);
		}

		return req;
	}

	protected BaseRequest<WmsOpType> getBaseRequest(HttpServletRequest request,
			Map<String, Object> headers) {
		OgcServiceInfo<WmsOpType> serviceInfo = getServiceInfo(request);
		BaseRequest<WmsOpType> req = new BaseRequest<WmsOpType>();
		req.setServiceinfo(serviceInfo);
		return req;
	}

	/**
	 * @param request
	 */
	private OgcServiceInfo<WmsOpType> getServiceInfo(HttpServletRequest request) {
		int port = request.getServerPort();
		String base = "http://" + request.getServerName();
		if (port != 80) {
			base += ":" + port;
		}
		base += "/wms";
		OgcServiceInfo<WmsOpType> rval = new OgcServiceInfo<WmsOpType>(base);

		OgcOperationInfo<WmsOpType> cap = new OgcOperationInfo<WmsOpType>(
				WmsOpType.GetCapabilities);
		cap.setHttpGetRes(base + "?request=" + CAP_PARAM);
		cap.addFormat("text/xml");
		rval.addOperationInfo(cap);

		OgcOperationInfo<WmsOpType> map = new OgcOperationInfo<WmsOpType>(
				WmsOpType.GetMap);
		map.setHttpGetRes(base + "?request=" + MAP_PARAM);
		map.addFormat("image/gif");
		map.addFormat("image/png");
		// map.addFormat("image/tiff");
		map.addFormat("image/jpeg");
		rval.addOperationInfo(map);

		OgcOperationInfo<WmsOpType> info = new OgcOperationInfo<WmsProvider.WmsOpType>(
				WmsOpType.GetFeatureInfo);
		info.setHttpGetRes(base + "?request=" + FEAT_PARAM);
		List<String> formats = Arrays.asList(GmlFeatureFormatter.mimeType,
				JsonFeatureFormatter.mimeType, HtmlFeatureFormatter.mimeType);
		info.setFormats(formats);
		rval.addOperationInfo(info);

		OgcOperationInfo<WmsOpType> legend = new OgcOperationInfo<WmsProvider.WmsOpType>(
				WmsOpType.GetLegendGraphic);
		legend.setHttpGetRes(base + "?request=" + LEG_PARAM);
		legend.addFormat("image/gif");
		legend.addFormat("image/png");
		legend.addFormat("image/jpeg");
		rval.addOperationInfo(legend);

		return rval;
	}

	/**
	 * @param headers
	 * @return
	 * @throws WmsException
	 */
	protected GetFeatureInfoRequest getFeatureInfoReq(
			Map<String, Object> headers) throws WmsException {
		// TODO lookup provider based on version
		// String version = getString(headers.get(VERSION_HEADER));
		GetMapRequest mapReq = parseMapRequest(headers);

		String[] layers = getStringArr(headers.get(QLAYERS_HEADER));
		String format = getString(headers.get(IFORMAT_HEADER));
		Integer i = getInt(headers.get(I_HEADER));
		Integer j = getInt(headers.get(J_HEADER));
		String exFormat = getString(headers.get(EXCEP_FORMAT_HEADER));
		GetFeatureInfoRequest req = new GetFeatureInfoRequest(mapReq, layers,
				i, j, format);
		Integer count = getInt(headers.get(FCOUNT_HEADER));
		if (count != null) {
			req.setFeatureCount(count);
		}
		req.setExceptionFormat(exFormat);
		return req;
	}

	protected GetLegendGraphicRequest getLegendGraphicReq(
			Map<String, Object> headers) throws WmsException {
		String layer = getString(headers.get(LAYER_HEADER));
		String style = getString(headers.get(STYLE_HEADER));
		Integer width = getInt(headers.get(WIDTH_HEADER));
		Integer height = getInt(headers.get(HEIGHT_HEADER));
		String time = getString(headers.get(TIME_HEADER));
		String elevation = getString(headers.get(ELEVATION_HEADER));
		Map<String, String> dimensions = getDimensions(headers);
		StyledLayerDescriptor sld = getSLD(headers);
		String rule = getString(headers.get(RULE_HEADER));
		String scale = getString(headers.get(SCALE_HEADER));
		String featureType = getString(headers.get(FEATURE_TYPE_HEADER));
		String bgcolor = getString(headers.get(BGCOLOR_HEADER));
		Boolean transparent = getBool(headers.get(TRANSPARENT_HEADER));
		GetLegendGraphicRequest req = new GetLegendGraphicRequest(layer, style,
				width, height, time, elevation, dimensions, sld, rule, scale,
				featureType);
		req.setTransparent(transparent);
		req.setBgcolor(bgcolor);
		return req;
	}

	protected GetMapRequest parseMapRequest(Map<String, Object> headers)
			throws WmsException {
		String[] layers = getStringArr(headers.get(LAYERS_HEADER));
		String[] styles = getStringArr(headers.get(STYLES_HEADER));
		if (styles == null) {
			throw new WmsException(Code.MissingParameterValue,
					"style parameter not in request.");
		}

		String crs = getString(headers.get(CRS_HEADER));
		String bbox = getBbox(headers.get(BBOX_HEADER));
		Integer width = getInt(headers.get(WIDTH_HEADER));
		Integer height = getInt(headers.get(HEIGHT_HEADER));
		// String format = getString(headers.get(FORMAT_HEADER));
		Boolean transparent = getBool(headers.get(TRANSPARENT_HEADER));
		String bgcolor = getString(headers.get(BGCOLOR_HEADER));
		// String exceptionFormat = getString(headers.get(EXCEP_FORMAT_HEADER));
		String time = getString(headers.get(TIME_HEADER));
		String elevation = getString(headers.get(ELEVATION_HEADER));
		StyledLayerDescriptor sld = getSLD(headers);
		Map<String, String> dimensions = getDimensions(headers);
		return new GetMapRequest(layers, styles, crs, bbox, width, height,
				transparent, bgcolor, time, elevation, dimensions, sld);
	}

	protected StyledLayerDescriptor getSLD(Map<String, Object> headers)
			throws WmsException {
		StyledLayerDescriptor rval = null;
		String urlStr = getString(headers.get(SLD_HEADER));
		InputStream body = null;
		if (urlStr != null) {
			try {
				URL url = new URL(urlStr);
				body = url.openStream();
			} catch (Exception e) {
				String msg = "Unable to open SLD at " + urlStr;
				log.error(msg, e);
				throw new WmsException(Code.InvalidParameterValue, msg);
			}
		} else if (headers.containsKey(SLD_BODY_HEADER)) {
			String str = getString(headers.get(SLD_BODY_HEADER));
			body = new ByteArrayInputStream(str.getBytes(Charset
					.forName("UTF-8")));
		}
		if (body != null) {
			String version = getString(headers.get(SLD_VERSION_HEADER));
			rval = parseSldXml(version, body);
		}
		return rval;
	}

	protected StyledLayerDescriptor parseSldXml(String version, InputStream body)
			throws WmsException {
		SldParser parser = sldRegistry.getParser(version);
		if (parser == null) {
			throw new WmsException(Code.InvalidParameterValue,
					"Missing or unknown SLD version");
		}
		try {
			return parser.parse(body);
		} catch (Throwable t) {
			String msg = "Unable to parse SLD";
			log.error(msg, t);
			throw new WmsException(Code.InvalidParameterValue, msg);
		}
	}

	@SuppressWarnings("unchecked")
	protected Map<String, String> getDimensions(Map<String, Object> headers) {
		Map<String, String> rval = new CaseInsensitiveMap();
		for (String key : headers.keySet()) {
			if (key.toLowerCase().startsWith("dim_")) {
				String dim = key.substring(4);
				rval.put(dim, (String) headers.get(key));
			}
		}
		return rval;
	}

	/**
	 * @param object
	 * @return
	 */
	protected String getBbox(Object object) {
		String rval = null;
		if (object instanceof String) {
			rval = (String) object;
		} else if (object instanceof Envelope) {
			Envelope env = (Envelope) object;
			rval = String.format("%f,%f,%f,%f", env.getMinX(), env.getMinY(),
					env.getMaxX(), env.getMinY());
		}
		return rval;
	}

	protected OgcResponse handleError(WmsException e, String exceptionFormat) {
		return provider.getError(e, exceptionFormat);
	}

	protected String getString(Object obj) {
		String rval = null;
		if (obj != null) {
			if (obj instanceof String) {
				rval = (String) obj;
			}
		}
		return rval;
	}

	protected String[] getStringArr(Object obj) {
		String[] rval = null;
		if (obj != null) {
			if (obj instanceof String[]) {
				rval = (String[]) obj;
			} else if (obj instanceof String) {
				rval = ((String) obj).split(",");
			}
		}
		return rval;
	}

	protected Integer getInt(Object obj) {
		Integer rval = null;
		if (obj != null) {
			if (obj instanceof Integer) {
				rval = (Integer) obj;
			} else if (obj instanceof String) {
				rval = getInt((String) obj);
			}
		}
		return rval;
	}

	protected Integer getInt(String str) {
		Integer rval = null;
		try {
			rval = Integer.parseInt(str);
		} catch (Exception e) {
			// leave rval as null
		}
		return rval;
	}

	protected Boolean getBool(Object obj) {
		Boolean rval = null;
		if (obj != null) {
			if (obj instanceof Boolean) {
				rval = (Boolean) obj;
			} else if (obj instanceof String) {
				try {
					rval = Boolean.parseBoolean((String) obj);
				} catch (Exception e) {
					// leave rval as null
				}
			}
		}
		return rval;
	}

	protected void sendResponse(HttpServletResponse httpRes,
			OgcResponse response) throws Exception {
		try {
			OgcResponseOutput.output(response, httpRes);
		} catch (OgcException e) {
			OgcResponse error = handleError(new WmsException(e), null);
			OgcResponseOutput.output(error, httpRes);
		}
	}

}
