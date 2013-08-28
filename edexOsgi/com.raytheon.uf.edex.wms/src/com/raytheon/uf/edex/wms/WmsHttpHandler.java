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

import org.geotools.styling.StyledLayerDescriptor;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.feature.JsonFeatureFormatter;
import com.raytheon.uf.edex.ogc.common.http.MimeType;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpHandler;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpRequest;
import com.raytheon.uf.edex.ogc.common.output.IOgcHttpResponse;
import com.raytheon.uf.edex.ogc.common.output.ServletOgcResponse;
import com.raytheon.uf.edex.wms.IWmsProvider.WmsOpType;
import com.raytheon.uf.edex.wms.format.GmlFeatureFormatter;
import com.raytheon.uf.edex.wms.format.HtmlFeatureFormatter;
import com.raytheon.uf.edex.wms.sld.SldParser;
import com.raytheon.uf.edex.wms.sld.SldParserRegistry;
import com.vividsolutions.jts.geom.Envelope;

public class WmsHttpHandler extends OgcHttpHandler {

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

    public static final String CAP_PARAM = "getcapabilities";

    public static final String MAP_PARAM = "getmap";

    public static final String FEAT_PARAM = "getfeatureinfo";

    public static final String LEG_PARAM = "getlegendgraphic";

    protected IWmsProvider provider;

    protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    protected SldParserRegistry sldRegistry;

    public WmsHttpHandler(IWmsProvider provider, SldParserRegistry sldRegistry) {
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
        IOgcHttpResponse response = new ServletOgcResponse(
                ogcRequest.getResponse());
        if (ogcRequest.isPost()) {
            InputStream is = ogcRequest.getInputStream();
            sendResponse(response, provider.handlePost(is));
            return;
        }
        MimeType exceptionFormat = OgcResponse.TEXT_XML_MIME;
        OgcResponse rval;
        try {
            Map<String, Object> headers = ogcRequest.getHeaders();
            String exFormatStr = getString(headers, EXCEP_FORMAT_HEADER);
            if (exFormatStr != null && !exFormatStr.isEmpty()) {
                exceptionFormat = new MimeType(exFormatStr);
            }

            rval = validateExceptionFormat(exceptionFormat);
            if (rval != null) {
                sendResponse(response, rval);
            }

            String reqName = getString(headers, REQUEST_HEADER);
            if (reqName == null) {
                throw new OgcException(Code.MissingParameterValue,
                        "Missing parameter: " + REQUEST_HEADER);
            }
            rval = getResponse(reqName, ogcRequest.getRequest(), headers);
        } catch (OgcException e) {
            rval = handleError(e, exceptionFormat);
        }

        sendResponse(response, rval);
    }

    /**
     * @param exceptionFormat
     * @return
     */
    private OgcResponse validateExceptionFormat(MimeType exceptionFormat) {
        WmsException ex = validateExceptionFormatAsException(exceptionFormat);
        if (ex != null) {
            return provider.getError(ex, OgcResponse.TEXT_XML_MIME);
        }
        return null;
    }

    private WmsException validateExceptionFormatAsException(
            MimeType exceptionFormat) {
        if (!exceptionFormat.equalsIgnoreParams(OgcResponse.TEXT_HTML_MIME)
                && !exceptionFormat
                        .equalsIgnoreParams(OgcResponse.TEXT_XML_MIME)
                && !exceptionFormat
                        .equalsIgnoreParams(OgcResponse.APP_VND_OGC_SE_XML)) {
            return new WmsException(WmsException.Code.InvalidParameterValue,
                    "exceptions parameter invalid");
        }
        return null;
    }

    protected OgcResponse getResponse(String reqName,
            HttpServletRequest request, Map<String, Object> headers) {
        BaseRequest<WmsOpType> req;
        OgcResponse rval;
        MimeType exceptionFormat = OgcResponse.TEXT_XML_MIME;
        try {
            exceptionFormat = getMimeType(headers, EXCEP_FORMAT_HEADER);
            if (exceptionFormat == null) {
                exceptionFormat = OgcResponse.TEXT_XML_MIME;
            }
            req = getRequest(reqName, request, headers);
            if (req != null) {
                req.setVersion(getString(headers, VERSION_HEADER));
                req.setFormat(getMimeType(headers, FORMAT_HEADER));
                req.setUpdateSequence(getString(headers, UPDATESEQ_HEADER));
                req.setUserName(getString(headers, USER_HEADER));
                req.setRoles(getStringArr(headers, ROLES_HEADER));
                req.setExceptionFormat(exceptionFormat);
                rval = req.execute(provider);
            } else {
                throw new OgcException(Code.OperationNotSupported,
                        "No such operation: " + reqName);
            }
        } catch (OgcException e) {
            rval = handleError(e, exceptionFormat);
        }
        return rval;
    }

    protected BaseRequest<WmsOpType> getRequest(String reqName,
            HttpServletRequest request, Map<String, Object> headers)
            throws OgcException {
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
        base += "/wms?service=wms";
        OgcServiceInfo<WmsOpType> rval = new OgcServiceInfo<WmsOpType>(base);

        OgcOperationInfo<WmsOpType> cap = new OgcOperationInfo<WmsOpType>(
                WmsOpType.GetCapabilities);
        cap.setHttpGetRes(base);
        cap.addFormat("text/xml");
        rval.addOperationInfo(cap);

        OgcOperationInfo<WmsOpType> map = new OgcOperationInfo<WmsOpType>(
                WmsOpType.GetMap);
        map.setHttpGetRes(base);
        map.addFormat("image/gif");
        map.addFormat("image/png");
        // map.addFormat("image/tiff");
        map.addFormat("image/jpeg");
        rval.addOperationInfo(map);

        OgcOperationInfo<WmsOpType> info = new OgcOperationInfo<IWmsProvider.WmsOpType>(
                WmsOpType.GetFeatureInfo);
        info.setHttpGetRes(base);
        List<String> formats = Arrays.asList(
                GmlFeatureFormatter.mimeType.toString(),
                JsonFeatureFormatter.mimeType.toString(),
                HtmlFeatureFormatter.mimeType.toString());
        info.setFormats(formats);
        rval.addOperationInfo(info);

        OgcOperationInfo<WmsOpType> legend = new OgcOperationInfo<IWmsProvider.WmsOpType>(
                WmsOpType.GetLegendGraphic);
        legend.setHttpGetRes(base);
        legend.addFormat("image/gif");
        legend.addFormat("image/png");
        legend.addFormat("image/jpeg");
        rval.addOperationInfo(legend);

        return rval;
    }

    /**
     * @param headers
     * @return
     * @throws OgcException
     */
    protected GetFeatureInfoRequest getFeatureInfoReq(
            Map<String, Object> headers) throws OgcException {
        // TODO lookup provider based on version
        // String version = getString(headers, VERSION_HEADER);
        GetMapRequest mapReq = parseMapRequest(headers);

        String[] layers = getStringArr(headers, QLAYERS_HEADER);
        MimeType format = getMimeType(headers, IFORMAT_HEADER);
        Integer i = getInt(headers, I_HEADER);
        Integer j = getInt(headers, J_HEADER);
        MimeType exFormat = getMimeType(headers, EXCEP_FORMAT_HEADER);
        GetFeatureInfoRequest req = new GetFeatureInfoRequest(mapReq, layers,
                i, j, format);
        Integer count = getInt(headers, FCOUNT_HEADER);
        if (count != null) {
            req.setFeatureCount(count);
        }
        req.setExceptionFormat(exFormat);
        return req;
    }

    protected GetLegendGraphicRequest getLegendGraphicReq(
            Map<String, Object> headers) throws OgcException {
        String layer = getString(headers, LAYER_HEADER);
        String style = getString(headers, STYLE_HEADER);
        Integer width = getInt(headers, WIDTH_HEADER);
        Integer height = getInt(headers, HEIGHT_HEADER);
        String time = getString(headers, TIME_HEADER);
        String elevation = getString(headers, ELEVATION_HEADER);
        Map<String, String> dimensions = getDimensions(headers);
        StyledLayerDescriptor sld = getSLD(headers);
        String rule = getString(headers, RULE_HEADER);
        String scale = getString(headers, SCALE_HEADER);
        String featureType = getString(headers, FEATURE_TYPE_HEADER);
        String bgcolor = getString(headers, BGCOLOR_HEADER);
        Boolean transparent = getBool(headers, TRANSPARENT_HEADER);
        GetLegendGraphicRequest req = new GetLegendGraphicRequest(layer, style,
                width, height, time, elevation, dimensions, sld, rule, scale,
                featureType);
        req.setTransparent(transparent);
        req.setBgcolor(bgcolor);
        return req;
    }

    protected GetMapRequest parseMapRequest(Map<String, Object> headers)
            throws OgcException {
        String[] layers = getStringArr(headers, LAYERS_HEADER);
        String[] styles = getStringArr(headers, STYLES_HEADER);
        if (styles == null) {
            throw new OgcException(Code.MissingParameterValue,
                    "style parameter not in request.");
        }

        String crs = getString(headers, CRS_HEADER);
        String bbox = getBbox(headers, BBOX_HEADER);
        Integer width = getInt(headers, WIDTH_HEADER);
        Integer height = getInt(headers, HEIGHT_HEADER);
        // String format = getString(headers, FORMAT_HEADER);
        Boolean transparent = getBool(headers, TRANSPARENT_HEADER);
        String bgcolor = getString(headers, BGCOLOR_HEADER);
        // String exceptionFormat = getString(headers, EXCEP_FORMAT_HEADER);
        String time = getString(headers, TIME_HEADER);
        String[] times = parseTimes(time, layers.length);
        String elevation = getString(headers, ELEVATION_HEADER);
        StyledLayerDescriptor sld = getSLD(headers);
        Map<String, String> dimensions = getDimensions(headers);
        return new GetMapRequest(layers, styles, crs, bbox, width, height,
                transparent, bgcolor, times, elevation, dimensions, sld);
    }

    protected String[] parseTimes(String timeStr, int layerNum)
            throws OgcException {
        if (timeStr == null || timeStr.isEmpty()) {
            return new String[layerNum];
        }
        String[] parts = timeStr.split(",", -1);
        String[] rval;
        if (parts.length == layerNum) {
            rval = copyWithoutEmpties(parts);
        } else if (parts.length == 1) {
            rval = new String[layerNum];
            for (int i = 0; i < rval.length; ++i) {
                rval[i] = parts[0];
            }
        } else {
            throw new OgcException(Code.InvalidParameterValue,
                    "invalid time string");
        }
        return rval;
    }

    /**
     * @param arr
     * @return copy of array where empty strings are replace with nulls
     */
    protected String[] copyWithoutEmpties(String[] arr) {
        String[] rval = new String[arr.length];
        for (int i = 0; i < arr.length; ++i) {
            if (!arr[i].trim().isEmpty()) {
                rval[i] = arr[i];
            }
        }
        return rval;
    }

    protected StyledLayerDescriptor getSLD(Map<String, Object> headers)
            throws OgcException {
        StyledLayerDescriptor rval = null;
        String urlStr = getString(headers, SLD_HEADER);
        InputStream body = null;
        if (urlStr != null) {
            try {
                URL url = new URL(urlStr);
                body = url.openStream();
            } catch (Exception e) {
                String msg = "Unable to open SLD at " + urlStr;
                log.error(msg, e);
                throw new OgcException(Code.InvalidParameterValue, msg);
            }
        } else if (headers.containsKey(SLD_BODY_HEADER)) {
            String str = getString(headers, SLD_BODY_HEADER);
            body = new ByteArrayInputStream(str.getBytes(Charset
                    .forName("UTF-8")));
        }
        if (body != null) {
            String version = getString(headers, SLD_VERSION_HEADER);
            rval = parseSldXml(version, body);
        }
        return rval;
    }

    protected StyledLayerDescriptor parseSldXml(String version, InputStream body)
            throws OgcException {
        SldParser parser = sldRegistry.getParser(version);
        if (parser == null) {
            throw new OgcException(Code.InvalidParameterValue,
                    "Missing or unknown SLD version");
        }
        try {
            return parser.parse(body);
        } catch (Throwable t) {
            String msg = "Unable to parse SLD";
            log.error(msg, t);
            throw new OgcException(Code.InvalidParameterValue, msg);
        }
    }

    /**
     * @param object
     * @return
     * @throws WmsException
     */
    protected String getBbox(Map<String, Object> map, String key)
            throws OgcException {
        Object object = map.get(key);
        String rval = null;
        if (object instanceof String) {
            rval = (String) object;
        } else if (object instanceof Envelope) {
            Envelope env = (Envelope) object;
            rval = String.format("%f,%f,%f,%f", env.getMinX(), env.getMinY(),
                    env.getMaxX(), env.getMinY());
        } else if (object instanceof String[]) {
            throw new OgcException(Code.InvalidParameterValue,
                    "Multiple values for parameter: " + key);
        }
        return rval;
    }

    @Override
    protected OgcResponse handleError(OgcException e, MimeType exceptionFormat) {
        return provider.getError(new WmsException(e), exceptionFormat);
    }

}
