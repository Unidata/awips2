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
 * May 30. 2013 753        dhladky      updates
 * Jun 11, 2013 2101       dhladky      More speed improvements
 *
 */
package com.raytheon.uf.edex.wfs;

import java.io.InputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.util.HarvesterServiceManager;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ServiceConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcPrefix;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.OgcTimeRange;
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

    protected final String REQUEST_HEADER;

    protected final String BLANK;

    protected final String CAP_PARAM;

    protected final String DESC_PARAM;

    protected final String GET_PARAM;

    protected final String OUTFORMAT_HEADER;

    protected final String RESTYPE_HEADER;

    protected final String PROPNAME_HEADER;

    protected final String MAXFEAT_HEADER;

    protected final String SRSNAME_HEADER;

    protected final String TYPENAME_HEADER;

    protected final String FEATID_HEADER;

    protected final String FILTER_HEADER;

    protected final String BBOX_HEADER;

    protected final String SORTBY_HEADER;

    protected final String NS_HEADER;

    protected final String PORT_HEADER;

    protected final String TIME_HEADER;

    protected final String PLUS;

    protected final String SPACE;

    protected int PORT;

    protected final String WFS;

    protected final String VERSION;

    protected final String OUTPUTFORMAT;

    protected static final Pattern nspattern = Pattern
            .compile("xmlns\\((\\S+)=(\\S+)\\)");

    protected static final Pattern doubleBackSlash = Pattern.compile("\\\\");

    protected static final Pattern slash = Pattern.compile("/");

    protected static final String comma = ",";
    
    protected static final Pattern commaPattern = Pattern.compile(",");

    protected static final String parenMatcher = "\\s*\\(";

    protected static final String questionMark = "?";

    protected static final String equals = "=";

    protected static final String lamdas = "λλλ";

    protected static final String escapedComma = "\\\\,";

    protected static final Pattern sortBys = Pattern.compile("\\s+");

    protected final String COLON = ":";

    protected static final Pattern parensPattern = Pattern.compile(parenMatcher);

    protected static final Pattern escapedCommaPattern = Pattern.compile(escapedComma);

    protected static final Pattern lamdasPattern = Pattern.compile(lamdas);
    
    protected WfsProvider provider;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WfsHttpHandler.class);

    protected static ServiceConfig wfsServiceConfig;

    private static ThreadLocal<SimpleDateFormat> ogcDateFormat = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {

            SimpleDateFormat sdf = new SimpleDateFormat(getServiceConfig()
                    .getDateConfig().getFormats().get(0));
            return sdf;
        }

    };

    public WfsHttpHandler(WfsProvider provider) {

        REQUEST_HEADER = getServiceConfig().getConstantByName("REQUEST_HEADER")
                .getValue();
        TIME_HEADER = getServiceConfig().getConstantByName("TIME_HEADER")
                .getValue();
        CAP_PARAM = getServiceConfig().getConstantByName("CAP_PARAM")
                .getValue();
        DESC_PARAM = getServiceConfig().getConstantByName("DESC_PARAM")
                .getValue();
        GET_PARAM = getServiceConfig().getConstantByName("GET_PARAM")
                .getValue();
        OUTFORMAT_HEADER = getServiceConfig().getConstantByName(
                "OUTFORMAT_HEADER").getValue();
        RESTYPE_HEADER = getServiceConfig().getConstantByName("RESTYPE_HEADER")
                .getValue();
        PROPNAME_HEADER = getServiceConfig().getConstantByName(
                "PROPNAME_HEADER").getValue();
        MAXFEAT_HEADER = getServiceConfig().getConstantByName("MAXFEAT_HEADER")
                .getValue();
        SRSNAME_HEADER = getServiceConfig().getConstantByName("SRSNAME_HEADER")
                .getValue();
        TYPENAME_HEADER = getServiceConfig().getConstantByName(
                "TYPENAME_HEADER").getValue();
        FEATID_HEADER = getServiceConfig().getConstantByName("FEATID_HEADER")
                .getValue();
        FILTER_HEADER = getServiceConfig().getConstantByName("FILTER_HEADER")
                .getValue();
        BBOX_HEADER = getServiceConfig().getConstantByName("BBOX_HEADER")
                .getValue();
        SORTBY_HEADER = getServiceConfig().getConstantByName("SORTBY_HEADER")
                .getValue();
        VERSION = getServiceConfig().getConstantByName("VERSION").getValue();
        PORT_HEADER = getServiceConfig().getConstantByName("PORT_HEADER")
                .getValue();
        PORT = Integer.valueOf(getServiceConfig().getConstantByName("PORT")
                .getValue());
        WFS = getServiceConfig().getConstantByName("WFS").getValue();
        NS_HEADER = getServiceConfig().getConstantByName("NS_HEADER")
                .getValue();
        BLANK = getServiceConfig().getConstantByName("BLANK").getValue();
        OUTPUTFORMAT = getServiceConfig().getConstantByName("OUTPUTFORMAT")
                .getValue();
        PLUS = getServiceConfig().getConstantByName("PLUS").getValue();
        SPACE = getServiceConfig().getConstantByName("SPACE").getValue();

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
            statusHandler.info("Request from: "+req.getRequest().getRemoteAddr());
            long time = System.currentTimeMillis();
            handleInternal(req);
            long time2 = System.currentTimeMillis();
            statusHandler.info("Processed: "
                    + req.getRequest().getQueryString() + " in "
                    + (time2 - time) + " ms");
        } catch (Exception e) {
            statusHandler.error("Unable to handle request", e);
        }
    }

    protected void handleInternal(OgcHttpRequest req) throws Exception {
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

        String[] rval = commaPattern.split(escapedCommaPattern.matcher(str).replaceAll(lamdas));
        for (int i = 0; i < rval.length; ++i) {
            rval[i] = lamdasPattern.matcher(rval[i]).replaceAll(comma);
        }
        return rval;
    }

    protected String[] splitOnSlash(String str) {
        String[] times = slash.split(str);
        return times;
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
                statusHandler
                        .warn("Can't read the max number of features in request! "
                                + max);
            }
        }
        String outputformat = getHeader(OUTFORMAT_HEADER, headers);
        if (outputformat != null) {
            rval.setOutputformat(outputformat);
        }
        Map<String, String> nsmap = getNameSpaceMap(headers);
        String[] bboxes = splitOnParens(BBOX_HEADER, headers);
        String[] times = splitOnParens(TIME_HEADER, headers);
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
            if (i < times.length) {
                fq.setTimeRange(getTimeRange(times[i]));
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
        int index = typename.lastIndexOf(":");
        String type = typename.substring(index + 1);
        String namespace = typename.substring(0, index);
        List<QualifiedName> rval = new LinkedList<QualifiedName>();
        if (index == 0) {
            // default names to the edex namespace
            rval.add(new QualifiedName(OgcNamespace.EDEX, typename, null));
        } else {
            rval.add(new QualifiedName(namespace, type, type));
        }
        return rval;
    }

    protected String[] splitOnParens(String name, Map<String, Object> headers) {
        String val = getHeader(name, headers);
        String[] rval;
        if (val != null) {
            rval = doubleBackSlash.split(parensPattern.matcher(val).replaceAll(BLANK));
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
            String[] parts = sortBys.split(s);
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
                statusHandler.error("couldn't parse Bounding Box!", e);
            }
        }// else TODO handle non 2d WGS84
        return rval;
    }

    protected OgcTimeRange getTimeRange(String stimes) {
        OgcTimeRange otr = null;
        // we assume it is a start time going up to present
        try {

            String[] intimes = splitOnSlash(stimes);
            String[] times = new String[intimes.length];

            for (int i = 0; i < intimes.length; i++) {
                // TODO figure out why "+" for TimeZone gets hacked out of times
                String newtime = intimes[i].replace(SPACE, PLUS);
                times[i] = newtime;
            }

            if (times.length > 0) {
                Date endDate = null;
                Date startDate = null;

                if (times.length == 1) {
                    endDate = new Date(System.currentTimeMillis());
                    startDate = ogcDateFormat.get().parse(times[0]);

                } else {
                    endDate = ogcDateFormat.get().parse(times[1]);
                    startDate = ogcDateFormat.get().parse(times[0]);
                }

                otr = new OgcTimeRange(startDate, endDate);
            }
        } catch (ParseException pe) {
            statusHandler.error("couldn't parse times!", pe);
        }

        return otr;
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

    protected String[] getHeaderTimes(String name, Map<String, Object> headers) {
        String[] rval;
        String value = getHeader(name, headers);
        if (value != null) {
            rval = splitOnSlash(value);
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
        String base = PORT_HEADER + host;
        if (port != PORT) {
            base += COLON + port;
        }
        base += slash.pattern() + WFS;
        OgcServiceInfo<WfsOpType> rval = new OgcServiceInfo<WfsOpType>(base);
        StringBuilder getCapGet = new StringBuilder();
        getCapGet.append(base).append(questionMark).append(REQUEST_HEADER)
                .append(equals).append(CAP_PARAM);
        StringBuffer getFeatureGet = new StringBuffer();
        getFeatureGet.append(base).append(questionMark).append(REQUEST_HEADER)
                .append(equals).append(GET_PARAM);
        StringBuffer descFeatureGet = new StringBuffer();
        descFeatureGet.append(base).append(questionMark).append(REQUEST_HEADER)
                .append(equals).append(DESC_PARAM);
        rval.addOperationInfo(getOp(getCapGet.toString(), base,
                WfsOpType.GetCapabilities));
        rval.addOperationInfo(getOp(descFeatureGet.toString(), base,
                WfsOpType.DescribeFeatureType));
        OgcOperationInfo<WfsOpType> getFeat = getOp(getFeatureGet.toString(),
                base, WfsOpType.GetFeature);
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
        rval.addVersion(VERSION);
        rval.addFormat(OUTPUTFORMAT);
        return rval;
    }

    protected void sendResponse(HttpServletResponse httpRes,
            OgcResponse response) throws Exception {
        OgcResponseOutput.output(response, httpRes);
    }

    /**
     * Get service config
     * 
     * @return
     */
    protected static ServiceConfig getServiceConfig() {
        if (wfsServiceConfig == null) {
            wfsServiceConfig = HarvesterServiceManager.getInstance()
                    .getServiceConfig(ServiceType.WFS);
        }

        return wfsServiceConfig;
    }

}