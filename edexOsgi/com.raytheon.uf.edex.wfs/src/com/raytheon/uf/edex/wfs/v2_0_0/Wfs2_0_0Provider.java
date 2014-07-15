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

import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import net.opengis.gml.v_3_2_1.FeaturePropertyType;
import net.opengis.ows.v_1_1_0.ExceptionReport;
import net.opengis.ows.v_1_1_0.ExceptionType;
import net.opengis.wfs.v_2_0_0.DescribeFeatureTypeType;
import net.opengis.wfs.v_2_0_0.DescribeStoredQueriesResponseType;
import net.opengis.wfs.v_2_0_0.FeatureCollectionType;
import net.opengis.wfs.v_2_0_0.GetCapabilitiesType;
import net.opengis.wfs.v_2_0_0.GetFeatureType;
import net.opengis.wfs.v_2_0_0.GetPropertyValueType;
import net.opengis.wfs.v_2_0_0.ListStoredQueriesResponseType;
import net.opengis.wfs.v_2_0_0.ListStoredQueriesType;
import net.opengis.wfs.v_2_0_0.MemberPropertyType;
import net.opengis.wfs.v_2_0_0.ObjectFactory;
import net.opengis.wfs.v_2_0_0.ParameterType;
import net.opengis.wfs.v_2_0_0.QueryType;
import net.opengis.wfs.v_2_0_0.StoredQueryDescriptionType;
import net.opengis.wfs.v_2_0_0.StoredQueryListItemType;
import net.opengis.wfs.v_2_0_0.StoredQueryType;
import net.opengis.wfs.v_2_0_0.ValueCollectionType;
import net.opengis.wfs.v_2_0_0.WFSCapabilitiesType;

import org.apache.commons.collections.map.CaseInsensitiveMap;
import org.apache.ws.commons.schema.utils.NamespaceMap;
import org.opengis.feature.simple.SimpleFeature;
import org.springframework.context.ApplicationContext;
import org.w3.xmlschema.Schema;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.http.MimeType;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.ErrorType;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.feature.GmlUtils;
import com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter;
import com.raytheon.uf.edex.ogc.common.http.EndpointInfo;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpHandler;
import com.raytheon.uf.edex.ogc.common.output.IOgcHttpResponse;
import com.raytheon.uf.edex.ogc.common.output.OgcResponseOutput;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.provider.AbstractWfsProvider;
import com.raytheon.uf.edex.wfs.provider.FeatureDescriber;
import com.raytheon.uf.edex.wfs.provider.FeatureFetcher.CountedFeatures;
import com.raytheon.uf.edex.wfs.provider.Gml32FeatureFetcher;
import com.raytheon.uf.edex.wfs.querystore.FileSystemQueryStore;
import com.raytheon.uf.edex.wfs.querystore.IQueryStore;
import com.raytheon.uf.edex.wfs.querystore.IStoredQueryCallback;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.request.DescFeatureTypeReq;
import com.raytheon.uf.edex.wfs.request.DescQueryReq;
import com.raytheon.uf.edex.wfs.request.FeatureQuery;
import com.raytheon.uf.edex.wfs.request.FeatureQuery.QFilterType;
import com.raytheon.uf.edex.wfs.request.GetCapReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq.ResultType;
import com.raytheon.uf.edex.wfs.request.ListQueriesReq;
import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.raytheon.uf.edex.wfs.request.WfsRequest;
import com.raytheon.uf.edex.wfs.request.WfsRequest.Type;
import com.raytheon.uf.edex.wfs.soap2_0_0.util.DescribeFeatureTypeResponseType;
import com.raytheon.uf.edex.wfs.util.XMLGregorianCalendarConverter;

/**
 * WFS 2.0 implementation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012            bclement     Initial creation
 * Sep 18, 2013 #411       skorolev     Added required RESPONSE METADATA
 * Nov 11, 2013 2539       bclement     moved registry/marshal to parent
 * Jul 15, 2014 3373       bclement     jaxb manager api changes
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class Wfs2_0_0Provider extends AbstractWfsProvider implements
        IStoredQueryCallback {

    public static final String version = "2.0.0";

    public static final MimeType GML_MIME = GmlUtils.GML32_TYPE;

    protected final Capabilities capabilities;

    protected final Gml32FeatureFetcher features;

    protected final FeatureDescriber describer;

    protected final ObjectFactory wfsFactory = new ObjectFactory();

    protected final net.opengis.gml.v_3_2_1.ObjectFactory gmlFactory = new net.opengis.gml.v_3_2_1.ObjectFactory();

    protected final IQueryStore queryStore;

    protected final NamespaceContext nsContext;

    public Wfs2_0_0Provider(WfsRegistryImpl registry) {
        super(registry);
        this.capabilities = new Capabilities(registry);
        this.features = new Gml32FeatureFetcher(registry);
        this.describer = new FeatureDescriber(registry, this);
        // this name must match preloaded store in utility directory
        this.queryStore = new FileSystemQueryStore(registry, "wfsQueryStore");
        Map<String, Object> reverseNsMap = new HashMap<String, Object>(
                WfsRegistryImpl.NS_MAP.size());
        for (Entry<String, String> e : WfsRegistryImpl.NS_MAP.entrySet()) {
            reverseNsMap.put(e.getValue(), e.getKey());
        }
        nsContext = new NamespaceMap(reverseNsMap);
    }

    /**
     * Unit tests
     */
    protected Wfs2_0_0Provider() {
        super(null);
        this.capabilities = null;
        this.features = null;
        this.describer = null;
        this.queryStore = null;
        this.nsContext = null;
    }

    /**
     * Create an error response
     * 
     * @param e
     * @param exceptionFormat
     * @return
     */
    public OgcResponse getError(WfsException e, MimeType exceptionFormat) {
        ExceptionType et = new ExceptionType();
        et.setExceptionCode(e.getCode().toString());
        et.setExceptionText(Arrays.asList(e.getMessage()));
        ExceptionReport report = new ExceptionReport();
        report.setException(Arrays.asList(et));
        String rval = "";
        if (exceptionFormat == null) {
            exceptionFormat = OgcResponse.TEXT_XML_MIME;
        }
        if (exceptionFormat.equalsIgnoreParams(OgcResponse.TEXT_XML_MIME)) {
            try {
                rval = registry.marshal(report);
            } catch (JAXBException e1) {
                log.error("Unable to marshal WFS response", e1);
                rval = "<ows:ExceptionReport version=\"1.1.0\"\n";
                rval += "xsi:schemaLocation=\"http://www.opengis.net/ows\"\n";
                rval += "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \n"
                        + "xmlns:ows=\"http://www.opengis.net/ows\">\n";
                rval += "<ows:Exception exceptionCode=\"" + e.getCode()
                        + "\">\n";
                rval += "<ows:ExceptionText>" + e.getMessage()
                        + "</ows:ExceptionText>\n";
                rval += "</ows:Exception></ows:ExceptionReport>\n";
            }
        } else if (exceptionFormat
                .equalsIgnoreParams(OgcResponse.TEXT_HTML_MIME)) {
            rval = "<html xmlns=\"http://www.w3.org/1999/xhtml\">";
            rval += "<br>An error occurred performing the request:<br>";
            rval += "<br>Error Code: " + e.getCode().toString();
            rval += "<br>Message: " + e.getMessage() + "</html>";
        }
        OgcResponse resp = new OgcResponse(rval, exceptionFormat, TYPE.TEXT);
        switch (e.getCode()) {
        case OperationProcessingFailed:
            resp.setError(ErrorType.INT_ERR);
            break;
        default:
            resp.setError(ErrorType.BAD_REQ);
        }
        return resp;
    }


    /**
     * Decode post request from input stream
     * 
     * @param in
     * @return Error-type request object if unable to decode request
     */
    public WfsRequest getRequest(InputStream in) {
        Object obj;
        WfsRequest rval;
        try {
            obj = registry.unmarshal(in);
        } catch (Exception e) {
            log.error("Unable to decode request", e);
            return getDecodeError(OgcResponse.TEXT_XML_MIME);
        }
        if (obj instanceof GetCapabilitiesType) {
            rval = new GetCapReq((GetCapabilitiesType) obj);
        } else if (obj instanceof GetFeatureType) {
            try {
                rval = new GetFeatureReq((GetFeatureType) obj, this);
            } catch (WfsException e) {
                rval = new WfsRequest(Type.ERROR);
                rval.setRawrequest(e);
            }
        } else if (obj instanceof DescribeFeatureTypeType) {
            rval = new DescFeatureTypeReq((DescribeFeatureTypeType) obj);
        } else if (obj instanceof ListStoredQueriesType) {
            rval = new ListQueriesReq();
        } else {
            rval = getDecodeError(OgcResponse.TEXT_XML_MIME);
        }
        if (rval.getType() != Type.ERROR) {
            rval.setRawrequest(obj);
        }
        return rval;
    }

    /**
     * Create an error-type request for decoding problems
     * 
     * @param exceptionFormat
     * @return invalid request error
     */
    public WfsRequest getDecodeError(MimeType exceptionFormat) {
        return getRequestError(new WfsException(Code.INVALID_REQUEST,
                "Unable to decode request"), exceptionFormat);
    }

    /**
     * Create an error-type request.
     * 
     * @param e
     * @param exceptionFormat
     * @return
     */
    public WfsRequest getRequestError(WfsException e, MimeType exceptionFormat) {
        OgcResponse error = getError(e, exceptionFormat);
        WfsRequest rval = new WfsRequest(Type.ERROR);
        rval.setRawrequest(error);
        return rval;
    }

    /**
     * Get capabilities as object
     * 
     * @param request
     * @param serviceInfo
     * @return
     * @throws WfsException
     */
    public WFSCapabilitiesType getCapabilities(GetCapReq request,
            OgcServiceInfo<WfsOpType> serviceInfo) throws WfsException {
        return capabilities.getCapabilities(request, serviceInfo);
    }

    /**
     * Get capabilities as object
     * 
     * @param request
     * @param info
     * @return
     * @throws WfsException
     */
    public WFSCapabilitiesType getCapabilities(GetCapReq request,
            EndpointInfo info) throws WfsException {
        return capabilities.getCapabilities(request, getServiceInfo(info));
    }

    /**
     * Handle get capabilities request for http servlet
     * 
     * @param request
     * @param serviceInfo
     * @param resp
     * @throws Exception
     *             on unrecoverable error that was unable to be sent via
     *             response
     */
    public void handleCapabilities(GetCapReq request,
            OgcServiceInfo<WfsOpType> serviceInfo, IOgcHttpResponse resp)
            throws Exception {
        try {
            WFSCapabilitiesType cap = getCapabilities(request, serviceInfo);
            JAXBElement<WFSCapabilitiesType> rval = wfsFactory
                    .createWFSCapabilities(cap);
            marshalResponse(rval, OgcResponse.TEXT_XML_MIME, resp);
        } catch (WfsException e) {
            OgcResponse response = getError(e, null);
            OgcResponseOutput.sendText(response, resp);
        }
    }

    /**
     * Build service info based on endpoint. The results of this method are not
     * cache-able since it uses the hostname that the client used to reach the
     * service.
     * 
     * @param info
     * @return
     */
    public OgcServiceInfo<WfsOpType> getServiceInfo(EndpointInfo info) {
        String base = info.getProtocol() + "://" + info.getHost();
        int port = info.getPort();
        if (port != 80) {
            base += ":" + port;
        }
        base += info.getPath();
        OgcServiceInfo<WfsOpType> rval = new OgcServiceInfo<WfsOpType>(base);
        for (WfsOpType val : WfsOpType.values()) {
            rval.addOperationInfo(getOp(base, val, info));
        }
        return rval;
    }

    /**
     * Get operation info for op type
     * 
     * @param get
     * @param post
     * @param type
     * @return
     */
    protected OgcOperationInfo<WfsOpType> getOp(String base, WfsOpType type,
            EndpointInfo info) {
        OgcOperationInfo<WfsOpType> rval = new OgcOperationInfo<WfsOpType>(type);
        rval.setHttpBaseHostname(info.getHost());
        if (!info.isPostOnly()) {
            rval.setHttpGetRes(base);
        }
        rval.setHttpPostRes(base);
        rval.addVersion(version);
        rval.setPostEncoding(info.getEncoding());
        rval.addFormat(OgcResponse.TEXT_XML_MIME.toString());
        return rval;
    }



    /**
     * Get features as JAXB object
     * 
     * @param request
     * @param info
     * @return
     * @throws WfsException
     *             if return format isn't a supported GML version
     */
    public FeatureCollectionType getFeatureGML(GetFeatureReq request,
            EndpointInfo info) throws WfsException {
        return getFeatureGML(request, getServiceInfo(info));
    }

    /**
     * Get features as JAXB object
     * 
     * @param request
     * @param serviceinfo
     * @return
     * @throws WfsException
     *             if return format isn't a supported GML version
     */
    public FeatureCollectionType getFeatureGML(GetFeatureReq request,
            OgcServiceInfo<WfsOpType> serviceinfo) throws WfsException {
        FeatureCollectionType featColl;
        MimeType format = request.getOutputformat();
        ensureGML32(format);
        featColl = wrap(features.getFeatures(request, serviceinfo, false));
        return featColl;
    }

    /**
     * @param format
     * @throws WfsException
     *             if format isn't GML 3.2
     */
    public void ensureGML32(MimeType format) throws WfsException {
        if (GmlUtils.isGml(format)) {
            if (!GmlUtils.areCompatible(format, GML_MIME)) {
                throw new WfsException(Code.InvalidParameterValue,
                        "Unsupported GML Version: " + format);
            }
        } else {
            String msg = String.format(
                    "Output format '%s' not supported for protocol", format);
            throw new WfsException(Code.InvalidParameterValue, msg);
        }
    }

    /**
     * Wrap counted features in JAXB feature collection type
     * 
     * @param features
     * @return
     */
    public FeatureCollectionType wrap(
            CountedFeatures<FeaturePropertyType> features) {
        FeatureCollectionType rval = new FeatureCollectionType();
        List<MemberPropertyType> members = new ArrayList<MemberPropertyType>(
                features.features.size());
        for (FeaturePropertyType feature : features.features) {
            MemberPropertyType member = new MemberPropertyType();
            member.setContent(Arrays.asList((Object) feature
                    .getAbstractFeature()));
            members.add(member);
        }
        rval.setMember(members);
        rval.setNumberMatched(Long.toString(features.count));
        rval.setNumberReturned(BigInteger.valueOf(features.features.size()));
        rval.setTimeStamp(XMLGregorianCalendarConverter.getCurrentTimeStamp());
        return rval;
    }

    /**
     * Handle get feature request for http servlet. Response cannot be reused
     * after this method is called.
     * 
     * @param request
     * @param serviceinfo
     * @param response
     * @throws Exception
     *             if unable to send response
     */
    public void handleGetFeature(GetFeatureReq request,
            OgcServiceInfo<WfsOpType> serviceinfo, IOgcHttpResponse response)
            throws Exception {
        FeatureCollectionType featColl;
        MimeType format = request.getOutputformat();
        try {
            if (GmlUtils.isGml(format)) {
                if (!GmlUtils.areCompatible(format, GML_MIME)) {
                    throw new WfsException(Code.InvalidParameterValue,
                            "Unsupported GML Version: " + format);
                }
                // use JAXB instead of gml simple feature formatter
                CountedFeatures<FeaturePropertyType> counted = features
                        .getFeatures(request, serviceinfo, false);
                featColl = wrap(counted);
                marshalResponse(wfsFactory.createFeatureCollection(featColl),
                        GML_MIME, response);
            } else {
                if (request.getResulttype() == ResultType.hits) {
                    throw new WfsException(Code.InvalidParameterValue,
                            "Hits result not supported in format: " + format);
                }
                SimpleFeatureFormatter formatter = getFormatter(format);
                List<List<SimpleFeature>> res = features.getSimpleFeatures(
                        request, serviceinfo);
                sendFormatted(formatter, res, response);
            }
        } catch (WfsException e) {
            // this will only succeed if stream/writer haven't been extracted
            // from response
            OgcResponse err = getError(e, request.getExceptionFormat());
            OgcResponseOutput.output(err, response);
        }
    }

    /**
     * Marshal simple features through response using formatter. Response cannot
     * be reused after this method is called.
     * 
     * @param formatter
     * @param res
     * @param response
     * @throws Exception
     *             if unable to send response
     */
    protected void sendFormatted(SimpleFeatureFormatter formatter,
            List<List<SimpleFeature>> res, IOgcHttpResponse response)
            throws Exception {
        OutputStream out = null;
        try {
            out = response.getOutputStream();
            formatter.format(res, out);
            out.flush();
        } catch (Exception e) {
            log.error("Problem formatting features", e);
            OgcResponse err = getError(new WfsException(
                    Code.OperationProcessingFailed), null);
            OgcResponseOutput.sendText(err, response, out);
        } finally {
            if (out != null) {
                out.close();
            }
        }
    }

    /**
     * Find formatter for mime type
     * 
     * @param format
     * @return
     * @throws WfsException
     *             if no matching formatter is found
     */
    protected SimpleFeatureFormatter getFormatter(MimeType format)
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
        throw new WfsException(Code.InvalidParameterValue,
                "Unsupported format: " + format);
    }

    /**
     * @param request
     * @param info
     * @return
     * @throws WfsException
     */
    public DescribeFeatureTypeResponseType describeFeatureType(
            DescFeatureTypeReq request, EndpointInfo info) throws WfsException {
        return describeFeatureType(request, getServiceInfo(info));
    }

    /**
     * @param request
     * @param serviceInfo
     * @return
     * @throws WfsException
     */
    public DescribeFeatureTypeResponseType describeFeatureType(
            DescFeatureTypeReq request, OgcServiceInfo<WfsOpType> serviceInfo)
            throws WfsException {
        String xml = describer.describe(request, serviceInfo);
        if (xml == null) {
            throw new WfsException(Code.InvalidParameterValue,
                    "Unknown type name(s)");
        }
        try {
            Schema s = (Schema) registry.unmarshal(xml);
            return new DescribeFeatureTypeResponseType(s);
        } catch (JAXBException e) {
            throw new WfsException(Code.OperationProcessingFailed);
        }
    }

    /**
     * Handle describe feature type request for http servlet. Response cannot be
     * used after this method is called.
     * 
     * @param request
     * @param serviceInfo
     * @param response
     * @throws Exception
     *             if unable to send response
     */
    public void handleDescribeFeatureType(DescFeatureTypeReq request,
            OgcServiceInfo<WfsOpType> serviceInfo, IOgcHttpResponse response)
            throws Exception {
        try {
            String xml = describer.describe(request, serviceInfo);
            if (xml == null) {
                throw new WfsException(Code.INVALID_REQUEST,
                        "Unknown type name(s)");
            }
            OgcResponse ogcResp = new OgcResponse(xml, GML_MIME, TYPE.TEXT);
            OgcResponseOutput.output(ogcResp, response);
        } catch (WfsException e) {
            OgcResponse ogcResp = getError(e, null);
            OgcResponseOutput.sendText(ogcResp, response);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.WfsProvider#getVersion()
     */
    @Override
    public String getVersion() {
        return version;
    }

    /**
     * @param headers
     * @return
     * @throws OgcException
     *             if unable to decode request
     */
    protected WfsRequest getRequestFromHeaders(Map<String, Object> headers)
            throws OgcException {
        WfsRequest rval = null;
        getExceptionFormat(headers);
        String req = OgcHttpHandler.getString(headers, REQUEST_HEADER);
        if (req.equalsIgnoreCase(CAP_PARAM)) {
            rval = new GetCapReq();
        } else if (req.equalsIgnoreCase(DESC_PARAM)) {
            rval = buildDescFeatureReq(headers);
        } else if (req.equalsIgnoreCase(GET_PARAM)) {
            rval = buildGetFeatureReq(headers);
        } else if (req.equalsIgnoreCase(LIST_QUERY_PARAM)) {
            rval = new ListQueriesReq();
        } else if (req.equalsIgnoreCase(DESC_QUERY_PARAM)) {
            String[] ids = OgcHttpHandler.getStringArr(headers,
                    QUERY_IDS_HEADER);
            if (ids == null) {
                ids = new String[0];
            }
            rval = new DescQueryReq(Arrays.asList(ids));
        }

        if (rval == null) {
            throw new OgcException(OgcException.Code.InvalidRequest,
                    "Unable to decode request");
        }

        return rval;
    }

    /**
     * Get and validate exception format
     * 
     * @param headers
     * @return XML format if no exception format is specified
     * @throws OgcException
     *             if format is invalid
     */
    protected MimeType getExceptionFormat(Map<String, Object> headers)
            throws OgcException {
        String exFormatStr = OgcHttpHandler.getString(headers,
                EXCEP_FORMAT_HEADER);
        MimeType rval;
        if (exFormatStr == null || exFormatStr.isEmpty()) {
            rval = OgcResponse.TEXT_XML_MIME;
        } else {
            try {
                rval = new MimeType(exFormatStr);
            } catch (IllegalArgumentException e) {
                throw new OgcException(OgcException.Code.InvalidParameterValue,
                        e.getMessage());
            }
        }
        if (!rval.equalsIgnoreParams(OgcResponse.TEXT_HTML_MIME)
                && !rval.equalsIgnoreParams(OgcResponse.TEXT_XML_MIME)) {
            throw new OgcException(OgcException.Code.InvalidParameterValue,
                    "exceptions parameter invalid");
        }
        return rval;
    }

    /**
     * @param headers
     * @return
     * @throws OgcException
     */
    protected GetFeatureReq buildGetFeatureReq(Map<String, Object> headers)
            throws OgcException {
        GetFeatureReq rval = new GetFeatureReq();
        String resType = OgcHttpHandler.getString(headers, RESTYPE_HEADER);
        if (resType != null) {
            ResultType valueOf = GetFeatureReq.ResultType.valueOf(resType);
            if (valueOf != null) {
                rval.setResulttype(valueOf);
            }
        }
        Integer max = OgcHttpHandler.getInt(headers, MAXFEAT_HEADER);
        if (max != null) {
            rval.setMaxFeatures(max);
        }
        MimeType outputformat = OgcHttpHandler.getMimeType(headers,
                OUTFORMAT_HEADER);
        if (outputformat != null) {
            rval.setOutputformat(outputformat);
        }
        Map<String, String> nsmap = getNameSpaceMap(headers);
        String[] bboxes = splitOnParens(BBOX_HEADER, headers);
        String[] filters = splitOnParens(FILTER_HEADER, headers);
        String[] sorts = splitOnParens(SORTBY_HEADER, headers);
        String[] props = splitOnParens(PROPNAME_HEADER, headers);
        String[] srsnames = splitOnParens(SRSNAME_HEADER, headers);
        String[] types = OgcHttpHandler.getStringArr(headers, TYPENAME_HEADER);
        if (types == null) {
            types = new String[0];
        }
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
        String storedQueryId = OgcHttpHandler.getString(headers,
                QUERY_IDS_HEADER);
        if (storedQueryId != null) {
            throw new OgcException(
                    com.raytheon.uf.edex.ogc.common.OgcException.Code.InvalidRequest,
                    "Stored Query not supported for GET method");
        }
        return rval;
    }

    /**
     * @param headers
     * @return
     * @throws OgcException
     */
    protected DescFeatureTypeReq buildDescFeatureReq(Map<String, Object> headers)
            throws OgcException {
        DescFeatureTypeReq rval = new DescFeatureTypeReq();
        String outputformat = OgcHttpHandler.getString(headers,
                OUTFORMAT_HEADER);
        if (outputformat != null) {
            rval.setOutputformat(outputformat);
        }
        String typename = OgcHttpHandler.getString(headers, TYPENAME_HEADER);
        if (typename != null) {
            Map<String, String> nsmap = getNameSpaceMap(headers);
            rval.setTypenames(getTypeNames(typename, nsmap));
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.WfsProvider#handlePost(java.io.InputStream,
     * com.raytheon.uf.edex.ogc.common.http.EndpointInfo,
     * javax.servlet.http.HttpServletResponse)
     */
    @Override
    public void handlePost(InputStream body, EndpointInfo info,
            IOgcHttpResponse response) throws Exception {
        WfsRequest request = getRequest(body);
        handleInternal(request, info, response);
    }

    /**
     * @param request
     * @param info
     * @param response
     * @throws Exception
     */
    protected void handleInternal(WfsRequest request, EndpointInfo info,
            IOgcHttpResponse response) throws Exception {
        OgcServiceInfo<WfsOpType> serviceInfo = getServiceInfo(info);
        OgcResponse err = null;
        switch (request.getType()) {
        case GetCapabilities:
            handleCapabilities((GetCapReq) request, serviceInfo, response);
            break;
        case DescribeFeature:
            handleDescribeFeatureType((DescFeatureTypeReq) request,
                    serviceInfo, response);
            break;
        case GetFeature:
            handleGetFeature((GetFeatureReq) request, serviceInfo, response);
            break;
        case ListStoredQueries:
            handleListQueries((ListQueriesReq) request, serviceInfo, response);
            break;
        case DescribeStoredQueries:
            handleDescQueries((DescQueryReq) request, serviceInfo, response);
        case ERROR:
            err = (OgcResponse) request.getRawrequest();
            break;
        default:
            if (err == null) {
                err = getError(new WfsException(Code.INVALID_REQUEST,
                        "Unsupported request: " + request.getType()), null);
            }
            OgcResponseOutput.output(err, response);
        }
    }

    /**
     * @param request
     * @param serviceInfo
     * @param response
     * @throws Exception
     */
    private void handleDescQueries(DescQueryReq request,
            OgcServiceInfo<WfsOpType> serviceInfo, IOgcHttpResponse response)
            throws Exception {
        try {
            DescribeStoredQueriesResponseType descriptions = describeQueries(
                    request, serviceInfo);
            try {
                marshalResponse(
                        wfsFactory
                                .createDescribeStoredQueriesResponse(descriptions),
                        GML_MIME, response);
            } catch (Exception e) {
                log.error("Unable to marshal response", e);
                throw new WfsException(Code.OperationProcessingFailed);
            }
        } catch (WfsException e) {
            OgcResponse ogcResp = getError(e, null);
            OgcResponseOutput.sendText(ogcResp, response);
        }
    }

    /**
     * Describe stored queries
     * 
     * @param request
     * @param info
     * @return
     * @throws WfsException
     */
    public DescribeStoredQueriesResponseType describeQueries(
            DescQueryReq request, EndpointInfo info) throws WfsException {
        return describeQueries(request, getServiceInfo(info));
    }

    /**
     * Describe stored queries
     * 
     * @param request
     * @param info
     * @return
     * @throws WfsException
     */
    public DescribeStoredQueriesResponseType describeQueries(
            DescQueryReq request, OgcServiceInfo<WfsOpType> info)
            throws WfsException {
        DescribeStoredQueriesResponseType rval = new DescribeStoredQueriesResponseType();
        List<String> ids = request.getIds();
        if (ids.isEmpty()) {
            try {
                ids = queryStore.list();
            } catch (OgcException e) {
                log.error("Problem getting queries", e);
                throw new WfsException(Code.OperationProcessingFailed);
            }
        }
        List<StoredQueryDescriptionType> descriptions = new ArrayList<StoredQueryDescriptionType>(
                ids.size());
        for (String id : ids) {
            try {
                StoredQueryDescriptionType desc = queryStore.retrieve(id);
                if (desc == null) {
                    throw new WfsException(Code.InvalidParameterValue,
                            "Unable to find query with id: " + id);
                }
                descriptions.add(desc);
            } catch (OgcException e) {
                log.error("problem retrieving query", e);
                throw new WfsException(Code.OperationProcessingFailed);
            }
        }
        rval.setStoredQueryDescription(descriptions);
        return rval;
    }

    /**
     * List stored queries
     * 
     * @param request
     * @param serviceInfo
     * @param response
     * @throws Exception
     */
    private void handleListQueries(ListQueriesReq request,
            OgcServiceInfo<WfsOpType> serviceInfo, IOgcHttpResponse response)
            throws Exception {
        try {
            ListStoredQueriesResponseType queries = listQueries(serviceInfo);
            try {
                marshalResponse(
                        wfsFactory.createListStoredQueriesResponse(queries),
                        GML_MIME, response);
            } catch (Exception e) {
                log.error("Unable to marshal response", e);
                throw new WfsException(Code.OperationProcessingFailed);
            }
        } catch (WfsException e) {
            OgcResponse ogcResp = getError(e, null);
            OgcResponseOutput.sendText(ogcResp, response);
        }
    }

    /**
     * List stored queries
     * 
     * @param info
     * @return
     * @throws WfsException
     */
    public ListStoredQueriesResponseType listQueries(EndpointInfo info)
            throws WfsException {
        return listQueries(getServiceInfo(info));
    }

    /**
     * List stored queries
     * 
     * @param serviceInfo
     * @return
     * @throws WfsException
     */
    public ListStoredQueriesResponseType listQueries(
            OgcServiceInfo<WfsOpType> serviceInfo) throws WfsException {
        ListStoredQueriesResponseType rval = new ListStoredQueriesResponseType();
        List<String> ids;
        try {
            ids = queryStore.list();
        } catch (OgcException e) {
            log.error("Unable to list stored queries", e);
            throw new WfsException(Code.OperationProcessingFailed);
        }
        List<StoredQueryListItemType> queries = new ArrayList<StoredQueryListItemType>(
                ids.size());
        for (String id : ids) {
            StoredQueryListItemType item = new StoredQueryListItemType();
            item.setId(id);
            queries.add(item);
        }
        rval.setStoredQuery(queries);
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.WfsProvider#handleGet(java.util.Map,
     * com.raytheon.uf.edex.ogc.common.http.EndpointInfo,
     * javax.servlet.http.HttpServletResponse)
     */
    @Override
    public void handleGet(Map<String, Object> headers, EndpointInfo info,
            IOgcHttpResponse response) throws Exception {
        WfsRequest req;
        try {
            req = getRequestFromHeaders(headers);
        } catch (OgcException e) {
            req = getRequestError(new WfsException(e),
                    OgcResponse.TEXT_XML_MIME);
        }
        handleInternal(req, info, response);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.querystore.StoredQueryCallback#getQueries(net
     * .opengis.wfs.v_2_0_0.StoredQueryType)
     */
    @Override
    public List<FeatureQuery> getQueries(StoredQueryType sqt)
            throws WfsException {
        String id = sqt.getId();
        List<ParameterType> params = sqt.getParameter();
        Map<String, String> paramMap = getQueryParams(params);
        try {
            List<QueryType> res = queryStore.resolve(id, paramMap);
            if (res.isEmpty()) {
                throw new WfsException(Code.InvalidParameterValue,
                        "Unknown query id: " + id);
            }
            List<FeatureQuery> rval = new ArrayList<FeatureQuery>(res.size());
            for (QueryType qt : res) {
                FeatureQuery fq = new FeatureQuery(qt);
                List<QualifiedName> qnames = fq.getTypeNames();
                if (qnames.isEmpty()) {
                    fq.setTypeNames(getAllFeatureTypes());
                }
                rval.add(fq);
            }
            return rval;
        } catch (OgcException e) {
            log.error("Problem resolving stored query", e);
            // TODO this could be a problem with the input, return better error
            throw new WfsException(Code.OperationProcessingFailed);
        }
    }

    /**
     * @return a list of all feature type names
     */
    private List<QualifiedName> getAllFeatureTypes() {
        List<WfsFeatureType> features = registry.getFeatures();
        List<QualifiedName> rval = new ArrayList<QualifiedName>(features.size());
        for (WfsFeatureType feature : features) {
            rval.add(feature.getName());
        }
        return rval;
    }

    /**
     * Extract parameter types into string map
     * 
     * @param params
     * @return
     * @throws WfsException
     */
    @SuppressWarnings("unchecked")
    private Map<String, String> getQueryParams(List<ParameterType> params)
            throws WfsException {
        Map<String, String> rval = new CaseInsensitiveMap(params.size());
        for (ParameterType param : params) {
            String key = param.getName();
            if (!param.isSetContent() || param.getContent().isEmpty()) {
                throw new WfsException(Code.MissingParameterValue,
                        "Parameter content cannot be empty");
            }
            Object object = param.getContent().get(0);
            if (object instanceof JAXBElement<?>) {
                try {
                    String xml = registry.marshal(object, true);
                    rval.put(key, xml);
                } catch (Exception e) {
                    log.error("Problem marshalling parameter value", e);
                    // is it possible that this isn't our fault?
                    throw new WfsException(Code.OperationProcessingFailed);
                }
            } else {
                rval.put(key, object.toString());
            }
        }
        return rval;
    }

    /**
     * Get property values from features
     * 
     * @param request
     * @param info
     * @return
     * @throws Exception
     */
    public ValueCollectionType getPropertyValues(GetPropertyValueType request,
            EndpointInfo info) throws WfsException {
        OgcServiceInfo<WfsOpType> serviceinfo = getServiceInfo(info);
        GetFeatureReq featReq = new GetFeatureReq(request, this);
        MimeType format = featReq.getOutputformat();
        ensureGML32(format);
        CountedFeatures<FeaturePropertyType> counted = features.getFeatures(
                featReq, serviceinfo, false);
        return getValues(counted, request);
    }

    /**
     * Extract property values form features using request
     * 
     * @param counted
     * @param request
     * @return
     * @throws WfsException
     */
    private ValueCollectionType getValues(
            CountedFeatures<FeaturePropertyType> counted,
            GetPropertyValueType request) throws WfsException {
        ValueCollectionType rval = new ValueCollectionType();
        XPath xpath = XPathFactory.newInstance().newXPath();
        xpath.setNamespaceContext(nsContext);
        List<MemberPropertyType> values = new ArrayList<MemberPropertyType>(
                counted.features.size());
        for (FeaturePropertyType prop : counted.features) {
            try {
                Node node = registry.marshalToNode(prop.getAbstractFeature());
                NodeList nlist = (NodeList) xpath.evaluate(
                        request.getValueReference(), node,
                        XPathConstants.NODESET);
                for (int i = 0; i < nlist.getLength(); ++i) {
                    MemberPropertyType member = new MemberPropertyType();
                    member.setContent(Arrays.asList((Object) nlist.item(i)));
                    values.add(member);
                }
            } catch (JAXBException e) {
                log.error("problem converting feature to Node", e);
                throw new WfsException(Code.OperationProcessingFailed);
            } catch (ParserConfigurationException e) {
                log.error("problem converting feature to Node", e);
                throw new WfsException(Code.OperationProcessingFailed);
            } catch (XPathExpressionException e) {
                String msg;
                Throwable t = e;
                do {
                    msg = t.getMessage();
                    t = t.getCause();
                } while (msg == null);
                throw new WfsException(Code.InvalidParameterValue,
                        "Invalid XPath expression: " + msg);
            }
        }
        rval.setMember(values);
        rval.setNumberMatched(Long.toString(counted.count));
        rval.setNumberReturned(BigInteger.valueOf(counted.features.size()));
        rval.setTimeStamp(XMLGregorianCalendarConverter.getCurrentTimeStamp());
        return rval;
    }

    /**
     * @return the registry
     */
    public WfsRegistryImpl getRegistry() {
        return registry;
    }

    /**
     * @return the features
     */
    public Gml32FeatureFetcher getFeatures() {
        return features;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.WfsProvider#getHttpServiceLocation()
     */
    @Override
    public String getHttpServiceLocation() {
        String location = "";
        String port = registry.getHttpServicePort();
        if (!port.equals("80")) {
            location += ":" + port;
        }
        String path = registry.getHttpServicePath();
        location += "/" + path + "?service=" + Capabilities.SERV_TYPE
                + "&version=" + version;
        return location;
    }

}
