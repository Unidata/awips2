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
package com.raytheon.uf.edex.wfs.v1_1_0;

import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import net.opengis.gml.v_3_1_1.FeaturePropertyType;
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

import org.opengis.feature.simple.SimpleFeature;
import org.springframework.context.ApplicationContext;
import org.w3.xmlschema.Schema;

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
import com.raytheon.uf.edex.ogc.common.http.MimeType;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpHandler;
import com.raytheon.uf.edex.ogc.common.output.IOgcHttpResponse;
import com.raytheon.uf.edex.ogc.common.output.OgcResponseOutput;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.provider.AbstractWfsProvider;
import com.raytheon.uf.edex.wfs.provider.FeatureDescriber;
import com.raytheon.uf.edex.wfs.provider.FeatureFetcher.CountedFeatures;
import com.raytheon.uf.edex.wfs.provider.Gml31FeatureFetcher;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.request.DescFeatureTypeReq;
import com.raytheon.uf.edex.wfs.request.FeatureQuery;
import com.raytheon.uf.edex.wfs.request.FeatureQuery.QFilterType;
import com.raytheon.uf.edex.wfs.request.GetCapReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq.ResultType;
import com.raytheon.uf.edex.wfs.request.TransReq;
import com.raytheon.uf.edex.wfs.request.WfsRequest;
import com.raytheon.uf.edex.wfs.request.WfsRequest.Type;
import com.raytheon.uf.edex.wfs.soap2_0_0.util.DescribeFeatureTypeResponseType;

/**
 * WFS 1.1.0 implementation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 * Nov 11, 2013 2539        bclement    moved registry/marshal to parent
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class Wfs1_1_0Provider extends AbstractWfsProvider {

    public static final String version = "1.1.0";

    public static final MimeType GML_MIME = GmlUtils.GML311_OLD_TYPE;

	protected final Capabilities capabilities;

    protected final Gml31FeatureFetcher features;

    protected final Transactor transactor;

    protected final FeatureDescriber describer;

	protected final ObjectFactory wfsFactory = new ObjectFactory();

	public Wfs1_1_0Provider(WfsRegistryImpl registry) {
        super(registry);
		this.capabilities = new Capabilities(registry);
        this.features = new Gml31FeatureFetcher(registry);
        this.describer = new FeatureDescriber(registry, this);
		this.transactor = new Transactor();
	}

    /**
     * Unit tests
     */
	protected Wfs1_1_0Provider() {
        super(null);
        this.capabilities = null;
        this.features = null;
        this.describer = null;
        this.transactor = null;
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
				rval = "<ows:ExceptionReport version=\"1.0.0\"";
				rval += "xsi:schemaLocation=\"http://www.opengis.net/ows\"";
				rval += "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
						+ "xmlns:ows=\"http://www.opengis.net/ows\">";
				rval += "<ows:Exception exceptionCode=\"" + e.getCode() + "\">";
				rval += "<ows:ExceptionText>" + e.getMessage()
						+ "</ows:ExceptionText>";
				rval += "</ows:Exception></ows:ExceptionReport>";
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
        base += "/" + info.getPath() + "?service=" + Capabilities.SERV_TYPE
                + "&version=" + version;
        OgcServiceInfo<WfsOpType> rval = new OgcServiceInfo<WfsOpType>(base);
        String getCapGet = base;
        String getFeatureGet = base;
        String descFeatureGet = base;
        rval.addOperationInfo(getOp(getCapGet, base, WfsOpType.GetCapabilities,
                info.getHost()));
        rval.addOperationInfo(getOp(descFeatureGet, base,
                WfsOpType.DescribeFeatureType, info.getHost()));
        rval.addOperationInfo(getOp(getFeatureGet, base, WfsOpType.GetFeature,
                info.getHost()));
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
    protected OgcOperationInfo<WfsOpType> getOp(String get, String post,
            WfsOpType type, String host) {
        OgcOperationInfo<WfsOpType> rval = new OgcOperationInfo<WfsOpType>(type);
        rval.setHttpBaseHostname(host);
        rval.setHttpGetRes(get);
        rval.setHttpPostRes(post);
        rval.addVersion(version);
        rval.addFormat(OgcResponse.TEXT_XML_MIME.toString());
        return rval;
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

        if (GmlUtils.isGml(format)) {
            if (!GmlUtils.areCompatible(format, GML_MIME)) {
                throw new WfsException(Code.InvalidParameterValue,
                        "Unsupported GML Version: " + format);
            }
            featColl = wrap(features.getFeatures(request, serviceinfo));
        } else {
            String msg = String.format(
                    "Output format '%s' not supported for protocol",
                    format);
            throw new WfsException(Code.InvalidParameterValue, msg);
        }
        return featColl;
    }

    /**
     * Wrap counted features in JAXB feature collection type
     * 
     * @param features
     * @return
     */
    protected FeatureCollectionType wrap(
            CountedFeatures<FeaturePropertyType> features) {
        FeatureCollectionType rval = new FeatureCollectionType();
        List<FeaturePropertyType> members = rval.getFeatureMember();
        members.addAll(features.features);
        rval.setNumberOfFeatures(new BigInteger("" + features.count));
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
                featColl = wrap(features.getFeatures(request, serviceinfo));
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
     * Handle transaction request.
     * 
     * @deprecated unmaintained
     * @param request
     * @return
     */
    @Deprecated
    public TransactionResponseType transaction(TransReq request) {
        return transactor.transaction(request);
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
            throw new WfsException(Code.INVALID_REQUEST, "Unknown type name(s)");
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
        String[] times = splitOnParens(TIME_HEADER, headers);
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
            if (i < times.length) {
                fq.setTimeRange(getTimeRange(times[i]));
            }
            rval.addQuery(fq);
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
        case ERROR:
            err = (OgcResponse) request.getRawrequest();
        default:
            if (err == null) {
                err = getError(new WfsException(Code.INVALID_REQUEST,
                        "Unsupported request: " + request.getType()), null);
            }
            OgcResponseOutput.output(err, response);
        }
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
