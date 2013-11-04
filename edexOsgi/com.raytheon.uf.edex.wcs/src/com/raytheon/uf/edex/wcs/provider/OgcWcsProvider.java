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

package com.raytheon.uf.edex.wcs.provider;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.DatatypeConverter;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import net.opengis.gml.v_3_1_1.TimePositionType;
import net.opengis.ows.v_1_1_0.AbstractReferenceBaseType;
import net.opengis.ows.v_1_1_0.BoundingBoxType;
import net.opengis.ows.v_1_1_0.CodeType;
import net.opengis.ows.v_1_1_0.ExceptionReport;
import net.opengis.ows.v_1_1_0.ExceptionType;
import net.opengis.ows.v_1_1_0.GetCapabilitiesType;
import net.opengis.ows.v_1_1_0.ReferenceGroupType;
import net.opengis.ows.v_1_1_0.ReferenceType;
import net.opengis.wcs.v_1_1_2.AxisSubset;
import net.opengis.wcs.v_1_1_2.Capabilities;
import net.opengis.wcs.v_1_1_2.CoverageDescriptionType;
import net.opengis.wcs.v_1_1_2.CoverageDescriptions;
import net.opengis.wcs.v_1_1_2.CoveragesType;
import net.opengis.wcs.v_1_1_2.DescribeCoverage;
import net.opengis.wcs.v_1_1_2.GetCapabilities;
import net.opengis.wcs.v_1_1_2.GetCoverage;
import net.opengis.wcs.v_1_1_2.ObjectFactory;
import net.opengis.wcs.v_1_1_2.RangeSubsetType;
import net.opengis.wcs.v_1_1_2.RangeSubsetType.FieldSubset;
import net.opengis.wcs.v_1_1_2.TimePeriodType;
import net.opengis.wcs.v_1_1_2.TimeSequenceType;

import org.apache.commons.codec.binary.Base64InputStream;
import org.apache.cxf.helpers.IOUtils;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcPrefix;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.ErrorType;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.http.EndpointInfo;
import com.raytheon.uf.edex.ogc.common.http.MimeType;
import com.raytheon.uf.edex.ogc.common.jaxb.OgcJaxbManager;
import com.raytheon.uf.edex.ogc.common.output.IOgcHttpResponse;
import com.raytheon.uf.edex.ogc.common.spatial.Composite3DBoundingBox;
import com.raytheon.uf.edex.ogc.common.spatial.CrsLookup;
import com.raytheon.uf.edex.wcs.CoverageStoreEndpoint;
import com.raytheon.uf.edex.wcs.CoveragesHolder;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.WcsException.Code;
import com.raytheon.uf.edex.wcs.WcsProvider;
import com.raytheon.uf.edex.wcs.format.IWcsDataFormatter;
import com.raytheon.uf.edex.wcs.reg.Coverage;
import com.raytheon.uf.edex.wcs.reg.CoverageDescription;
import com.raytheon.uf.edex.wcs.reg.IWcsSource;
import com.raytheon.uf.edex.wcs.reg.RangeAxis;
import com.raytheon.uf.edex.wcs.reg.RangeField;
import com.raytheon.uf.edex.wcs.reg.RangeField.InterpolationType;
import com.raytheon.uf.edex.wcs.reg.WcsSourceAccessor;
import com.raytheon.uf.edex.wcs.request.DescCoverageRequest;
import com.raytheon.uf.edex.wcs.request.GetCapRequest;
import com.raytheon.uf.edex.wcs.request.GetCoverageRequest;
import com.raytheon.uf.edex.wcs.request.WcsRequest;
import com.raytheon.uf.edex.wcs.request.WcsRequest.Type;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 
 * </pre>
 * 
 * @author cwmitche
 * @version 1
 */
public class OgcWcsProvider implements WcsProvider {

    public enum WcsOpType {
        GetCapabilities, GetCoverage, DescribeCoverage
    }

    protected static final String version = "1.1.2";

    protected static final String svcTitle = "EDEX Coverage Server";

    protected final OgcJaxbManager jaxbManager;

    protected final String COVERAGE_ID = "cid:Coverage-";

    protected final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    private volatile DescCoverageBuilder _descCoverage;

    private volatile CapabilitiesBuilder _capbuilder;

    protected boolean base64 = true;

    protected final net.opengis.ows.v_1_1_0.ObjectFactory owsFactory = new net.opengis.ows.v_1_1_0.ObjectFactory();

    protected final ObjectFactory wcsFactory = new ObjectFactory();

    protected static final Map<String, String> NS_MAP = new ConcurrentHashMap<String, String>();

    private int restPort = 8085;

    private String covStorePath = "coverage";

    static {
        NS_MAP.put(OgcNamespace.EDEX, OgcPrefix.EDEX);
        NS_MAP.put(OgcNamespace.GML, OgcPrefix.GML);
        NS_MAP.put(OgcNamespace.OGC, OgcPrefix.OGC);
        NS_MAP.put(OgcNamespace.OWS110, OgcPrefix.OWS);
        NS_MAP.put(OgcNamespace.WFS, OgcPrefix.WFS);
        NS_MAP.put(OgcNamespace.XSI, OgcPrefix.XSI);
        NS_MAP.put(OgcNamespace.WCS112, OgcPrefix.WCS);
        NS_MAP.put(OgcNamespace.XLINK, OgcPrefix.XLINK);
    }

    public OgcWcsProvider(OgcJaxbManager manager) throws JAXBException {
        this.jaxbManager = manager;
        jaxbManager.setPrefixMap(NS_MAP);
    }

    protected DescCoverageBuilder getCoverageBuilder() {
        if (_descCoverage == null) {
            synchronized (wcsFactory) {
                if (_descCoverage == null) {
                    List<String> formats = WcsSourceAccessor.getFormats();
                    if (formats.isEmpty()) {
                        // don't remember list if empty, may not be initialized
                        // yet
                        return new DescCoverageBuilder(formats);
                    } else {
                        _descCoverage = new DescCoverageBuilder(formats);
                    }
                }
            }
        }
        return _descCoverage;
    }

    protected CapabilitiesBuilder getCapBuilder() {
        if (_capbuilder == null) {
            synchronized (wcsFactory) {
                if (_capbuilder == null) {
                    List<String> formats = WcsSourceAccessor.getFormats();
                    if (formats.isEmpty()) {
                        // don't remember list if empty, may not be initialized
                        // yet
                        return new CapabilitiesBuilder(formats);
                    } else {
                        _capbuilder = new CapabilitiesBuilder(formats);
                    }
                }
            }
        }
        return _capbuilder;
    }

    @Override
    public OgcResponse describeCoverageType(
            OgcServiceInfo<WcsOpType> serviceinfo, DescCoverageRequest request) {
        CoverageDescriptions rval;
        try {
            rval = describeCoverage(serviceinfo, request);
            return marshalResponse(rval);
        } catch (WcsException e) {
            return getError(e, request.getExceptionFormat());
        }
    }

    public CoverageDescriptions describeCoverage(EndpointInfo info,
            DescCoverageRequest request) throws WcsException {
        return describeCoverage(getServiceInfo(info), request);
    }

    public CoverageDescriptions describeCoverage(
            OgcServiceInfo<WcsOpType> serviceinfo, DescCoverageRequest request)
            throws WcsException {

        CoverageDescriptions rval = new CoverageDescriptions();
        String[] externalIds = request.getExternalIds();
        List<CoverageDescriptionType> descs = null;
        if (externalIds == null) {
            throw new WcsException(Code.MissingParameterValue);
        }
        String[] internalIds = request.getInternalIds();
        descs = new ArrayList<CoverageDescriptionType>(externalIds.length);
        for (int i = 0; i < externalIds.length; ++i) {
            String external = externalIds[i];
            String internal = internalIds[i];
            descs.add(descCov(external, internal));
        }
        rval.setCoverageDescription(descs);
        return rval;
    }

    protected CoverageDescriptionType descCov(String externalId,
            String internalId) throws WcsException {
        IWcsSource<?, ?> source = WcsSourceAccessor.getSource(internalId);
        if (source == null) {
            throw new WcsException(Code.InvalidParameterValue,
                    "Coverage ID not found");
		}
        CoverageDescription cd = source.describeCoverage(internalId);
        cd.setIdentifier(externalId);
        return getCoverageBuilder().getCoverageDescriptionType(cd);
    }

    @Override
    public OgcResponse getCapabilities(OgcServiceInfo<WcsOpType> serviceinfo,
            GetCapRequest request) {
        Capabilities capabilities = getCapBuilder().getCapabilities(
                serviceinfo, WcsSourceAccessor.getCoverages(true));
        return marshalResponse(capabilities);
    }

	public Capabilities getCapabilities(EndpointInfo info,
			GetCapabilities request) {
		return getCapBuilder().getCapabilities(getServiceInfo(info),
				getCoverages(true));
	}

	/**
	 * @param summary
	 * @return list of coverages with mapped identifiers
	 */
	private List<CoverageDescription> getCoverages(boolean summary) {
		List<CoverageDescription> coverages = WcsSourceAccessor
				.getCoverages(true);
		for (CoverageDescription desc : coverages) {
			String id = CustomIdMap.internalToExternal(desc.getIdentifier());
			desc.setIdentifier(id);
		}
		return coverages;
    }

    private OgcServiceInfo<WcsOpType> getServiceInfo(EndpointInfo info) {
        int port = info.getPort();
        String base = "http://" + info.getHost();
        if (port != 80) {
            base += ":" + port;
        }
        base += info.getPath();
        OgcServiceInfo<WcsOpType> rval = new OgcServiceInfo<WcsOpType>(base);
        rval.addOperationInfo(getOp(base, base, WcsOpType.GetCapabilities, info));
        rval.addOperationInfo(getOp(base, base, WcsOpType.DescribeCoverage,
                info));
        rval.addOperationInfo(getOp(base, base, WcsOpType.GetCoverage, info));
        return rval;
    }

    protected OgcOperationInfo<WcsOpType> getOp(String get, String post,
            WcsOpType type, EndpointInfo info) {
        OgcOperationInfo<WcsOpType> rval = new OgcOperationInfo<WcsOpType>(type);
        rval.setHttpBaseHostname(info.getHost());
        if (!info.isPostOnly()) {
            rval.setHttpGetRes(get);
        }
        rval.setHttpPostRes(post);
        rval.addVersion(version);
        rval.addAcceptVersions(version);
        rval.addService("WCS");
        rval.addFormat("text/xml");
        rval.setPostEncoding(info.getEncoding());
        return rval;
    }

    public CoveragesHolder getCoverage(EndpointInfo info,
            GetCoverageRequest request) throws WcsException {
        String format = request.getFormat();
        // try fast lookup first
        Map<String, IWcsDataFormatter> formatMap = WcsSourceAccessor
                .getFormatMap();
        IWcsDataFormatter formatter = formatMap.get(format);
        if (formatter == null) {
            // check to see if any formatter handles it
            for (Entry<String, IWcsDataFormatter> e : formatMap.entrySet()) {
                if (e.getValue().matchesFormat(format)) {
                    formatter = e.getValue();
                    break;
                }
            }
            if (formatter == null) {
                // no match found
                throw new WcsException(Code.InvalidFormat);
            }

        }
        String externalId = request.getExternalId();
        Coverage coverage = requestCoverage(request);
		coverage.setName(CustomIdMap.internalToExternal(coverage.getName()));
        CoveragesHolder holder = new CoveragesHolder();
        holder.setContentType(format);
        Map<String, byte[]> data = new HashMap<String, byte[]>();
        String href;
        if (request.isStore()) {
            try {
                File f = formatter.store(coverage);
                href = String.format("http://%s:%d/%s?%s=%s", info.getHost(),
                        restPort, covStorePath,
                        CoverageStoreEndpoint.ID_HEADER, f.getName());
            } catch (Exception e) {
                log.error("Problem formatting coverage", e);
                throw new WcsException(Code.InternalServerError);
            }
        } else {
			href = getCoverageId(externalId);
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            InputStream in = null;
            try {
                in = formatter.format(coverage);
                IOUtils.copy(in, out);
                out.flush();
            } catch (Exception e) {
                log.error("Problem formatting coverage", e);
                throw new WcsException(Code.InternalServerError);
            } finally {
                close(in, out);
            }
            data.put(href, out.toByteArray());
        }
		CoveragesType rval = getCoverageOgcResponse(externalId, href);
        holder.setMetadata(rval);
        holder.setData(data);
        return holder;
    }

    private Coverage requestCoverage(GetCoverageRequest request)
            throws WcsException {
        String id = request.getInternalId();
        IWcsSource<?, ?> source = WcsSourceAccessor.getSource(id);
        if (source == null) {
            throw new WcsException(Code.InvalidParameterValue,
                    "Coverage ID not found");
        }
        Composite3DBoundingBox bbox = request.getBbox();
        return source.getCoverage(id, request.getTimeSequence(), bbox,
                request.getFields());
    }

    private void close(InputStream in, OutputStream out) {
        try {
            if (in != null) {
                in.close();
            }
            if (out != null) {
                out.close();
            }
        } catch (Throwable t) {
            log.error("Unable to close streams", t);
        }
    }

    @Override
    public void getCoverage(OgcServiceInfo<WcsOpType> serviceinfo,
            GetCoverageRequest request, IOgcHttpResponse httpResp) {
        try {
            String format = request.getFormat();
            IWcsDataFormatter formatter = WcsSourceAccessor.getFormatMap().get(
                    format);
			if (formatter == null) {
				throw new WcsException(Code.InvalidFormat);
			}
            String externalId = request.getExternalId();
			Coverage coverage = requestCoverage(request);
			coverage.setName(CustomIdMap.internalToExternal(coverage.getName()));
			CoveragesType coveragesType = getCoverageOgcResponse(externalId,
					getCoverageId(externalId));

            try {
                // FIXME this code block makes my eyes bleed, needs to be
                // refactored
                OutputStream out = httpResp.getOutputStream();
                if (request.isDefacto()) {
                    httpResp.setContentType(formatter.getIdentifier());
                    InputStream in = formatter.format(coverage);
                    if (base64) {
                        in = new Base64InputStream(in, true);
                    }
                    IOUtils.copy(in, out);
                    out.flush();
                    out.close();
                    in.close();
                    return;
                }
                long timestamp = System.currentTimeMillis();
                String part = "_Part_" + timestamp;
                httpResp.setContentType("multipart/related;boundary=" + part);

                PrintStream stream = new PrintStream(out);
                String cid = getCoverageId(externalId);

                stream.println("--" + part);
                stream.println("Content-Type: text/xml");
                stream.println("Content-ID: <urn:ogc:wcs:1.1:coverages>");
                stream.println();
                stream.println(jaxbManager.marshal(wcsFactory
                        .createCoverages(coveragesType)));
                stream.println("--" + part);
                stream.println("Content-Type: " + formatter.getIdentifier());
                if (base64) {
                    stream.println("Content-Transfer-Encoding: base64");
                }
                stream.println("Content-ID: <" + cid + ">");
                stream.println();
                stream.flush();
                InputStream in = formatter.format(coverage);
                if (base64) {
                    in = new Base64InputStream(in, true);
                }
                IOUtils.copy(in, out);
                out.flush();
                out.close();
                in.close();
            } catch (Exception e) {
                log.error("Problem formatting coverage", e);
                throw new WcsException(Code.InternalServerError);
            }
        } catch (WcsException e) {
            sendTextResponse(httpResp,
                    getError(e, request.getExceptionFormat()));
        }
    }

    protected void sendTextResponse(IOgcHttpResponse httpRes,
            OgcResponse response) {
        MimeType mimetype = response.getMimetype();
        httpRes.setContentType(mimetype.toString());
        try {
            Writer writer = new OutputStreamWriter(httpRes.getOutputStream());
            writer.write(response.getBody().toString());
            writer.close();
        } catch (IOException e) {
            log.error("Problem sending response", e);
        }
    }

    /**
     * @param bbox
     * @return
     * @throws WcsException
     */
    protected Envelope getEnvelope(OgcBoundingBox bbox) throws WcsException {
        if (bbox == null) {
            throw new WcsException(Code.MissingParameterValue,
                    "Missing bounding box");
        }
        return new Envelope(bbox.getMinx(), bbox.getMaxx(), bbox.getMiny(),
                bbox.getMaxy());
    }

    /**
     * @param bbox
     * @return
     * @throws WcsException
     */
    protected CoordinateReferenceSystem getCrs(OgcBoundingBox bbox)
            throws WcsException {
        if (bbox != null) {
            try {
                return CrsLookup.lookup(bbox.getCrs());
            } catch (Exception e) {
                log.error("Unable to decode crs: " + bbox.getCrs(), e);
            }
        }
        throw new WcsException(Code.MissingParameterValue,
                "Missing bounding box.");
    }

    protected String getCoverageId(String identifier) {
        return COVERAGE_ID + identifier;
    }

    protected CoveragesType getCoverageOgcResponse(String identifier,
            String href) {

        CoveragesType coverages = new CoveragesType();
        List<ReferenceGroupType> coverageList = new ArrayList<ReferenceGroupType>();
        ReferenceGroupType rgt = new ReferenceGroupType();

        // Abstract
        // rgt.setAbstract(WcsJaxbUtils.getAsLangString("Abstract for "
        // + request.getIdentifier()));
        // Title
        // rgt.setTitle(WcsJaxbUtils.getAsLangString(request.getIdentifier()));

        // Identifier
        CodeType ident = new CodeType();
        ident.setValue(identifier);
        rgt.setIdentifier(ident);

        // Data reference
        ReferenceType rt = new ReferenceType();
        rt.setHref(href);

        List<JAXBElement<? extends AbstractReferenceBaseType>> refTypeList = new ArrayList<JAXBElement<? extends AbstractReferenceBaseType>>();
        refTypeList.add(owsFactory.createReference(rt));
        rgt.setAbstractReferenceBase(refTypeList);
        coverageList.add(rgt);
        coverages.setCoverage(coverageList);

        return coverages;

    }

    @Override
    public OgcResponse getError(WcsException e, MimeType exceptionFormat) {

        net.opengis.ows.v_1_1_0.ExceptionType et = new ExceptionType();
        et.setExceptionCode(e.getCode().toString());
        et.setExceptionText(getAsList(e.getMessage()));
        net.opengis.ows.v_1_1_0.ExceptionReport report = new ExceptionReport();
        report.setException(getAsList(et));
        String rval = "";
        MimeType mimeType = OgcResponse.TEXT_XML_MIME;
        if (exceptionFormat.equalsIgnoreParams(OgcResponse.TEXT_XML_MIME)) {
            try {
                rval = jaxbManager.marshal(report);
                mimeType = OgcResponse.TEXT_XML_MIME;
            } catch (JAXBException e1) {
                log.error("Unable to marshal WCS response", e1);
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
        } else if (exceptionFormat
                .equalsIgnoreParams(OgcResponse.TEXT_HTML_MIME)) {
            rval = "<html xmlns=\"http://www.w3.org/1999/xhtml\">";
            rval += "<br>An error occurred performing the request:<br>";
            rval += "<br>Error Code: " + e.getCode().toString();
            rval += "<br>Message: " + e.getMessage() + "</html>";
            mimeType = OgcResponse.TEXT_HTML_MIME;
        }

        OgcResponse resp = new OgcResponse(rval, mimeType, TYPE.TEXT);
        switch (e.getCode()) {
        case InternalServerError:
            resp.setError(ErrorType.INT_ERR);
            break;
        case OperationNotSupported:
            resp.setError(ErrorType.NOT_IMPLEMENTED);
            break;
        default:
            resp.setError(ErrorType.BAD_REQ);
        }
        return resp;
    }

    protected net.opengis.wms.v_1_3_0.Exception getExceptionInfo() {
        net.opengis.wms.v_1_3_0.Exception rval = new net.opengis.wms.v_1_3_0.Exception();
        rval.setFormat(getAsList("XML"));
        return rval;
    }

    @Override
    public WcsRequest getRequest(InputStream in) {
        Object obj;
        WcsRequest rval;
        try {
            String xml = getXml(in);
            obj = jaxbManager.unmarshal(xml);
            rval = getType(obj);
        } catch (Throwable e) {
            log.error("Unable to decode request", e);
            rval = new WcsRequest(Type.ERROR);
        }
        if (rval.getType().equals(Type.ERROR)) {
            OgcResponse error = getError(new WcsException(Code.InvalidRequest,
                    "Unable to decode request"), OgcResponse.TEXT_XML_MIME);
            rval.setRequest(error);
        }
        return rval;
    }

    public <T> List<T> getAsList(T... args) {
        List<T> rval = new LinkedList<T>();
        for (T t : args) {
            rval.add(t);
        }
        return rval;
    }

    protected WcsRequest getType(Object obj) {
        WcsRequest rval;
        if (obj instanceof GetCapabilitiesType) {
            rval = new GetCapRequest();
        } else if (obj instanceof GetCoverage) {
            rval = unwrap((GetCoverage) obj);
        } else if (obj instanceof DescribeCoverage) {
            DescribeCoverage req = (DescribeCoverage) obj;
            List<String> ids = req.getIdentifier();
            DescCoverageRequest dcr = new DescCoverageRequest();
            dcr.setIdentifiers(ids);
            rval = dcr;
        } else {
            rval = new WcsRequest(Type.ERROR);
        }
        return rval;
    }

    /**
     * @param obj
     * @return
     */
    protected WcsRequest unwrap(GetCoverage req) {
        try {
            return new GetCoverageRequest(req);
        } catch (WcsException e) {
            WcsRequest rval = new WcsRequest(Type.ERROR);
            rval.setRequest(e);
            return rval;
        }
    }

    protected List<RangeField> transform(RangeSubsetType subset) {
        if (subset == null) {
            return null;
        }
        List<FieldSubset> fields = subset.getFieldSubset();
        if (fields == null || fields.isEmpty()) {
            return null;
        }
        List<RangeField> rval = new ArrayList<RangeField>(fields.size());
        for (FieldSubset fs : fields) {
            rval.add(transform(fs));
        }
        return rval;
    }

    protected RangeField transform(FieldSubset fs) {
        String id = fs.getIdentifier().getValue();

        RangeField rval = new RangeField(id, null);
        String interp = fs.getInterpolationType();
        if (interp != null) {
            rval.setDefaultInterpolation(InterpolationType.valueOf(interp));
        }
        rval.setAxis(transform(fs.getAxisSubset()));
        return rval;
    }

    /**
     * @param axisSubset
     * @return
     */
    protected List<RangeAxis> transform(List<AxisSubset> axisSubset) {
        if (axisSubset == null || axisSubset.isEmpty()) {
            return null;
        }
        List<RangeAxis> rval = new ArrayList<RangeAxis>(axisSubset.size());
        for (AxisSubset as : axisSubset) {
            Set<String> keys = new HashSet<String>(as.getKey());
            rval.add(new RangeAxis(as.getIdentifier(), keys));
        }
        return rval;
    }

    protected OgcBoundingBox transform(BoundingBoxType bbox) {
        OgcBoundingBox rval = new OgcBoundingBox();
        rval.setCrs(bbox.getCrs());
        List<Double> lower = bbox.getLowerCorner();
        List<Double> upper = bbox.getUpperCorner();
        rval.setMaxx(upper.get(0));
        rval.setMaxy(upper.get(1));
        rval.setMinx(lower.get(0));
        rval.setMiny(lower.get(1));
        return rval;
    }

    protected DataTime getTime(TimeSequenceType tst) {
        if (tst == null) {
            return null;
        }
        List<Object> times = tst.getTimePositionOrTimePeriod();
        if (times == null || times.isEmpty()) {
            return null;
        }
        if (times.size() != 1) {
            log.warn("multiple times not supported in request, taking first");
        }
        Object timeObj = times.get(0);
        return parseTime(timeObj);
    }

    protected DataTime parseTime(Object timeObj) {
        if (timeObj instanceof TimePositionType) {
            TimePositionType pos = (TimePositionType) timeObj;
            return new DataTime(getTime(pos));
        } else {
            TimePeriodType period = (TimePeriodType) timeObj;
            Date start = getTime(period.getBeginPosition());
            Date end = getTime(period.getEndPosition());
            TimeRange range = new TimeRange(start, end);
            DataTime rval = new DataTime(start);
            rval.setValidPeriod(range);
            return rval;
        }
    }

    protected Date getTime(TimePositionType pos) {
        List<String> values = pos.getValue();
        if (values == null || values.isEmpty()) {
            log.error("unable to parse time");
            return null;
        }
        return DatatypeConverter.parseDateTime(values.get(0)).getTime();
    }

    protected String getXml(InputStream in) throws IOException {

        BufferedReader reader = new BufferedReader(new InputStreamReader(in));
        String rval = "";
        String line = "";
        while ((line = reader.readLine()) != null) {
            rval += line + "\n";
        }
        return rval;
    }

    protected OgcResponse marshalResponse(Object jaxbobject) {
        OgcResponse rval;
        try {
            String xml = jaxbManager.marshal(jaxbobject);
            rval = new OgcResponse(xml, OgcResponse.TEXT_XML_MIME, TYPE.TEXT);
        } catch (JAXBException e) {
            log.error("Unable to marshal WCS response", e);
            // TODO: real error code
            rval = getError(new WcsException(Code.OperationNotSupported),
                    OgcResponse.TEXT_XML_MIME);
        }
        return rval;
    }

    public boolean isBase64() {
        return base64;
    }

    public void setBase64(boolean base64) {
        this.base64 = base64;
    }

    /**
     * @return the restPort
     */
    public int getRestPort() {
        return restPort;
    }

    /**
     * @param restPort
     *            the restPort to set
     */
    public void setRestPort(int restPort) {
        this.restPort = restPort;
    }

    /**
     * @return the covStorePath
     */
    public String getCovStorePath() {
        return covStorePath;
    }

    /**
     * @param covStorePath
     *            the covStorePath to set
     */
    public void setCovStorePath(String covStorePath) {
        this.covStorePath = covStorePath;
    }

}
