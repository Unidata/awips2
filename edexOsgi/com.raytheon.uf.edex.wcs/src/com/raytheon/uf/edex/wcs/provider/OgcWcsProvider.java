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
 * 
 *
 */

package com.raytheon.uf.edex.wcs.provider;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpServletResponse;
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
import net.opengis.wcs.v_1_1_2.DomainSubsetType;
import net.opengis.wcs.v_1_1_2.GetCoverage;
import net.opengis.wcs.v_1_1_2.GridCrsType;
import net.opengis.wcs.v_1_1_2.ObjectFactory;
import net.opengis.wcs.v_1_1_2.OutputType;
import net.opengis.wcs.v_1_1_2.RangeSubsetType;
import net.opengis.wcs.v_1_1_2.RangeSubsetType.FieldSubset;
import net.opengis.wcs.v_1_1_2.TimePeriodType;
import net.opengis.wcs.v_1_1_2.TimeSequenceType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.springframework.context.ApplicationContext;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcPrefix;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.ErrorType;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.jaxb.OgcJaxbManager;
import com.raytheon.uf.edex.ogc.common.spatial.CrsLookup;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.WcsException.Code;
import com.raytheon.uf.edex.wcs.WcsProvider;
import com.raytheon.uf.edex.wcs.format.WcsDataFormatter;
import com.raytheon.uf.edex.wcs.reg.Coverage;
import com.raytheon.uf.edex.wcs.reg.CoverageDescription;
import com.raytheon.uf.edex.wcs.reg.RangeAxis;
import com.raytheon.uf.edex.wcs.reg.RangeField;
import com.raytheon.uf.edex.wcs.reg.RangeField.InterpolationType;
import com.raytheon.uf.edex.wcs.reg.WcsSource;
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

	protected OgcJaxbManager jaxbManager;

	protected String COVERAGE_ID = "cid:Coverage-";

	protected Log log = LogFactory.getLog(this.getClass());

	protected DescCoverageBuilder descCoverage;

	protected CapabilitiesBuilder capbuilder;

	protected WcsSourceAccessor reg = new WcsSourceAccessor();

	protected boolean base64 = true;

	protected net.opengis.ows.v_1_1_0.ObjectFactory owsFactory = new net.opengis.ows.v_1_1_0.ObjectFactory();

	protected ObjectFactory wcsFactory = new ObjectFactory();

	protected static final Map<String, String> NS_MAP = new ConcurrentHashMap<String, String>();

	protected static final Map<String, WcsDataFormatter> formatMap = new HashMap<String, WcsDataFormatter>();

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

	public OgcWcsProvider() throws JAXBException {
		this.jaxbManager = new OgcJaxbManager(new Class<?>[] {
				ObjectFactory.class,
				net.opengis.ows.v_1_1_0.ObjectFactory.class,
				net.opengis.gml.v_3_1_1.ObjectFactory.class,
				net.opengis.wms.v_1_3_0.ObjectFactory.class });
		jaxbManager.setPrefixMap(NS_MAP);
		ApplicationContext ctx = EDEXUtil.getSpringContext();
		String[] beans = ctx.getBeanNamesForType(WcsDataFormatter.class);
		for (String bean : beans) {
			WcsDataFormatter df = (WcsDataFormatter) ctx.getBean(bean);
			formatMap.put(df.getIdentifier(), df);
		}
		List<String> formats = new ArrayList<String>(formatMap.keySet());
		this.descCoverage = new DescCoverageBuilder(formats);
		this.capbuilder = new CapabilitiesBuilder(formats);
	}

	@Override
	public OgcResponse describeCoverageType(
			OgcServiceInfo<WcsOpType> serviceinfo, DescCoverageRequest request) {
		CoverageDescriptions rval = new CoverageDescriptions();
		String[] ids = request.getIdentifiers();
		List<CoverageDescriptionType> descs = null;
		if (ids == null) {
			return getError(new WcsException(Code.MissingParameterValue),
					request.getExceptionFormat());
		}
		descs = new ArrayList<CoverageDescriptionType>(ids.length);
		for (String id : ids) {
			try {
				descs.add(descCov(id));
			} catch (WcsException e) {
				return getError(e, request.getExceptionFormat());
			}
		}
		rval.setCoverageDescription(descs);
		return marshalResponse(rval);
	}

	protected CoverageDescriptionType descCov(String id) throws WcsException {
		WcsSource source = reg.getSource(id);
		if (source == null) {
			throw new WcsException(Code.LayerNotDefined);
		}
		CoverageDescription cd = source.describeCoverage(id);
		return descCoverage.getCoverageDescriptionType(cd);
	}

	@Override
	public OgcResponse getCapabilities(OgcServiceInfo<WcsOpType> serviceinfo,
			GetCapRequest request) {
		Capabilities capabilities = capbuilder.getCapabilities(serviceinfo,
				reg.getCoverages());
		return marshalResponse(capabilities);
	}

	@Override
	public void getCoverage(OgcServiceInfo<WcsOpType> serviceinfo,
			GetCoverageRequest request, HttpServletResponse httpResp) {
		try {
			String format = request.getFormat();
			WcsDataFormatter formatter = formatMap.get(format);
			if (formatter == null) {
				throw new WcsException(Code.InvalidFormat);
			}
			String id = request.getIdentifier();
			WcsSource source = reg.getSource(id);
			if (source == null) {
				throw new WcsException(Code.LayerNotDefined);
			}
			OgcBoundingBox bbox = request.getBbox();
			CoordinateReferenceSystem crs = getCrs(bbox);
			Envelope env = getEnvelope(bbox);
			Coverage coverage = source.getCoverage(id,
					request.getTimeSequence(), crs, env, request.getFields());
			CoveragesType coveragesType = getCoverageOgcResponse(serviceinfo,
					id);

			try {
				OutputStream out = httpResp.getOutputStream();
				if (request.isDefacto()) {
					httpResp.setContentType(formatter.getIdentifier());
					formatter.format(coverage, out, false);
					out.flush();
					out.close();
					return;
				}
				long timestamp = System.currentTimeMillis();
				String part = "_Part_" + timestamp;
				httpResp.setContentType("multipart/related;boundary=" + part);

				PrintStream stream = new PrintStream(out);
				String cid = getCoverageId(id);

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
				formatter.format(coverage, out, base64);
				out.flush();
				out.close();
			} catch (Exception e) {
				log.error("Problem formatting coverage", e);
				throw new WcsException(Code.InternalServerError);
			}
		} catch (WcsException e) {
			sendTextResponse(httpResp,
					getError(e, request.getExceptionFormat()));
		}
	}

	protected void sendTextResponse(HttpServletResponse httpRes,
			OgcResponse response) {
		String mimetype = response.getMimetype();
		httpRes.setContentType(mimetype);
		try {
			PrintWriter writer = httpRes.getWriter();
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

	protected CoveragesType getCoverageOgcResponse(
			OgcServiceInfo<WcsOpType> serviceinfo, String identifier) {

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
		String cid = getCoverageId(identifier);
		ReferenceType rt = new ReferenceType();
		rt.setHref(cid);

		List<JAXBElement<? extends AbstractReferenceBaseType>> refTypeList = new ArrayList<JAXBElement<? extends AbstractReferenceBaseType>>();
		refTypeList.add(owsFactory.createReference(rt));
		rgt.setAbstractReferenceBase(refTypeList);
		coverageList.add(rgt);
		coverages.setCoverage(coverageList);

		return coverages;

	}

	@Override
	public OgcResponse getError(WcsException e, String exceptionFormat) {

		net.opengis.ows.v_1_1_0.ExceptionType et = new ExceptionType();
		et.setExceptionCode(e.getCode().toString());
		et.setExceptionText(getAsList(e.getMessage()));
		net.opengis.ows.v_1_1_0.ExceptionReport report = new ExceptionReport();
		report.setException(getAsList(et));
		String rval = "";
		String mimeType = OgcResponse.TEXT_XML_MIME;
		if (exceptionFormat.equalsIgnoreCase(OgcResponse.TEXT_XML_MIME)) {
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
		} else if (exceptionFormat.equalsIgnoreCase(OgcResponse.TEXT_HTML_MIME)) {
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
			dcr.setIdentifiers(ids.toArray(new String[ids.size()]));
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
		GetCoverageRequest rval = new GetCoverageRequest();
		DomainSubsetType ds = req.getDomainSubset();
		rval.setTimeSequence(getTime(ds.getTemporalSubset()));
		BoundingBoxType bbox = (BoundingBoxType) ds.getBoundingBox().getValue();
		rval.setBbox(transform(bbox));
		rval.setIdentifier(req.getIdentifier().getValue());
		OutputType output = req.getOutput();
		rval.setFormat(output.getFormat());
		GridCrsType gridCRS = output.getGridCRS();
		if (gridCRS != null) {
			rval.setGridBaseCrs(gridCRS.getGridBaseCRS());
			rval.setGridOffsets(gridCRS.getGridOffsets());
			rval.setGridOrigin(gridCRS.getGridOrigin());
			rval.setGridType(gridCRS.getGridType());
		}
		rval.setFields(transform(req.getRangeSubset()));
		return rval;
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
			rval = new OgcResponse(xml, "text/xml", TYPE.TEXT);
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

}
