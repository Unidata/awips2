/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package com.raytheon.uf.edex.wcs;

import java.io.InputStream;
import java.util.Calendar;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.bind.DatatypeConverter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpHandler;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpRequest;
import com.raytheon.uf.edex.ogc.common.output.OgcResponseOutput;
import com.raytheon.uf.edex.wcs.WcsException.Code;
import com.raytheon.uf.edex.wcs.provider.OgcWcsProvider.WcsOpType;
import com.raytheon.uf.edex.wcs.reg.RangeField;
import com.raytheon.uf.edex.wcs.reg.RangeParseException;
import com.raytheon.uf.edex.wcs.request.DefactoEnabler;
import com.raytheon.uf.edex.wcs.request.DescCoverageRequest;
import com.raytheon.uf.edex.wcs.request.GetCapRequest;
import com.raytheon.uf.edex.wcs.request.GetCoverageRequest;
import com.raytheon.uf.edex.wcs.request.GetDataRequest;
import com.raytheon.uf.edex.wcs.request.WcsRequest;
import com.raytheon.uf.edex.wcs.request.WcsRequest.Type;

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
 * @author
 * @version 1
 */
public class WcsHttpHandler implements OgcHttpHandler {

	protected WcsProvider provider;

	public static final String REQUEST_HEADER = "request";

	public static final String VERSION_HEADER = "version";

	public static final String FORMAT_HEADER = "format";

	public static final String UPDATESEQ_HEADER = "updatesequence";

	public static final String BBOX_HEADER = "boundingbox";

	public static final String MIME_HEADER = "Content-Type";

	protected static final String CAP_PARAM = "getcapabilities";

	protected static final String GET_PARAM = "getcoverage";

	protected static final String DESC_PARAM = "describecoverage";

	protected static final String GET_DATA_PARAM = "getdata";

	protected static final String IDENTIFIERS = "identifiers";

	protected static final String IDENTIFIER = "identifier";

	protected static final String DOMAIN_SUBSET = "domainsubset";

	protected static final String OUTPUT = "output";

	protected static final String GROUP = "group";

	protected static final String DATASET = "dataset";

	protected static final String RANGE_SUBSET = "rangesubset";

	protected static final String LAYER = "layer";

	protected static final String TIME_SEQUENCE = "timesequence";

	protected static final String STORE = "store";

	protected static final String GRID_BASE_CRS = "gridbasecrs";

	protected static final String GRID_TYPE = "gridtype";

	protected static final String GRID_ORIGIN = "gridorigin";

	protected static final String GRID_OFFSETS = "gridoffsets";

	private Log log = LogFactory.getLog(this.getClass());

	public WcsHttpHandler(WcsProvider provider) {
		this.provider = provider;
	}

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
		HttpServletRequest httpRequest = ogcRequest.getRequest();
		Map<String, Object> headers = ogcRequest.getHeaders();
		OgcResponse rval = null;

		OgcServiceInfo<WcsOpType> serviceInfo = getServiceInfo(httpRequest);
		WcsRequest request;

		if (ogcRequest.isPost()) {
			InputStream is = httpRequest.getInputStream();
			request = provider.getRequest(is);
		} else {
			request = getRequestFromHeaders(headers);
		}

		// Validate exception format parameter
		rval = validateExceptionFormat(request);
		if (rval != null) {
			sendResponse(response, rval);
		}

		switch (request.getType()) {
		case DescribeCoverage:
			rval = provider.describeCoverageType(serviceInfo,
					(DescCoverageRequest) request);
			break;
		case GetCapabilities:
			rval = provider.getCapabilities(serviceInfo,
					(GetCapRequest) request);
			break;
		case GetCoverage:
			// get coverage does it's own output
			GetCoverageRequest gcr = (GetCoverageRequest) request;
			if (DefactoEnabler.HEADER_VALUE.equals(headers
					.get(DefactoEnabler.HEADER_KEY))) {
				gcr.setDefacto(true);
			}
			provider.getCoverage(serviceInfo, gcr, response);
			return;
		case ERROR:
			rval = (OgcResponse) request.getRequest();
			break;
		}

		sendResponse(response, rval);
	}

	/**
	 * @param request
	 * @return
	 */
	private OgcResponse validateExceptionFormat(WcsRequest request) {
		if (!request.getExceptionFormat().equalsIgnoreCase(
				OgcResponse.TEXT_HTML_MIME)
				&& !request.getExceptionFormat().equalsIgnoreCase(
						OgcResponse.TEXT_XML_MIME)) {
			return provider.getError(
					new WcsException(Code.InvalidParameterValue,
							"exceptions parameter invalid"),
					OgcResponse.TEXT_XML_MIME);
		}
		return null;
	}

	private OgcServiceInfo<WcsOpType> getServiceInfo(HttpServletRequest request) {
		// FIXME get address from spring
		int port = request.getServerPort();
		String base = "http://" + request.getServerName();
		if (port != 80) {
			base += ":" + port;
		}
		base += request.getPathInfo();
		OgcServiceInfo<WcsOpType> rval = new OgcServiceInfo<WcsOpType>(base);
		rval.addOperationInfo(getOp(base + "?request=" + CAP_PARAM, base,
				WcsOpType.GetCapabilities));
		rval.addOperationInfo(getOp(base + "?request=describecoverage", base,
				WcsOpType.DescribeCoverage));
		rval.addOperationInfo(getOp(base + "?request=getcoverage", base,
				WcsOpType.GetCoverage));

		return rval;
	}

	protected OgcOperationInfo<WcsOpType> getOp(String get, String post,
			WcsOpType type) {
		OgcOperationInfo<WcsOpType> rval = new OgcOperationInfo<WcsOpType>(type);
		// FIXME get version from provider
		rval.setHttpGetRes(get);
		rval.setHttpPostRes(post);
		rval.addVersion("1.1.2");
		rval.addAcceptVersions("1.1.2");
		rval.addService("WCS");
		rval.addFormat("text/xml");
		return rval;
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
				try {
					rval = Integer.parseInt((String) obj);
				} catch (Exception e) {
					// leave rval as null
				}
			}
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

	protected WcsRequest getRequestFromHeaders(Map<String, Object> headers) {
		WcsRequest rval = null;
		Object obj = headers.get(REQUEST_HEADER);
		String exceptionFormat = OgcResponse.TEXT_XML_MIME;
		if (obj instanceof String) {
			exceptionFormat = getString(headers.get(EXCEP_FORMAT_HEADER));
			if (exceptionFormat == null || exceptionFormat.isEmpty()) {
				exceptionFormat = OgcResponse.TEXT_XML_MIME;
			}
			String req = (String) obj;
			if (req.equalsIgnoreCase(CAP_PARAM)) {
				rval = new GetCapRequest();
			} else if (req.equalsIgnoreCase(DESC_PARAM)) {
				rval = buildDescCoverageRequest(headers);
			} else if (req.equalsIgnoreCase(GET_PARAM)) {
				try {
					rval = buildGetCoverageRequest(headers);
				} catch (RangeParseException e) {
					log.error(e);
					rval = new WcsRequest(Type.ERROR);
					String msg = "Invalid range parameter";
					WcsException ex = new WcsException(
							Code.InvalidParameterValue, msg);
					rval.setRequest(provider.getError(ex, exceptionFormat));
				}
			} else if (req.equalsIgnoreCase(GET_DATA_PARAM)) {
				rval = buildGetDataRequest(headers);
			}
		}

		if (rval == null) {
			OgcResponse error = provider.getError(new WcsException(
					Code.InvalidRequest, "Unable to decode request"),
					exceptionFormat);
			rval = new WcsRequest(Type.ERROR);
			rval.setRequest(error);
		}

		rval.setExceptionFormat(exceptionFormat);

		return rval;
	}

	private WcsRequest buildGetDataRequest(Map<String, Object> headers) {
		GetDataRequest rval = new GetDataRequest();

		String groupName = getHeader(GROUP, headers);
		if (groupName != null) {
			rval.setGroup(groupName);
		}

		String datasetName = getHeader(DATASET, headers);
		if (datasetName != null) {
			rval.setDataset(datasetName);
		}

		return rval;
	}

	private WcsRequest buildGetCoverageRequest(Map<String, Object> headers)
			throws RangeParseException {
		GetCoverageRequest rval = new GetCoverageRequest();

		String identifier = getHeader(IDENTIFIER, headers);
		rval.setIdentifier(identifier);

		String format = getHeader(FORMAT_HEADER, headers);
		rval.setFormat(format);

		String timeSequence = getHeader(TIME_SEQUENCE, headers);
		if (timeSequence != null) {
			rval.setTimeSequence(parseTime(timeSequence));
		}

		String rangesubset = getHeader(RANGE_SUBSET, headers);
		if (rangesubset != null) {
			rval.setFields(RangeField.getRanges(rangesubset));
		}

		String bbox = getHeader(BBOX_HEADER, headers);
		if (bbox != null) {
			rval.setBbox(parseBbox(bbox));
		}

		return rval;
	}

	protected DataTime parseTime(String timeStr) {
		// TODO handle multi-times and periods in ISO 8601
		Calendar cal = DatatypeConverter.parseDateTime(timeStr);
		return new DataTime(cal);
	}

	private WcsRequest buildDescCoverageRequest(Map<String, Object> headers) {
		DescCoverageRequest rval = new DescCoverageRequest();

		String outputformat = getHeader(OUTPUT, headers);
		if (outputformat != null) {
			rval.setOutputformat(outputformat);
		}

		String[] identifiers = getHeaderArr(IDENTIFIERS, headers);
		if (identifiers != null) {
			rval.setIdentifiers(identifiers);
		}

		return rval;
	}

	protected OgcBoundingBox parseBbox(String bbox) {
		String[] parts = bbox.split(",");
		OgcBoundingBox rval = null;
		if (parts.length == 5) {
			rval = new OgcBoundingBox();
			try {
				rval.setMinx(Double.parseDouble(parts[0]));
				rval.setMiny(Double.parseDouble(parts[1]));
				rval.setMaxx(Double.parseDouble(parts[2]));
				rval.setMaxy(Double.parseDouble(parts[3]));
				rval.setCrs(parts[4]);
			} catch (NumberFormatException e) {
				// FIXME return error
			}
		}// else TODO handle non 2d WGS84
		return rval;
	}

	protected String[] getHeaderArr(String name, Map<String, Object> headers) {
		String[] rval;
		String value = getHeader(name, headers);
		if (value != null) {
			rval = value.split(",");
		} else {
			rval = new String[0];
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

	protected void sendResponse(HttpServletResponse httpRes,
			OgcResponse response) throws Exception {
		OgcResponseOutput.output(response, httpRes);
	}

	protected String getLayerLevel(String rangesubset) {
		String layerValue = "0";

		if (rangesubset != null) {
			int datasetIndex = rangesubset.indexOf(DATASET);
			if (datasetIndex > -1) {
				int layerIndex = rangesubset.indexOf(LAYER, datasetIndex);
				if (layerIndex > -1) {
					int layerStartIndex = rangesubset.indexOf("[", layerIndex);
					if (layerStartIndex > -1) {
						int layerEndIndex = rangesubset.indexOf("]",
								layerStartIndex);
						layerValue = rangesubset.substring(layerStartIndex + 1,
								layerEndIndex);
					}
				}
			}
		}

		return layerValue;
	}
}
