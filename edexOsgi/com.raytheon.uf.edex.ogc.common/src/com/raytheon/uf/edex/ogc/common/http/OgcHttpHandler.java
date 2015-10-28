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
/**
 * 
 */
package com.raytheon.uf.edex.ogc.common.http;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.ws.rs.core.Response.Status;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.apache.commons.collections.map.CaseInsensitiveMap;
import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.http.AcceptHeaderParser;
import com.raytheon.uf.common.http.AcceptHeaderValue;
import com.raytheon.uf.common.http.MimeType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;
import com.raytheon.uf.edex.ogc.common.output.IOgcHttpResponse;
import com.raytheon.uf.edex.ogc.common.output.OgcResponseOutput;
import com.raytheon.uf.edex.ogc.common.output.ServletOgcResponse;


/**
 * Abstract base for HTTP handlers. Provides common utility methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011                     bclement     Initial creation
 * Nov 11, 2013 2539        bclement    added accept encoding parsing
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class OgcHttpHandler {

    public static final String USER_HEADER = "username";

    public static final String ROLES_HEADER = "roles";

    public static final String EXCEP_FORMAT_HEADER = "exceptions";

    public static final String VERSION_HEADER = "version";

    public static final String ACCEPT_VERSIONS_HEADER = "acceptversions";

    public static final String ACCEPT_ENC_HEADER = "accept-encoding";

    public abstract void handle(OgcHttpRequest request);

    private IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    public static MimeType getMimeType(Map<String, Object> map, String key)
            throws OgcException {
        String str = getString(map, key);
        if (str == null || str.isEmpty()) {
            return null;
        }
        try {
            return new MimeType(str);
        } catch (IllegalArgumentException e) {
            throw new OgcException(Code.InvalidParameterValue, e);
        }
    }

    public static String getString(Map<String, Object> map, String key)
            throws OgcException {
        Object obj = map.get(key);
        String rval = null;
        if (obj != null) {
            if (obj instanceof String) {
                rval = (String) obj;
            } else if (obj instanceof String[]) {
                // they may have multiple values for the entry
                String[] arr = (String[]) obj;
                rval = arr[0];
                for (int i = 1; i < arr.length; ++i) {
                    if (!rval.equalsIgnoreCase(arr[i])) {
                        throw new OgcException(Code.InvalidParameterValue,
                                "Multiple values for parameter: " + key);
                    }
                }
            } else if (obj instanceof Collection<?>) {
                Collection<?> col = (Collection<?>) obj;
                Iterator<?> i = col.iterator();
                rval = i.next().toString();
                while (i.hasNext()) {
                    if (!rval.equalsIgnoreCase(i.next().toString())) {
                        throw new OgcException(Code.InvalidParameterValue,
                                "Multiple values for parameter: " + key);
                    }
                }
            }
        }
        return rval;
    }

    public static String getVersionInHeader(Map<String, Object> headerMap)
            throws OgcException {
        return getString(headerMap, VERSION_HEADER);
    }

    public static String[] getAcceptVersionInHeader(
            Map<String, Object> headerMap) throws OgcException {
        return getStringArr(headerMap, ACCEPT_VERSIONS_HEADER);
    }

    /**
     * @param in
     * @return null if not found
     * @throws XMLStreamException
     */
    public static String[] getAttributeArrInRoot(InputStream in, String attrib)
            throws XMLStreamException {
        String vstr = getAttributeInRoot(in, attrib);
        if (vstr == null) {
            return null;
        }
        return StringUtils.split(vstr, ',');
    }

    /**
     * @param in
     * @param attrib
     * @return null if not found
     * @throws XMLStreamException
     */
    public static String getAttributeInRoot(InputStream in, String attrib)
            throws XMLStreamException {
        XMLStreamReader reader = XMLInputFactory.newInstance()
                .createXMLStreamReader(in);
        while (reader.next() != XMLStreamReader.START_ELEMENT) {
        }
        return getAttributeValue(reader, attrib);
    }

    /**
     * @param reader
     * @param attrib
     * @return null if attribute not found
     */
    protected static String getAttributeValue(XMLStreamReader reader,
            String attrib) {
        int count = reader.getAttributeCount();
        for (int i = 0; i < count; ++i) {
            String local = reader.getAttributeLocalName(i);
            if (local.equalsIgnoreCase(attrib)) {
                String ns = reader.getAttributeNamespace(i);
                return reader.getAttributeValue(ns, local);
            }
        }
        return null;
    }

    public static String[] getStringArr(Map<String, Object> map, String key) {
        Object obj = map.get(key);
        String[] rval = null;
        if (obj != null) {
            if (obj instanceof String[]) {
                rval = (String[]) obj;
            } else if (obj instanceof String) {
                rval = ((String) obj).split(",", -1);
            } else if (obj instanceof Collection<?>) {
                Collection<?> col = (Collection<?>) obj;
                rval = new String[col.size()];
                Iterator<?> iter = col.iterator();
                for (int i = 0; iter.hasNext(); ++i) {
                    rval[i] = iter.next().toString();
                }
            }
        }
        return rval;
    }

    public static Integer getInt(Map<String, Object> map, String key)
            throws OgcException {
        Object obj = map.get(key);
        Integer rval = null;
        if (obj != null) {
            if (obj instanceof Integer) {
                rval = (Integer) obj;
            } else {
                // try to decode string
                rval = getInt(getString(map, key));
            }
        }
        return rval;
    }

    public static Integer getInt(String str) {
        Integer rval = null;
        try {
            rval = Integer.parseInt(str);
        } catch (Exception e) {
            // leave rval as null
        }
        return rval;
    }

    public static Boolean getBool(Map<String, Object> map, String key)
            throws OgcException {
        Object obj = map.get(key);
        Boolean rval = null;
        if (obj != null) {
            if (obj instanceof Boolean) {
                rval = (Boolean) obj;
            } else {
                // decode as string
                rval = getBool(getString(map, key));
            }
        }
        return rval;
    }

    public static Boolean getBool(String str) {
        Boolean rval = null;
        try {
            rval = Boolean.parseBoolean(str);
        } catch (Exception e) {
            // leave rval as null
        }
        return rval;
    }

    protected OgcResponse handleError(OgcException e, MimeType exceptionFormat) {
        log.error("Error handler not configured for http handler: "
                + this.getClass());
        String rval = "<ows:ExceptionReport version=\"1.0.0\"\n";
        rval += "xsi:schemaLocation=\"http://www.opengis.net/ows\"\n";
        rval += "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \n"
                + "xmlns:ows=\"http://www.opengis.net/ows\">\n";
        rval += "<ows:Exception exceptionCode=\"" + e.getCode() + "\">\n";
        rval += "<ows:ExceptionText>" + e.getMessage()
                + "</ows:ExceptionText>\n";
        rval += "</ows:Exception></ows:ExceptionReport>\n";
        return new OgcResponse(rval, OgcResponse.TEXT_XML_MIME, TYPE.TEXT);
    }

    protected void sendResponse(IOgcHttpResponse httpRes, OgcResponse response)
            throws Exception {
        try {
            OgcResponseOutput.output(response, httpRes);
        } catch (OgcException e) {
            OgcResponse error = handleError(e, null);
            OgcResponseOutput.output(error, httpRes);
        }
    }

    @SuppressWarnings("unchecked")
    protected static Map<String, String> getDimensions(
            Map<String, Object> headers) {
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
     * Check if accept encoding header is present and modify response
     * accordingly.
     * 
     * @param headers
     * @param response
     * @throws OgcException
     *             on internal server error
     * @throws OgcHttpErrorException
     *             on HTTP protocol error condition
     */
    protected void acceptEncodingCheck(Map<String, Object> headers,
            ServletOgcResponse response) throws OgcException,
            OgcHttpErrorException {
        Object obj = headers.get(ACCEPT_ENC_HEADER);
        if (obj == null) {
            // omitted accept encoding signifies that they accept anything
            return;
        }
        if (!(obj instanceof String)) {
            log.error("Unsupported header type encountered: " + obj.getClass());
            throw new OgcException(Code.InternalServerError);
        }
        String encoding = (String) obj;
        boolean gzipAcceptable = false;
        boolean anyAcceptable = false;
        for (AcceptHeaderValue value : new AcceptHeaderParser(encoding)) {
            if (value.getEncoding().toLowerCase().contains("gzip")
                    && value.isAcceptable()) {
                gzipAcceptable = true;
            } else if (value.getEncoding().trim().equals("*")) {
                anyAcceptable = true;
            }
        }
        if (gzipAcceptable) {
            response.enableGzip();
        } else if (!anyAcceptable) {
            throw new OgcHttpErrorException(406);
        }
    }

    /**
     * Output HTTP protocol error
     * 
     * @param response
     * @param errorCode
     * @throws IOException
     */
    protected void outputHttpError(ServletOgcResponse response, int errorCode)
            throws IOException {
        response.setStatus(errorCode);
        response.setContentType("text/plain");
        Writer writer = null;
        try {
            writer = new OutputStreamWriter(response.getOutputStream());
            writer.write(Status.fromStatusCode(errorCode).toString());
        } finally {
            if (writer != null) {
                writer.flush();
                writer.close();
            }
        }
    }
}
