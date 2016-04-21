/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.provider;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.http.MimeType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcPrefix;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcTimeRange;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpHandler;
import com.raytheon.uf.edex.ogc.common.output.IOgcHttpResponse;
import com.raytheon.uf.edex.ogc.common.output.OgcResponseOutput;
import com.raytheon.uf.edex.wfs.IWfsProvider;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.raytheon.uf.edex.wfs.request.SortBy;
import com.raytheon.uf.edex.wfs.request.SortBy.Order;

/**
 * Abstract base for WFS providers for specific service versions. Provides
 * common utility methods and constants.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2012            bclement     Initial creation
 * Nov 11, 2013 2539        bclement    moved registry/marshal from children
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public abstract class AbstractWfsProvider implements IWfsProvider {

    public static final String REQUEST_HEADER = "request";

    public static final String CAP_PARAM = "getcapabilities";

    public static final String DESC_PARAM = "describefeaturetype";

    public static final String GET_PARAM = "getfeature";

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

    public static final String OUTFORMAT_HEADER = "outputformat";

    public static final String EXCEP_FORMAT_HEADER = "exceptions";

    public static final String LIST_QUERY_PARAM = "liststoredqueries";

    public static final String DESC_QUERY_PARAM = "describestoredqueries";

    public static final String QUERY_IDS_HEADER = "storedquery_id";

    public static final String TIME_HEADER = "time";

    protected static final Pattern nspattern = Pattern
            .compile("xmlns\\((\\S+)=(\\S+)\\)");

    protected final String slashes = "\\\\,";

    protected final String lamdas = "λλλ";

    protected final Pattern sortBys = Pattern.compile("\\s+");

    protected final Pattern slash = Pattern.compile("\\\\");

    protected final Pattern comma = Pattern.compile(",");

    protected final IUFStatusHandler statusHandler = UFStatus.getHandler(this
            .getClass());

    private static ThreadLocal<SimpleDateFormat> ogcDateFormat = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {

            SimpleDateFormat sdf = new SimpleDateFormat(
                    "yyyy-MM-dd'T'HH:mm:ss.SSSZ");
            return sdf;
        }

    };

    protected final WfsRegistryImpl registry;

    protected final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    /**
     * 
     */
    public AbstractWfsProvider(WfsRegistryImpl registry) {
        this.registry = registry;
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

    protected Map<String, String> getNameSpaceMap(Map<String, Object> headers)
            throws OgcException {
        Map<String, String> nsmap = new HashMap<String, String>();
        String ns = OgcHttpHandler.getString(headers, NS_HEADER);
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
        // TODO Use pattern for replace
        String val = str.replaceAll(slashes, lamdas);
        String[] rval = comma.split(val);
        for (int i = 0; i < rval.length; ++i) {
            rval[i] = rval[i].replaceAll(lamdas, comma.pattern());
        }
        return rval;
    }

    protected String[] splitOnSlash(String str) {
        String[] times = slash.split(str);
        return times;
    }

    protected String[] splitOnParens(String name, Map<String, Object> headers)
            throws OgcException {
        String val = OgcHttpHandler.getString(headers, name);
        String[] rval;
        if (val != null) {
            val = val.replaceAll("\\s*\\(", "");
            rval = val.split("\\)");
        } else {
            rval = new String[0];
        }
        return rval;
    }

    protected OgcTimeRange getTimeRange(String stimes) {
        OgcTimeRange otr = null;
        // we assume it is a start time going up to present
        try {

            String[] times = splitOnSlash(stimes);

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
     * Read input stream to string
     * 
     * @param in
     * @return
     * @throws IOException
     */
    protected String getXml(InputStream in) throws IOException {
        java.util.Scanner scanner = new java.util.Scanner(in);
        try {
            return scanner.useDelimiter("\\A").next();
        } finally {
            scanner.close();
        }
    }

    /**
     * Marshal object through response. Response cannot be reused after this
     * method is called.
     * 
     * @param jaxbobject
     * @param mimeType
     * @param response
     * @throws Exception
     *             on unrecoverable error attempting to send response
     */
    protected void marshalResponse(Object jaxbobject, MimeType mimeType,
            IOgcHttpResponse response) throws Exception {
        OutputStream out = null;
        try {
            out = response.getOutputStream();
            response.setContentType(mimeType.toString());
            registry.marshal(jaxbobject, out);
        } catch (Exception e) {
            log.error("Unable to marshal WFS response", e);
            OgcResponse err = getError(new WfsException(
                    Code.OperationProcessingFailed), null);
            OgcResponseOutput.sendText(err, response, out);
        } finally {
            if (out != null) {
                out.flush();
                out.close();
            }
        }
    }

    /**
     * Create an error response
     * 
     * @param e
     * @param exceptionFormat
     * @return
     */
    public abstract OgcResponse getError(WfsException e,
            MimeType exceptionFormat);

}
