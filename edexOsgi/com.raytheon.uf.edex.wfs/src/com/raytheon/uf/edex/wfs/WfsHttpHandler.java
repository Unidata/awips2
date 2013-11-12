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
 * 
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 * May 30. 2013 753        dhladky      updates
 *
 */
package com.raytheon.uf.edex.wfs;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;

import javax.servlet.http.HttpServletRequest;
import javax.xml.stream.XMLStreamException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.InvalidVersionException;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.Version;
import com.raytheon.uf.edex.ogc.common.http.EndpointInfo;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpErrorException;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpHandler;
import com.raytheon.uf.edex.ogc.common.http.OgcHttpRequest;
import com.raytheon.uf.edex.ogc.common.output.OgcResponseOutput;
import com.raytheon.uf.edex.ogc.common.output.ServletOgcResponse;

/**
 * WFS http handler. Delegates WFS HTTP requests to providers using version
 * negotiation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WfsHttpHandler extends OgcHttpHandler {

    protected final TreeMap<Version, IWfsProvider> providers;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WfsHttpHandler.class);

    protected static final int READ_LIMIT = 8 * 1024;

    public WfsHttpHandler(IWfsProvider... providers) {
        this.providers = new TreeMap<Version, IWfsProvider>();
        for (IWfsProvider p : providers) {
            try {
                Version v = new Version(p.getVersion());
                this.providers.put(v, p);
            } catch (InvalidVersionException e) {
                statusHandler.error("Invalid version for provider: " + p, e);
            }
        }
    }

    @Override
    public void handle(OgcHttpRequest req) {
        try {
            statusHandler.info("Request from: "
                    + req.getRequest().getRemoteAddr());
            long time = System.currentTimeMillis();
            handleInternal(req);
            long time2 = System.currentTimeMillis();
            statusHandler.info("Processed: " + req.getRequest().getRemoteAddr()
                    + " in " + (time2 - time) + " ms");
        } catch (Exception e) {
            statusHandler.error("Unable to handle request", e);
        }
    }

    protected void handleInternal(OgcHttpRequest req) throws Exception {
        Map<String, Object> headers = req.getHeaders();
        ServletOgcResponse response = new ServletOgcResponse(req.getResponse());

        HttpServletRequest httpReq = req.getRequest();
        int port = httpReq.getLocalPort();
        String host = httpReq.getServerName();
        String path = "wfs";
        // TODO dynamic path and protocol
        EndpointInfo info = new EndpointInfo(host, port, path);
        try {
            try {
                acceptEncodingCheck(headers, response);
            } catch (OgcHttpErrorException e) {
                // there was a problem with the acceptable encodings
                outputHttpError(response, e.getCode());
                return;
            }
            if (req.isPost()) {
                InputStream is = req.getInputStream();
                BufferedInputStream bufin = new BufferedInputStream(is);
                IWfsProvider provider = getProviderPost(bufin);
                provider.handlePost(bufin, info, response);
                is.close();
            } else {
                IWfsProvider provider = getProviderGet(headers);
                provider.handleGet(headers, info, response);
            }
        } catch (OgcException e) {
            // this will only work if response hasn't been used yet
            OgcResponse err = handleError(e, OgcResponse.TEXT_XML_MIME);
            OgcResponseOutput.output(err, response);
        }

    }

    protected IWfsProvider getProviderGet(Map<String, Object> headers)
            throws OgcException {
        String vstr = getVersionInHeader(headers);
        String[] vstrs;
        IWfsProvider rval;
        if (vstr != null) {
            rval = match(vstr);
        } else if ((vstrs = getAcceptVersionInHeader(headers)) != null) {
            rval = match(vstrs);
        } else {
            rval = providers.lastEntry().getValue();
        }
        return rval;
    }

    protected IWfsProvider getProviderPost(BufferedInputStream bufin)
            throws OgcException {
        bufin.mark(READ_LIMIT);
        String vstr = getVersion(bufin);
        String[] vstrs;
        IWfsProvider rval;
        if (vstr != null) {
            rval = match(vstr);
        } else if ((vstrs = getAcceptVersions(bufin)) != null) {
            rval = match(vstrs);
        } else {
            rval = providers.lastEntry().getValue();
        }
        return rval;
    }

    private IWfsProvider match(String... versions) throws OgcException {
        for (String s : versions) {
            Version v = parseVersion(s);
            IWfsProvider provider = providers.get(v);
            if (provider != null) {
                return provider;
            }
        }
        throw new OgcException(Code.VersionNegotiationFailed,
                "Unsupported version(s): " + Arrays.toString(versions));
    }

    private Version parseVersion(String version) throws OgcException {
        Version v;
        try {
            v = new Version(version);
        } catch (InvalidVersionException e) {
            throw new OgcException(Code.InvalidParameterValue,
                    "Invalid version string: " + version);
        }
        return v;
    }

    private String getVersion(BufferedInputStream bufin) throws OgcException {
        String vstr;
        try {
            vstr = getAttributeInRoot(bufin, VERSION_HEADER);
        } catch (XMLStreamException e) {
            throw new OgcException(Code.InvalidRequest,
                    "Unable to parse request XML");
        }
        reset(bufin);
        return vstr;
    }

    private String[] getAcceptVersions(BufferedInputStream bufin)
            throws OgcException {
        String[] rval;
        try {
            rval = getAttributeArrInRoot(bufin, ACCEPT_VERSIONS_HEADER);
        } catch (XMLStreamException e) {
            throw new OgcException(Code.InvalidRequest,
                    "Unable to parse request XML");
        }
        reset(bufin);
        return rval;
    }

    private void reset(BufferedInputStream bufin) throws OgcException {
        try {
            bufin.reset();
        } catch (IOException e) {
            statusHandler.error("Unable to reset buffer", e);
            throw new OgcException(Code.InternalServerError);
        }
    }

}