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
package com.raytheon.collaboration.dataserver;

import java.io.File;
import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.util.log.Log;
import org.eclipse.jetty.util.log.Logger;

import com.raytheon.collaboration.dataserver.storage.FileManager;
import com.raytheon.uf.common.http.AcceptHeaderParser;
import com.raytheon.uf.common.http.AcceptHeaderValue;

/**
 * Servlet for storing and retrieving collaboration data objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 05, 2014 2756       bclement    Initial creation
 * Mar 11, 2014 2827       bclement    pass response object to FileManager in doGet
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DataService extends HttpServlet {

    private static final long serialVersionUID = 3828421078339628468L;

    private final File base;

    private final FileManager manager;

    private final Logger log = Log.getLogger(this.getClass());

    public static final String XML_CONTENT_TYPE = "text/xml";

    public static final String HTML_CONTENT_TYPE = "text/html";

    public static final String BINARY_CONTENT_TYPE = "application/octet-stream";

    public static final String ACCEPT_HEADER = "accept";

    private final boolean legacyMode;

    /**
     * @param base
     *            base storage directory
     */
    public DataService(File base) {
        this.base = base;
        if (!base.exists()) {
            if (!base.mkdirs()) {
                throw new IllegalStateException(
                        "Unable to create storage base directory: "
                                + base.getAbsolutePath());
            }
        }
        this.manager = new FileManager(base);
        this.legacyMode = Config.getBool(Config.LEGACY_MODE_KEY,
                Config.LEGACY_MODE_DEFAULT);
    }

    /**
     * 
     */
    public DataService() {
        this(new File(Config.getProp(Config.STOREDIR_KEY,
                Config.STOREDIR_DEFAULT)));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest
     * , javax.servlet.http.HttpServletResponse)
     */
    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp)
            throws ServletException, IOException {
        try {
            File file = getFile(req);
            if (!file.exists()) {
                throw new RestException(HttpServletResponse.SC_NOT_FOUND,
                        "No Such Resource: " + file.getAbsolutePath());
            }
            if (file.isDirectory()) {
                if (acceptsXml(req)) {
                    resp.setContentType(XML_CONTENT_TYPE);
                    manager.readDirectoryAsXml(file, resp);
                } else {
                    resp.setContentType(HTML_CONTENT_TYPE);
                    manager.readDirectoryAsHtml(file, resp);
                }
            } else {
                resp.setContentType(BINARY_CONTENT_TYPE);
                manager.readFile(file, resp);
            }
        } catch (IOException e) {
            log.warn("Problem handling GET", e);
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (RestException e) {
            log.debug(e.getLocalizedMessage(), e);
            resp.sendError(e.getCode());
        }
    }

    /**
     * @param req
     * @return true if request has an accepts header that lists xml content type
     */
    private boolean acceptsXml(HttpServletRequest req) {
        String header = req.getHeader(ACCEPT_HEADER);
        if (header == null || header.trim().isEmpty()) {
            return false;
        }
        for (AcceptHeaderValue value : new AcceptHeaderParser(header)) {
            String type = value.getEncoding();
            if (type.equalsIgnoreCase(XML_CONTENT_TYPE) && value.isAcceptable()) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.servlet.http.HttpServlet#doPut(javax.servlet.http.HttpServletRequest
     * , javax.servlet.http.HttpServletResponse)
     */
    @Override
    protected void doPut(HttpServletRequest req, HttpServletResponse resp)
            throws ServletException, IOException {
        try {
            File file = getFile(req);
            manager.writeFile(req.getInputStream(), file);
        } catch (IOException e) {
            log.warn("Problem handling PUT", e);
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (RestException e) {
            log.debug(e.getLocalizedMessage(), e);
            resp.sendError(e.getCode());
        }
    }

    /**
     * Create file object from request path
     * 
     * @param req
     * @return
     */
    private File getFile(HttpServletRequest req) {
        String pathInfo = req.getPathInfo();
        return new File(base, pathInfo);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.servlet.http.HttpServlet#doDelete(javax.servlet.http.HttpServletRequest
     * , javax.servlet.http.HttpServletResponse)
     */
    @Override
    protected void doDelete(HttpServletRequest req, HttpServletResponse resp)
            throws ServletException, IOException {
        try {
            File file = getFile(req);
            if (!file.exists()) {
                throw new RestException(HttpServletResponse.SC_NOT_FOUND,
                        "No Such Resource: " + file.getAbsolutePath());
            }
            manager.delete(file);
        } catch (IOException e) {
            log.warn("Problem handling DELETE", e);
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (RestException e) {
            log.debug(e.getLocalizedMessage(), e);
            resp.sendError(e.getCode());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.servlet.http.HttpServlet#service(javax.servlet.http.HttpServletRequest
     * , javax.servlet.http.HttpServletResponse)
     */
    @Override
    protected void service(HttpServletRequest req, HttpServletResponse resp)
            throws ServletException, IOException {
        String method = req.getMethod();
        // clients before 14.3 used DAV which uses MKCOL. This isn't needed
        // since we create directories on put.
        if (legacyMode && "MKCOL".equalsIgnoreCase(method)) {
            resp.setStatus(HttpServletResponse.SC_OK);
            return;
        }
        super.service(req, resp);
    }

}
