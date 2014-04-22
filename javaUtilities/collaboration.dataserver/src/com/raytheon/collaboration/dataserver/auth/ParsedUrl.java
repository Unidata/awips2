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
package com.raytheon.collaboration.dataserver.auth;

import java.net.URI;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.client.utils.URIUtils;

import com.raytheon.collaboration.dataserver.RestException;
import com.raytheon.uf.common.util.StringUtil;

/**
 * Parsed HTTP request url
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ParsedUrl {

    /**
     * <pre>
     * /session_data/[sessionid]/[userid]
     * </pre>
     */
    private static final Pattern PATH_PATTERN = Pattern
            .compile("^/?([^/]+)/([^/]+)(/([^/]+))?.*$");

    /**
     * parent directory reference (..) anywhere in path
     */
    private static final Pattern REL_PATH_PATTERN = Pattern
            .compile("^\\.\\./|/\\.\\./|/\\.\\.$|^\\.\\.$");

    private final String hostString;

    private final String pathString;

    private final String sessionid;

    private final String userid;

    /**
     * @param hostString
     * @param pathString
     * @param sessionid
     * @param userid
     */
    private ParsedUrl(String hostString, String pathString, String sessionid,
            String userid) {
        this.hostString = hostString;
        this.pathString = pathString;
        this.sessionid = sessionid;
        this.userid = userid;
    }

    /**
     * Parse URL of request
     * 
     * @param httpReq
     * @return
     * @throws RestException
     */
    public static ParsedUrl parse(HttpServletRequest httpReq)
            throws RestException {
        URI uri = URI.create(httpReq.getRequestURL().toString());
        String hostHeader = URIUtils.extractHost(uri).toHostString();
        if (StringUtil.isEmptyString(hostHeader)) {
            throw new RestException(HttpServletResponse.SC_BAD_REQUEST,
                    "Unable to determine host string");
        }
        String path = uri.getPath();
        Matcher m = REL_PATH_PATTERN.matcher(path);
        if (m.find()) {
            throw new RestException(HttpServletResponse.SC_BAD_REQUEST,
                    "Relative path inderection is not allowed");
        }
        m = PATH_PATTERN.matcher(path);
        if (!m.matches()) {
            throw new RestException(HttpServletResponse.SC_BAD_REQUEST,
                    "Malformed session data path");
        }
        /*
         * first part of url will always be the path to the servlet
         * (session_data). The next is enforced as the sessionid and the data
         * provider's userid after that. The userid is optional because topic
         * owners can delete the entire session tree
         */
        String sessionid = getSafeGroup(m, 2);
        String userid = getSafeGroup(m, 4);
        return new ParsedUrl(hostHeader, path, sessionid, userid);
    }

    /**
     * Get trimmed matcher group
     * 
     * @param m
     * @param group
     * @return null if group was empty
     */
    public static String getSafeGroup(Matcher m, int group) {
        String rval = m.group(group);
        if (rval != null) {
            rval = rval.trim();
        }
        return rval;
    }

    /**
     * @return the hostString
     */
    public String getHostString() {
        return hostString;
    }

    /**
     * @return the pathString
     */
    public String getPathString() {
        return pathString;
    }

    /**
     * @return the sessionid
     */
    public String getSessionid() {
        return sessionid;
    }

    /**
     * @return the userid
     */
    public String getUserid() {
        return userid;
    }

}
