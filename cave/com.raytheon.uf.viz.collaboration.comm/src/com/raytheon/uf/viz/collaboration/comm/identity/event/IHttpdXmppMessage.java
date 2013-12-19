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
package com.raytheon.uf.viz.collaboration.comm.identity.event;

import java.util.regex.Pattern;

/**
 * Used to store constants that are used to validate, analyze, and parse status
 * and configuration messages associated with the AWIPS II httpd collaboration
 * server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2012            bkowal     Initial creation
 * Dec 18, 2013 2562       bclement     removed preamble from patterns
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public interface IHttpdXmppMessage {
    // Constant Strings
    public static final String URL_PARAMETER_NAME = "sessionDataHttpURL";

    public static final String ERROR_PARAMETER_NAME = "error";

    static final String SUFFIX_REGEX = " : .+";

    static final String COLLABORATION_URL_REGEX = "http://.+:[1-9][0-9]*/session_data/";

    // Regex Patterns
    public static final Pattern configErrorPattern = Pattern
            .compile(ERROR_PARAMETER_NAME + SUFFIX_REGEX);

    public static final Pattern configURLPattern = Pattern
            .compile(URL_PARAMETER_NAME + SUFFIX_REGEX);

    public static final Pattern urlPattern = Pattern
            .compile(COLLABORATION_URL_REGEX);
}
