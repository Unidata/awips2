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
package com.raytheon.uf.common.comm;

import org.junit.Ignore;

/**
 * Http constants
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2013 1763       dhladky      Initial creation
 * Jun 17, 2013 2106       djohnson     Use username/password from HttpTestConstants.
 * Jul 15, 2013 2180       dhladky      Updated for encryption
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@Ignore
public class HttpProxyTestConstants extends HttpTestConstants {

    public static final int HTTPS_PORT = 8888;

    public static final String REALM = "MADISOGC";
    
    public static final String CONTEXT = "wfs";
    
    // 32 character length
    public static final String PROVIDER_KEY = "1qaz2wsx3edc4rfv5tgb6yhn6yhn7ujm8";

    public static final String HOST = "dev11";
    
    public static final String REMOTE_HOST = "stormy";

    public static final int HTTP_PORT = 8085;

    public static final String HTTPS_URI = "https://" + HOST + ":" + HTTPS_PORT
            + "/wfs";

    public static final String HTTP_URI = "http://" + REMOTE_HOST + ":" + HTTP_PORT
            + "/wfs";

    public static final int PORT = HTTP_PORT;
}
