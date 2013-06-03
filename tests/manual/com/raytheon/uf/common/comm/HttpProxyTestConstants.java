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

/**
 * Http constants
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 11, 2013  1763     dhladky      Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class HttpProxyTestConstants {

    public static final int HTTPS_PORT = 8888;

    public static final String USERNAME = "user";

    public static final String PASSWD = "password";

    public static final String REALM = "MADISOGC";
    
    public static final String CONTEXT = "wfs";

    public static final String HOST = "dev11";
    
    public static final String REMOTE_HOST = "dev05";

    public static final int HTTP_PORT = 8085;

    public static final String HTTPS_URI = "https://" + HOST + ":" + HTTPS_PORT
            + "/wfs";

    public static final String HTTP_URI = "http://" + REMOTE_HOST + ":" + HTTP_PORT
            + "/wfs";

    public static final int PORT = HTTP_PORT;
}
