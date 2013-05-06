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
 * Mar 20, 2013   1786     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HttpTestConstants {

    public static final int HTTPS_PORT = 8443;

    public static final String USERNAME = "user";

    public static final String PASSWD = "password";

    public static final String REALM = "Private!";

    public static final String HOST = "localhost";

    public static final int HTTP_PORT = 9999;

    public static final String HTTPS_URI = "https://" + HOST + ":" + HTTPS_PORT
            + "/test";

    public static final String HTTP_URI = "http://" + HOST + ":" + HTTP_PORT
            + "/test";

    public static final int PORT = HTTP_PORT;
}
