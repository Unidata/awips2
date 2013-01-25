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
package com.raytheon.dods;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import dods.dap.DODSException;
import dods.dap.ServerVersion;

/**
 * A strategy for making HTTP requests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 6, 2012  634        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface HttpConnectStrategy {

    String DODS_ERROR = "dods_error";

    /**
     * Get the {@link InputStream} for the {@link URL}.
     * 
     * @param url
     *            the url
     * @return the input stream
     * @throws IOException
     *             on error
     * @throws DODSException
     */
    InputStream getInputStream(java.net.URL url) throws IOException,
            DODSException;

    /**
     * Return the server version.
     * 
     * @return the server version
     */
    ServerVersion getServerVersion();

    /**
     * Set the proxy information.
     * 
     * @param proxyHost
     *            the proxy host
     * @param proxyPort
     *            the proxy port
     */
    void setProxy(String proxyHost, int proxyPort);

    /**
     * Set whether or not deflating is allowed.
     * 
     * @param deflate
     *            whether to allow deflating
     */
    void setDeflate(boolean deflate);

    /**
     * Set the connection timeout.
     * 
     * @param connectionTimeout
     *            the connection timeout
     */
    void setConnectionTimeout(int connectionTimeout);

    /**
     * Set the socket timeout.
     * 
     * @param socketTimeout
     *            the socket timeout
     */
    void setSocketTimeout(int socketTimeout);
}
