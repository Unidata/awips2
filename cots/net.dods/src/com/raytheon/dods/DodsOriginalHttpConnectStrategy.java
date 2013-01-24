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
import java.net.URLConnection;

import dods.dap.DConnect;
import dods.dap.DODSException;
import dods.dap.ServerVersion;

/**
 * The original DODS http connection strategy.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 6, 2012  634        djohnson     Extracted from an older DConnect version.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DodsOriginalHttpConnectStrategy extends BaseHttpConnectStrategy {

    private boolean acceptDeflate;

    private int connectionTimeout;

    private int socketTimeout;

    @Override
    public InputStream getInputStream(URL url) throws IOException,
            DODSException {

        URLConnection connection = url.openConnection();
        connection.setConnectTimeout(connectionTimeout);
        connection.setReadTimeout(socketTimeout);

        if (acceptDeflate)
            connection.setRequestProperty(ACCEPT_ENCODING_HEADER, DEFLATE);
        connection.connect();

        InputStream is = null;
        try {
            is = connection.getInputStream(); // get the HTTP InputStream

            // check headers
            String type = connection.getHeaderField(CONTENT_DESCRIPTION);
            DConnect.handleContentDesc(is, type);

            ver = new ServerVersion(connection.getHeaderField(XDODS_SERVER));

            String encoding = connection.getContentEncoding();
            // System.err.println("Content Encoding: " + encoding);
            return DConnect.handleContentEncoding(is, encoding);

        } catch (Exception e) {
            System.out
                    .println("DConnect exception: " + e.getLocalizedMessage());
        }

        return null;
    }


    @Override
    public void setProxy(String proxyHost, int proxyPort) {
        System.setProperty("http.proxyHost", proxyHost);
        System.setProperty("http.proxyPort", Integer.toString(proxyPort));
    }

    @Override
    public void setDeflate(boolean deflate) {
        this.acceptDeflate = deflate;
    }

    @Override
    public void setConnectionTimeout(int connectionTimeout) {
        this.connectionTimeout = connectionTimeout;
    }

    @Override
    public void setSocketTimeout(int socketTimeout) {
        this.socketTimeout = socketTimeout;
    }
}
