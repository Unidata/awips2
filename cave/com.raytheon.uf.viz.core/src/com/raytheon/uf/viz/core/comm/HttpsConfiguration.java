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
package com.raytheon.uf.viz.core.comm;

import java.net.URI;
import java.net.URISyntaxException;

import com.raytheon.uf.common.comm.IHttpsConfiguration;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Https Configuration Class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 9, 2013    1786     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HttpsConfiguration implements IHttpsConfiguration {
    private int httpsPort = 443;

    private int httpPort = 80;

    /**
     * Constructor.
     */
    public HttpsConfiguration() {
        init();
    }

    private void init() {
        String localizationServer = LocalizationManager.getInstance()
                .getLocalizationServer();
        try {
            URI uri = new URI(localizationServer);
            if (uri.getScheme().equals("http")) {
                httpPort = uri.getPort();
            } else if (uri.getScheme().equals("https")) {
                httpsPort = uri.getPort();
                if (httpsPort == -1) {
                    httpsPort = 443; // The default https port
                }
            } else {
                throw new URISyntaxException(uri.toString(), "Invalid server");
            }
        } catch (URISyntaxException e) {
            System.err.println("Invalid localization server setting.");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getHttpsPort() {
        return httpsPort;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getHttpPort() {
        return httpPort;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("HTTP Port: ").append(this.httpPort).append("\n");
        sb.append("HTTPS Port: ").append(this.httpsPort).append("\n");

        return sb.toString();
    }
}
