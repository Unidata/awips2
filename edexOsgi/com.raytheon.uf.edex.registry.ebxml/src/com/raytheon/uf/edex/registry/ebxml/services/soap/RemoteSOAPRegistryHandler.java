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
package com.raytheon.uf.edex.registry.ebxml.services.soap;

import com.raytheon.uf.common.registry.ebxml.SOAPRegistryManager;

/**
 * Remote version of the {@link SOAPRegistryManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 06, 2014   #3141     dhladky     initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class RemoteSOAPRegistryHandler extends RemoteSOAPRegistryManager {

    /** remote registry host **/
    private String host;

    /** remote registry port **/
    private String port;

    /** remote registry protocol **/
    private String protocol;

    /** remote host complete precursor URL */
    public String remoteHostUrl = null;

    /** default constructor **/
    public RemoteSOAPRegistryHandler() {

    }

    /** Bean constructor **/
    public RemoteSOAPRegistryHandler(RegistrySOAPServices rss) {
        super(rss);
    }

    @Override
    protected String getRemoteHost() {

        if (remoteHostUrl == null) {
            StringBuffer buf = new StringBuffer();
            buf.append(protocol);
            buf.append("://");
            buf.append(host);
            buf.append(":");
            buf.append(port);

            remoteHostUrl = buf.toString();
        }

        return remoteHostUrl;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public String getPort() {
        return port;
    }

    public void setPort(String port) {
        this.port = port;
    }

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }
}
