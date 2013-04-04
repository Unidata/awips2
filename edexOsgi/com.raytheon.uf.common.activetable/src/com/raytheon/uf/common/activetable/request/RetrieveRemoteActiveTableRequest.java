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
package com.raytheon.uf.common.activetable.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to run the requestAT utility, which will send messages over the MHS
 * to neighboring sites for their active table entries that are relevant to this
 * site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 6, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class RetrieveRemoteActiveTableRequest implements IServerRequest {

    @DynamicSerializeElement
    private String serverHost;

    @DynamicSerializeElement
    private int serverPort;

    @DynamicSerializeElement
    private String serverProtocol;

    @DynamicSerializeElement
    private String mhsId;

    @DynamicSerializeElement
    private String siteId;

    @DynamicSerializeElement
    private String ancfAddress;

    @DynamicSerializeElement
    private String bncfAddress;

    @DynamicSerializeElement
    private String transmitScript;

    /**
     * No argument constructor. Not intended to be used by anyone, except
     * DynamicSerialize.
     */
    public RetrieveRemoteActiveTableRequest() {
        // no-op
    }

    /**
     * Build a RetrieveRemoteActiveTableRequest.
     * 
     * @param serverHost
     *            Host name of system running requestAT.
     * @param serverPort
     *            Port upon which system runs.
     * @param serverProtocol
     *            MHS protocol string for server.
     * @param mhsId
     *            MHS ID of this system.
     * @param siteId
     *            Site ID of this system.
     * @param ancfAddress
     *            Configured ANCF URL.
     * @param bncfAddress
     *            Configured BNCF URL.
     * @param transmitScript
     *            Command to run to transmit requestAT output over MHS.
     */
    public RetrieveRemoteActiveTableRequest(String serverHost, int serverPort,
            String serverProtocol, String mhsId, String siteId,
            String ancfAddress, String bncfAddress, String transmitScript) {
        this.serverHost = serverHost;
        this.serverPort = serverPort;
        this.serverProtocol = serverProtocol;
        this.mhsId = mhsId;
        this.siteId = siteId;
        this.ancfAddress = ancfAddress;
        this.bncfAddress = bncfAddress;
        this.transmitScript = transmitScript;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("RetrieveRemoteActiveTableRequest [serverHost=");
        builder.append(serverHost);
        builder.append(", serverPort=");
        builder.append(serverPort);
        builder.append(", serverProtocol=");
        builder.append(serverProtocol);
        builder.append(", mhsId=");
        builder.append(mhsId);
        builder.append(", siteId=");
        builder.append(siteId);
        builder.append(", ancfAddress=");
        builder.append(ancfAddress);
        builder.append(", bncfAddress=");
        builder.append(bncfAddress);
        builder.append(", transmitScript=");
        builder.append(transmitScript);
        builder.append("]");
        return builder.toString();
    }

    public String getServerHost() {
        return serverHost;
    }

    public void setServerHost(String serverHost) {
        this.serverHost = serverHost;
    }

    public int getServerPort() {
        return serverPort;
    }

    public void setServerPort(int serverPort) {
        this.serverPort = serverPort;
    }

    public String getServerProtocol() {
        return serverProtocol;
    }

    public void setServerProtocol(String serverProtocol) {
        this.serverProtocol = serverProtocol;
    }

    public String getMhsId() {
        return mhsId;
    }

    public void setMhsId(String mhsId) {
        this.mhsId = mhsId;
    }

    public String getSiteId() {
        return siteId;
    }

    public void setSiteId(String siteId) {
        this.siteId = siteId;
    }

    public String getAncfAddress() {
        return ancfAddress;
    }

    public void setAncfAddress(String ancfAddress) {
        this.ancfAddress = ancfAddress;
    }

    public String getBncfAddress() {
        return bncfAddress;
    }

    public void setBncfAddress(String bncfAddress) {
        this.bncfAddress = bncfAddress;
    }

    public String getTransmitScript() {
        return transmitScript;
    }

    public void setTransmitScript(String transmitScript) {
        this.transmitScript = transmitScript;
    }
}
