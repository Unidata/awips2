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

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to run the utility sendAT, which will send active table entries
 * relevant to the specified sites via MHS.
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
public class SendActiveTableRequest implements IServerRequest {

    @DynamicSerializeElement
    private String serverHost;

    @DynamicSerializeElement
    private Integer serverPort;

    @DynamicSerializeElement
    private String serverProtocol;

    @DynamicSerializeElement
    private String serverSite;

    @DynamicSerializeElement
    private String mhsId;

    @DynamicSerializeElement
    private List<String> sites;

    @DynamicSerializeElement
    private List<String> filterSites;

    @DynamicSerializeElement
    private List<String> mhsSites;

    @DynamicSerializeElement
    private Float issueTime;

    @DynamicSerializeElement
    private Map<String, Integer> countDict;

    @DynamicSerializeElement
    private String fileName;

    @DynamicSerializeElement
    private String xmlIncoming;

    @DynamicSerializeElement
    private String transmitScript;

    /**
     * No argument constructor. Not intended to be used by anyone, except
     * DynamicSerialize.
     */
    public SendActiveTableRequest() {
        // no-op
    }

    /**
     * Builds a SendActiveTableRequest.
     * 
     * @param serverHost
     *            Host name of system running sendAT.
     * @param serverPort
     *            Port upon which system runs.
     * @param serverProtocol
     *            MHS protocol string for server.
     * @param serverSite
     *            Site ID of this system.
     * @param mhsId
     *            MHS ID of this system.
     * @param sites
     *            Sites to collect active table entries for.
     * @param filterSites
     *            Sites to filter out of active table entries.
     * @param mhsSites
     *            optional field for legacy systems. MHS IDs of server to send
     *            results to.
     * @param issueTime
     *            Time request was made. Epoch time in seconds.
     * @param countDict
     *            Counts of records requesting server has for each site in
     *            <code>sites</code>.
     * @param fileName
     *            File name containing records to send out.
     * @param xmlIncoming
     *            MHS XML data about the request.
     * @param transmitScript
     *            Command to run to send results via MHS.
     */
    public SendActiveTableRequest(String serverHost, int serverPort,
            String serverProtocol, String serverSite, String mhsId,
            List<String> sites, List<String> filterSites,
            List<String> mhsSites, float issueTime,
            Map<String, Integer> countDict, String fileName,
            String xmlIncoming, String transmitScript) {
        this.serverHost = serverHost;
        this.serverPort = serverPort;
        this.serverProtocol = serverProtocol;
        this.serverSite = serverSite;
        this.mhsId = mhsId;
        this.sites = sites;
        this.filterSites = filterSites;
        this.mhsSites = mhsSites;
        this.issueTime = issueTime;
        this.countDict = countDict;
        this.fileName = fileName;
        this.xmlIncoming = xmlIncoming;
        this.transmitScript = transmitScript;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("SendActiveTableRequest [serverHost=");
        builder.append(serverHost);
        builder.append(", serverPort=");
        builder.append(serverPort);
        builder.append(", serverProtocol=");
        builder.append(serverProtocol);
        builder.append(", serverSite=");
        builder.append(serverSite);
        builder.append(", mhsId=");
        builder.append(mhsId);
        builder.append(", sites=");
        builder.append(sites);
        builder.append(", filterSites=");
        builder.append(filterSites);
        builder.append(", mhsSites=");
        builder.append(mhsSites);
        builder.append(", issueTime=");
        builder.append(issueTime);
        builder.append(", countDict=");
        builder.append(countDict);
        builder.append(", fileName=");
        builder.append(fileName);
        builder.append(", xmlIncoming=");
        builder.append(xmlIncoming);
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

    public Integer getServerPort() {
        return serverPort;
    }

    public void setServerPort(Integer serverPort) {
        this.serverPort = serverPort;
    }

    public String getServerProtocol() {
        return serverProtocol;
    }

    public void setServerProtocol(String serverProtocol) {
        this.serverProtocol = serverProtocol;
    }

    public String getServerSite() {
        return serverSite;
    }

    public void setServerSite(String serverSite) {
        this.serverSite = serverSite;
    }

    public String getMhsId() {
        return mhsId;
    }

    public void setMhsId(String mhsId) {
        this.mhsId = mhsId;
    }

    public List<String> getSites() {
        return sites;
    }

    public void setSites(List<String> sites) {
        this.sites = sites;
    }

    public List<String> getFilterSites() {
        return filterSites;
    }

    public void setFilterSites(List<String> filterSites) {
        this.filterSites = filterSites;
    }

    public List<String> getMhsSites() {
        return mhsSites;
    }

    public void setMhsSites(List<String> mhsSites) {
        this.mhsSites = mhsSites;
    }

    public Float getIssueTime() {
        return issueTime;
    }

    public void setIssueTime(Float issueTime) {
        this.issueTime = issueTime;
    }

    public Map<String, Integer> getCountDict() {
        return countDict;
    }

    public void setCountDict(Map<String, Integer> countDict) {
        this.countDict = countDict;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getXmlIncoming() {
        return xmlIncoming;
    }

    public void setXmlIncoming(String xmlIncoming) {
        this.xmlIncoming = xmlIncoming;
    }

    public String getTransmitScript() {
        return transmitScript;
    }

    public void setTransmitScript(String transmitScript) {
        this.transmitScript = transmitScript;
    }
}
