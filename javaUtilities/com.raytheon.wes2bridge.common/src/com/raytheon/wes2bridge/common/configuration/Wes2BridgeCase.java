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
package com.raytheon.wes2bridge.common.configuration;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Represents a Wes2Bridge test case that will be used to configure a new edex
 * environment.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2014 3521       bkowal      Initial creation
 * Apr 20, 2015 4392       dlovely     Removed un-used JMX port configuration
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

@XmlRootElement(name = "Wes2BridgeCase")
@XmlAccessorType(XmlAccessType.NONE)
public class Wes2BridgeCase {
    @XmlElement
    private String name;

    @XmlElement
    private String dataArchiveRoot;

    @XmlElement
    private int databasePort;

    @XmlElement
    private int edexHttpPort;

    @XmlElement
    private int qpidHttpPort;

    @XmlElement
    private int jmsPort;

    @XmlElement
    private int httpdPypiesPort;

    @XmlElement
    private int pypiesLoggingPort;

    /**
     * 
     */
    public Wes2BridgeCase() {
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the dataArchiveRoot
     */
    public String getDataArchiveRoot() {
        return dataArchiveRoot;
    }

    /**
     * @param dataArchiveRoot
     *            the dataArchiveRoot to set
     */
    public void setDataArchiveRoot(String dataArchiveRoot) {
        this.dataArchiveRoot = dataArchiveRoot;
    }

    /**
     * @return the databasePort
     */
    public int getDatabasePort() {
        return databasePort;
    }

    /**
     * @param databasePort
     *            the databasePort to set
     */
    public void setDatabasePort(int databasePort) {
        this.databasePort = databasePort;
    }

    /**
     * @return the edexHttpPort
     */
    public int getEdexHttpPort() {
        return edexHttpPort;
    }

    /**
     * @param edexHttpPort
     *            the edexHttpPort to set
     */
    public void setEdexHttpPort(int edexHttpPort) {
        this.edexHttpPort = edexHttpPort;
    }

    /**
     * @return the qpidHttpPort
     */
    public int getQpidHttpPort() {
        return qpidHttpPort;
    }

    /**
     * @param qpidHttpPort
     *            the qpidHttpPort to set
     */
    public void setQpidHttpPort(int qpidHttpPort) {
        this.qpidHttpPort = qpidHttpPort;
    }

    /**
     * @return the jmsPort
     */
    public int getJmsPort() {
        return jmsPort;
    }

    /**
     * @param jmsPort
     *            the jmsPort to set
     */
    public void setJmsPort(int jmsPort) {
        this.jmsPort = jmsPort;
    }

    /**
     * @return the httpdPypiesPort
     */
    public int getHttpdPypiesPort() {
        return httpdPypiesPort;
    }

    /**
     * @param httpdPypiesPort
     *            the httpdPypiesPort to set
     */
    public void setHttpdPypiesPort(int httpdPypiesPort) {
        this.httpdPypiesPort = httpdPypiesPort;
    }

    /**
     * @return the pypiesLoggingPort
     */
    public int getPypiesLoggingPort() {
        return pypiesLoggingPort;
    }

    /**
     * @param pypiesLoggingPort
     *            the pypiesLoggingPort to set
     */
    public void setPypiesLoggingPort(int pypiesLoggingPort) {
        this.pypiesLoggingPort = pypiesLoggingPort;
    }
}