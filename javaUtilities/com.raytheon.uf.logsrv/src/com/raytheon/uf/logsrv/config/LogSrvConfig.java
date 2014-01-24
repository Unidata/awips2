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
package com.raytheon.uf.logsrv.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.Validate;

/**
 * A configuration for the logging service.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class LogSrvConfig {

    @XmlElement()
    private String clusterName;

    @XmlElement
    private String databaseDir;

    @XmlElement
    private String fromAddress;

    @XmlElement
    private String smtpHost;

    @XmlElement
    private int smtpPort;

    @XmlElement
    private String toAddress;

    @XmlElement
    private String timeToSend;

    public String getFromAddress() {
        return fromAddress;
    }

    public void setFromAddress(String fromAddress) {
        this.fromAddress = fromAddress;
    }

    public String getSmtpHost() {
        return smtpHost;
    }

    public void setSmtpHost(String smtpHost) {
        this.smtpHost = smtpHost;
    }

    public int getSmtpPort() {
        return smtpPort;
    }

    public void setSmtpPort(int smtpPort) {
        this.smtpPort = smtpPort;
    }

    public String getToAddress() {
        return toAddress;
    }

    public void setToAddress(String toAddress) {
        this.toAddress = toAddress;
    }

    public String getClusterName() {
        return clusterName;
    }

    public void setClusterName(String clusterName) {
        this.clusterName = clusterName;
    }

    public String getTimeToSend() {
        return timeToSend;
    }

    public void setTimeToSend(String timeToSend) {
        this.timeToSend = timeToSend;
    }

    public String getDatabaseDir() {
        return databaseDir;
    }

    public void setDatabaseDir(String databaseDir) {
        this.databaseDir = databaseDir;
    }

    /**
     * Validates that the config has every value set.
     */
    public void validate() {
        Validate.notEmpty(clusterName, "Config must include a clusterName");
        Validate.notEmpty(databaseDir, "Config must include a databaseDir");
        Validate.notEmpty(fromAddress, "Config must include a fromAddress");
        Validate.notEmpty(smtpHost, "Config must include an smtpHost");
        Validate.notEmpty(timeToSend, "Config must include a timeToSend");
        Validate.notEmpty(toAddress, "Config must include a toAddress");
        if (smtpPort <= 0) {
            throw new IllegalArgumentException(
                    "Config must include an smtpPort");
        }
    }

}
