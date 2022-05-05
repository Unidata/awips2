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
package com.raytheon.uf.edex.backupsvc.service;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.backupsvc.BackupHost;
import com.raytheon.uf.common.util.SizeUtil;

/**
 * Configuration XML file for backup service
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 2, 2016  5937       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

@XmlRootElement(name = "backupServiceConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class BackupServiceConfig {
    @XmlElement(name = "pollIntervalSeconds")
    private int pollIntervalSeconds = 300;

    @XmlElement(name = "rateLimitKBps")
    private int rateLimitKBps = 16;

    @XmlElement(name = "bigJobSize")
    private int bigJobSize = (int) (5 * SizeUtil.BYTES_PER_MB);

    @XmlElementWrapper(name = "hosts", required = true)
    @XmlElement(name = "host")
    private List<BackupHost> hosts = new ArrayList<>();

    public List<BackupHost> getHosts() {
        return hosts;
    }

    public void setHosts(List<BackupHost> hosts) {
        this.hosts = hosts;
    }

    public int getPollIntervalSeconds() {
        return pollIntervalSeconds;
    }

    public void setPollIntervalSeconds(int pollIntervalSeconds) {
        this.pollIntervalSeconds = pollIntervalSeconds;
    }

    public int getRateLimitKBps() {
        return rateLimitKBps;
    }

    public void setRateLimitKBps(int rateLimitKBps) {
        this.rateLimitKBps = rateLimitKBps;
    }

    public int getBigJobSize() {
        return bigJobSize;
    }

    public void setBigJobSize(int bigJobSize) {
        this.bigJobSize = bigJobSize;
    }
}
