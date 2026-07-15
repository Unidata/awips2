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

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.backupsvc.BackupHost;
import com.raytheon.uf.common.backupsvc.BackupServiceFile;
import com.raytheon.uf.common.util.SizeUtil;

/**
 * Configuration XML file for backup service
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------- ----------- --------------------------
 * Nov 2, 2016  5937        tgurney     Initial creation
 * Apr 15 2021  84655       lsingh      Combined localizationBackupList.txt
 *                                      and backupSvc.xml into one file.
 *                                      Added include and exclude for files.
 * Jul 15, 2021 88368       Gang Chen   Added configuration field purgeTimePeriod
 *                                      and its getter and setter methods
 * Aug 09, 2021 84654      Robert.Blum  Add localDomains.
 * Aug 30, 2021 93179     Amanuel Challa Added edex cutoff version.
 * </pre>
 *
 * @author tgurney
 */

@XmlRootElement(name = "backupServiceConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class BackupServiceConfig {

    private static final String NEW_BACKUPSVC_EDEX_COMPATIBLE_VERSION = "21.4.1";

    @XmlElement(name = "pollIntervalSeconds")
    private int pollIntervalSeconds = 300;

    @XmlElement(name = "rateLimitKBps")
    private int rateLimitKBps = 16;

    @XmlElement(name = "bigJobSize")
    private int bigJobSize = (int) (5 * SizeUtil.BYTES_PER_MB);

    @XmlElement(name = "purgeTimePeriodDays")
    private int purgeTimePeriodDays = 14;

    @XmlElementWrapper(name = "localDomains", required = true)
    @XmlElement(name = "domain")
    private List<String> localDomains = new ArrayList<>();

    @XmlElement(name = "edexCutoffVersion")
    private String edexCutoffVersion = NEW_BACKUPSVC_EDEX_COMPATIBLE_VERSION;

    @XmlElementWrapper(name = "hosts", required = true)
    @XmlElement(name = "host")
    private List<BackupHost> hosts = new ArrayList<>();

    @XmlElementWrapper(name = "include", required = true)
    @XmlElement(name = "file")
    private List<BackupServiceFile> includeFileList = new ArrayList<>();

    @XmlElementWrapper(name = "exclude", required = true)
    @XmlElement(name = "file")
    private List<BackupServiceFile> excludeFileList = new ArrayList<>();

    public List<BackupHost> getHosts() {
        return hosts;
    }

    public void setHosts(List<BackupHost> hosts) {
        this.hosts = hosts;
    }

    public List<String> getLocalDomains() {
        return localDomains;
    }

    public void setLocalDomains(List<String> localDomains) {
        this.localDomains = localDomains;
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

    public int getPurgeTimePeriodDays() {
        return purgeTimePeriodDays;
    }

    public void setPurgeTimePeriodDays(int purgeTimePeriodDays) {
        this.purgeTimePeriodDays = purgeTimePeriodDays;
    }

    public String getEdexCutoffVersion() {
        return edexCutoffVersion;
    }

    public void setEdexCutoffVersion(String edexVersion) {
        this.edexCutoffVersion = edexVersion;
    }

    public List<BackupServiceFile> getIncludeFileList() {
        return includeFileList;
    }

    public void setIncludeFileList(List<BackupServiceFile> includeFileList) {
        this.includeFileList = includeFileList;
    }

    public List<BackupServiceFile> getExcludeFileList() {
        return excludeFileList;
    }

    public void setExcludeFileList(List<BackupServiceFile> excludeFileList) {
        this.excludeFileList = excludeFileList;
    }

}
