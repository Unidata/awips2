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
package com.raytheon.uf.common.backupsvc.response;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A Response Object containing the contents of backupSvc.xml file
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/15/2021   84638     Amanuel.Challa Initial creation
 * Aug 12, 2021 84654     Robert.Blum    Added local domain info.
 * Aug 30, 2021 93179     Amanuel.Challa Added EDEX cutoff Version info.
 * Oct 08, 2021 97253     Robert.Blum    Add purge config value.
 *
 * </pre>
 *
 * @author Amanuel.Challa
 * @version 1.0
 */
@DynamicSerialize
public class BackupConfigFileResponse implements ISerializableObject {
    /**
     * backupsvc.xml file contents
     */
    @DynamicSerializeElement
    private int pollIntervalSeconds;

    @DynamicSerializeElement
    private int rateLimitKBps;

    @DynamicSerializeElement
    private int bigJobSize;

    @DynamicSerializeElement
    private int purgeTimePeriodDays;

    @DynamicSerializeElement
    private String edexCutoffVersion;

    @DynamicSerializeElement
    private Exception exceptions;

    @DynamicSerializeElement
    List<String> includeFileList = new ArrayList<>();

    @DynamicSerializeElement
    List<String> excludeFileList = new ArrayList<>();

    @DynamicSerializeElement
    List<String> hostInfoList = new ArrayList<>();

    @DynamicSerializeElement
    List<String> localDomains = new ArrayList<>();

    public List<String> getHostInfoList() {
        return hostInfoList;
    }

    public void setHostInfoList(List<String> hostInfoList) {
        this.hostInfoList = hostInfoList;
    }

    public BackupConfigFileResponse() {
    }

    public List<String> getIncludeFileList() {
        return includeFileList;
    }

    public void setIncludeFileList(List<String> includeFileList) {
        this.includeFileList = includeFileList;
    }

    public List<String> getExcludeFileList() {
        return excludeFileList;
    }

    public void setExcludeFileList(List<String> excludeFileList) {
        this.excludeFileList = excludeFileList;
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

    public int getPurgeTimePeriodDays() {
        return purgeTimePeriodDays;
    }

    public void setPurgeTimePeriodDays(int purgeTimePeriodDays) {
        this.purgeTimePeriodDays = purgeTimePeriodDays;
    }

    public int getBigJobSize() {
        return bigJobSize;
    }

    public void setBigJobSize(int bigJobSize) {
        this.bigJobSize = bigJobSize;
    }

    public Exception getExceptions() {
        return exceptions;
    }

    public void setExceptions(Exception exceptions) {
        this.exceptions = exceptions;
    }

    public List<String> getLocalDomains() {
        return localDomains;
    }

    public void setLocalDomains(List<String> localDomains) {
        this.localDomains = localDomains;
    }

    public String getEdexCutoffVersion() {
        return edexCutoffVersion;
    }

    public void setEdexCutoffVersion(String edexVersion) {
        this.edexCutoffVersion = edexVersion;
    }
}
