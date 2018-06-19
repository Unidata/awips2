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
package com.raytheon.uf.common.backupsvc.request;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to queue up a backup job
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2016 5937       tgurney     Initial creation
 * Jul 20, 2017 6352       tgurney     Add min/maxversionRequired
 *
 * </pre>
 *
 * @author tgurney
 */

@DynamicSerialize
public class BackupEnqueueRequest implements IServerRequest {
    @DynamicSerializeElement
    private IServerRequest request;

    @DynamicSerializeElement
    private String jobName;

    /** Lower number = higher priority. May be negative */
    @DynamicSerializeElement
    private int priority;

    @DynamicSerializeElement
    private String minVersionRequired;

    @DynamicSerializeElement
    private String maxVersionRequired;

    @DynamicSerializeElement
    private List<String> hosts;

    public IServerRequest getRequest() {
        return request;
    }

    public void setRequest(IServerRequest request) {
        this.request = request;
    }

    public String getJobName() {
        return jobName;
    }

    public void setJobName(String jobName) {
        this.jobName = jobName;
    }

    public int getPriority() {
        return priority;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public List<String> getHosts() {
        return hosts;
    }

    public void setHosts(List<String> hosts) {
        this.hosts = hosts;
    }

    public String getMinVersionRequired() {
        return minVersionRequired;
    }

    public void setMinVersionRequired(String minVersionRequired) {
        this.minVersionRequired = minVersionRequired;
    }

    public String getMaxVersionRequired() {
        return maxVersionRequired;
    }

    public void setMaxVersionRequired(String maxVersionRequired) {
        this.maxVersionRequired = maxVersionRequired;
    }

}
