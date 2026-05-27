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
package com.raytheon.uf.common.dataplugin.gfe.svcbu;

import java.util.Date;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Object used to show state of a current service backup operation. Presents
 * both the name and the progress of the operation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2015  #4103     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
@DynamicSerialize
public class ServiceBackupJobStatus {

    @DynamicSerializeElement
    private String jobName;

    @DynamicSerializeElement
    private JobProgress jobStatus;

    @DynamicSerializeElement
    private Date timeStamp;

    public ServiceBackupJobStatus() {
        // no-op
    }

    public ServiceBackupJobStatus(String jobName, JobProgress jobStatus,
            Date date) {
        this.jobName = jobName;
        this.jobStatus = jobStatus;
        this.timeStamp = date;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "ServiceBackupJobStatus [jobName=" + jobName + ", jobStatus="
                + jobStatus + ", timeStamp=" + timeStamp + "]";
    }

    public String getJobName() {
        return jobName;
    }

    public void setJobName(String jobName) {
        this.jobName = jobName;
    }

    public JobProgress getJobStatus() {
        return jobStatus;
    }

    public void setJobStatus(JobProgress jobStatus) {
        this.jobStatus = jobStatus;
    }

    public Date getTimeStamp() {
        return timeStamp;
    }

    public void setTimeStamp(Date timeStamp) {
        this.timeStamp = timeStamp;
    }
}
