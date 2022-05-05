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
package com.raytheon.uf.edex.backupsvc.database;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.hibernate.annotations.Index;

/**
 * Backup job that includes a host name and data to send to that host
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2016 5937       tgurney     Initial creation
 * Jul 20, 2017 6352       tgurney     Add min/maxversionRequired
 * Apr 06, 2021 22487      smoorthy    increase field size of jobName
 *
 * </pre>
 *
 * @author tgurney
 */

@Entity
@SequenceGenerator(initialValue = 1, name = "backup_jobseq", sequenceName = "backup_jobseq", allocationSize = 1)
@Table(name = "backup_job")
public class BackupJob implements Serializable {

    private static final long serialVersionUID = 1L;

    @Column
    @Id
    @GeneratedValue(generator = "backup_jobseq", strategy = GenerationType.SEQUENCE)
    private long id;

    @Column(nullable = false, length = 256)
    private String jobName;

    @Column(nullable = false)
    private int priority;

    @Column(name = "host", nullable = false, length = 128)
    @Index(name = "backupJobHostIdx")
    private String host;

    /**
     * NOTE: There is no getter for this field. Access should be done via a
     * primary key lookup on backup_blob table, hence the separate backupBlobId
     * field.
     */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "backup_blob_id")
    private BackupBlob requestBlob;

    @Column(nullable = false)
    private long createdTime;

    @Column(nullable = false)
    private int blobSize;

    @Column(name = "backup_blob_id", insertable = false, updatable = false)
    private long backupBlobId;

    @Column(length = 16)
    private String minVersionRequired;

    @Column(length = 16)
    private String maxVersionRequired;

    public int getPriority() {
        return priority;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public String getJobName() {
        return jobName;
    }

    public void setJobName(String jobName) {
        this.jobName = jobName;
    }

    public long getId() {
        return id;
    }

    public void setRequestBlob(BackupBlob requestBlob) {
        this.requestBlob = requestBlob;
        this.blobSize = requestBlob.getBlob().length;
    }

    public long getCreatedTime() {
        return createdTime;
    }

    public void setCreatedTime(long createdTime) {
        this.createdTime = createdTime;
    }

    public int getBlobSize() {
        return blobSize;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public long getBackupBlobId() {
        return backupBlobId;
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
