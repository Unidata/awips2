package com.raytheon.uf.common.dataplugin.backupsvc.database;

import java.io.Serializable;
import java.util.Arrays;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Backup service jobinfo table contains job info content to be sent or
 * received.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------  --------------------------
 * 03/16/2021   88364       Gang Chen   Initial creation
 * Jun 22, 2021 92788       Gang Chen   Add jobFileLocation and change
 *                                      jobName to jobFilePath field
 * Jul 13, 2021 92922       Robert.Blum Store the IServerRequest as a blob, so it can be
 *                                      routed/sent at the recipient site.
 * Aug 19, 2021 95003       Robert.Blum Extend PDO to for hibernate auto creation.
 *
 * </pre>
 *
 * @author gchen
 */

@Entity
@SequenceGenerator(initialValue = 1, name = "backup_svc_jobinfo_id_seq", sequenceName = "backup_svc_jobinfo_id_seq", allocationSize = 1)
@Table(name = "backup_svc_jobinfo")
@DynamicSerialize
public class BackupSvcJobInfo extends PersistableDataObject<Long>
        implements Serializable {

    private static final long serialVersionUID = 174711403490661068L;

    @Column
    @Id
    @GeneratedValue(generator = "backup_svc_jobinfo_id_seq", strategy = GenerationType.AUTO)
    @DynamicSerializeElement
    private long id;

    @Column
    @DynamicSerializeElement
    private byte[] requestBlob;

    @Column(nullable = false)
    @DynamicSerializeElement
    private int blobSize;

    @Column(nullable = false, length = 256)
    @DynamicSerializeElement
    private String jobFileLocation;

    @Column(nullable = false, length = 256)
    @DynamicSerializeElement
    private String jobFilePath;

    public BackupSvcJobInfo() {

    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public void setRequestBlob(byte[] requestBlob) {
        this.requestBlob = requestBlob;
        this.blobSize = requestBlob.length;
    }

    public byte[] getRequestBlob() {
        return requestBlob;
    }

    public int getBlobSize() {
        return blobSize;
    }

    public void setBlobSize(int blobSize) {
        this.blobSize = blobSize;
    }

    public String getJobFileLocation() {
        return jobFileLocation;
    }

    public void setJobFileLocation(String jobFileLocation) {
        this.jobFileLocation = jobFileLocation;
    }

    public String getJobFilePath() {
        return jobFilePath;
    }

    public void setJobFilePath(String jobFilePath) {
        this.jobFilePath = jobFilePath;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + blobSize;
        result = prime * result + (int) (id ^ (id >>> 32));
        result = prime * result
                + ((jobFileLocation == null) ? 0 : jobFileLocation.hashCode());
        result = prime * result
                + ((jobFilePath == null) ? 0 : jobFilePath.hashCode());
        result = prime * result + Arrays.hashCode(requestBlob);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        BackupSvcJobInfo other = (BackupSvcJobInfo) obj;
        if (blobSize != other.blobSize) {
            return false;
        }
        if (id != other.id) {
            return false;
        }
        if (jobFileLocation == null) {
            if (other.jobFileLocation != null) {
                return false;
            }
        } else if (!jobFileLocation.equals(other.jobFileLocation)) {
            return false;
        }
        if (jobFilePath == null) {
            if (other.jobFilePath != null) {
                return false;
            }
        } else if (!jobFilePath.equals(other.jobFilePath)) {
            return false;
        }
        if (!Arrays.equals(requestBlob, other.requestBlob)) {
            return false;
        }
        return true;
    }
}
