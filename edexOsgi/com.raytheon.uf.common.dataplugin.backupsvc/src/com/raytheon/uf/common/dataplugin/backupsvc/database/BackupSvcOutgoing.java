package com.raytheon.uf.common.dataplugin.backupsvc.database;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Backup service outgoing table contains service job information to be sent.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------  --------------------------
 * 03/16/2021   88364       Gang Chen   Initial creation
 * Jun 01, 2021 91044       Robert.Blum Renamed compatibleVersion.
 * Jun 22, 2021 92788       Gang Chen   Modify systemVersion default
 * Jul 13, 2021 92922       Robert.Blum Added additional fields for site to site coms.
 * Aug 19, 2021 95003       Robert.Blum Extend PDO to for hibernate auto creation.
 *
 * </pre>
 *
 * @author gchen
 */

@Entity
@SequenceGenerator(initialValue = 1, name = "backup_svc_outgoing_bksvc_id_seq", sequenceName = "backup_svc_outgoing_bksvc_id_seq", allocationSize = 1)
@Table(name = "backup_svc_outgoing")
@DynamicSerialize
public class BackupSvcOutgoing extends PersistableDataObject<Long>
        implements Serializable {

    private static final long serialVersionUID = 7126309335230255243L;

    @Column(name = "bksvc_id")
    @Id
    @GeneratedValue(generator = "backup_svc_outgoing_bksvc_id_seq", strategy = GenerationType.AUTO)
    @DynamicSerializeElement
    private long bksvcId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "jobinfo_id")
    @DynamicSerializeElement
    private BackupSvcJobInfo backupSvcJobInfo;

    @Column(name = "jobinfo_id", insertable = false, updatable = false)
    @DynamicSerializeElement
    private long jobInfoId;

    @Column(nullable = false, length = 128)
    @DynamicSerializeElement
    private String component;

    @Column(nullable = false, length = 20)
    @DynamicSerializeElement
    private String senderHostName;

    @Column(nullable = true, length = 5)
    @DynamicSerializeElement
    private String senderPort = System.getenv("HTTP_PORT");

    @Column(nullable = false, length = 20)
    @DynamicSerializeElement
    private String recipientHostName;

    @Column(nullable = true, length = 9)
    @DynamicSerializeElement
    private String senderSite;

    @Column(nullable = false, length = 9)
    @DynamicSerializeElement
    private String recipientSite;

    @Column(nullable = false)
    @DynamicSerializeElement
    private long updateTime;

    @Column(length = 16)
    @DynamicSerializeElement
    private String systemVersion = "Undefined";

    @Column(length = 16)
    @DynamicSerializeElement
    private String recipientVersion = "Undefined";

    @Column(nullable = false, length = 10)
    @DynamicSerializeElement
    private String status;

    public BackupSvcOutgoing() {

    }

    public long getBksvcId() {
        return bksvcId;
    }

    public void setBksvcId(long bksvcId) {
        this.bksvcId = bksvcId;
    }

    public BackupSvcJobInfo getBackupSvcJobInfo() {
        return backupSvcJobInfo;
    }

    public void setBackupSvcJobInfo(BackupSvcJobInfo backupSvcJobInfo) {
        this.backupSvcJobInfo = backupSvcJobInfo;
    }

    public long getJobInfoId() {
        return jobInfoId;
    }

    public void setJobInfoId(long jobInfoId) {
        this.jobInfoId = jobInfoId;
    }

    public String getComponent() {
        return component;
    }

    public void setComponent(String component) {
        this.component = component;
    }

    public String getSenderHostName() {
        return senderHostName;
    }

    public void setSenderHostName(String senderHostName) {
        this.senderHostName = senderHostName;
    }

    public String getSenderPort() {
        return senderPort;
    }

    public void setSenderPort(String senderPort) {
        this.senderPort = senderPort;
    }

    public String getRecipientHostName() {
        return recipientHostName;
    }

    public void setRecipientHostName(String recipientHostName) {
        this.recipientHostName = recipientHostName;
    }

    public String getSenderSite() {
        return senderSite;
    }

    public void setSenderSite(String senderSite) {
        this.senderSite = senderSite;
    }

    public String getRecipientSite() {
        return recipientSite;
    }

    public void setRecipientSite(String recipientSite) {
        this.recipientSite = recipientSite;
    }

    public long getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(long updateTime) {
        this.updateTime = updateTime;
    }

    public String getSystemVersion() {
        return systemVersion;
    }

    public void setSystemVersion(String systemVersion) {
        this.systemVersion = systemVersion;
    }

    public String getRecipientVersion() {
        return recipientVersion;
    }

    public void setRecipientVersion(String recipientVersion) {
        this.recipientVersion = recipientVersion;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((backupSvcJobInfo == null) ? 0
                : backupSvcJobInfo.hashCode());
        result = prime * result + (int) (bksvcId ^ (bksvcId >>> 32));
        result = prime * result
                + ((component == null) ? 0 : component.hashCode());
        result = prime * result + (int) (jobInfoId ^ (jobInfoId >>> 32));
        result = prime * result + ((recipientHostName == null) ? 0
                : recipientHostName.hashCode());
        result = prime * result
                + ((recipientSite == null) ? 0 : recipientSite.hashCode());
        result = prime * result + ((recipientVersion == null) ? 0
                : recipientVersion.hashCode());
        result = prime * result
                + ((senderHostName == null) ? 0 : senderHostName.hashCode());
        result = prime * result
                + ((senderPort == null) ? 0 : senderPort.hashCode());
        result = prime * result
                + ((senderSite == null) ? 0 : senderSite.hashCode());
        result = prime * result + ((status == null) ? 0 : status.hashCode());
        result = prime * result
                + ((systemVersion == null) ? 0 : systemVersion.hashCode());
        result = prime * result + (int) (updateTime ^ (updateTime >>> 32));
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
        BackupSvcOutgoing other = (BackupSvcOutgoing) obj;
        if (backupSvcJobInfo == null) {
            if (other.backupSvcJobInfo != null) {
                return false;
            }
        } else if (!backupSvcJobInfo.equals(other.backupSvcJobInfo)) {
            return false;
        }
        if (bksvcId != other.bksvcId) {
            return false;
        }
        if (component == null) {
            if (other.component != null) {
                return false;
            }
        } else if (!component.equals(other.component)) {
            return false;
        }
        if (jobInfoId != other.jobInfoId) {
            return false;
        }
        if (recipientHostName == null) {
            if (other.recipientHostName != null) {
                return false;
            }
        } else if (!recipientHostName.equals(other.recipientHostName)) {
            return false;
        }
        if (recipientSite == null) {
            if (other.recipientSite != null) {
                return false;
            }
        } else if (!recipientSite.equals(other.recipientSite)) {
            return false;
        }
        if (recipientVersion == null) {
            if (other.recipientVersion != null) {
                return false;
            }
        } else if (!recipientVersion.equals(other.recipientVersion)) {
            return false;
        }
        if (senderHostName == null) {
            if (other.senderHostName != null) {
                return false;
            }
        } else if (!senderHostName.equals(other.senderHostName)) {
            return false;
        }
        if (senderPort == null) {
            if (other.senderPort != null) {
                return false;
            }
        } else if (!senderPort.equals(other.senderPort)) {
            return false;
        }
        if (senderSite == null) {
            if (other.senderSite != null) {
                return false;
            }
        } else if (!senderSite.equals(other.senderSite)) {
            return false;
        }
        if (status == null) {
            if (other.status != null) {
                return false;
            }
        } else if (!status.equals(other.status)) {
            return false;
        }
        if (systemVersion == null) {
            if (other.systemVersion != null) {
                return false;
            }
        } else if (!systemVersion.equals(other.systemVersion)) {
            return false;
        }
        if (updateTime != other.updateTime) {
            return false;
        }
        return true;
    }
}
