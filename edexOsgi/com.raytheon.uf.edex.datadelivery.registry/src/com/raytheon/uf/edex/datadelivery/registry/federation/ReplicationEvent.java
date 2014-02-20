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
package com.raytheon.uf.edex.datadelivery.registry.federation;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;

/**
 * <pre>
 * 
 * Class encapsulating a registry event
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/19/2014    2769        bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
@Entity
@Table(name = "registryreplicationevents")
public class ReplicationEvent implements IPersistableDataObject<Long> {

    @Id
    @GeneratedValue
    private Long id;

    @Column
    private String source;

    @Column
    private String eventType;

    @Column
    private long eventTime;

    @Column
    private String objectId;

    @Column
    private String objectType;

    @Column
    private String replicatedTo;

    public ReplicationEvent() {
        super();
    }

    public ReplicationEvent(String eventType, long eventTime, String objectId,
            String objectType, String source) {
        super();
        this.eventType = eventType;
        this.eventTime = eventTime;
        this.objectId = objectId;
        this.objectType = objectType;
        this.source = source;
    }

    @Override
    public Long getIdentifier() {
        return getId();
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public long getEventTime() {
        return eventTime;
    }

    public void setEventTime(long eventTime) {
        this.eventTime = eventTime;
    }

    public String getObjectId() {
        return objectId;
    }

    public void setObjectId(String objectId) {
        this.objectId = objectId;
    }

    public String getObjectType() {
        return objectType;
    }

    public void setObjectType(String objectType) {
        this.objectType = objectType;
    }

    public String getReplicatedTo() {
        return replicatedTo;
    }

    public void setReplicatedTo(String replicatedTo) {
        this.replicatedTo = replicatedTo;
    }

    public void addReplicatedTo(String recipient) {
        if (this.replicatedTo == null) {
            this.replicatedTo = ":" + recipient + ":";
        } else if (!this.replicatedTo.contains(recipient)) {
            this.replicatedTo += ":" + recipient + ":";
        }
    }

    public String getSource() {
        return source;
    }

}
