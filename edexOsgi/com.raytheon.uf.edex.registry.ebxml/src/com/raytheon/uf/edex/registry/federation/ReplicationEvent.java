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
package com.raytheon.uf.edex.registry.federation;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;

/**
 * <pre>
 * 
 * Class encapsulating a registry event
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 19, 2014  2769     bphillip  Initial Creation
 * May 11, 2015  4448     bphillip  Separated EBXML Registry from Data Delivery
 * May 20, 2016  5638     dhladky   Event time Comparator added.
 * Aug 09, 2016  5810     tjensen   Refactor replication
 * Aug 29, 2019  7836     bsteffen  Keep track of only latest event for each site.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
@Entity
@Table(name = "registry_replication_events")
@org.hibernate.annotations.Table(appliesTo = "registry_replication_events", indexes = {
        @Index(name = "registry_replication_events_time", columnNames = {
                "eventTime" }) })
@SequenceGenerator(name = "REPLICATIONEVENT_GENERATOR", sequenceName = "replicationevent_seq", allocationSize = 100)
public class ReplicationEvent
        implements IPersistableDataObject<Long>, Comparable<ReplicationEvent> {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "REPLICATIONEVENT_GENERATOR")
    private Long id;

    @Column
    private String source;

    @Column
    private String eventType;

    @Column(nullable = false)
    private Date eventTime;

    @Column
    private String objectId;

    @Column
    private String objectType;

    public ReplicationEvent() {
        super();
    }

    public ReplicationEvent(String eventType, Date eventTime, String objectId,
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

    public Date getEventTime() {
        return eventTime;
    }

    public void setEventTime(Date eventTime) {
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

    public String getSource() {
        return source;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((eventTime == null) ? 0 : eventTime.hashCode());
        result = prime * result
                + ((eventType == null) ? 0 : eventType.hashCode());
        result = prime * result + ((id == null) ? 0 : id.hashCode());
        result = prime * result
                + ((objectId == null) ? 0 : objectId.hashCode());
        result = prime * result
                + ((objectType == null) ? 0 : objectType.hashCode());
        result = prime * result + ((source == null) ? 0 : source.hashCode());
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
        ReplicationEvent other = (ReplicationEvent) obj;
        if (eventTime == null) {
            if (other.eventTime != null) {
                return false;
            }
        } else if (!eventTime.equals(other.eventTime)) {
            return false;
        }
        if (eventType == null) {
            if (other.eventType != null) {
                return false;
            }
        } else if (!eventType.equals(other.eventType)) {
            return false;
        }
        if (id == null) {
            if (other.id != null) {
                return false;
            }
        } else if (!id.equals(other.id)) {
            return false;
        }
        if (objectId == null) {
            if (other.objectId != null) {
                return false;
            }
        } else if (!objectId.equals(other.objectId)) {
            return false;
        }
        if (objectType == null) {
            if (other.objectType != null) {
                return false;
            }
        } else if (!objectType.equals(other.objectType)) {
            return false;
        }
        if (source == null) {
            if (other.source != null) {
                return false;
            }
        } else if (!source.equals(other.source)) {
            return false;
        }
        return true;
    }

    @Override
    public int compareTo(ReplicationEvent o) {
        return this.getEventTime().compareTo(o.getEventTime());
    }
}
