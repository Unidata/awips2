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

import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;

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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
@Entity
@Table(name = "registry_replication_events")
@SequenceGenerator(name = "REPLICATIONEVENT_GENERATOR", sequenceName = "replicationevent_seq", allocationSize = 100)
public class ReplicationEvent implements IPersistableDataObject<Long>,
        Comparable<ReplicationEvent> {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "REPLICATIONEVENT_GENERATOR")
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

    @OneToMany(mappedBy = "event", fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @Fetch(FetchMode.SUBSELECT)
    private Set<ReplicationSiteEvent> replicateTo;

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
        this.replicateTo = new HashSet<>();
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

    public Set<ReplicationSiteEvent> getReplicateTo() {
        return replicateTo;
    }

    public void setReplicateTo(Set<ReplicationSiteEvent> replicateTo) {
        this.replicateTo = replicateTo;
    }

    /**
     * Add the given registry to the event's list of registries that need to be
     * replicated to. Also add the event to the registry.
     * 
     * @param registry
     *            registry to add.
     */
    public void addReplicateTo(String site) {
        ReplicationSiteEvent siteEvent = new ReplicationSiteEvent(this, site);
        getReplicateTo().add(siteEvent);
    }

    public String getSource() {
        return source;
    }

    /**
     * Calculate the hash code for this instance.
     * 
     * @return The calculated hash code.
     */
    @Override
    public int hashCode() {
        int prime = 31;
        int result = 1;
        result = (prime * result) + (int) (eventTime ^ (eventTime >>> 32));
        result = (prime * result)
                + ((eventType == null) ? 0 : eventType.hashCode());
        result = (prime * result)
                + ((objectId == null) ? 0 : objectId.hashCode());
        result = (prime * result)
                + ((objectType == null) ? 0 : objectType.hashCode());
        result = (prime * result) + ((source == null) ? 0 : source.hashCode());
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
        if (this.eventTime != other.eventTime) {
            return false;
        }
        if (eventType == null) {
            if (other.eventType != null) {
                return false;
            }
        } else if (!eventType.equals(other.eventType)) {
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

        return this.eventTime > o.eventTime ? 1
                : (this.eventTime < o.eventTime ? -1 : 0);
    }
}
