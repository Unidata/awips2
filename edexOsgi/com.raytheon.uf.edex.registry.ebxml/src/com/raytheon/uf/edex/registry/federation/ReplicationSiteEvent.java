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

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.MapsId;
import javax.persistence.Table;

import org.hibernate.annotations.ForeignKey;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.edex.registry.federation.ReplicationSiteEvent.ReplicationSiteEventId;

/**
 * 
 * Class encapsulating a mapping between a registry event and a site
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 8, 2016  5810      tjensen     Initial creation
 * 
 * </pre>
 * 
 * @author tjensen
 * @version 1.0
 */
@Entity
@Table(name = "registry_replication_site_events")
public class ReplicationSiteEvent implements
        IPersistableDataObject<ReplicationSiteEventId> {
    @Embeddable
    public static class ReplicationSiteEventId implements Serializable {
        private static final long serialVersionUID = 1L;

        @Column
        private Long eventId;

        @Column
        private String registryId;

        public String getRegistryId() {
            return registryId;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = (prime * result)
                    + ((eventId == null) ? 0 : eventId.hashCode());
            result = (prime * result)
                    + ((registryId == null) ? 0 : registryId.hashCode());
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
            ReplicationSiteEventId other = (ReplicationSiteEventId) obj;
            if (eventId == null) {
                if (other.eventId != null) {
                    return false;
                }
            } else if (!eventId.equals(other.eventId)) {
                return false;
            }
            if (registryId == null) {
                if (other.registryId != null) {
                    return false;
                }
            } else if (!registryId.equals(other.registryId)) {
                return false;
            }
            return true;
        }
    }

    @EmbeddedId
    private ReplicationSiteEventId id = new ReplicationSiteEventId();

    @ManyToOne(optional = false)
    @MapsId("eventId")
    @ForeignKey(name = "fk_registry_replication_events_id")
    private ReplicationEvent event;

    public ReplicationSiteEvent() {
        super();
    }

    /**
     * @return the id
     */
    public ReplicationSiteEventId getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(ReplicationSiteEventId id) {
        this.id = id;
    }

    /**
     * @return the event
     */
    public ReplicationEvent getEvent() {
        return event;
    }

    /**
     * @param event
     *            the event to set
     */
    public void setEvent(ReplicationEvent event) {
        this.event = event;
    }

    public ReplicationSiteEvent(ReplicationEvent event, String registryId) {
        super();
        this.event = event;
        this.id.eventId = event.getId();
        this.id.registryId = registryId;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result) + ((id == null) ? 0 : id.hashCode());
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
        ReplicationSiteEvent other = (ReplicationSiteEvent) obj;
        if (id == null) {
            if (other.id != null) {
                return false;
            }
        } else if (!id.equals(other.id)) {
            return false;
        }
        return true;
    }

    @Override
    public ReplicationSiteEventId getIdentifier() {
        return getId();
    }

}
