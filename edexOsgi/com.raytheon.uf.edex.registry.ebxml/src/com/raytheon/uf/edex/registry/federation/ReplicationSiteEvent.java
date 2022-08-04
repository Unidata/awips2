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
import javax.persistence.Id;
import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.edex.registry.ebxml.services.rest.RegistryFederationManager;

/**
 * 
 * Map for keeping track of which {@link ReplicationEvent}s need to be sent to a
 * single remote registry. During normal replication this tracks the id of the
 * last event sent, but after a synchronize operation the time is used.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 8, 2016  5810      tjensen     Initial creation
 * Aug 29, 2019 7836      bsteffen    Keep track of only latest event for each site.
 * 
 * </pre>
 * 
 * @author tjensen
 * @version 1.0
 */
@Entity
@Table(name = "registry_replication_site_events")
public class ReplicationSiteEvent implements IPersistableDataObject<String> {

    @Id
    @Column
    private String registryId;

    @Column
    private Long eventId;

    @Column(nullable = false)
    private Date eventTime;

    public ReplicationSiteEvent() {

    }

    public ReplicationSiteEvent(String registryId, Date eventTime) {
        this(registryId, null, eventTime);
    }

    public ReplicationSiteEvent(String registryId, Long eventId,
            Date eventTime) {
        this.registryId = registryId;
        this.eventId = eventId;
        this.eventTime = eventTime;
    }

    public String getRegistryId() {
        return registryId;
    }

    public void setRegistryId(String registryId) {
        this.registryId = registryId;
    }

    public Long getEventId() {
        return eventId;
    }

    public void setEventId(Long eventId) {
        this.eventId = eventId;
    }

    public Date getEventTime() {
        return eventTime;
    }

    public void setEventTime(Date eventTime) {
        this.eventTime = eventTime;
    }

    @Override
    public String getIdentifier() {
        return registryId;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((eventId == null) ? 0 : eventId.hashCode());
        result = prime * result
                + ((eventTime == null) ? 0 : eventTime.hashCode());
        result = prime * result
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
        ReplicationSiteEvent other = (ReplicationSiteEvent) obj;
        if (eventId == null) {
            if (other.eventId != null) {
                return false;
            }
        } else if (!eventId.equals(other.eventId)) {
            return false;
        }
        if (eventTime == null) {
            if (other.eventTime != null) {
                return false;
            }
        } else if (!eventTime.equals(other.eventTime)) {
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

    @Override
    public String toString() {
        return "ReplicationSiteEvent [registryId=" + registryId + ", eventId="
                + eventId + ", eventTime=" + eventTime + "]";
    }

    public boolean needsSync() {
        return System.currentTimeMillis() - eventTime
                .getTime() > RegistryFederationManager.autoSynchIntervalInMs;
    }

}
