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
package com.raytheon.uf.edex.datadelivery.registry.replication;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * Container class holding the upstream and downstream servers used for registry
 * replication via subscription/notification
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/9/2013     1802        bphillip    Initial implementation
 * 5/21/2013    1707        bphillip    Removed unused fields
 * 6/4/2013     1707        bphillip    Renamed and changed fields for clarity
 * 10/30/2013   1538        bphillip    getRegistryReplicationServers returns empty list if no servers are specified
 * 2/27/2014    2769        bphillip    Changed replicationservers variable to set of strings
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "NotificationServers")
@XmlAccessorType(XmlAccessType.NONE)
public class NotificationServers {

    /** The server located upstream from this server */
    @XmlElements({ @XmlElement(name = "registry") })
    private CopyOnWriteArraySet<String> registryReplicationServers;

    public Set<String> getRegistryReplicationServers() {
        if (registryReplicationServers == null) {
            registryReplicationServers = new CopyOnWriteArraySet<String>();
        }
        return registryReplicationServers;
    }

    public void setRegistryReplicationServers(
            Set<String> registryReplicationServers) {
        this.registryReplicationServers = new CopyOnWriteArraySet<String>(
                registryReplicationServers);
    }

    public void addReplicationServer(String registryId) {
        this.getRegistryReplicationServers().add(registryId);
    }

    public void removeReplicationServer(String registryId) {
        this.getRegistryReplicationServers().remove(registryId);
    }
}
