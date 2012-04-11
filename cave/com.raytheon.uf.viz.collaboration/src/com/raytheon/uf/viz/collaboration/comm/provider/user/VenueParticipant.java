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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

@DynamicSerialize
public class VenueParticipant extends RosterId implements IVenueParticipant {

    private static String CONF_ID = "conference.";

    private Map<String, String> properties;

    public VenueParticipant() {
        this(null, null);
    }

    /**
     * 
     */
    public VenueParticipant(String name, String host) {
        this(name, host, null);
    }

    /**
     * 
     */
    public VenueParticipant(String name, String host, String resource) {
        super(name, host, resource);
        this.name = name;
        this.host = host;
        this.properties = new HashMap<String, String>();
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IPropertied#setProperty(java.lang.String,
     *      java.lang.String)
     */
    @Override
    public void setProperty(String key, String value) {
        properties.put(key, value);
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IPropertied#getProperty(java.lang.String,
     *      java.lang.String)
     */
    @Override
    public String getProperty(String key, String defaultValue) {
        String value = properties.get(key);
        if (value == null) {
            value = defaultValue;
        }
        return value;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IPropertied#getProperties()
     */
    @Override
    public Collection<Property> getProperties() {
        return null;
    }

    /**
     * Return the identifier as a qualified field. Removes the "domain"
     * conference from the host string if found.
     * 
     * @return The qualified id.
     */
    @Override
    public IQualifiedID getQualifiedId() {
        String hostName = host;
        if (hostName != null) {
            if (hostName.startsWith(CONF_ID)) {
                hostName = hostName.substring(CONF_ID.length());
            }
        }

        UserId id = new UserId(getName(), hostName);
        id.setResource(resource);

        return id;
    }

    public void setProperties(Map<String, String> properties) {
        this.properties = properties;
    }

    @Override
    public String toString() {
        return this.getFQName();
    }

}
