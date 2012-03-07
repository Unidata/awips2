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

public class VenueParticipant implements IVenueParticipant {
    
    private Map<String, String> properties;

    private String name;

    private String nickname;

    private String host;

    private String resource;
    
    /**
     * 
     */
    public VenueParticipant() {
        properties = new HashMap<String, String>();
    }

    /**
     * 
     */
    public VenueParticipant(String name, String nickName) {
        this(name, nickName, null);
    }

    /**
     * 
     */
    public VenueParticipant(String name, String nickName, String host) {
        this();
        this.name = name;
        this.nickname = nickName;
        this.host = host;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IPropertied#setProperty(java.lang.String, java.lang.String)
     */
    @Override
    public void setProperty(String key, String value) {
        properties.put(key, value);
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IPropertied#getProperty(java.lang.String, java.lang.String)
     */
    @Override
    public String getProperty(String key, String defaultValue) {
        String value = properties.get(key);
        if(value == null) {
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
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant#getName()
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant#getNickname()
     */
    @Override
    public String getNickname() {
        return nickname;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant#setName(java.lang.String)
     */
    @Override
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant#getNickname(java.lang.String)
     */
    @Override
    public void setNickname(String nickName) {
        this.nickname = nickName;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#setHost(java.lang.String)
     */
    @Override
    public void setHost(String hostName) {
        host = hostName;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#getHost()
     */
    @Override
    public String getHost() {
        return host;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#setResource(java.lang.String)
     */
    @Override
    public void setResource(String resource) {
        this.resource = resource;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#getResource()
     */
    @Override
    public String getResource() {
        return resource;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.ID#getFQName()
     */
    @Override
    public String getFQName() {
        return null;
    }

}
