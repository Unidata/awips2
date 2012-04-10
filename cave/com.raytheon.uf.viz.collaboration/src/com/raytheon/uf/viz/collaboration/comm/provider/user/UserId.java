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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
public class UserId implements IQualifiedID {

    @DynamicSerializeElement
    protected String name;

    @DynamicSerializeElement
    protected String host;

    @DynamicSerializeElement
    protected String resource;

    /**
     * 
     */
    public UserId() {

    }

    /**
     * 
     * @param userName
     * @param hostName
     */
    public UserId(String userName, String hostName) {
        this.name = userName;
        this.host = hostName;
        resource = null;
    }

    /**
     * 
     * @param userName
     * @param hostName
     * @param resourceName
     */
    public UserId(String userName, String hostName, String resourceName) {
        this.name = userName;
        this.host = hostName;
        resource = resourceName;
    }

    /**
     * @param userName
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#setUserName(java.lang.String)
     */
    @Override
    public void setName(String userName) {
        name = userName;
    }

    /**
     * @return The user name associated with this id.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#getUserName()
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * 
     * @param hostName
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#setHostName(java.lang.String)
     */
    @Override
    public void setHost(String hostName) {
        host = hostName;
    }

    /**
     * 
     * @return The host name associated with this id.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#getHostName()
     */
    @Override
    public String getHost() {
        return host;
    }

    /**
     * 
     * @param resourceName
     *            The resource associated with this id.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#setResourceName(java.lang.String)
     */
    @Override
    public void setResource(String resourceName) {
        resource = resourceName;
    }

    /**
     * 
     * @return The resource associated with this id.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#getResource()
     */
    @Override
    public String getResource() {
        return resource;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#getFQName()
     */
    @Override
    public String getFQName() {
        StringBuilder sb = new StringBuilder(name);
        sb.append("@");
        sb.append(host);
        sb.append("/");
        if (resource != null) {
            sb.append(resource);
        } else {
            // TODO need a better way around this ECF/XMPP flaw that is
            // requiring a resource for peerToPeer to go through
            sb.append("resource");
        }
        return sb.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((host == null) ? 0 : host.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result
                + ((resource == null) ? 0 : resource.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        UserId other = (UserId) obj;
        if (host == null) {
            if (other.host != null)
                return false;
        } else if (!host.equals(other.host))
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (resource == null) {
            if (other.resource != null)
                return false;
        } else if (!resource.equals(other.resource))
            return false;
        return true;
    }

}
