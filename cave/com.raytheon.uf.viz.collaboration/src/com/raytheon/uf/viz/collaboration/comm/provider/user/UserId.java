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
public class UserId implements IQualifiedID {

    private String name;
    
    private String host;
    
    private String resource;

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
     * @param resourceName The resource associated with this id.
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
        if(resource != null) {
            sb.append("/");
            sb.append(resource);
        }
        return sb.toString();
    }

}
