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
 * Qualified id for a venue
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class VenueId implements IQualifiedID {

    private String host;

    private String resource;

    private String name;

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
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.ID#setName(java.lang.String)
     */
    @Override
    public void setName(String venueName) {
        name = venueName;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.ID#getName()
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.ID#getFQName()
     */
    @Override
    public String getFQName() {
        // FIXME
        return null;
    }

}
