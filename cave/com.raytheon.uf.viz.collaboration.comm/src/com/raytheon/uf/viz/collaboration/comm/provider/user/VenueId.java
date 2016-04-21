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

import org.jivesoftware.smack.util.StringUtils;

import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;

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
 * Feb 13, 2014 2751       bclement    removed resource, fixed getFQN
 * May 19, 2014 3180       bclement    added isSameVenue() fromString() and hashcode/equals
 * Jun 16, 2014 3288       bclement    added constructors, default subdomain
 * Oct 08, 2014 3705       bclement    added single string constructor, compareTo()
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class VenueId implements IQualifiedID, Comparable<VenueId> {

    public static final String DEFAULT_SUBDOMAIN = "conference";

    private String host;

    private String name;

    /**
     * 
     */
    public VenueId() {
    }

    public VenueId(String jid) {
        this.name = StringUtils.parseName(jid);
        this.host = StringUtils.parseServer(jid);
    }

    /**
     * @param host
     * @param name
     */
    public VenueId(String host, String name) {
        this.host = host;
        this.name = name;
    }

    /**
     * subdomain and domain are combined to create the host
     * 
     * @param subdomain
     * @param domain
     * @param name
     */
    public VenueId(String subdomain, String domain, String name) {
        this.host = subdomain + "." + domain;
        this.name = name;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#setHost(java.lang.String)
     */
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
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.ID#setName(java.lang.String)
     */
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
        return name + "@" + host;
    }

    /**
     * @param venueId
     * @return true if argument represents the same venue on the server
     */
    public boolean isSameVenue(String venueId) {
        boolean rval;
        if (venueId == null) {
            rval = false;
        } else {
            rval = this.equals(fromString(venueId));
        }
        return rval;
    }

    /**
     * @param other
     * @return true if argument represents the same venue on the server
     */
    public boolean isSameVenue(VenueId other) {
        return this.equals(other);
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
        VenueId other = (VenueId) obj;
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
        return true;
    }

    /**
     * @param venueId
     *            in the form room@host
     * @return
     */
    public static VenueId fromString(String venueId) {
        VenueId rval = new VenueId();
        rval.setName(Tools.parseName(venueId));
        rval.setHost(Tools.parseHost(venueId));
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(VenueId o) {
        int rval;
        if (o == null) {
            rval = 1;
        } else if (this.name == null) {
            if (o.name == null) {
                rval = 0;
            } else {
                rval = -1;
            }
        } else {
            if (o.name == null) {
                rval = 1;
            } else {
                rval = this.name.compareTo(o.name);
            }
        }
        return rval;
    }
}
