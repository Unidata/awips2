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

import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;

/**
 * Parsed ID string from venue. Not guaranteed to have username, but will always
 * have alias.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2014            bclement     Initial creation
 * Feb 13, 2014 2751       bclement     no longer is a subclass of UserId
 * Apr 22, 2014 3056       bclement     made equals case insensitive
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@DynamicSerialize
@XmlRootElement(name = "participant")
public class VenueParticipant implements IUser {

    @DynamicSerializeElement
    private String handle;

    @DynamicSerializeElement
    private String host;

    @DynamicSerializeElement
    private UserId userid;

    @DynamicSerializeElement
    private String room;

    /**
     * 
     */
    public VenueParticipant() {
    }

    /**
     * @param room
     *            name of venue
     * @param hostName
     *            qualified name of host including conference subdomain
     * @param handle
     *            public name of user in room
     * @param userid
     *            actual userid of user
     */
    public VenueParticipant(String room, String hostName, String handle,
            UserId userid) {
        this(room, hostName, handle);
        this.userid = userid;
    }

    /**
     * @param room
     *            name of venue
     * @param hostName
     *            qualified name of host including conference subdomain
     * @param handle
     *            public name of user in room
     */
    public VenueParticipant(String room, String hostName, String handle) {
        this.room = room;
        this.host = hostName;
        this.handle = handle;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.provider.user.UserId#hashCode()
     */
    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(host);
        builder.append(handle);
        builder.append(room);
        return builder.toHashCode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.provider.user.UserId#equals(java
     * .lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof VenueParticipant)) {
            return false;
        }
        /*
         * the xmpp server lower cases room names so we may get them back as
         * lower case when we have them locally as upper/mixed case. Treat case
         * insensitive.
         */
        VenueParticipant other = (VenueParticipant) obj;
        if (!stringFieldEquals(this.handle, other.handle)){
            return false;
        }
        if (!stringFieldEquals(this.room, other.room)) {
            return false;
        }
        if (!stringFieldEquals(this.host, other.host)) {
            return false;
        }
        return true;
    }

    /**
     * @param field
     * @param other
     * @return true if both arguments are null or arguments are equal ignoring
     *         case
     */
    private boolean stringFieldEquals(String field, String other) {
        boolean rval;
        if (field == null) {
            rval = other == null;
        } else {
            rval = field.equalsIgnoreCase(other);
        }
        return rval;
    }

    /**
     * @param id
     * @return true if id represents same venue participant as this one
     */
    public boolean isSameUser(String id) {
        if (!IDConverter.isFromRoom(id)) {
            return hasActualUserId() && userid.isSameUser(id);
        }
        VenueParticipant other = IDConverter.convertFromRoom(null, id);
        return isSameUser(other);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.user.IUser#isSameUser
     * (com.raytheon.uf.viz.collaboration.comm.identity.user.IUser)
     */
    @Override
    public boolean isSameUser(IUser other) {
        if (other instanceof UserId) {
            return hasActualUserId() && userid.isSameUser(other);
        }
        if (!(other instanceof VenueParticipant)) {
            return false;
        }
        return equals(other);
    }

    @Override
    public String toString() {
        return getFQName();
    }

    /**
     * @return id of room that this participant is in (includes hostname)
     */
    public String getRoomId() {
        return room + "@" + host;
    }

    /**
     * @return true if actually userid of participant is known
     */
    public boolean hasActualUserId() {
        return this.userid != null;
    }

    /**
     * @return the handle
     */
    public String getHandle() {
        return handle;
    }

    /**
     * @param handle
     *            the handle to set
     */
    public void setHandle(String handle) {
        this.handle = handle;
    }

    /**
     * @return the host
     */
    public String getHost() {
        return host;
    }

    /**
     * @param host
     *            the host to set
     */
    public void setHost(String host) {
        this.host = host;
    }

    /**
     * @return the userid
     */
    public UserId getUserid() {
        return userid;
    }

    /**
     * @param userid
     *            the userid to set
     */
    public void setUserid(UserId userid) {
        this.userid = userid;
    }

    /**
     * name of venue
     * 
     * @return the room
     */
    public String getRoom() {
        return room;
    }

    /**
     * @param room
     *            name of venue
     */
    public void setRoom(String room) {
        this.room = room;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#getName
     * ()
     */
    @Override
    public String getName() {
        return handle;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID#getFQName
     * ()
     */
    @Override
    public String getFQName() {
        return getRoomId() + "/" + handle;
    }

}
