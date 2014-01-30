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

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

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
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@DynamicSerialize
@XmlRootElement(name = "participant")
public class VenueParticipant extends UserId {

    /**
     * 
     */
    public VenueParticipant() {
    }

    /**
     * @param userName
     * @param hostName
     */
    public VenueParticipant(String userName, String hostName) {
        super(userName, hostName);
    }

    /**
     * @param userName
     * @param hostName
     * @param resource
     */
    public VenueParticipant(String userName, String hostName, String handle) {
        this(userName, hostName);
        setAlias(handle);
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
        builder.append(alias);
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
        if (!(obj instanceof UserId)) {
            return false;
        }
        UserId user = (UserId) obj;
        EqualsBuilder builder = new EqualsBuilder();
        builder.append(alias, user.alias);
        builder.append(host, user.host);
        return builder.isEquals();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.provider.user.UserId#isSameUser
     * (java.lang.String)
     */
    @Override
    public boolean isSameUser(String id) {
        if (!IDConverter.isFromRoom(id)) {
            return false;
        }
        UserId other = IDConverter.convertFromRoom(null, id);
        return isSameUser(other);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.provider.user.UserId#isSameUser
     * (com.raytheon.uf.viz.collaboration.comm.provider.user.UserId)
     */
    @Override
    public boolean isSameUser(UserId other) {
        return equals(other);
    }

}
