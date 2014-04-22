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

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterGroup;

/**
 * Shared group managed by xmpp server. Cannot be modified by client.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 23, 2014 2701       bclement    Initial creation
 * Feb 17, 2014 2800       bclement    added equals/hashcode
 * Mar 27, 2014 2632       mpduff      Changed to call a convenience method.
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SharedGroup {

    private final RosterGroup delegate;

    /**
     * @param group
     */
    public SharedGroup(RosterGroup group) {
        this.delegate = group;
    }

    /**
     * @return name of group
     */
    public String getName() {
        return delegate.getName();
    }

    /**
     * @return collection of entries in this group
     */
    public Collection<RosterEntry> getEntries() {
        return delegate.getEntries();
    }

    /**
     * @param entry
     * @return true if entry is in this group
     */
    public boolean contains(RosterEntry entry) {
        return delegate.contains(entry);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(delegate.getName());
        return builder.toHashCode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof SharedGroup)) {
            return false;
        }
        SharedGroup other = (SharedGroup) obj;
        return this.getName().equals(other.getName());
    }

}
