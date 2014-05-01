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
package com.raytheon.uf.common.dataplugin.gfe.reference;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;

/**
 * GroupID contains the unique identifier by which to identify an edit area
 * group.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 1, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GroupID implements Comparable<GroupID> {

    private String name;

    private boolean protect;

    private LocalizationLevel access;

    /**
     * Default Constructor.
     */
    public GroupID() {
        this((String) null);
    }

    /**
     * Constructor taking the name to uniquely identify this reference set.
     * 
     * @param name
     */
    public GroupID(final String name) {
        this(name, false, LocalizationLevel.UNKNOWN);
    }

    /**
     * Constructor taking the name to uniquely identify this reference set.
     * Also, can set the Protect flag and access mode.
     * 
     * @param name
     * @param protect
     * @param access
     */
    public GroupID(final String name, boolean protect, LocalizationLevel access) {
        this.name = name;
        this.protect = protect;
        this.access = access;
    }

    /**
     * Returns the name associated with this identifier.
     * 
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * Returns true if this identifier is valid.
     * 
     * implementation
     * 
     * A valid identifier has a length of non-zero.
     * 
     * @return true if valid
     */
    public boolean isValid() {
        return (name.length() != 0);
    }

    /**
     * Protected GroupID objects should not be deleted by the user.
     * 
     * @return true if protected
     */
    public boolean isProtected() {
        return protect;
    }

    /**
     * Returns the access level (BASE, SITE, USER).
     * 
     * @return the access level
     */
    public LocalizationLevel getAccess() {
        return access;
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
        result = prime * result + ((access == null) ? 0 : access.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + (protect ? 1231 : 1237);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final GroupID other = (GroupID) obj;
        if (access == null) {
            if (other.access != null) {
                return false;
            }
        } else if (!access.equals(other.access)) {
            return false;
        }
        if (name == null) {
            if (other.name != null) {
                return false;
            }
        } else if (!name.equals(other.name)) {
            return false;
        }
        if (protect != other.protect) {
            return false;
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "(" + name + ",P=" + protect + ",A=" + access + ")";
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(GroupID o) {
        return this.name.compareTo(o.name);
    }
}
