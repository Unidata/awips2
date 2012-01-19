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
 * ReferenceID contains the unique identifier for which to identify a
 * ReferenceData (reference set).
 * 
 * implementation
 * 
 * At the present time, a ReferenceID is simply a String.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/01/2008              randerso    Initial port	
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ReferenceID {

    // private static final char special[] = { '(', '<', '>', '=', '!', '~',
    // '+',
    // '*', '-', ')' };

    // private static final int numChar = special.length;

    private String name;

    private boolean protect;

    private LocalizationLevel access;

    /**
     * Returns true if the name is okay. It searches through for special
     * characters. Special characters are allowed just as long as there are no
     * spaces also.
     * 
     * @param string
     * @return
     */
    // COMMENTED OUT, NO USE FOUND
    // private static boolean specialCharOkay(final String string) {
    //
    // // any special characters?
    // int pos = 0;
    // boolean specialFound = false;
    // for (int i = 0; i < numChar; i++)
    // if (string.indexOf(special[i], pos) >= 0) {
    // specialFound = true;
    // break;
    // }
    //
    // // if special character found, then spaces aren't permitted.
    // if (specialFound && string.indexOf(' ', pos) >= 0)
    // return false;
    // else
    // return true;
    // }
    /**
     * Default Constructor.
     */
    public ReferenceID() {
        this((String) null);
    }

    /**
     * Constructor taking the name to uniquely identify this reference set.
     * 
     * @param name
     */
    public ReferenceID(final String name) {
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
    public ReferenceID(final String name, boolean protect, LocalizationLevel access) {
        this.name = name;
        this.protect = protect;
        this.access = access;
    }

    /**
     * Returns the name associated with this identifier.
     * 
     * @return
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
     * @return
     */
    public boolean isValid() {
        return (name.length() != 0);
    }

    /**
     * Protected ReferenceID objects should not be deleted by the user.
     * 
     * @return
     */
    public boolean isProtected() {
        return protect;
    }

    /**
     * Returns the access level.
     * 
     * @return
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
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final ReferenceID other = (ReferenceID) obj;
        if (access == null) {
            if (other.access != null)
                return false;
        } else if (!access.equals(other.access))
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (protect != other.protect)
            return false;
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
}
