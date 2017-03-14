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
package com.raytheon.uf.common.dataplugin.gfe.sample;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;

/**
 * Contains the identifier to uniquely identify a SampleData set.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 14, 2008	   879      rbell       Initial creation
 * Sep 15, 2014  #3592      randerso    Code cleanup, JavaDoc improvement
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */

public class SampleId implements Cloneable {

    private String name;

    private boolean protect;

    private LocalizationLevel access;

    /**
     * Default Constructor.
     */
    public SampleId() {
        this((String) null);
    }

    /**
     * Constructor taking the name to uniquely identify this sample data.
     * 
     * @param name
     */
    public SampleId(String name) {
        this(name, false, LocalizationLevel.UNKNOWN);
    }

    /**
     * Constructor taking the name to uniquely identify this sample data. Also,
     * can set the Protect flag and access mode.
     * 
     * @param name
     * @param protect
     * @param access
     */
    public SampleId(final String name, boolean protect, LocalizationLevel access) {
        this.name = name;
        this.protect = protect;
        this.access = access;
    }

    /**
     * Copy constructor
     * 
     * @param rhs
     */
    public SampleId(SampleId rhs) {
        this(new String(rhs.name), rhs.protect, rhs.access);
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the protect
     */
    public boolean isProtected() {
        return this.protect;
    }

    /**
     * @return the access
     */
    public LocalizationLevel getAccess() {
        return this.access;
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
        return (this.name.length() != 0);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result)
                + ((this.access == null) ? 0 : this.access.hashCode());
        result = (prime * result)
                + ((this.name == null) ? 0 : this.name.hashCode());
        result = (prime * result) + (this.protect ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null) {
            return false;
        }
        if (getClass() != o.getClass()) {
            return false;
        }
        final SampleId rhs = (SampleId) o;
        if (this.access == null) {
            if (rhs.access != null) {
                return false;
            }
        } else if (!this.access.equals(rhs.access)) {
            return false;
        }
        if (this.name == null) {
            if (rhs.name != null) {
                return false;
            }
        } else if (!this.name.equals(rhs.name)) {
            return false;
        }
        if (this.protect != rhs.protect) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "(" + this.name + ",P=" + this.protect + ",A=" + this.access
                + ")";
    }

    /**
     * @return the protect
     */
    public boolean isProtect() {
        return protect;
    }

    /**
     * @param protect
     *            the protect to set
     */
    public void setProtect(boolean protect) {
        this.protect = protect;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @param access
     *            the access to set
     */
    public void setAccess(LocalizationLevel access) {
        this.access = access;
    }

    @Override
    public SampleId clone() {
        return new SampleId(this);
    }
}
