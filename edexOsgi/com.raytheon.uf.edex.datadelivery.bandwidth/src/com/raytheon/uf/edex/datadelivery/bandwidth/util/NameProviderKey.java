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
package com.raytheon.uf.edex.datadelivery.bandwidth.util;


/**
 * Object holding name and provider Strings to act as a hash key.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2013   2636     mpduff      Initial creation.
 * Jan 13, 2014   2636     mpduff      Removed the apache commons Equals/Hashcode builder.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class NameProviderKey {

    private final String name;

    private final String provider;

    /**
     * Constructor.
     * 
     * @param name
     *            The name
     * @param provider
     *            The provider
     */
    public NameProviderKey(String name, String provider) {
        this.name = name;
        this.provider = provider;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the provider
     */
    public String getProvider() {
        return provider;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.name + " " + this.provider;
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
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result
                + ((provider == null) ? 0 : provider.hashCode());
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
        if (!(obj instanceof NameProviderKey)) {
            return false;
        }
        NameProviderKey other = (NameProviderKey) obj;
        if (name == null) {
            if (other.name != null) {
                return false;
            }
        } else if (!name.equals(other.name)) {
            return false;
        }
        if (provider == null) {
            if (other.provider != null) {
                return false;
            }
        } else if (!provider.equals(other.provider)) {
            return false;
        }
        return true;
    }
}
