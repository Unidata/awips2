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
package com.raytheon.uf.common.wmo;

import java.io.Serializable;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2008       1538 jkorman     Initial creation
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class AFOSProductId implements Serializable {
    
    private static final long serialVersionUID = 1L;

    private String ccc;

    private String nnn;

    private String xxx;

    /**
     * 
     */
    public AFOSProductId() {
    }
    
    
    /**
     * 
     * @param cccId
     * @param nnnId
     * @param xxxId
     */
    public AFOSProductId(String cccId, String nnnId, String xxxId) {
        ccc = cccId;
        nnn = nnnId;
        xxx = xxxId;
    }

    /**
     * 
     * @param prodId
     */
    public AFOSProductId(String prodId) {
        ccc = prodId.substring(0,3);
        nnn = prodId.substring(3,6);
        xxx = prodId.substring(6);
    }
    
    /**
     * 
     * @param prodId
     */
    public AFOSProductId(String prodId, String site) {
        if(prodId != null) {
            if(prodId.length() <= 6) {
                prodId = site + prodId;
            } else {
                if(prodId.startsWith("AAA")) {
                    prodId = site + prodId.substring(3);
                }
            }
        }
        ccc = prodId.substring(0,3);
        nnn = prodId.substring(3,6);
        xxx = prodId.substring(6);
    }
    
    /**
     * 
     * @return the ccc
     */
    public String getCcc() {
        return ccc;
    }

    /**
     * 
     * @param ccc
     *            the ccc to set
     */
    public void setCcc(String ccc) {
        this.ccc = ccc;
    }

    /**
     * 
     * @return the nnn
     */
    public String getNnn() {
        return nnn;
    }

    /**
     * 
     * @param nnn
     *            the nnn to set
     */
    public void setNnn(String nnn) {
        this.nnn = nnn;
    }

    /**
     * 
     * @return the xxx
     */
    public String getXxx() {
        return xxx;
    }

    /**
     * 
     * @param xxx
     *            the xxx to set
     */
    public void setXxx(String xxx) {
        this.xxx = xxx;
    }

    /**
     * Are all fields in this id not null?
     * @return Not null status.
     */
    public boolean isFilled() {
        return ((ccc != null)&&(nnn != null)&&(xxx != null));
    }
    
    
    /**
     * Generate the HashCode for this object. Eclipse generated.
     * 
     * @see java.lang.Object#hashCode()
     * @return The computed hashcode.
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((ccc == null) ? 0 : ccc.hashCode());
        result = prime * result + ((nnn == null) ? 0 : nnn.hashCode());
        result = prime * result + ((xxx == null) ? 0 : xxx.hashCode());
        return result;
    }

    /**
     * Determine if this object is equal to another object.
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
        final AFOSProductId other = (AFOSProductId) obj;
        if (ccc == null) {
            if (other.ccc != null) {
                return false;
            }
        } else if (!ccc.equals(other.ccc)) {
            return false;
        }
        if (nnn == null) {
            if (other.nnn != null) {
                return false;
            }
        } else if (!nnn.equals(other.nnn)) {
            return false;
        }
        if (xxx == null) {
            if (other.xxx != null) {
                return false;
            }
        } else if (!xxx.equals(other.xxx)) {
            return false;
        }
        return true;
    }

    /**
     * Get the string representation of this instance as a StringBuilder.
     * 
     * @param buffer
     *            A StringBuilder reference to receive data. If this reference
     *            is null, a new instance is created.
     * @return The string representation of this instance as a StringBuilder.
     */
    public StringBuilder toStringBuilder(StringBuilder buffer) {
        if (buffer == null) {
            buffer = new StringBuilder();
        }
        buffer.append(ccc);
        buffer.append(nnn);
        buffer.append(xxx);

        return buffer;
    }

    /**
     * Get the string representation of this instance.
     * 
     * @return The string representation of this instance.
     */
    public String toString() {
        return toStringBuilder(null).toString();
    }

    /**
     * Make a copy of an AFOSProductId.
     * @param oldId ProductId to copy.
     * @return The ProductId copy. 
     */
    public static final AFOSProductId copy(AFOSProductId oldId) {
        String ccc = oldId.getCcc();
        String nnn = oldId.getNnn();
        String xxx = oldId.getXxx();
        return new AFOSProductId(ccc,nnn,xxx);
    }
}
