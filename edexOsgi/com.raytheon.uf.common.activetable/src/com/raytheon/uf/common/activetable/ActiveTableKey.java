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
package com.raytheon.uf.common.activetable;

import javax.persistence.Column;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Primary key for ActiveTableRecord
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/22/2015   4522       randerso    Create proper primary key for ActiveTableRecord
 * 03/17/2016   5426       randerso    Add issueYear to primary key
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@DynamicSerialize
public class ActiveTableKey extends PersistableDataObject {

    private static final long serialVersionUID = 1L;

    @Column(length = 4)
    @DynamicSerializeElement
    protected String officeid;

    @Column(length = 2)
    @DynamicSerializeElement
    protected String phen;

    @Column(length = 1)
    @DynamicSerializeElement
    protected String sig;

    @DataURI(position = 5)
    @Column(length = 4)
    @DynamicSerializeElement
    protected String etn;

    @Column(length = 6)
    @DynamicSerializeElement
    protected String ugcZone;

    @Column
    @DynamicSerializeElement
    protected int issueYear;

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result) + ((etn == null) ? 0 : etn.hashCode());
        result = (prime * result) + issueYear;
        result = (prime * result)
                + ((officeid == null) ? 0 : officeid.hashCode());
        result = (prime * result) + ((phen == null) ? 0 : phen.hashCode());
        result = (prime * result) + ((sig == null) ? 0 : sig.hashCode());
        result = (prime * result)
                + ((ugcZone == null) ? 0 : ugcZone.hashCode());
        return result;
    }

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
        ActiveTableKey other = (ActiveTableKey) obj;
        if (etn == null) {
            if (other.etn != null) {
                return false;
            }
        } else if (!etn.equals(other.etn)) {
            return false;
        }
        if (issueYear != other.issueYear) {
            return false;
        }
        if (officeid == null) {
            if (other.officeid != null) {
                return false;
            }
        } else if (!officeid.equals(other.officeid)) {
            return false;
        }
        if (phen == null) {
            if (other.phen != null) {
                return false;
            }
        } else if (!phen.equals(other.phen)) {
            return false;
        }
        if (sig == null) {
            if (other.sig != null) {
                return false;
            }
        } else if (!sig.equals(other.sig)) {
            return false;
        }
        if (ugcZone == null) {
            if (other.ugcZone != null) {
                return false;
            }
        } else if (!ugcZone.equals(other.ugcZone)) {
            return false;
        }
        return true;
    }

    /**
     * @return the officeid
     */
    public String getOfficeid() {
        return officeid;
    }

    /**
     * @param officeid
     *            the officeid to set
     */
    public void setOfficeid(String officeid) {
        this.officeid = officeid;
    }

    /**
     * @return the phen
     */
    public String getPhen() {
        return phen;
    }

    /**
     * @param phen
     *            the phen to set
     */
    public void setPhen(String phen) {
        this.phen = phen;
    }

    /**
     * @return the sig
     */
    public String getSig() {
        return sig;
    }

    /**
     * @param sig
     *            the sig to set
     */
    public void setSig(String sig) {
        this.sig = sig;
    }

    /**
     * @return the etn
     */
    public String getEtn() {
        return etn;
    }

    /**
     * @param etn
     *            the etn to set
     */
    public void setEtn(String etn) {
        this.etn = etn;
    }

    /**
     * @return the ugcZone
     */
    public String getUgcZone() {
        return ugcZone;
    }

    /**
     * @param ugcZone
     *            the ugcZone to set
     */
    public void setUgcZone(String ugcZone) {
        this.ugcZone = ugcZone;
    }

    /**
     * @return the issueYear
     */
    public int getIssueYear() {
        return issueYear;
    }

    /**
     * @param issueYear
     *            the issueYear to set
     */
    public void setIssueYear(int issueYear) {
        this.issueYear = issueYear;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(getOfficeid()).append(".").append(getPhen()).append(".")
                .append(getSig()).append(".").append(getEtn()).append(" ")
                .append(getUgcZone());
        return sb.toString();
    }
}