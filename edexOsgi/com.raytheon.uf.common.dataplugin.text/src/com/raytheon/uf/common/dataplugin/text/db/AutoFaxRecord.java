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
package com.raytheon.uf.common.dataplugin.text.db;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2010            bfarmer     Initial creation
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */
@Entity
@DynamicSerialize
public class AutoFaxRecord implements ISerializableObject, Serializable {
    private static final long serialVersionUID = 8039777445522207743L;

    @Id
    @DynamicSerializeElement
    protected AutoFaxRecordPK id;

    @Column(nullable = false, length = 256)
    @DynamicSerializeElement
    protected String phoneNumber;

    @Column(nullable = false, length = 256)
    @DynamicSerializeElement
    protected String recipient;

    @Column(nullable = false, length = 256)
    @DynamicSerializeElement
    protected String company;

    public AutoFaxRecord() {

    }

    public AutoFaxRecord(String afosPil, String faxNumber, String phoneNumber,
            String recipient, String company) {
        this.id = new AutoFaxRecordPK(afosPil, faxNumber);
        this.phoneNumber = phoneNumber;
        this.recipient = recipient;
        this.company = company;
    }

    /**
     * @return the id
     */
    public AutoFaxRecordPK getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(AutoFaxRecordPK id) {
        this.id = id;
    }

    /**
     * @return the phoneNumber
     */
    public String getPhoneNumber() {
        return phoneNumber;
    }

    /**
     * @param phoneNumber
     *            the phoneNumber to set
     */
    public void setPhoneNumber(String phoneNumber) {
        this.phoneNumber = phoneNumber;
    }

    /**
     * @return the recipient
     */
    public String getRecipient() {
        return recipient;
    }

    /**
     * @param recipient
     *            the recipient to set
     */
    public void setRecipient(String recipient) {
        this.recipient = recipient;
    }

    /**
     * @return the company
     */
    public String getCompany() {
        return company;
    }

    /**
     * @param company
     *            the company to set
     */
    public void setCompany(String company) {
        this.company = company;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AutoFaxRecord) {
            AutoFaxRecord temp = (AutoFaxRecord) obj;
            if (temp.id.getAfosPil().equals(this.id.getAfosPil())
                    && temp.id.getFaxNumber().equals(this.id.getFaxNumber())) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
}
