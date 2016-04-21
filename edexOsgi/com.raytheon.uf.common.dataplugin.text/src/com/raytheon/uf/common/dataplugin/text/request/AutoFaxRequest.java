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
package com.raytheon.uf.common.dataplugin.text.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

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
@DynamicSerialize
public class AutoFaxRequest implements IServerRequest {

    @DynamicSerializeElement
    private String afosPil;

    @DynamicSerializeElement
    private String faxNumber;

    @DynamicSerializeElement
    private String phoneNumber;

    @DynamicSerializeElement
    private String recipient;

    @DynamicSerializeElement
    private String company;

    @DynamicSerializeElement
    private boolean deleteRecord = false;

    /**
     * @return the afosPil
     */
    public String getAfosPil() {
        return afosPil;
    }

    public AutoFaxRequest() {

    }

    public AutoFaxRequest(String afosPil, String faxNumber, String phoneNumber,
            String recipient, String company, boolean deleteRecord) {
        super();
        this.afosPil = afosPil;
        this.faxNumber = faxNumber;
        this.phoneNumber = phoneNumber;
        this.recipient = recipient;
        this.company = company;
        this.deleteRecord = deleteRecord;
    }

    /**
     * @param afosPil
     *            the afosPil to set
     */
    public void setAfosPil(String afosPil) {
        this.afosPil = afosPil;
    }

    /**
     * @return the faxNumber
     */
    public String getFaxNumber() {
        return faxNumber;
    }

    /**
     * @param faxNumber
     *            the faxNumber to set
     */
    public void setFaxNumber(String faxNumber) {
        this.faxNumber = faxNumber;
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

    /**
     * @return the deleteRecord
     */
    public boolean isDeleteRecord() {
        return deleteRecord;
    }

    /**
     * @param deleteRecord
     *            the deleteRecord to set
     */
    public void setDeleteRecord(boolean deleteRecord) {
        this.deleteRecord = deleteRecord;
    }
}
