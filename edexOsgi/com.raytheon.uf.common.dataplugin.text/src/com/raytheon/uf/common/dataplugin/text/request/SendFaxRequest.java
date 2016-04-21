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
 * Oct 21, 2010            bfarmer     Initial creation
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */
@DynamicSerialize
public class SendFaxRequest implements IServerRequest {
    @DynamicSerializeElement
    private String faxNumber;

    @DynamicSerializeElement
    private String faxTitle;

    @DynamicSerializeElement
    private String faxCompany;

    @DynamicSerializeElement
    private String faxText;

    @DynamicSerializeElement
    private String faxRecipient;

    /**
     * @return the faxNumber
     */
    public String getFaxNumber() {
        return faxNumber;
    }

    public SendFaxRequest () {
        
    }
    public SendFaxRequest(String faxNumber, String faxTitle, String faxCompany,
            String faxText, String faxRecipient) {
        super();
        this.faxNumber = faxNumber;
        this.faxTitle = faxTitle;
        this.faxCompany = faxCompany;
        this.faxText = faxText;
        this.faxRecipient = faxRecipient;
    }

    /**
     * @param faxNumber
     *            the faxNumber to set
     */
    public void setFaxNumber(String faxNumber) {
        this.faxNumber = faxNumber;
    }

    /**
     * @return the faxTitle
     */
    public String getFaxTitle() {
        return faxTitle;
    }

    /**
     * @param faxTitle
     *            the faxTitle to set
     */
    public void setFaxTitle(String faxTitle) {
        this.faxTitle = faxTitle;
    }

    /**
     * @return the faxCompany
     */
    public String getFaxCompany() {
        return faxCompany;
    }

    /**
     * @param faxCompany
     *            the faxCompany to set
     */
    public void setFaxCompany(String faxCompany) {
        this.faxCompany = faxCompany;
    }

    /**
     * @return the faxText
     */
    public String getFaxText() {
        return faxText;
    }

    /**
     * @param faxText
     *            the faxText to set
     */
    public void setFaxText(String faxText) {
        this.faxText = faxText;
    }

    /**
     * @return the faxRecipient
     */
    public String getFaxRecipient() {
        return faxRecipient;
    }

    /**
     * @param faxRecipient
     *            the faxRecipient to set
     */
    public void setFaxRecipient(String faxRecipient) {
        this.faxRecipient = faxRecipient;
    }

}
