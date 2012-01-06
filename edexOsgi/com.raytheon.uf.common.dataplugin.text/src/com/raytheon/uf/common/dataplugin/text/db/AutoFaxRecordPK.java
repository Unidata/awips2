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
import javax.persistence.Embeddable;

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
@Embeddable
@DynamicSerialize
public class AutoFaxRecordPK implements ISerializableObject, Serializable {
    private static final long serialVersionUID = -4108711982214631325L;

    @Column(nullable = false, length = 256)
    @DynamicSerializeElement
    private String afosPil;

    @Column(nullable = false, length = 256)
    @DynamicSerializeElement
    private String faxNumber;

    public AutoFaxRecordPK() {

    }

    public AutoFaxRecordPK(String afosPil, String faxNumber) {
        this.afosPil = afosPil;
        this.faxNumber = faxNumber;
    }

    /**
     * @return the afosPil
     */
    public String getAfosPil() {
        return afosPil;
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

}
