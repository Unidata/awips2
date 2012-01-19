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

package com.raytheon.uf.common.localization.msgs;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Defines the response to a get command
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2007            chammack    Initial Creation.	
 * May 19, 2007     #1127   randerso    Implemented error reporting
 * Aug 22, 2008  #1502     bclement    Added JAXB/Serializable annotations
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GetUtilityResponse extends AbstractUtilityResponse {

    @XmlElement
    @DynamicSerializeElement
    protected byte[] data;

    @XmlAttribute
    @DynamicSerializeElement
    protected String checksum;

    /**
     * Constructor
     */
    public GetUtilityResponse() {

    }

    /**
     * Constructor
     * 
     * @param context
     * @param fileName
     * @param data
     */
    public GetUtilityResponse(LocalizationContext context, String errorMessage,
            String fileName, byte[] data, String checksum) {
        super(context, fileName, errorMessage);
        this.data = data;
        this.checksum = checksum;
    }

    /**
     * @return the data
     */
    public byte[] getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(byte[] data) {
        this.data = data;
    }

    /**
     * @return the checksum
     */
    public String getChecksum() {
        return checksum;
    }

    /**
     * @param checksum
     *            the checksum to set
     */
    public void setChecksum(String checksum) {
        this.checksum = checksum;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.msg.utility.AbstractUtilityResponse#getFormattedErrorMessage()
     */
    @Override
    public String getFormattedErrorMessage() {
        return "Error retrieving " + getContextRelativePath() + ": "
                + getErrorText();
    }

}
