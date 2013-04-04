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
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Defines the delete localization response
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2007            njensen     Initial creation
 * May 19, 2007     #1127   randerso    Implemented error reporting
 * Aug 22, 2008  #1502     bclement    Added JAXB/Serializable annotations
 * 
 * </pre>
 * 
 * @author njensen
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DeleteUtilityResponse extends AbstractUtilityResponse {

    @XmlAttribute
    @DynamicSerializeElement
    private long timeStamp;

    public DeleteUtilityResponse() {

    }

    public DeleteUtilityResponse(LocalizationContext context, String errorText,
            String fileName, long timeStamp) {
        super(context, fileName, errorText);
        this.timeStamp = timeStamp;
    }

    /**
     * @return the timeStamp
     */
    public long getTimeStamp() {
        return timeStamp;
    }

    /**
     * @param timeStamp
     *            the timeStamp to set
     */
    public void setTimeStamp(long timeStamp) {
        this.timeStamp = timeStamp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.msg.utility.AbstractUtilityResponse#
     * getFormattedErrorMessage()
     */
    @Override
    public String getFormattedErrorMessage() {
        return "Error deleting " + getContextRelativePath() + ": "
                + getErrorText();
    }
}
