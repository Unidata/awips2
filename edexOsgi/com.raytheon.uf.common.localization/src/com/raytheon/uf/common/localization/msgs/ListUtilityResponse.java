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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Defines the response to a list command
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
public class ListUtilityResponse extends AbstractUtilityResponse {

    @XmlElement
    @DynamicSerializeElement
    protected ListResponseEntry[] entries;

    /**
     * Constructor
     */
    public ListUtilityResponse() {

    }
    
    public ListUtilityResponse(ListResponseEntry[] entries) {
        this.entries = entries;
    }

    /**
     * Constructor
     * 
     * @param context
     * @param entries
     */
    public ListUtilityResponse(LocalizationContext context, String subPath,
            String errorMessage, ListResponseEntry[] entries) {
        super(context, subPath, errorMessage);
        this.entries = entries;
    }

    /**
     * @return the entries
     */
    public ListResponseEntry[] getEntries() {
        return entries;
    }

    /**
     * @param entries
     *            the entries to set
     */
    public void setEntries(ListResponseEntry[] entries) {
        this.entries = entries;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.msg.utility.AbstractUtilityResponse#getFormattedErrorMessage()
     */
    @Override
    public String getFormattedErrorMessage() {
        return "Error retrieving file listing for " + getContextRelativePath()
                + ": " + getErrorText();
    }

}
