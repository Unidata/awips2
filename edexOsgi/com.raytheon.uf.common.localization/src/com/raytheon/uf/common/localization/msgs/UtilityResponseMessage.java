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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Defines the localization response
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2007            chammack    Initial Creation.	
 * Aug 22, 2008  #1502     bclement    Added JAXB/Serializable annotations
 * Oct 01, 2013  2361      njensen     Removed XML annotations
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@DynamicSerialize
public class UtilityResponseMessage {

    @DynamicSerializeElement
    protected AbstractUtilityResponse[] responses;

    /**
     * Constructor
     */
    public UtilityResponseMessage() {

    }

    /**
     * Constructor
     * 
     * @param responses
     */
    public UtilityResponseMessage(AbstractUtilityResponse[] responses) {
        this.responses = responses;
    }

    /**
     * @return the responses
     */
    public AbstractUtilityResponse[] getResponses() {
        return responses;
    }

    /**
     * @param responses
     *            the responses to set
     */
    public void setResponses(AbstractUtilityResponse[] responses) {
        this.responses = responses;
    }

}
