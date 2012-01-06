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
package com.raytheon.edex.transform.shef;

/**
 * Expose the JMX service methods for the Shef transformers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2008       1659 jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface ShefTransformerInterface {

    /**
     * 
     * @return
     */
    public String getServiceName();

    /**
     * Get a count of messages processed since startup or the last reset.
     * 
     * @return Message count.
     */
    public int getMessageCount();

    /**
     * Reset the message count to zero.
     */
    public void clearMessageCount();

    /**
     * Get the text of the last message that was encoded.
     * 
     * @return The text of the last message that was encoded.
     */
    public String getLastMessage();

}
