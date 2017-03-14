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
package com.raytheon.viz.xdat;

import java.util.ArrayList;

/**
 * XdatMessageDecoder contains an array of decoded messages.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 9, 2009            wkwock2     Initial creation
 * May 3, 2011  #9194      lvenable   Complete refactor.
 * 
 * </pre>
 * 
 * @author wkwock2
 * @version 1.0
 */

public class XdatMessageDecoder {

    /**
     * Array of decoded messages.
     */
    private ArrayList<DecodedMessage> decodedMsgs = new ArrayList<DecodedMessage>();

    /**
     * Constructor
     * 
     * @param xdatMessages
     *            Array of data lines.
     */
    public XdatMessageDecoder(ArrayList<String> xdatMessages) {
        decodeMessage(xdatMessages);
    }

    /**
     * Decode each data line. If the data line is invalid then don't add it to
     * the decoded message array.
     * 
     * @param xdatMessages
     *            Array of data lines.
     */
    private void decodeMessage(ArrayList<String> xdatMessages) {

        DecodedMessage decodedMsg;

        for (String str : xdatMessages) {
            decodedMsg = new DecodedMessage(str);
            if (decodedMsg.isValidMessage() == true) {
                decodedMsgs.add(decodedMsg);
            }
        }
    }

    /**
     * Get the decoded messages.
     * 
     * @return Array of decoded messages.
     */
    public ArrayList<DecodedMessage> getDecodedMessages() {
        return decodedMsgs;
    }

    /**
     * Check if there are decoded messages.
     * 
     * @return True if there are decoded messages.
     */
    public boolean hasDecodedMessages() {
        return !decodedMsgs.isEmpty();
    }
}
