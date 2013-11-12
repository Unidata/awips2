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
package com.raytheon.uf.edex.wmo.message;

import java.util.Arrays;

import com.raytheon.edex.esb.Headers;

/**
 * WMOMessage that handles XML. Specifically, removes data trailing the ending
 * XML tag.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2013  2506       bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class XmlWMOMessage extends WMOMessage {

    /**
     * Default Constructor.
     */
    public XmlWMOMessage() {
    }

    /**
     * @param wmoMessage
     * @param headers
     */
    public XmlWMOMessage(String wmoMessage, Headers headers) {
        super(wmoMessage, headers);
    }

    /**
     * @param wmoMessage
     * @param headers
     */
    public XmlWMOMessage(byte[] wmoMessage, Headers headers) {
        super(wmoMessage, headers);
    }

    /**
     * Set the binary data that comprises the body of this message.
     * 
     * @return The binary data.
     */
    public void setMessageBody(byte[] binaryData) {
        super.setMessageBody(getMessageBodyXML(binaryData));
    }

    private byte[] getMessageBodyXML(byte[] messageBody) {
        // assumes last '>' is a part of the last xml tag
        final char LAST_XML_CHARACTER = '>';
        int lastIndex = messageBody.length;

        for (int i = messageBody.length - 1; i > -1; --i) {
            if (messageBody[i] == LAST_XML_CHARACTER) {
                lastIndex = i + 1;
                break;
            }
        }
        return Arrays.copyOf(messageBody, lastIndex);
    }

}
