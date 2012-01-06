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
package com.raytheon.edex.msg;

import com.raytheon.uf.common.message.response.AbstractResponseMessage;

/**
 * Represents a generic XML response message from EDEX.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12Feb2007    TO5         MW Fegan    Initial Creation.
 * 09May2007    TO7         njensen     Changed contents to AbstractDataRecord.
 * 
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */

public class ResponseMessageXML extends AbstractResponseMessage {

    /**
     * the response contents
     */
    // private IMarshallable contents = null;
    /**
     * No arg constructor.
     */
    public ResponseMessageXML() {
        // intentionally empty
    }

    /**
     * Private constructor. The message will contain the text from the contents.
     * 
     * @param contents
     *            the contents of the message.
     */
    // private ResponseMessageXML(IMarshallable contents) {
    // this.fileType = "text/xml";
    // this.dataURI = "";
    // this.validTime = new Date();
    // this.contents = contents;
    // }
    /**
     * Static method that creates the XML Response MEssagte. Uses the private
     * constructor to create the message object.
     * 
     * @param contents
     *            the text contents of the message
     * 
     * @return the response object
     */
    // public static ResponseMessageXML generateXMLResponse(IMarshallable
    // contents) {
    // return new ResponseMessageXML(contents);
    // }
    /**
     * @return the contents
     */
    // public IMarshallable getContents() {
    // return contents;
    // }
    /**
     * @param contents
     *            the contents to set
     */
    // public void setContents(IMarshallable contents) {
    // this.contents = contents;
    // }
}
