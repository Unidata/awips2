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
package com.raytheon.uf.common.message.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response message for holding any serializable object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 12, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ResponseMessageGeneric extends AbstractResponseMessage {

    /**
     * the response contents
     */
    @DynamicSerializeElement
    @XmlElement
    private Object contents = null;

    /**
     * No arg constructor.
     */
    public ResponseMessageGeneric() {
        // intentionally empty
    }

    /**
     * Private constructor. The message will contain the text from the contents.
     * 
     * @param contents
     *            the contents of the message.
     */
    public ResponseMessageGeneric(Object contents) {
        this.fileType = "text/xml";
        this.dataURI = "";
        this.validTime = new Date();
        this.contents = contents;
    }

    /**
     * Static method that creates the Response Message. Uses the private
     * constructor to create the message object.
     * 
     * @param contents
     *            the text contents of the message
     * 
     * @return the response object
     */
    public static ResponseMessageGeneric generateResponse(Object contents) {
        return new ResponseMessageGeneric(contents);
    }

    /**
     * @return the contents
     */
    public Object getContents() {
        return contents;
    }

    /**
     * @param contents
     *            the contents to set
     */
    public void setContents(Object contents) {
        this.contents = contents;
    }

    public static List<ResponseMessageGeneric> wrap(List<Object> objects) {
        List<ResponseMessageGeneric> retVal = new ArrayList<ResponseMessageGeneric>();
        for (Object o : objects) {
            retVal.add(new ResponseMessageGeneric(o));
        }
        return retVal;
    }

}
