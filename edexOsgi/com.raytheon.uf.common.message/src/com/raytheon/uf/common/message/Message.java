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

package com.raytheon.uf.common.message;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents the message wrapper for EDEX responses
 * 
 * SOFTWARE HISTORY
 * 
 * Date Ticket# Engineer Description ------------ ---------- -----------
 * -------------------------- 08-06-2006 chammack Initial creation. 27Apr2007
 * 208 MW Fegan Added body text.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Message implements ISerializableObject {

    private String correlationID;

    @XmlElement
    @DynamicSerializeElement
    private Header header;

    @XmlElement
    @DynamicSerializeElement
    private Body body;

    private String text;

    /**
     * @return the body
     */
    public Body getBody() {
        return body;
    }

    /**
     * @param body
     *            the body to set
     */
    public void setBody(Body body) {
        this.body = body;
    }

    /**
     * @return the header
     */
    public Header getHeader() {
        return header;
    }

    /**
     * @param header
     *            the header to set
     */
    public void setHeader(Header header) {
        this.header = header;
    }

    /**
     * @return the correlationID
     */
    public String getCorrelationID() {
        return correlationID;
    }

    /**
     * @param correlationID
     *            the correlationID to set
     */
    public void setCorrelationID(String correlationID) {
        this.correlationID = correlationID;
    }

    /**
     * @return the text
     */
    public String getText() {
        return text;
    }

    /**
     * @param text
     *            the text to set
     */
    public void setText(String text) {
        this.text = text;
    }
}
