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

/**
 * 
 */
package com.raytheon.edex.msg;

import java.net.URI;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.message.response.AbstractResponseMessage;

/**
 * A response message for URI arrays
 * 
 * This response message contains an array of URI objects. A URI response
 * contains only the locations of weather products, instead of transmitting
 * base64-encoded data.
 * 
 * SOFTWARE HISTORY
 * 
 * Date Ticket# Engineer Description ------------ ---------- -----------
 * -------------------------- 06/14/06 pheaberl Initial creation.
 * 
 * </pre>
 * 
 * @author pheaberl
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ResponseMessageURI extends AbstractResponseMessage {

    private static final long serialVersionUID = 1L;

    @XmlElement
    private URI[] productURI;

    /**
     * Retrieve an array of URIs
     * 
     * @return An array
     */
    public URI[] getProductURI() {
        return productURI;
    }

    /**
     * Sets an array of URIs
     * 
     * @param productURI
     *            An array of URIs
     */
    public void setProductURI(URI[] productURI) {
        this.productURI = productURI;
    }

}
