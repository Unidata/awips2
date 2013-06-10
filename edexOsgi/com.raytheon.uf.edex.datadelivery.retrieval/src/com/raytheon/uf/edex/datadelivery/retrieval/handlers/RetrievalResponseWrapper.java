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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;

/**
 * Plugin data objects and the retrieval information they are associated with.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Feb 12, 2013 1543       djohnson     Takes retrieval response rather than plugin data objects.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RetrievalResponseWrapper {

    @XmlAnyElement(lax = true)
    @DynamicSerializeElement
    private IRetrievalResponse retrievalResponse;

    /**
     * Constructor.
     */
    public RetrievalResponseWrapper() {
    }

    /**
     * Constructor.
     * 
     * @param attributeXml
     * @param response
     */
    public RetrievalResponseWrapper(IRetrievalResponse response) {
        this.retrievalResponse = response;
    }

    /**
     * @return the retrievalResponse
     */
    public IRetrievalResponse getRetrievalResponse() {
        return retrievalResponse;
    }

    /**
     * @param retrievalResponse
     *            the retrievalResponse to set
     */
    public void setRetrievalResponse(IRetrievalResponse retrievalResponse) {
        this.retrievalResponse = retrievalResponse;
    }

}
