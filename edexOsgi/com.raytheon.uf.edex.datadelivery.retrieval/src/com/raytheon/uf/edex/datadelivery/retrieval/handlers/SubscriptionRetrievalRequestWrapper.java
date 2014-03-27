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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Wrapper for objects placed on common retrieval queue
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2014            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SubscriptionRetrievalRequestWrapper {

    @XmlElements({ @XmlElement(name="retrievalRequestWrapper") })
    @DynamicSerializeElement
    private List<RetrievalRequestWrapper> retrievalRequestWrappers;
    
    @XmlAttribute
    @DynamicSerializeElement
    private Network network;

    /**
     * Constructor.
     */
    public SubscriptionRetrievalRequestWrapper() {
        
    }

    /**
     * Constructor
     * 
     * @param network
     * @param retrievalRequestWrappers
     */
    public SubscriptionRetrievalRequestWrapper(Network network, List<RetrievalRequestWrapper> retrievalRequestWrappers) {
        this.setNetwork(network);
        this.retrievalRequestWrappers = retrievalRequestWrappers;
    }

    public Network getNetwork() {
        return network;
    }

    public void setNetwork(Network network) {
        this.network = network;
    }
    
    public void setRetrievalRequestWrappers(List<RetrievalRequestWrapper> retrievalRequestWrappers) {
        this.retrievalRequestWrappers = retrievalRequestWrappers;
    }
    
    public List<RetrievalRequestWrapper> getRetrievalRequestWrappers() {
        return retrievalRequestWrappers;
    }

}
