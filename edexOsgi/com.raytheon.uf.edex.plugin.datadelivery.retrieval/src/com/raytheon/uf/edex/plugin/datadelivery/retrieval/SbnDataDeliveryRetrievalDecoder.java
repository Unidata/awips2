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
package com.raytheon.uf.edex.plugin.datadelivery.retrieval;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.retrieval.util.RetrievalGeneratorUtilities;

/**
 * Decodes data delivery retrievals from the SBN feed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2013 1648       djohnson     Initial creation
 * Jan 30, 2014 2686       dhladky      refactor of retrieval.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SbnDataDeliveryRetrievalDecoder extends AbstractDecoder {
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SbnDataDeliveryRetrievalDecoder.class);

    private String destinationUri;
    
    private Network network = Network.SBN;
    
    public SbnDataDeliveryRetrievalDecoder(String destinationUri) {
        this.destinationUri = destinationUri;
    }

    /**
     * Process the data.
     * 
     * @param data
     *            the data
     * @param headers
     *            the headers
     */
    public void process(byte[] data, Headers headers) {
        // drops to common retrieval queue for processing/persistence
        String xml = new String(data);
        try {
            Object[] payload = new Object[]{xml};
            RetrievalGeneratorUtilities.sendToRetrieval(destinationUri, network, payload);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Couldn't send SBN data to Retrieval Queue", e);
        }
    }

}
