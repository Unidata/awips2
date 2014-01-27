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

import java.util.concurrent.ConcurrentLinkedQueue;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractDecoder;

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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SbnDataDeliveryRetrievalDecoder extends AbstractDecoder {

    private final ConcurrentLinkedQueue<String> sbnRetrievalQueue;

    public SbnDataDeliveryRetrievalDecoder(ConcurrentLinkedQueue<String> queue) {
        this.sbnRetrievalQueue = queue;
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
        this.sbnRetrievalQueue.add(new String(data));
    }

}
