package com.raytheon.uf.edex.datadelivery.retrieval.response;

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


import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;

/**
 * Provider Retrieval Response wrapper -extend this class if you wish
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2011            dhladky     Initial creation
 * 
 * </pre>
 * 
 * /
 * 
 * @author dhladky
 * @version 1.0
 */

public class RetrievalResponse implements IRetrievalResponse {

    private RetrievalAttribute attXML;

    private Object[] payLoad;

    public RetrievalResponse(RetrievalAttribute attXML) {
        this.attXML = attXML;
    }

    public void setPayLoad(Object[] payLoad) {
        this.payLoad = payLoad;
    }

    public Object[] getPayLoad() {
        return payLoad;
    }

    public RetrievalAttribute getAttribute() {
        return attXML;
    }

}
