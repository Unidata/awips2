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
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
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
 * Feb 12, 2013 1543       djohnson    Abstract class now.
 * 
 * </pre>
 * 
 * /
 * 
 * @author dhladky
 * @version 1.0
 */
@DynamicSerialize
public abstract class RetrievalResponse implements IRetrievalResponse {

    @DynamicSerializeElement
    private RetrievalAttribute attribute;

    @DynamicSerializeElement
    private Object payLoad;

    public RetrievalResponse() {

    }

    public RetrievalResponse(RetrievalAttribute attribute) {
        this.attribute = attribute;
    }

    @Override
    public void setPayLoad(Object payLoad) {
        this.payLoad = payLoad;
    }

    @Override
    public Object getPayLoad() {
        return payLoad;
    }

    @Override
    public RetrievalAttribute getAttribute() {
        return attribute;
    }

    public void setAttribute(RetrievalAttribute attribute) {
        this.attribute = attribute;
    }

}
