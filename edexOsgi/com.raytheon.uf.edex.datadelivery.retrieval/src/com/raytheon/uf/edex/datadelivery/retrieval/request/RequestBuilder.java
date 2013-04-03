package com.raytheon.uf.edex.datadelivery.retrieval.request;

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
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;

/**
 * Request XML translation related utilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2012            dhladky     Initial creation
 * Aug 12, 2012 1022       djohnson    Add {@link Immutable}.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public abstract class RequestBuilder implements IRetrievalRequestBuilder {

    private final RetrievalAttribute ra;

    protected RequestBuilder(RetrievalAttribute ra) {
        this.ra = ra;
    }

    public RetrievalAttribute getRetrievalAttribute() {
        return ra;
    }
}
