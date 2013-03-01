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
package com.raytheon.uf.edex.activetable;

import com.raytheon.uf.common.activetable.GetVtecAttributeRequest;
import com.raytheon.uf.common.activetable.GetVtecAttributeResponse;
import com.raytheon.uf.common.activetable.VTECPartners;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Handles a request for an attribute from the VTECPartners file for a
 * particular site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 2, 2010            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class GetVtecAttributeHandler implements
        IRequestHandler<GetVtecAttributeRequest> {

    @Override
    public GetVtecAttributeResponse handleRequest(
            GetVtecAttributeRequest request) throws Exception {
        VTECPartners vtecPartners = VTECPartners.getInstance(request
                .getSiteId());
        Object obj = vtecPartners.getattr(request.getAttribute(),
                request.getDefaultValue());
        GetVtecAttributeResponse response = new GetVtecAttributeResponse();
        response.setValue(obj);
        return response;
    }

}
