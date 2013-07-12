package com.raytheon.uf.edex.datadelivery.service.services;
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
import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest.RequestType;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest.Status;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.datadelivery.retrieval.util.ProviderCredentialsUtil;


/**
 * A ProviderKeyRequestHandler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 14, 2013 2184      dhladky     Initial creation
 *  
 * @author dhladky
 * @version 1.0
 */

public class ProviderKeyRequestHandler implements IRequestHandler<ProviderKeyRequest> {

    @Override
    public Object handleRequest(ProviderKeyRequest request) throws Exception {
                
        if (request.getRequestType() == RequestType.SAVE) {
            
            boolean status = ProviderCredentialsUtil.saveCredentials(request.getProviderKey(),
                    request.getProvider());
            if (status) {
                request.setStatus(Status.SUCCESS);
            } else {
                request.setStatus(Status.FAILURE);
            }
            
        } else if (request.getRequestType() == RequestType.RETRIEVE) {

            Provider provider = request.getProvider();
            Connection conn = ProviderCredentialsUtil
                    .retrieveCredentials(request.getProvider().getName());
            if (conn != null) {
                provider.setConnection(conn);
                request.setProvider(provider);
                request.setStatus(Status.SUCCESS);
            } else {
                request.setStatus(Status.FAILURE);
            }
        }

        return request;
    }
}
