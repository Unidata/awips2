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
import com.raytheon.uf.common.datadelivery.registry.ProviderCredentials;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest.RequestType;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest.Status;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
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
 * Jul 14, 2013  2184      dhladky     Initial creation
 * Aug 23, 2013  2180      mpduff      Implement changes to ProviderCredentialsUtil
 *  
 * @author dhladky
 * @version 1.0
 */

public class ProviderKeyRequestHandler implements
        IRequestHandler<ProviderKeyRequest> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProviderKeyRequestHandler.class);

    /**
     * {@inheritDoc}
     */
    @Override
    public Object handleRequest(ProviderKeyRequest request) throws Exception {
        if (request.getRequestType() == RequestType.SAVE) {
            ProviderCredentials creds = new ProviderCredentials(
                    request.getProvider(), request.getProviderKey());
            creds = ProviderCredentialsUtil.saveCredentials(creds);
            if (creds.getStatus() == Status.SUCCESS) {
                request.setStatus(Status.SUCCESS);
            } else {
                request.setStatus(Status.FAILURE);
                request.setMessage(creds.getMessage());
            }
        } else if (request.getRequestType() == RequestType.RETRIEVE) {
            Provider provider = request.getProvider();
            ProviderCredentials creds = null;
            Connection conn = null;
            try {
                creds = ProviderCredentialsUtil.retrieveCredentials(request
                        .getProvider().getName());

                conn = creds.getConnection();
            } catch (Exception e) {
                String msg = "Error retrieving credentials";
                statusHandler.error(msg, e);
                request.setStatus(Status.FAILURE);
                request.setMessage(msg);
                return request;
            }

            if (creds != null) {
                provider.setConnection(conn);
                request.setProvider(provider);
                request.setStatus(Status.SUCCESS);
            }
        } else if (request.getRequestType() == RequestType.DELETE) {
            ProviderCredentials creds = new ProviderCredentials(
                    request.getProvider(), request.getProviderKey());
            creds = ProviderCredentialsUtil.deleteCredentials(creds);

            request.setStatus(creds.getStatus());
            if (creds.getStatus() == Status.FAILURE) {
                request.setMessage(creds.getMessage());
            }
        }

        return request;
    }
}
