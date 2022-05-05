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
package com.raytheon.uf.edex.plugin.text.dbsrv.handler;

import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Iterator;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.text.RemoteRetrievalResponse;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.RemoteRetrievalRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.plugin.text.db.TextDB;
import com.raytheon.uf.edex.requestsrv.router.RemoteServerRequestRouter;

/**
 * Request handler for RemoteRetrievalRequests.
 *
 * If request is addressed to my host name it will be processed, if not it will
 * be routed to the specifed server
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * ???                              Initial creation
 * May 20, 2014  2536     bclement  moved from edex.textdb to edex.plugin.text
 * Jan 18, 2016  4562     tjensen   Moved from edex.plugin.text to
 *                                  edex.plugin.text.dbsrv
 * Aug 28, 2018  7134     randerso  Reworked handler for new remote request
 *                                  implementation
 *
 * </pre>
 *
 */
public class RemoteRetrievalHandler
        implements IRequestHandler<RemoteRetrievalRequest> {
    private static final String PRODUCT_REQUEST_STATUS = "Product Request Status";

    private Logger logger = LoggerFactory
            .getLogger(RemoteRetrievalHandler.class);

    @Override
    public RemoteRetrievalResponse handleRequest(RemoteRetrievalRequest request)
            throws Exception {
        RemoteRetrievalResponse response = null;

        /*
         * if the addressee is my host name, process the request
         */
        String myHostName = SystemUtil.getHostName();
        if (myHostName.equals(request.getAddressee())) {
            // handle request for local site
            TextDB textdb = new TextDB();
            List<StdTextProduct> productList = textdb.executeAFOSCommand(
                    request.getAfosID(), EDEXUtil.getEdexSite(), true, false,
                    null);

            // filter out products older than requested time
            Iterator<StdTextProduct> iter = productList.iterator();
            while (iter.hasNext()) {
                StdTextProduct product = iter.next();
                if (product.getRefTime() <= request.getMostRecentTime()) {
                    iter.remove();
                }
            }

            StringBuilder msg = new StringBuilder();
            msg.append("The request for ");
            msg.append(request.getAfosID());

            if (productList.isEmpty()) {
                msg.append(" was not satisfied.\n");
                msg.append(
                        "No newer versions of this product were found in the remote server's database.");
            } else {
                msg.append(" was successful.");
            }
            response = new RemoteRetrievalResponse(!productList.isEmpty(),
                    PRODUCT_REQUEST_STATUS, msg.toString());
            response.setProductList(productList);

            logger.info(
                    "Remote textdb request processed:\n    " + msg.toString());

        }

        /*
         * addressee is not my host name, route request to remote server
         */
        else {
            RemoteServerRequestRouter router = new RemoteServerRequestRouter(
                    remoteURL(request.getAddressee()));

            try {
                logger.info("Routing remote textdb request for "
                        + request.getAfosID() + " to "
                        + request.getAddressee());
                Object resp = router.route(request);

                if (resp instanceof RemoteRetrievalResponse) {
                    response = (RemoteRetrievalResponse) resp;

                    if (response.isOk()
                            && !response.getProductList().isEmpty()) {
                        TextDB textDB = new TextDB(true);
                        for (StdTextProduct product : response
                                .getProductList()) {
                            textDB.writeProduct(product);
                        }
                    }
                } else if (resp != null) {
                    response = new RemoteRetrievalResponse(false,
                            PRODUCT_REQUEST_STATUS,
                            "Unexpected response of type "
                                    + resp.getClass().getName()
                                    + " received from " + request.getAddressee()
                                    + "\n" + resp.toString());
                } else {
                    response = new RemoteRetrievalResponse(false,
                            PRODUCT_REQUEST_STATUS,
                            "Null response received from "
                                    + request.getAddressee());
                }
            } catch (RemoteException e) {
                logger.error(e.getLocalizedMessage(), e);

                response = new RemoteRetrievalResponse(false,
                        PRODUCT_REQUEST_STATUS,
                        "Unable to communicate with remote server "
                                + request.getAddressee());
            }
            logger.info("Remote request response received from "
                    + request.getAddressee() + ":\n    "
                    + response.getStatusMessage());
        }
        return response;
    }

    private String remoteURL(String addressee) {
        String remoteUrl;
        try {
            URL localUrl = new URL(System.getenv("HTTP_SERVER"));
            remoteUrl = new URL(localUrl.getProtocol(), addressee,
                    localUrl.getPort(), localUrl.getFile()).toString();

        } catch (MalformedURLException e) {
            logger.error("Error parsing local HTTP server URL", e);
            remoteUrl = "http://" + addressee + ":9581/services";
        }

        return remoteUrl;
    }
}
