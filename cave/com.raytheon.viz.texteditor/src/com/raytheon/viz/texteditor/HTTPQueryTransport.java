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
package com.raytheon.viz.texteditor;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.dataplugin.text.dbsrv.IQueryTransport;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2008            jkorman     Initial creation
 * May 15, 2014 2536       bclement    better error handling for executeQuery()
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class HTTPQueryTransport implements IQueryTransport {

    private final String serviceURL;

    private static final IUFStatusHandler log = UFStatus
            .getHandler(HTTPQueryTransport.class);

    /**
     * 
     * @param urlBase
     * @param serviceName
     */
    public HTTPQueryTransport(String urlBase, String serviceName) {
        if(urlBase != null) {
            if (urlBase.endsWith("/")) {
                serviceURL = urlBase + serviceName;
            } else {
                serviceURL = urlBase + "/" + serviceName;
            }
        } else {
            throw new NullPointerException("urlBase address is null");
        }
    }

    /**
     * 
     * 
     * @see com.raytheon.uf.common.dataplugin.text.dbsrv.IQueryTransport#executeQuery(com.raytheon.uf.common.message.Message)
     */
    @Override
    public Message executeQuery(Message message) {

        HttpClient client = HttpClient.getInstance();

        try {
            String xml = SerializationUtil.marshalToXml(message);

            String response = client.post(serviceURL, xml);

            if(response != null) {
                message = SerializationUtil.unmarshalFromXml(Message.class,
                        response);
            }
        } catch (Exception e) {
            log.error("Error executing HTTP text query", e);
        }

        return message;
    }

}
