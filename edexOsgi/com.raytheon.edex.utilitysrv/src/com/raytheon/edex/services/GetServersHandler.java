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
package com.raytheon.edex.services;

import java.util.Collections;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.msgs.GetServersRequest;
import com.raytheon.uf.common.localization.msgs.GetServersResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.util.registry.GenericRegistry;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Handler class for retrieving the http and jms servers from the
 * environment.xml
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2009            mschenke     Initial creation
 * Sep 12, 2012 1167      djohnson     Add datadelivery servers.
 * Jan 14, 2013 1469      bkowal       No longer includes the hdf5 data directory
 *                                     in the response.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GetServersHandler extends GenericRegistry<String, String>
        implements IRequestHandler<GetServersRequest> {

    private static final Log logger = LogFactory.getLog(UtilityManager.class);

    @Override
    public GetServersResponse handleRequest(GetServersRequest request)
            throws Exception {
        GetServersResponse response = new GetServersResponse();
        String httpServer = System.getProperty("http.server");
        String jmsServer = System.getProperty("jms.server");
        String pypiesServer = System.getProperty("pypies.server");

        logger.info("http.server=" + httpServer);
        logger.info("jms.server=" + jmsServer);
        logger.info("pypies.server=" + pypiesServer);
        logger.info("server locations=" + registry);
        ;
        response.setHttpServer(httpServer);
        response.setJmsServer(jmsServer);
        response.setPypiesServer(pypiesServer);
        response.setServerLocations(Collections.unmodifiableMap(this.registry));

        return response;
    }
}
