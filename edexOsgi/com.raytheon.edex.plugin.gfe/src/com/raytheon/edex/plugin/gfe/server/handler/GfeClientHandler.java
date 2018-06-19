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
package com.raytheon.edex.plugin.gfe.server.handler;

import com.raytheon.edex.plugin.gfe.server.GfeClientController;
import com.raytheon.uf.common.dataplugin.gfe.request.GfeClientRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Request handler for GfeClientRequest
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 25, 2017  6092      randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class GfeClientHandler extends BaseGfeRequestHandler
        implements IRequestHandler<GfeClientRequest> {

    private GfeClientController controller;

    /**
     * Constructor
     *
     * @param controller
     *            the GfeClientController instance to be used to process
     *            requests
     */
    public GfeClientHandler(GfeClientController controller) {
        super();
        this.controller = controller;
    }

    @Override
    public Object handleRequest(GfeClientRequest request) throws Exception {
        // Verify IFP server is active for the siteID in the request
        getIfpServer(request);

        this.controller.processRequest(request);
        return null;
    }

}
