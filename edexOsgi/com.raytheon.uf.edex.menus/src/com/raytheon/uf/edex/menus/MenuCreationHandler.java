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
package com.raytheon.uf.edex.menus;

import com.raytheon.uf.common.menus.MenuCreationRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Request handler for {@link MenuCreationRequest} objects. Uses
 * {@link MenuCreationRegistry} to find object that will create the menus
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2011            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class MenuCreationHandler implements
        IRequestHandler<MenuCreationRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MenuCreationHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(MenuCreationRequest request) throws Exception {
        statusHandler.info("Creating menus for " + request.getSite());
        MenuCreationRegistry registry = MenuCreationRegistry.getInstance();

        long t0 = System.currentTimeMillis();
        for (String item : registry.getRegisteredObjects()) {
            try {
                registry.getRegisteredObject(item).createMenus(
                        request.getSite());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        statusHandler.info("Time to finish creating menus for "
                + request.getSite() + " : " + (System.currentTimeMillis() - t0)
                + "ms");
        return true;
    }
}
