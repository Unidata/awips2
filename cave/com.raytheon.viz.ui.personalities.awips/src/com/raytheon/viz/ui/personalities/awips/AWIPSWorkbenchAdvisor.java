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
package com.raytheon.viz.ui.personalities.awips;

import com.raytheon.uf.common.menus.MenuCreationRequest;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * AWIPS {@link VizWorkbenchAdvisor} that reqeusts menu creation service to run
 * before discovering dynamic menus
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AWIPSWorkbenchAdvisor extends VizWorkbenchAdvisor {

    @Override
    protected void createDynamicMenus() {
        // create the request to send to EDEX to generate the menus
        MenuCreationRequest request = new MenuCreationRequest();
        request.setSite(LocalizationManager.getInstance().getSite());
        try {
            ThriftClient.sendRequest(request);
        } catch (VizException e) {
            UFStatus.getHandler(AWIPSWorkbenchAdvisor.class).handle(
                    Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        super.createDynamicMenus();
    }

}
