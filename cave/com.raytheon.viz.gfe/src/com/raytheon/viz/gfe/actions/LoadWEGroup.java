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
package com.raytheon.viz.gfe.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.IWEGroupManager;

/**
 * 
 * Sets the current Weather Element Group
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 9, 2008				chammack	Initial creation
 * Apr 9, 2009  1288        rjpeter     Removed explicit refresh of SpatialDisplayManager.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class LoadWEGroup extends AbstractHandler {
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

        String name = event.getParameter("name");

        if (name == null)
            return null;

        IWEGroupManager weGroupMgr = DataManager.getCurrentInstance()
                .getWEGroupManager();

        IParmManager parmMgr = DataManager.getCurrentInstance()
                .getParmManager();
        ParmID[] parms = parmMgr.getAllAvailableParms();

        ParmID[] pidsToLoad = weGroupMgr.getParmIDs(name, parms);

        parmMgr.setDisplayedParms(pidsToLoad);

        return null;
    }
}
