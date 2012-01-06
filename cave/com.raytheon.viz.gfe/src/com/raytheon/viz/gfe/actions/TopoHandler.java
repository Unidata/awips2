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

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Handle the GFE Topography menu item
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 2, 2008		#1160	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TopoHandler extends AbstractHandler implements IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        DataManager dataMgr = DataManager.getCurrentInstance();
        IParmManager parmMgr = dataMgr.getParmManager();
        Parm topoParm = parmMgr.getParm(dataMgr.getTopoManager()
                .getCompositeParmID());
        boolean wanted = topoParm != null;
        wanted = !wanted;
        parmMgr.enableDisableTopoParm(wanted, true);

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.
     * menus.UIElement, java.util.Map)
     */
    @Override
    @SuppressWarnings(value = "unchecked")
    public void updateElement(final UIElement element, Map parameters) {
        DataManager dataMgr = DataManager.getCurrentInstance();
        if (dataMgr != null) {
            IParmManager parmMgr = dataMgr.getParmManager();
            Parm topoParm = parmMgr.getParm(dataMgr.getTopoManager()
                    .getCompositeParmID());
            final boolean checked = topoParm != null;

            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    element.setChecked(checked);
                }
            });
        }
    }
}
