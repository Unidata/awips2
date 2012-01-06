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

import java.util.Arrays;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.ui.EditorUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ToggleResourceHandler extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        DataManager dataManager = DataManager.getCurrentInstance();
        if (dataManager == null) {
            return null;
        }

        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();

        String indexStr = event.getParameter("index");
        int index = Integer.parseInt(indexStr);

        IParmManager parmManager = dataManager.getParmManager();
        ISpatialDisplayManager sdm = dataManager.getSpatialDisplayManager();

        Parm[] parms = parmManager.getDisplayedParms();

        Parm parm = null;
        if (index == 0) {
            for (Parm p : parms) {
                VisMode visMode = p.getDisplayAttributes().getVisMode();
                if (visMode.equals(VisMode.IMAGE)) {
                    parm = p;
                    break;
                }
            }
        } else if (index <= parms.length) {
            Arrays.sort(parms);
            parm = parms[parms.length - index];
        }

        if (parm != null) {
            ResourcePair rp = sdm.getResourcePair(parm);
            ResourceProperties props = rp.getProperties();
            sdm.makeVisible(parm, !props.isVisible(), false);
            if (editor != null) {
                editor.refresh();
            }
        }

        return null;
    }
}
