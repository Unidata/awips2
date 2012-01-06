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
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.vividsolutions.jts.geom.MultiPolygon;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 30, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class QuickSetHandler extends AbstractHandler implements IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String button = event.getParameter("button");

        try {
            DataManager.getCurrentInstance().getRefManager().handleQuickSet(
                    Integer.parseInt(button));
        } catch (NumberFormatException e) {
            Activator.getDefault().getLog().log(
                    new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                            "Invalid quickset button: " + button));
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.menus.UIElement,
     *      java.util.Map)
     */
    @Override
    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
        DataManager dm = DataManager.getCurrentInstance();
        if (dm == null) {
            return;
        }

        String text = "Empty";
        String button = (String) parameters.get("button");
        try {
            IReferenceSetManager refMgr = dm.getRefManager();
            ReferenceData refData = refMgr
                    .getQuickSet(Integer.parseInt(button));
            if (refData.isQuery()) {
                text = refData.getId().getName() + ": {" + refData.getQuery()
                        + "}";
            } else {
                MultiPolygon polygons = refData
                        .getPolygons(CoordinateType.GRID);
                if (polygons != null && !polygons.isEmpty()) {
                    text = refData.getId().getName() + ": Polygons";
                }
            }
        } catch (NumberFormatException e) {
            Activator.getDefault().getLog().log(
                    new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                            "Invalid quickset button: " + button));
        }

        element.setTooltip(text);
    }
}
