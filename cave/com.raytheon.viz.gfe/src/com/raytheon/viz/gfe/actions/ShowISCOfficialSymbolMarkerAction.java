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

import com.raytheon.viz.gfe.core.DataManager;

/**
 * Action to toggle the display of the official symbol on the ISC Markers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/20/09      1995       bphillip    Initial release
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ShowISCOfficialSymbolMarkerAction extends AbstractHandler implements
        IElementUpdater {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        DataManager dataMgr = DataManager.getCurrentInstance();
        dataMgr.getSpatialDisplayManager().setShowISCOfficialSymbolMarker(
                !dataMgr.getSpatialDisplayManager()
                        .isShowISCOfficialSymbolMarker());

        return null;
    }

    @SuppressWarnings("unchecked")
    @Override
    public void updateElement(UIElement element, Map parameters) {
        DataManager dm = DataManager.getCurrentInstance();
        if (dm == null) {
            return;
        }
        element.setChecked(DataManager.getCurrentInstance()
                .getSpatialDisplayManager().isShowISCOfficialSymbolMarker());
    }

}
