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

import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import com.raytheon.viz.ui.UiUtil;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ui.actions.AbstractDropDownMenuHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 2, 2008		#1053	randerso	Initial creation
 * Dec 19,2012		#15535	jdynina		Adjusted size for the button on demand
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class EditAreaModeButton extends AbstractDropDownMenuHandler implements
        IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.
     * menus.UIElement, java.util.Map)
     */
    @Override
    @SuppressWarnings("rawtypes")
    public void updateElement(UIElement element, Map parameters) {
        DataManager dm = DataManager.getCurrentInstance();
        if (dm == null) {
            return;
        }
        element.setText(dm.getRefManager().getMode().getSymbol());
        UiUtil.updateWindowCoolBar((IWorkbenchWindow) parameters
                .get("org.eclipse.ui.IWorkbenchWindow"));
    }

}
