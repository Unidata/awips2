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

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventHandler;

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.msgs.GMDisplayModeMsg;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.gridmanager.GridMode;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.actions.AbstractDropDownMenuHandler;
import com.raytheon.viz.ui.actions.HandlerTextSizer;

/**
 * Display Mode Button
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 9, 2008		    	dfitch  	Initial creation
 * Jan 15, 2016  5193       bsteffen    Do not update UI until pref store is ready.
 * Apr 04, 2016  5519       bsteffen    Keep toolbar text constant width.
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */
@SuppressWarnings("rawtypes")
public class DisplayModeButton extends AbstractDropDownMenuHandler
        implements IElementUpdater {

    @Override
    public void updateElement(UIElement element, Map parameters) {
        if (!Activator.getDefault().checkPreferenceStore(
                new UpdateCommandHandler(element, parameters))) {
            return;
        }
        GridMode mode = Message.inquireLastMessage(GMDisplayModeMsg.class)
                .getGridMode();
        HandlerTextSizer sizer = new HandlerTextSizer(Display.getCurrent());
        for (GridMode gridMode : GridMode.values()) {
            sizer.setMinIfWider(gridMode.toString());
        }
        String text = sizer.createAdjustedText(mode.toString());
        sizer.dispose();
        element.setText(text);
        UiUtil.updateWindowCoolBar((IWorkbenchWindow) parameters
                .get("org.eclipse.ui.IWorkbenchWindow"));
    }

    private class UpdateCommandHandler implements EventHandler {

        private final UIElement element;

        private final Map parameters;

        public UpdateCommandHandler(UIElement element, Map parameters) {
            this.element = element;
            this.parameters = parameters;
        }

        @Override
        public void handleEvent(final Event event) {
            updateElement(element, parameters);
        }

    }
}
