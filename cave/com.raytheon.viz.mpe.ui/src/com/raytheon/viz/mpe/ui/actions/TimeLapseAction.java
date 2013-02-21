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
package com.raytheon.viz.mpe.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.timelapse.TimeLapseDlg;

/**
 * Time lapse action, can start/stop time lapsing in MPE
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class TimeLapseAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IEditorPart part = HandlerUtil.getActiveEditor(event);
        if (part instanceof IDisplayPaneContainer) {
            IDisplayPaneContainer container = (IDisplayPaneContainer) part;
            String hourId = event.getParameter("Hour");
            if ("E".equals(hourId)) {
                // End looping
                MPEDisplayManager.stopLooping(container);
            } else if ("O".equals(hourId)) {
                // Open looping dialog
                TimeLapseDlg dialog = new TimeLapseDlg(part.getEditorSite()
                        .getShell());
                dialog.open();
            } else {
                // Loop for specified number of hours
                int hour = Integer.parseInt(hourId);
                MPEDisplayManager.startLooping(container, hour);
            }
        }

        return null;
    }
}
