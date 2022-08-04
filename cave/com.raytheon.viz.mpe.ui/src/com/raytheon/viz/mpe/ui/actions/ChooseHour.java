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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.ChooseDataPeriodDialog;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Action called when Choose Hour, Next Hour, or Previous Hour are selected from
 * the MPEControl menu.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2008            randerso     Initial creation
 * Feb 26, 2014    2842    mpduff      Use PlatformUI rather than HandlerUtil.
 * Mar 09, 2018    7135    mduff       Apply actions to all applicable MPEDisplayManger instances.
 * </pre>
 * 
 * @author randerso
 */

public class ChooseHour extends AbstractHandler {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ChooseHour.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        int increment = 0;
        String s = event.getParameter("increment");
        if (s != null) {
            try {
                increment = Integer.parseInt(s);
            } catch (NumberFormatException e) {
                statusHandler.error("Invalid increment set in plugin.xml: " + s,
                        e);
            }
        } else {
            statusHandler.error(
                    "Invalid increment set in plugin.xml.  Value is null");
        }

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        List<MPEDisplayManager> displayMgrs = new ArrayList<>();
        if (container != null) {
            if (container instanceof IMultiPaneEditor) {
                IMultiPaneEditor multiPane = (IMultiPaneEditor) container;
                IDisplayPane[] displayPanes = multiPane.getDisplayPanes();
                if (displayPanes != null) {
                    for (IDisplayPane idp : displayPanes) {
                        displayMgrs.add(MPEDisplayManager.getInstance(idp));
                    }
                }
            }
        } else {
            displayMgrs.add(MPEDisplayManager.getCurrent());
        }

        Date currentDate = displayMgrs.get(0).getCurrentEditDate();
        if ((increment == 0) || (currentDate == null)) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            ChooseDataPeriodDialog dialog = new ChooseDataPeriodDialog(shell);
            dialog.open();
        } else {
            Calendar cal = TimeUtil.newGmtCalendar();
            cal.setTime(currentDate);
            cal.add(Calendar.HOUR_OF_DAY, increment);
            Date newDate = cal.getTime();
            for (MPEDisplayManager dmgr : displayMgrs) {
                dmgr.setCurrentEditDate(newDate);
            }
        }
        return null;
    }
}
