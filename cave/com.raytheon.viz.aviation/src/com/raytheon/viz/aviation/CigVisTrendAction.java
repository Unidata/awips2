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
package com.raytheon.viz.aviation;

import java.io.FileNotFoundException;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.aviation.climatology.CigVisTrendDlg;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;

/**
 * Action class for launching the Ceiling/Visibility Trend dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 23, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
@Deprecated
// TODO no longer used remove from the base line
public class CigVisTrendAction extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CigVisTrendAction.class);

    private CigVisTrendDlg cigVisTrendDialog;

    private List<String> siteList;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        try {
            if (siteList == null) {
                siteList = TafSiteConfigFactory.getInstance().getSiteList();
            }
        } catch (ConfigurationException e) {
            statusHandler.handle(Priority.PROBLEM, "Configuration error", e);
            return null;
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Missing configuration file", e);
            return null;
        }

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        if (cigVisTrendDialog == null
                || cigVisTrendDialog.getShell().isDisposed()) {
            cigVisTrendDialog = new CigVisTrendDlg(shell, siteList,
                    StatusMessageType.Metar, null);
            cigVisTrendDialog.open();
            cigVisTrendDialog = null;
        } else {
            cigVisTrendDialog.getShell().setVisible(true);
            cigVisTrendDialog.getShell().setFocus();
        }

        return null;
    }
}
