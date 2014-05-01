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
package com.raytheon.viz.gfe.menu;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;

import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.msgs.ISCSendStatusChangedMsg;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.dialogs.isc.ISCRequestReplyDlg;
import com.raytheon.viz.gfe.dialogs.isc.SendISCDialog;

/**
 * Menu handler for enabling and disabling ISC related menu items
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2011            bphillip     Initial creation
 * Oct 25, 2012 1287       rferrel     Changes for non-blocking SendISCDialog
 *                                      and ISCRequestReplyDlg.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ISCMenuEnabler extends CompoundContributionItem {
    private SendISCDialog sendISCDlg;

    private ISCRequestReplyDlg iscRequestDlg;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        ActionContributionItem sendIntersiteItem = new ActionContributionItem(
                new Action("Send Intersite Grids") {
                    public void run() {
                        Shell shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();

                        DataManager dm = DataManager.getCurrentInstance();
                        if (dm == null) {
                            return;
                        }

                        if (sendISCDlg == null || sendISCDlg.getShell() == null
                                || sendISCDlg.isDisposed()) {
                            sendISCDlg = new SendISCDialog(shell, dm);
                            sendISCDlg.setBlockOnOpen(false);
                            sendISCDlg.open();
                        } else {
                            sendISCDlg.bringToTop();
                        }
                    }

                    @Override
                    public boolean isEnabled() {
                        if (DataManager.getCurrentInstance() == null) {
                            return false;
                        } else {
                            return (!DataManager.getCurrentInstance()
                                    .sendIscOnSave() || !DataManager
                                    .getCurrentInstance().sendIscOnPublish())
                                    && CAVEMode.getMode().equals(
                                            CAVEMode.OPERATIONAL)
                                    && DataManager.getCurrentInstance()
                                            .requestISC();
                        }
                    }
                });

        ActionContributionItem iscRequestReplyItem = new ActionContributionItem(
                new Action("ISC Request/Reply") {
                    @Override
                    public boolean isEnabled() {
                        if (DataManager.getCurrentInstance() == null) {
                            return false;
                        } else {
                            return CAVEMode.getMode().equals(
                                    CAVEMode.OPERATIONAL)
                                    && DataManager.getCurrentInstance()
                                            .requestISC();
                        }
                    }

                    @Override
                    public void run() {
                        if (iscRequestDlg == null
                                || iscRequestDlg.getShell() == null
                                || iscRequestDlg.isDisposed()) {
                            Shell shell = PlatformUI.getWorkbench()
                                    .getActiveWorkbenchWindow().getShell();
                            iscRequestDlg = new ISCRequestReplyDlg(shell);
                            iscRequestDlg.open();
                        } else {
                            iscRequestDlg.bringToTop();
                        }
                    }

                });

        ActionContributionItem iscSendEnableItem = new ActionContributionItem(
                new Action("ISC Send Enable", IAction.AS_CHECK_BOX) {

                    @Override
                    public boolean isEnabled() {
                        return getIscEnabled();
                    }

                    public void run() {
                        DataManager dm = DataManager.getCurrentInstance();
                        if (dm != null) {
                            boolean newState = !Message.inquireLastMessage(
                                    ISCSendStatusChangedMsg.class).isEnabled();
                            dm.enableISCsend(newState);
                        }
                    }

                    private boolean getIscEnabled() {
                        return CAVEMode.getMode().equals(CAVEMode.OPERATIONAL)
                                && DataManager.getCurrentInstance()
                                        .requestISC();
                    }

                    public boolean isChecked() {
                        return Message.inquireLastMessage(
                                ISCSendStatusChangedMsg.class).isEnabled();
                    }
                });

        return new IContributionItem[] { sendIntersiteItem,
                iscRequestReplyItem, iscSendEnableItem };
    }
}
