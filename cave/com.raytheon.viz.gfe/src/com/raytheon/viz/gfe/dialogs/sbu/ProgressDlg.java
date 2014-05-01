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
package com.raytheon.viz.gfe.dialogs.sbu;

import java.util.ArrayList;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.ServiceBackupMessageNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.ServiceBackupProgressNotification;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ProgressDlg extends CaveJFACEDialog implements
        INotificationObserver {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProgressDlg.class);

    private Label label;

    private ProgressBar progressBar;

    /**
     * @param parentShell
     */
    public ProgressDlg(Shell parentShell) {
        super(parentShell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Progress Bar");
    }

    public int open() {
        NotificationManagerJob.addObserver(ServiceBackupDlg.NOTIFY_TOPIC, this);
        return super.open();
    }

    public boolean close() {
        NotificationManagerJob.removeObserver(ServiceBackupDlg.NOTIFY_TOPIC,
                this);
        return super.close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        label = new Label(top, SWT.NONE);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.widthHint = this.convertWidthInCharsToPixels(50);
        layoutData.heightHint = this.convertHeightInCharsToPixels(3);
        label.setLayoutData(layoutData);

        progressBar = new ProgressBar(top, SWT.HORIZONTAL);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.widthHint = 500;
        layoutData.heightHint = 30;
        progressBar.setLayoutData(layoutData);
        progressBar.setMinimum(0);
        progressBar.setMaximum(100);
        progressBar.setSelection(0);

        return top;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "Close", true);
    }

    public void updateProgress(int percent) {
        final int value = Math.max(0, Math.min(100, percent));
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (progressBar != null && !progressBar.isDisposed()) {
                    progressBar.setSelection(value);
                }
            }
        });
    }

    public void updateMessage(final String message) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (label != null && !label.isDisposed()) {
                    label.setText("\n" + message + "\n");
                }
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.viz.core.notification.NotificationMessage[])
     */
    @SuppressWarnings("unchecked")
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        try {
            for (NotificationMessage msg : messages) {
                ArrayList<GfeNotification> notifications = (ArrayList<GfeNotification>) msg
                        .getMessagePayload();
                for (GfeNotification notification : notifications) {
                    if (notification instanceof ServiceBackupMessageNotification) {
                        ServiceBackupMessageNotification notify = (ServiceBackupMessageNotification) notification;
                        updateMessage(notify.getMessage());
                    } else if (notification instanceof ServiceBackupProgressNotification) {
                        ServiceBackupProgressNotification notify = (ServiceBackupProgressNotification) notification;
                        updateProgress(notify.getProgress());
                    }
                }
            }
        } catch (NotificationException e) {
            statusHandler.error("Error processing gfe notifications!!", e);
        }

    }
}
