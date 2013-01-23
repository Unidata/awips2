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
package com.raytheon.uf.viz.datadelivery.common.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Abstract view class that is a layer between the CaveSWTDialog and the dialogs
 * that implement this class. This class finalizes the preOpened() method so
 * that it will call a callback method when the dialog is disposed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public abstract class AbstractViewDlg extends CaveSWTDialog {

    /**
     * Callback called when the dialog is disposed.
     */
    private IDialogClosed callback = null;

    /**
     * Dialog ID.
     */
    private String dialogId = null;

    private Point shellSize;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param callback
     *            Callback for when the dialog closes.
     * @param shellSize
     *            The initial shell size.
     * @param id
     *            Dialog ID.
     */
    public AbstractViewDlg(Shell parentShell, IDialogClosed callback, Point shellSize, String id) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.INDEPENDENT_SHELL | CAVE.DO_NOT_BLOCK);

        this.callback = callback;
        this.shellSize = shellSize;
        this.dialogId = id;
    }

    @Override
    protected final void preOpened() {

        if (shellSize != null) {
            shell.setSize(shellSize);
        }

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                callCallback();
            }
        });

    }

    /**
     * Call the callback method when the dialog gets disposed.
     */
    private void callCallback() {

        if (callback == null || dialogId == null) {
            return;
        }

        callback.dialogClosed(dialogId);
    }
}
