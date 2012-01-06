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
package com.raytheon.viz.texteditor.dialogs;

import java.io.File;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.texteditor.msgs.IRecoverEditSessionCallback;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Contains the EditSessionRecoveryDialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2009 #2191      rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class EditSessionRecoveryDialog extends CaveJFACEDialog {

    private final IRecoverEditSessionCallback callback;

    private List sessionList;

    /**
     * Constructor
     * 
     * @param parentShell
     *            parent shell
     * @param dataManager
     *            DataManager for the associated window
     */
    public EditSessionRecoveryDialog(Shell parentShell,
            IRecoverEditSessionCallback callback) {
        super(parentShell);

        this.callback = callback;
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
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
        newShell.setText("Edit Session Recovery");
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
        createButton(parent, IDialogConstants.OK_ID, "Recover", false);
        createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", false);
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
        GridLayout layout = new GridLayout(1, false);
        top.setLayout(layout);

        sessionList = new List(top, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.widthHint = 450;
        data.heightHint = 250;
        sessionList.setLayoutData(data);
        String[] files = getSessions();

        if (files != null && files.length > 0) {
            for (String file : files) {
                sessionList.add(file);
            }
        }

        top.layout();
        return top;
    }

    @Override
    protected void okPressed() {
        int index = sessionList.getSelectionIndex();

        if (index >= 0) {
            final String session = sessionList.getItem(index);
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    callback.recoverSession(session);
                }

            });
        }

        super.okPressed();
    }

    private String[] getSessions() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER);
        LocalizationFile lFile = pm.getLocalizationFile(lc,
                TextEditorDialog.SAVED_SESSION_DIR);
        File dir = lFile.getFile();
        return dir.list();
    }
}
