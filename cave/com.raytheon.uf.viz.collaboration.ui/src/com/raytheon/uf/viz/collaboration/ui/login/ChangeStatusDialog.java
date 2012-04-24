package com.raytheon.uf.viz.collaboration.ui.login;

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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Mode;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Change the user Mode Status and the optional message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ChangeStatusDialog extends CaveSWTDialog {

    private Label userLabel;

    private Combo statusCombo;

    private Text messageTF;

    public ChangeStatusDialog(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM);
        setText("Change Status");
    }

    private Control createDialogArea(Composite parent) {
        GridData gd = null;
        Composite body = new Composite(parent, SWT.NONE);
        body.setLayout(new GridLayout(2, false));
        Label label = null;
        userLabel = new Label(body, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        userLabel.setLayoutData(gd);
        userLabel.setText("");

        label = new Label(body, SWT.NONE);
        label.setText("Status: ");
        statusCombo = new Combo(body, SWT.DEFAULT);

        for (IPresence.Mode mode : CollaborationUtils.statusModes) {
            statusCombo.add(mode.getMode());
        }

        label = new Label(body, SWT.NONE);
        label.setText("Message: ");

        messageTF = new Text(body, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = 200;
        messageTF.setLayoutData(gd);

        return body;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(1, false));
        createDialogArea(shell);
        createButtonBar(shell);
    }

    private void createButtonBar(Composite parent) {
        GridData gd = null;
        Composite bar = new Composite(parent, SWT.NONE);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        bar.setLayout(new GridLayout(0, true));
        bar.setLayoutData(gd);
        createButton(bar, IDialogConstants.OK_ID, "Send", true);

        createButton(bar, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void preOpened() {
        super.preOpened();
        IPreferenceStore prefStore = Activator.getDefault()
                .getPreferenceStore();
        userLabel.setText(prefStore.getString(CollabPrefConstants.P_USERNAME)
                + "@" + prefStore.getString(CollabPrefConstants.P_SERVER));
        statusCombo.select(statusCombo.indexOf(prefStore
                .getString(CollabPrefConstants.P_STATUS)));
        messageTF.setText(prefStore.getString(CollabPrefConstants.P_MESSAGE));
        messageTF.selectAll();
        statusCombo.setFocus();
    }

    /**
     * Creates a new button with the given id.
     * <p>
     * The <code>Dialog</code> implementation of this framework method creates a
     * standard push button, registers it for selection events including button
     * presses, and registers default buttons with its shell. The button id is
     * stored as the button's client data. If the button id is
     * <code>IDialogConstants.CANCEL_ID</code>, the new button will be
     * accessible from <code>getCancelButton()</code>. If the button id is
     * <code>IDialogConstants.OK_ID</code>, the new button will be accesible
     * from <code>getOKButton()</code>. Note that the parent's layout is assumed
     * to be a <code>GridLayout</code> and the number of columns in this layout
     * is incremented. Subclasses may override.
     * </p>
     * 
     * @param parent
     *            the parent composite
     * @param id
     *            the id of the button (see <code>IDialogConstants.*_ID</code>
     *            constants for standard dialog button ids)
     * @param label
     *            the label from the button
     * @param defaultButton
     *            <code>true</code> if the button is to be the default button,
     *            and <code>false</code> otherwise
     * 
     * @return the new button
     * 
     * @see #getCancelButton
     * @see #getOKButton()
     */
    protected Button createButton(Composite parent, int id, String label,
            boolean defaultButton) {
        // increment the number of columns in the button bar
        ((GridLayout) parent.getLayout()).numColumns++;
        Button button = new Button(parent, SWT.PUSH);
        button.setText(label);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = 70;
        button.setLayoutData(gd);
        button.setData(new Integer(id));
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                Integer val = (Integer) event.widget.getData();
                if (val != IDialogConstants.OK_ID) {
                    setReturnValue(null);
                } else {
                    IPresence.Mode mode = Mode.valueOf(statusCombo
                            .getItem(statusCombo.getSelectionIndex()));
                    String modeMessage = messageTF.getText().trim();
                    IPreferenceStore prefStore = Activator.getDefault()
                            .getPreferenceStore();
                    prefStore.setValue(CollabPrefConstants.P_STATUS,
                            mode.toString());
                    prefStore.setValue(CollabPrefConstants.P_MESSAGE,
                            modeMessage);
                }
                ChangeStatusDialog.this.getShell().dispose();
            }
        });
        if (defaultButton) {
            Shell shell = parent.getShell();
            if (shell != null) {
                shell.setDefaultButton(button);
            }
        }
        return button;
    }
}
