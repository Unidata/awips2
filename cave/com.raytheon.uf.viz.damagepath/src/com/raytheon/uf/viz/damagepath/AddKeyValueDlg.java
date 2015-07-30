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
package com.raytheon.uf.viz.damagepath;

import java.util.Collection;
import java.util.Collections;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to add or edit property key/value pairs for the
 * {@code EditGeoJsonPropertiesDlg}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2015  #4354     dgilling     Initial creation based on dialog from 
 *                                      lvenable.
 * May 01, 2015  #4354     dgilling     Add edit mode.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class AddKeyValueDlg extends CaveSWTDialog {

    private static final String EMPTY_STRING = "";

    private static final String EMPTY_KEY_MSG = "Enter a key.";

    private static final String USED_KEY_MSG = "Key %s is already in use.";

    private Text keyTF;

    private Text valueTF;

    private Button okBtn;

    private Label verificationLbl;

    private final Collection<String> reservedKeys;

    private final boolean editMode;

    private final String initialKey;

    private final String initialValue;

    public AddKeyValueDlg(Shell parentShell, Collection<String> reservedKeys) {
        this(parentShell, reservedKeys, false, null, null);
    }

    public AddKeyValueDlg(Shell parentShell, String key, String value) {
        this(parentShell, null, true, key, value);
    }

    private AddKeyValueDlg(Shell parentShell, Collection<String> reservedKeys,
            boolean editMode, String key, String value) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK
                | CAVE.MODE_INDEPENDENT);
        if (reservedKeys != null) {
            this.reservedKeys = reservedKeys;
        } else {
            this.reservedKeys = Collections.emptyList();
        }
        this.editMode = editMode;

        this.initialKey = key;
        this.initialValue = value;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        return gd;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        String dialogTitle = (!editMode) ? "Add GeoJSON Property"
                : "Edit GeoJSON Property";
        setText(dialogTitle);

        createKeyValueControls();
        createBottomButtons();
    }

    private void createKeyValueControls() {
        Composite keyValueComp = new Composite(shell, SWT.NONE);
        keyValueComp.setLayout(new GridLayout(2, false));
        keyValueComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        Label keyLbl = new Label(keyValueComp, SWT.NONE);
        keyLbl.setText("Key:");

        GridData gd = new GridData(130, SWT.DEFAULT);
        keyTF = new Text(keyValueComp, SWT.BORDER);
        keyTF.setLayoutData(gd);
        if (initialKey != null) {
            keyTF.setText(initialKey);
        }
        keyTF.setEnabled(!editMode);
        if (!editMode) {
            keyTF.addModifyListener(new ModifyListener() {

                @Override
                public void modifyText(ModifyEvent e) {
                    String newKey = keyTF.getText().trim();
                    if (reservedKeys.contains(newKey)) {
                        verificationLbl.setText(String.format(USED_KEY_MSG,
                                newKey));
                        okBtn.setEnabled(false);
                    } else if (newKey.isEmpty()) {
                        verificationLbl.setText(EMPTY_KEY_MSG);
                        okBtn.setEnabled(false);
                    } else {
                        verificationLbl.setText(EMPTY_STRING);
                        okBtn.setEnabled(true);
                    }

                    getShell().pack();
                    getShell().layout();
                }
            });
        }

        Label valueLbl = new Label(keyValueComp, SWT.NONE);
        valueLbl.setText("Value:");

        gd = new GridData(130, SWT.DEFAULT);
        valueTF = new Text(keyValueComp, SWT.BORDER);
        valueTF.setLayoutData(gd);
        if (initialValue != null) {
            valueTF.setText(initialValue);
        }

        if (!editMode) {
            verificationLbl = new Label(keyValueComp, SWT.NONE);
            verificationLbl.setText(EMPTY_KEY_MSG);
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
            gd.horizontalSpan = 2;
            verificationLbl.setLayoutData(gd);
        }
    }

    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        int buttonWidth = 70;

        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText(" OK ");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleOkAction();
            }
        });
        okBtn.setEnabled(!keyTF.getText().trim().isEmpty());

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText(" Cancel ");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(null);
                close();
            }
        });
    }

    private void handleOkAction() {
        Pair<String, String> pair = new Pair<>(keyTF.getText().trim(), valueTF
                .getText().trim());
        setReturnValue(pair);
        close();
    }
}
