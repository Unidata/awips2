package com.raytheon.uf.viz.damagepath;

import java.util.Collection;

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
 * Dialog to add new property key/value pairs for the
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

    public AddKeyValueDlg(Shell parentShell, Collection<String> reservedKeys) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK
                | CAVE.MODE_INDEPENDENT);
        this.reservedKeys = reservedKeys;
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
        setText("Add GeoJSON Property");

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
        keyTF.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                String newKey = keyTF.getText().trim();
                if (reservedKeys.contains(newKey)) {
                    verificationLbl.setText(String.format(USED_KEY_MSG, newKey));
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

        Label valueLbl = new Label(keyValueComp, SWT.NONE);
        valueLbl.setText("Value:");

        gd = new GridData(130, SWT.DEFAULT);
        valueTF = new Text(keyValueComp, SWT.BORDER);
        valueTF.setLayoutData(gd);

        verificationLbl = new Label(keyValueComp, SWT.NONE);
        verificationLbl.setText(EMPTY_KEY_MSG);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.horizontalSpan = 2;
        verificationLbl.setLayoutData(gd);

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
        okBtn.setEnabled(false);

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
