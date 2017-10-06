package gov.noaa.nws.mdl.viz.boundaryTool.ui.dialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * @author Mamoudou Ba
 * @version 1.0
 * 
 *          Dialog class which allows users to input a string defining a
 *          boundary type
 * 
 *          December 2015: Initial creation
 * 
 *          January 2016: extends to use CaveSWTDialog dialog, and remove
 *          unnecessary getters and setters
 */

class UserInputBndType extends CaveSWTDialog {
    protected static Boolean isOK = false;
    protected String userInput;

    public UserInputBndType(Shell parent) {
        this(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
    }

    public UserInputBndType(Shell parent, int style) {
        super(parent, style);
        setText("Input Dialog");
    }

    @Override
    protected void initializeComponents(final Shell shell) {

        shell.setLayout(new GridLayout(2, true));

        Label label = new Label(shell, SWT.NONE);
        label.setText("Please enter a boundary type:");
        GridData data = new GridData();
        data.horizontalSpan = 2;
        label.setLayoutData(data);

        final Text text = new Text(shell, SWT.BORDER);
        data = new GridData(GridData.FILL_HORIZONTAL);
        data.horizontalSpan = 2;
        text.setLayoutData(data);

        Button okButton = new Button(shell, SWT.PUSH);
        okButton.setText("OK");
        data = new GridData(GridData.FILL_HORIZONTAL);
        okButton.setLayoutData(data);

        okButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent arg0) {
                userInput = text.getText();
                isOK = true;
                close();
            }
        });

        Button cancelButton = new Button(shell, SWT.PUSH);
        cancelButton.setText("Cancel");
        data = new GridData(GridData.FILL_HORIZONTAL);
        cancelButton.setLayoutData(data);
        cancelButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                isOK = false;
                close();
            }
        });

        shell.setDefaultButton(okButton);
    }
}
