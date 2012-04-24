package gov.noaa.nws.ncep.viz.common.ui;
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
//import java.awt.event.KeyAdapter;

import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyAdapter;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * Copied from gfe
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2010            lvenable     Initial creation
 * 2/13/2012    #682       Q.Zhou       Make "Enter" default to OK
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class UserEntryDialog extends Dialog {
    private Shell shell;

    private Display display;

    private String dialogTitle;

    private String message;

    private String inputText;

    private Text inputTF;

    public UserEntryDialog(Shell parentShell, String title, String message,
            String defaultText) {
        super(parentShell, 0);

        this.dialogTitle = title;
        this.message = message;
        this.inputText = defaultText;
    }

    public String open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);

        if (dialogTitle != null) {
            shell.setText(dialogTitle);
        }

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        shell.setLayout(mainLayout);

        // Initialize data and all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        

        return inputText;
    }

    private void initializeComponents() {
        createInputControls();
        createBottomButtons();
    }

    private void createInputControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        controlComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label messageLbl = new Label(controlComp, SWT.NONE);
        messageLbl.setLayoutData(gd);

        if (this.message != null) {
            messageLbl.setText(message);
        }

        gd = new GridData(200, SWT.DEFAULT);
        inputTF = new Text(controlComp, SWT.BORDER);
        inputTF.setLayoutData(gd);

        if (this.inputText != null) {
            inputTF.setText(inputText);
            inputTF.setSelection(0,inputText.length());
            inputTF.setFocus();
        }
        
        inputTF.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {            	
            	if (SWT.CR == e.keyCode) {
            		inputText = inputTF.getText().trim();
            		shell.dispose();
            	}            	
             }
        });
    }

    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        buttonComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                inputText = inputTF.getText().trim();
                shell.dispose();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                inputText = null;
                shell.dispose();
            }
        });
    }
}
