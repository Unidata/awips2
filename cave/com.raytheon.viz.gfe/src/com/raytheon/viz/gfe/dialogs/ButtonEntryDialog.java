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
package com.raytheon.viz.gfe.dialogs;

import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2012            jdynina     Initial creation
 * 
 * </pre>
 * 
 * @author jdynina
 * @version 1.0
 */
public class ButtonEntryDialog extends Dialog {
    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    private String dialogTitle;

    private HashMap<String,String[]> inputMap;
    
    private String inputText = "";
    
    private String[] selections;

    public ButtonEntryDialog(Shell parentShell, String title, HashMap<String,String[]> map) {
        super(parentShell, 0);

        this.dialogTitle = title;
        this.inputMap = map;
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
        GridLayout gl = new GridLayout(inputMap.size(), false);
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        controlComp.setLayoutData(gd);
        
		selections = new String[inputMap.size()];
        
        Listener listener = new Listener () {
    		public void handleEvent(Event event) {
    			Button button = (Button)event.widget;
    			if (!button.getSelection()) return;

    			int i = 0;
    			for (String s : inputMap.keySet()) {
    				for (String value : inputMap.get(s)) {
    					if ((value == button.getText()) && 
    							(selections[i] != button.getText())) 
    						selections[i] = button.getText();
    				}
    				i++;
    			}
    		}
    	};

    	
        int i = 0;
        for (String s : inputMap.keySet()) {
        	int j = 0;
            
            Group group = new Group(controlComp, SWT.SHADOW_IN);
            group.setText(s);
            group.setLayout(new RowLayout(SWT.VERTICAL));
            
            for (String value : inputMap.get(s)) {
            	Button button = new Button(group, SWT.RADIO);
            	button.setText(value);
            	if (j == 0) button.setSelection(true);
            	if (j == 0) selections[i] = value;
            	button.addListener(SWT.Selection, listener);
            	j++;
            }
            i++;
        } 
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
            	for (String selection : selections) {
            		inputText = (inputText + selection + " ");
            	}
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
