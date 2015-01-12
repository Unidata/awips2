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
package com.raytheon.viz.mpe.ui.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.ui.ComparisonFields;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * Dialog to handle the selection of 1-hour precip fields to compare in MPE. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * December 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 * 
 */

public class SelectComparisonFieldsDialog extends AbstractMPEDialog {

	private MPEDisplayManager displayManager;
	
	private Combo fieldTypeCombo1 = null;
	private Combo fieldTypeCombo2 = null;
	
	private DisplayFieldData[] displayFieldDataArray;
    private String[] displayTypeNameArray;
    
   
	
	// ---------------------------------------------------------------------
    /**
     * Constructor.
     * 
     * @param parent
     */
    public SelectComparisonFieldsDialog(Shell parent, MPEDisplayManager displayManager) {
        super(parent);
        this.displayManager = displayManager;
    }

    /**
     * Open the dialog.
     * 
     * @return Return value (null).
     */
    public Object open() {
        Shell parent = this.getParent();
        Display display = parent.getDisplay();

        // Set up the shell and allow it to resize.
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MODELESS | SWT.RESIZE);
        shell.setText("Select Comparison Fields");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                        .getCurrentPerspectiveManager();
                if (mgr != null) {
                    mgr.removePespectiveDialog(SelectComparisonFieldsDialog.this);
                }
            }
        });

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();

        // Set the minimum size of the dialog so resizing does not hide
        // the buttons.
        shell.setMinimumSize(shell.getBounds().width, shell.getBounds().height);

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return null;
    }

    /**
     * Initialize the widget components.
     */
    private void initializeComponents() {

    	createProdListComp(shell);
        addSeparator(shell);
        createBottomButtons();

    }
    

    /**
     * Create the data options group and controls.
     */
    private void createProdListComp(Shell shell) {

        // Create a container to hold the label and the combo box.
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite prodListComp = new Composite(shell, SWT.NONE);
        GridLayout prodListCompLayout = new GridLayout(2, false);
        prodListComp.setLayout(prodListCompLayout);
        prodListComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label fieldTypeLabel1 = new Label(prodListComp, SWT.CENTER);
        fieldTypeLabel1.setText("Field 1:");
        fieldTypeLabel1.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        fieldTypeCombo1 = new Combo(prodListComp, SWT.LEFT | SWT.DROP_DOWN
                | SWT.READ_ONLY);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label fieldTypeLabel2 = new Label(prodListComp, SWT.CENTER);
        fieldTypeLabel2.setText("Field 2:");
        fieldTypeLabel2.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        fieldTypeCombo2 = new Combo(prodListComp, SWT.LEFT | SWT.DROP_DOWN
                | SWT.READ_ONLY);
        
        
        int selector1 = -1;
        int selector2 = -1;
        
        
        MPEDisplayManager displayMgr = MPEDisplayManager.getCurrent();    
        DisplayFieldData field1 = displayMgr.getComparisonFields().getField1();
        DisplayFieldData field2 = displayMgr.getComparisonFields().getField2();
        
        if (field1 == null)
        {
        	field1 = DisplayFieldData.rMosaic;
        }
        
        if (field2 == null)
        {
        	field2 = DisplayFieldData.rdMosaic;
        }
        
        
        if (displayFieldDataArray == null)
        {
        	  displayFieldDataArray = MPEDisplayManager.mpe_qpe_fields;
        }
        
        
        //find the index of the selected field
        for (selector1 = 0; selector1 < displayFieldDataArray.length; ++selector1) {
            if (displayFieldDataArray[selector1] == field1) {
                break;
            }
        }

        //find the index of the selected field
        for (selector2 = 0; selector2 < displayFieldDataArray.length; ++selector2) {
            if (displayFieldDataArray[selector2] == field2) {
                break;
            }
        }
        
        displayFieldDataArray = MPEDisplayManager.mpe_qpe_fields;
        displayTypeNameArray = new String[displayFieldDataArray.length];
        for (int i = 0; i < displayFieldDataArray.length; i++) {
            displayTypeNameArray[i] = displayFieldDataArray[i].toString();
        }

        
        //select the current field1
        fieldTypeCombo1.setTextLimit(35);
        fieldTypeCombo1.setLayoutData(gd);
        fieldTypeCombo1.setItems(displayTypeNameArray);
        fieldTypeCombo1.select(selector1);
        
        //select the current field2
        fieldTypeCombo2.setTextLimit(35);
        fieldTypeCombo2.setLayoutData(gd);
        fieldTypeCombo2.setItems(displayTypeNameArray);
        fieldTypeCombo2.select(selector2);
        
    }

   

    /**
     * Create the bottom OK and Cancel buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        int buttonWidth = 80;

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                okAction();
                close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Add a separator to the display.
     * 
     * @param parentComp
     *            Parent Composite
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

 
    /**
     * When the OK button is pressed, set the new field types to compare
     */
    private void okAction()
    {
    	String fieldName1 = fieldTypeCombo1.getText();
    	String fieldName2 = fieldTypeCombo2.getText();
    	
        DisplayFieldData newField1 = DisplayFieldData.fromDisplayNameString(fieldName1);
        DisplayFieldData newField2 = DisplayFieldData.fromDisplayNameString(fieldName2);
         
        ComparisonFields comparisonFields = displayManager.getComparisonFields();        
        comparisonFields.setField1(newField1);
        comparisonFields.setField2(newField2);
    }
}
