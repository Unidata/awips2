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
package com.raytheon.viz.mpe.ui.dialogs.polygon;

import java.awt.Point;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.polygon.RubberPolyData.PolygonEditAction;
import com.raytheon.viz.mpe.ui.rsc.MPEPolygonResource;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Draw Polygon Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2009 2685       mpduff      Initial creation
 * Jan 26, 2011 7761       bkowal      The value associated with polygons
 *                                     with the "scale" action will no
 *                                     longer be divided by 100.
 * Sep 11, 2013 #2353      lvenable    Fixed cursor memory leak.
 * Jan 29, 2014 16561      snaples     Updated processDrawPrecipValue to remove polygon wireframe after setting value.
 * Feb 2, 2014  16201      snaples      Added saved data flag support
 * 
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DrawPolygonDlg extends CaveSWTDialog {
    private static final String ADJUST_PRECIP_TEXT = "Adjust Precipitation Value";

    private static final String SUBSTITUTE_VALUE_TEXT = "Select Field To Substitute";

    private static final String MAKE_PERSISTENT = "Make Persistent";

	private DisplayFieldData[] displayFieldDataArray;
    private String[] displayTypeNameArray;
	
    /**
     * Bold Font.
     */
    private Font boldFont = null;

    /**
     * Normal font.
     */
    private Font font = null;

    /**
     * The field type selection Combo control.
     */
	private Combo fieldTypeCombo = null;
    
    /**
     * The precip value spinner control.
     */
    private Spinner precipSpinner = null;

    /**
     * The precip slider control.
     */
    private Scale precipSlider = null;

    /**
     * The points to perform operations on
     */
    private List<Point> points;

    /**
     * The wait mouse pointer.
     */
    private Cursor waitCursor = null;

    /** The substitute type */
    private DisplayFieldData subType = null;

    /** Checkbox for persistent. */
    private Button persistentChk = null;

    /** The polygon resource */
    private final MPEPolygonResource resource;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            The parent shell for this dialog.
     */
    public DrawPolygonDlg(Shell parentShell, MPEPolygonResource resource) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Edit Precipitation");
        this.resource = resource;
        waitCursor = parentShell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
        boldFont.dispose();
        resource.clearPolygons();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        boldFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.BOLD);
        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        // Initialize all of the controls and layoutsendCal
        initializeComponents();
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        createPersistentGroup();
        createSubGroup();
        createCloseBtn();
    }

    /**
     * Create the persistent group.
     */
    private void createPersistentGroup() {
        // Create adjust group
        Group persistentGroupComp = new Group(shell, SWT.NONE);
        persistentGroupComp.setFont(boldFont);
        persistentGroupComp.setText(ADJUST_PRECIP_TEXT);
        persistentGroupComp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(345, SWT.DEFAULT);
        persistentGroupComp.setLayoutData(gd);

        getPersistentChk(persistentGroupComp);
        getSliderComp(persistentGroupComp);
        getButtonComp(persistentGroupComp);
    }

    /**
     * Create the substitute group.
     */
    private void createSubGroup() {
        // Create substitute group
        Group subGroup = new Group(shell, SWT.NONE);
        subGroup.setFont(boldFont);
        subGroup.setText(SUBSTITUTE_VALUE_TEXT);
        subGroup.setLayout(new GridLayout(2, false));
        GridData gd = new GridData(345, SWT.DEFAULT);
        subGroup.setLayoutData(gd);

        createFieldCombo(subGroup);

        // Create Substitute button
        final Button subBtn = new Button(subGroup, SWT.PUSH);
        subBtn.setData(PolygonEditAction.SUB);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false, 2, 1);
        subBtn.setText("Substitute");
        subBtn.setLayoutData(gd);
        subBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                processDrawPrecipValue(subBtn);
            }
        });
    }

    /**
     * Create the close button.
     */
    private void createCloseBtn() {
        Button closeBtn = new Button(shell, SWT.PUSH);
        closeBtn.setText("Close");
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false, 1, 1);
        closeBtn.setAlignment(SWT.CENTER);
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.close();
            }
        });
    }

    /**
     * Build the persistent check box.
     * 
     * @param groupComp
     *            the parent composite
     */
    private void getPersistentChk(Group groupComp) {
        persistentChk = new Button(groupComp, SWT.CHECK);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        persistentChk.setLayoutData(gd);
        persistentChk.setText(MAKE_PERSISTENT);
        persistentChk.setFont(font);
    }

    /**
     * Build the slider/spinner composite.
     * 
     * @param groupComp
     *            the parent composite
     */
    private void getSliderComp(Group groupComp) {
        Composite comp = new Composite(groupComp, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(250, 30);
        precipSlider = new Scale(comp, SWT.HORIZONTAL);
        precipSlider.setMinimum(0);
        precipSlider.setMaximum(500);
        precipSlider.setIncrement(1);
        precipSlider.setLayoutData(gd);
        precipSlider.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                precipSpinner.setSelection(precipSlider.getSelection());
            }
        });

        // Create the Red color spinner.
        precipSpinner = new Spinner(comp, SWT.BORDER);
        gd = new GridData(30, SWT.DEFAULT);
        precipSpinner.setLayoutData(gd);
        precipSpinner.setMinimum(0);
        precipSpinner.setMaximum(500);
        precipSpinner.setSelection(precipSlider.getSelection());
        precipSpinner.setDigits(2);

        precipSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                precipSlider.setSelection(precipSpinner.getSelection());
            }
        });
    }

    /**
     * Build the 5 button composite.
     * 
     * @param groupComp
     *            the parent composite
     */
    private void getButtonComp(Group groupComp) {
        Composite comp = new Composite(groupComp, SWT.NONE);
        comp.setLayout(new GridLayout(5, false));

        PolygonEditAction[] editBtns = new PolygonEditAction[] {
                PolygonEditAction.SET, PolygonEditAction.RAISE,
                PolygonEditAction.LOWER, PolygonEditAction.SCALE,
                PolygonEditAction.SNOW };

        for (PolygonEditAction action : editBtns) {
            Button editBtn = new Button(comp, SWT.PUSH);
            editBtn.setText(action.toPrettyName());
            editBtn.setData(action);
            editBtn.setLayoutData(new GridData(60, SWT.DEFAULT));
            editBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    processDrawPrecipValue((Button) event.getSource());
                }
            });
        }
    }

    /**
     * Create the substitute check box widgets
     * 
     * @param groupComp
     *            The group composite
     */
    private void createFieldCombo(Group groupComp) {
        // Spacer
    	
        // Create a container to hold the label and the combo box.
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite prodListComp = new Composite(shell, SWT.NONE);
        GridLayout prodListCompLayout = new GridLayout(2, false);
        prodListComp.setLayout(prodListCompLayout);
        prodListComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label fieldTypeLabel = new Label(prodListComp, SWT.CENTER);
        fieldTypeLabel.setText(SUBSTITUTE_VALUE_TEXT);
        fieldTypeLabel.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        fieldTypeCombo = new Combo(groupComp, SWT.LEFT | SWT.DROP_DOWN
                | SWT.READ_ONLY);
        
        if (displayFieldDataArray == null)
        {
        	  displayFieldDataArray = MPEDisplayManager.mpe_qpe_fields;
        }
         	
  //      Label spaceLabel = new Label(groupComp, SWT.NONE);
  //      spaceLabel.setText("*****  ");
        
        int selectedFieldIndex = 0;
        
        //find the index of the selected field
        for (selectedFieldIndex = 0; selectedFieldIndex < displayFieldDataArray.length; selectedFieldIndex++)
        {
            if (displayFieldDataArray[selectedFieldIndex] == subType)
            {
                break;
            }
        }
        
        //create and initialize the display field type name array
        displayTypeNameArray = new String[displayFieldDataArray.length];
        
        for (int i = 0; i < displayFieldDataArray.length; i++) {
        	
        	String fieldName = displayFieldDataArray[i].toString();
       // 	System.out.println("DrawPolygon.createFieldCombo():  FieldName = :" + fieldName + ":");       	
            displayTypeNameArray[i] = fieldName;
        }
        
        //select the field
        fieldTypeCombo.setTextLimit(35);
        fieldTypeCombo.setLayoutData(gd);
        fieldTypeCombo.setItems(displayTypeNameArray);
        fieldTypeCombo.select(selectedFieldIndex);
        
        fieldTypeCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	String selectedFieldString = fieldTypeCombo.getText();
 //           	System.out.println("DrawPolygon.createFieldCombo(): selectedFieldString =  " +
 //           						selectedFieldString);
            	
            	subType = DisplayFieldData.fromDisplayNameString(selectedFieldString);
            	
 //           	if (subType != null)
//            	{
 //           		System.out.println("DrawPolygon.createFieldCombo(): subType =  " +
 //           			subType.toString());
 //           	}
          }
        });

    }
    
    

    /**
     * Process the selection.
     * 
     * @param btnSource
     *            Source object
     */
    private void processDrawPrecipValue(Button editBtn) {
        MPEDisplayManager dispMgr = MPEDisplayManager.getInstance(resource
                .getDescriptor().getRenderableDisplay());
        Cursor prevCursor = shell.getCursor();
        try {
            /* Apply the polygon and save it to the Polygon file. */
            shell.setCursor(waitCursor);

            // Divide precipSpinner selection by 100 since we have 2 decimal
            // digits when created. This give actual precip value
            double precipValue = precipSpinner.getSelection() / 100.0;
            Point[] editPoints = points.toArray(new Point[0]);
            PolygonEditAction action = (PolygonEditAction) editBtn.getData();
            boolean persistent = persistentChk.getSelection();
            DisplayFieldData subType = null;
            if (action == PolygonEditAction.SUB) {
                subType = this.subType;
            }

            DisplayFieldData displayedField = dispMgr.getDisplayFieldType();
            Date editDate = dispMgr.getCurrentEditDate();

            RubberPolyData newEdit = new RubberPolyData(action, subType,
                    precipValue, editPoints, true, persistent);

            List<RubberPolyData> polygonEdits = PolygonEditManager
                    .getPolygonEdits(displayedField, editDate);
            polygonEdits.add(newEdit);
            PolygonEditManager.writePolygonEdits(displayedField, editDate,
                    polygonEdits);
            resource.clearPolygons();
            dispMgr.setSavedData(false);
        } finally {
            shell.setCursor(prevCursor);
        }
    }

    /**
     * Set the rubber banded polygon data
     * 
     * @param polyData
     *            The RubberPolyData object to set
     */
    public void setPolygonPoints(List<Point> points) {
        this.points = points;
    }

}
