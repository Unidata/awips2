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
package com.raytheon.uf.viz.monitor.ui.dialogs;

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

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;

/**
 * Table attributes dialog for changing the visible column on a table.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class TableAttribDlg extends Dialog
{
    /**
     * Dialog shell.
     */
    private Shell shell;
    
    /**
     * The display control.
     */
    private Display display;
    
    /**
     * Application name.
     */
    private CommonConfig.AppName appName;
    
    /**
     * Common table configuration data.
     */
    private CommonTableConfig tableConfig;
    
    /**
     * Array of column check boxes.
     */
    private Button[] columnChkArray;
    
    /**
     * Columns that are selected to be hidden/visible.
     */
    private boolean[] selectedVisibleCols;
    
    /**
     * Constructor.
     * @param parent Parent shell.
     * @param appName Application name.
     * @param visibleCols Array of visible columns.
     */
    public TableAttribDlg(Shell parent, CommonConfig.AppName appName, boolean[] visibleCols)
    {
        super(parent, 0);
        
        this.appName = appName;   
        
        selectedVisibleCols = new boolean[visibleCols.length];
        System.arraycopy(visibleCols, 0, selectedVisibleCols, 0, visibleCols.length);
        
    }
    
    /**
     * Open method.
     * @return Boolean array of visible/hidden columns.
     */
    public boolean[] open()
    {        
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE);
        shell.setText("Attribs");
        
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);
      
        // Initialize all of the controls and layouts
        initializeComponents();
        
        shell.pack();        
        shell.open();
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch())
            {
                display.sleep();
            }
        }
        
        return selectedVisibleCols;
    }
    
    /**
     * Initialize components.
     */
    private void initializeComponents()
    {
        tableConfig = CommonTableConfig.getInstance();
        
        createColumnControls();
        
        createBottomButtons();
        
        /*
         * Check the columns that are visible on the table.
         */
        checkVisibleColumnButtons();
    }
    
    /**
     * Create the column check box controls.
     */
    private void createColumnControls()
    {
        int checkBoxWidth = 200;
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainControlComp = new Composite(shell, SWT.NONE);
        mainControlComp.setLayout(new GridLayout(3, false));
        mainControlComp.setLayoutData(gd);
        
        String[] colKeys = tableConfig.getTableColumnKeys(appName);
        
        columnChkArray = new Button[colKeys.length];
        
        int halfIndex = (colKeys.length + 1) / 2;
        
        /*
         * Create the left column controls
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite leftControlComp = new Composite(mainControlComp, SWT.NONE);
        leftControlComp.setLayout(new GridLayout(1, false));
        leftControlComp.setLayoutData(gd);
        
        for (int i = 0; i < halfIndex; i++)
        {
            gd = new GridData(checkBoxWidth, SWT.DEFAULT);
            Button b = new Button(leftControlComp, SWT.CHECK);
            b.setText(tableConfig.getTableColumnAttr(colKeys[i]).getName());
            b.setLayoutData(gd);
            columnChkArray[i] = b;
        }
        
        /*
         * Create a vertical separator bar
         */
        gd = new GridData(SWT.CENTER, SWT.FILL, true, true);
        Label sepLbl = new Label(mainControlComp, SWT.SEPARATOR | SWT.VERTICAL);
        sepLbl.setLayoutData(gd);  
        
        /*
         * Create the right column controls
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite rightControlComp = new Composite(mainControlComp, SWT.NONE);
        rightControlComp.setLayout(new GridLayout(1, false));
        rightControlComp.setLayoutData(gd);
        
        for (int i = halfIndex; i < colKeys.length; i++)
        {
            gd = new GridData(checkBoxWidth, SWT.DEFAULT);
            Button b = new Button(rightControlComp, SWT.CHECK);
            b.setText(tableConfig.getTableColumnAttr(colKeys[i]).getName());
            b.setLayoutData(gd);
            columnChkArray[i] = b;
        }
    }
    
    /**
     * Create the OK and Cancel buttons.
     */
    private void createBottomButtons()
    {
        addSeparator(shell);
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);
        
        gd = new GridData(100, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {                
                for (int i = 0; i < selectedVisibleCols.length; i++)
                {
                    selectedVisibleCols[i] = columnChkArray[i].getSelection();
                }
                
                shell.dispose();
            }
        });
        
        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                selectedVisibleCols = new boolean[0];
                shell.dispose();
            }
        });
    }
    
    /**
     * Add a separator bar to the display.
     * @param parentComp Parent composite.
     */
    private void addSeparator(Composite parentComp)
    {
        GridLayout gl = (GridLayout)parentComp.getLayout();
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);        
    }
    
    /**
     * Check the column check boxes to reflect the visible columns.
     */
    private void checkVisibleColumnButtons()
    {
        for (int i = 0; i < selectedVisibleCols.length; i++)
        {
            columnChkArray[i].setSelection(selectedVisibleCols[i]);
        }
    }
}
