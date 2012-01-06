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

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;

/**
 * Dialog for deleting stations.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2009            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class DeleteStationDlg extends Dialog
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
     * Return value when the shell is disposed.
     */
    private Boolean returnValue = false;
    
    /**
     * Dialog title.
     */
    private String dialogTitle;
    
    /**
     * Station list control.
     */
    private List stationList;
    
    /**
     * Control font.
     */
    private Font controlFont;
    
    /**
     * Constructor.
     * @param parent Parent shell.
     * @param appName Application name.
     */
    public DeleteStationDlg(Shell parent, CommonConfig.AppName appName)
    {    
        super(parent, 0);
        
        dialogTitle = appName.toString() + ": Delete a Newly Entered Station";
    }
    
    /**
     * Open method used to display the dialog.
     * @return True/False.
     */
    public Object open()
    {        
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText(dialogTitle);
        
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);
      
        // Initialize all of the controls and layouts
        initializeComponents();
        
        populate();
        
        shell.pack();
        
        shell.open();
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch())
            {
                display.sleep();
            }
        }
        
        controlFont.dispose();
        
        return returnValue;
    }
    
    /**
     * Initialize the components on the display.
     */
    private void initializeComponents()
    {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        
        createListControl();
        
        createBottomButtons();
    }
    
    /**
     * Create the list control.
     */
    private void createListControl()
    {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        
        Label stationLbl = new Label(listComp, SWT.NONE);
        stationLbl.setText("Available Stations to delete:");
        
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 350;
        gd.heightHint = 250;
        stationList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        stationList.setFont(controlFont);
        stationList.setLayoutData(gd);
    }
    
    /**
     * Create the Delete Station and Close buttons.
     */
    private void createBottomButtons()
    {        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);
        
        gd = new GridData(120, SWT.DEFAULT);
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete Station");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent event)
            {
                deleteSelected();
            }
        });
        
        gd = new GridData(120, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent event)
            {
                shell.dispose();
            }
        });
    }
    
    private void populate() {
        FogMonitorConfigurationManager configMan = FogMonitorConfigurationManager.getInstance();
        
        ArrayList<String> addedStations = configMan.getAddedStations();
        stationList.setItems(addedStations.toArray(new String[addedStations.size()]));
    }
    
    private void deleteSelected() {
        FogMonitorConfigurationManager configMan = FogMonitorConfigurationManager.getInstance();
        
        String selection = stationList.getItem(stationList.getSelectionIndex());
        
        configMan.removeStation(selection);
        stationList.remove(stationList.getSelectionIndex());
    }
}
