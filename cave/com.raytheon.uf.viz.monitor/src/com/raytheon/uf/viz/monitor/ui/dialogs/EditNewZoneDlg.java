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
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.monitor.xml.AreaIdXML.ZoneType;

/**
 * Dialog for editing new zones.
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
public class EditNewZoneDlg extends Dialog
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
     * Zone list control.
     */
    private List zoneList;
    
    /**
     * ID text control.
     */
    private Text idTF;
    
    /**
     * Latitude text control.
     */
    private Text latTF;
    
    /**
     * Longitude text control.
     */
    private Text lonTF;
    
    /**
     * Save button.
     */
    private Button saveBtn;
    
    /**
     * Delete button.
     */
    private Button deleteBtn;
    
    /**
     * Control font.
     */
    private Font controlFont;
    
    private Button marineRdo;
    
    private Button nonMarineRdo;

    /**
     * Constructor.
     * @param parent Parent shell.
     * @param appName Application name.
     */
    public EditNewZoneDlg(Shell parent, CommonConfig.AppName appName)
    {    
        super(parent, 0);
        
        dialogTitle = appName.toString() + ": Edit a Newly Added Zone";
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
        
        createTopLabel();
        
        createListAndTextControls();
        
        createBottomLabel();
        
        createCloseButton();
    }
    
    /**
     * Create the top label.
     */
    private void createTopLabel()
    {
        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(new GridLayout(1, false));
        labelComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        
        Label topLbl = new Label(labelComp, SWT.CENTER);
        topLbl.setText("Select Zone from the list box to edit");
        topLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
    }
    
    /**
     * Create the list and text controls.
     */
    private void createListAndTextControls()
    {
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        mainComp.setLayout(gl);
        
        /*
         * Add the Zone list control
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 185;
        gd.heightHint = 220;
        zoneList = new List(mainComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        zoneList.setFont(controlFont);
        zoneList.setLayoutData(gd);
        zoneList.addSelectionListener(new SelectionAdapter() {

            /* (non-Javadoc)
             * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleZoneSelection();
            }
            
        });
        
        /*
         * Add the text controls and the Save/Delete buttons
         */
        Composite textButtonComp = new Composite(mainComp, SWT.NONE);
        textButtonComp.setLayout(new GridLayout(2, true));
        
        int textWidth = 200;
        
        gd = new GridData();
        gd.horizontalSpan = 2;
        Label idLbl = new Label(textButtonComp, SWT.NONE);
        idLbl.setText("Id:");
        idLbl.setLayoutData(gd);
        
        gd = new GridData(textWidth, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        idTF = new Text(textButtonComp, SWT.BORDER);
        idTF.setLayoutData(gd);
        
        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 5;
        Label latLbl = new Label(textButtonComp, SWT.NONE);
        latLbl.setText("Lat:");
        latLbl.setLayoutData(gd);
        
        gd = new GridData(textWidth, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        latTF = new Text(textButtonComp, SWT.BORDER);
        latTF.setLayoutData(gd);
        
        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 5;
        Label lonLbl = new Label(textButtonComp, SWT.NONE);
        lonLbl.setText("Lon:");
        lonLbl.setLayoutData(gd);
        
        gd = new GridData(textWidth, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        lonTF = new Text(textButtonComp, SWT.BORDER);
        lonTF.setLayoutData(gd);
        
        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 15;
        marineRdo = new Button(textButtonComp, SWT.RADIO);
        marineRdo.setLayoutData(gd);
        marineRdo.setSelection(false);
        marineRdo.setText("Marine Station");
        
        gd = new GridData();
        gd.horizontalSpan = 2;
        nonMarineRdo = new Button(textButtonComp, SWT.RADIO);
        nonMarineRdo.setLayoutData(gd);
        nonMarineRdo.setSelection(true);
        nonMarineRdo.setText("Non-Marine Station");

        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, true);
        gd.widthHint = 80;
        gd.verticalIndent = 5;
        saveBtn = new Button(textButtonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent event)
            {
                saveSelected();
            }
        });
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, true);
        gd.widthHint = 80;
        gd.verticalIndent = 5;
        deleteBtn = new Button(textButtonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent event)
            {
                deleteSelected();                
            }
        });
    }
    
    /**
     * Create the bottom label centroid label.
     */
    private void createBottomLabel()
    {
        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(new GridLayout(1, false));
        labelComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        
        Label bottomLbl = new Label(labelComp, SWT.NONE);
        bottomLbl.setText("Centriod Lat/Lon use Decimal Degrees, West Longitude negative");
    }
    
    /**
     * Create the Close button.
     */
    private void createCloseButton()
    {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);
        
        // Add a separator label.
        Label sepLbl = new Label(buttonComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 100;
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
        
        ArrayList<String> newList = configMan.getAddedZones();
        zoneList.setItems(newList.toArray(new String[newList.size()]));
    }
    
    private void handleZoneSelection() {
        FogMonitorConfigurationManager configMan = FogMonitorConfigurationManager.getInstance();
        String zone = zoneList.getItem(zoneList.getSelectionIndex());

        AreaIdXML areaXml = configMan.getAreaXml(zone);
        
        // DR #7343: a null areaXml causes an "Unhandled event loop exception"
        if ( areaXml != null ) {
            idTF.setText(areaXml.getAreaId());
            latTF.setText(String.valueOf(areaXml.getCLat()));
            lonTF.setText(String.valueOf(areaXml.getCLon()));
            
            if (areaXml.getType() == ZoneType.REGULAR) {
                nonMarineRdo.setSelection(true);
                marineRdo.setSelection(false);
            } else {
                nonMarineRdo.setSelection(false);
                marineRdo.setSelection(true);
            }        	
        }
    }
    
    private void deleteSelected() {
        FogMonitorConfigurationManager configMan = FogMonitorConfigurationManager.getInstance();
        String area = zoneList.getItem(zoneList.getSelectionIndex());
        zoneList.remove(zoneList.getSelectionIndex());
        
        configMan.removeArea(area);   
        idTF.setText("");
        latTF.setText("");
        lonTF.setText("");
    }
    
    private void saveSelected() {
        FogMonitorConfigurationManager configMan = FogMonitorConfigurationManager.getInstance();
        String area = zoneList.getItem(zoneList.getSelectionIndex());

        double lat = Double.parseDouble(latTF.getText());
        double lon = Double.parseDouble(lonTF.getText());
        
        ZoneType type = ZoneType.REGULAR;
        if (marineRdo.getSelection()) {
            type = ZoneType.MARITIME;
        }
        
        configMan.removeArea(area);
        
        configMan.removeAddedArea(area);
        
        configMan.addArea(idTF.getText(), lat, lon, type, false);
    }
}
