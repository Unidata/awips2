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
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.xml.AreaIdXML.ZoneType;

/**
 * Dialog to display the control for adding a new zone.
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
public class AddNewZoneDlg extends Dialog
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
     * Application name.
     */
    private CommonConfig.AppName appName;
    
    /**
     * Marine zone radio button.
     */
    private Button marineZoneRdo;
    
    /**
     * County radio button.
     */
    private Button countyRdo;
    
    /**
     * ID text control.
     */
    private Text idTF;
    
    /**
     * Centroid latitude text control.
     */
    private Text centroidLatTF;
    
    /**
     * Centroid longitude text control.
     */
    private Text centroidLonTF;
    
    private Button marineRdo;
    
    private Button nonMarineRdo;
    
    private INewZoneStnAction macDlg; 
    
    /**
     * Constructor.
     * @param parent Parent shell.
     * @param appName Application name.
     */
    public AddNewZoneDlg(Shell parent, CommonConfig.AppName appName, INewZoneStnAction macDlg)
    {    
        super(parent, 0);
        
        this.appName = appName;
        this.macDlg = macDlg;
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
        shell.setText("Add a New Zone");
        
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
        
        return returnValue;
    }
    
    /**
     * Initialize the components on the display.
     */
    private void initializeComponents()
    {
        if (appName != CommonConfig.AppName.SNOW)
        {
            createTopZoneCountyControls();
        }
        else
        {
            createCountyZoneLabels();
        }
        
        createTextControls();
        createBottomButtons();
    }
    
    /**
     * Create the top zone county controls.
     */
    private void createTopZoneCountyControls()
    {
        Composite topComp = new Composite(shell, SWT.NONE);
        topComp.setLayout(new GridLayout(2, true));
        topComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        
        /*
         * Add the label.
         */
        Label topLbl = new Label(topComp, SWT.RIGHT);
        topLbl.setText("Please type in a new: ");
        topLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
        
        /*
         * Add the radio controls.
         */
        Composite radioComp = new Composite(topComp, SWT.NONE);
        radioComp.setLayout(new GridLayout(1, false));
        
        marineZoneRdo = new Button(radioComp, SWT.RADIO);
        marineZoneRdo.setText("Marine Zone");
        marineZoneRdo.setSelection(true);
        
        countyRdo = new Button(radioComp, SWT.RADIO);
        countyRdo.setText("County");
    }
    
    /**
     * Create the county zone labels.
     */
    private void createCountyZoneLabels()
    {
        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(new GridLayout(1, false));
        labelComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        
        Label topLbl = new Label(labelComp, SWT.CENTER);
        topLbl.setText("Please type in a new county/zone");
        topLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
    }
    
    /**
     * Create the text controls.
     */
    private void createTextControls()
    {
        Composite textComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        textComp.setLayout(gl);
        textComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label idLbl = new Label(textComp, SWT.RIGHT);
        idLbl.setText("Id (e.g. AMZ080):");
        idLbl.setLayoutData(gd);
        
        idTF = new Text(textComp, SWT.BORDER);
        idTF.setLayoutData(new GridData(120, SWT.DEFAULT));
        
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label centroidLatLbl = new Label(textComp, SWT.RIGHT);
        centroidLatLbl.setText("Centroid Lat (e.g. 29.198):");
        centroidLatLbl.setLayoutData(gd);
        
        centroidLatTF = new Text(textComp, SWT.BORDER);
        centroidLatTF.setLayoutData(new GridData(120, SWT.DEFAULT));
        
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label centroidLonLbl = new Label(textComp, SWT.RIGHT);
        centroidLonLbl.setText("Centroid Lon (e.g. -71.75):");
        centroidLonLbl.setLayoutData(gd);
        
        centroidLonTF = new Text(textComp, SWT.BORDER);
        centroidLonTF.setLayoutData(new GridData(120, SWT.DEFAULT));
        

        // gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        // marineRdo = new Button(textComp, SWT.RADIO);
        // marineRdo.setLayoutData(gd);
        // marineRdo.setSelection(false);
        // marineRdo.setText("Marine Station");
        //
        // gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        // nonMarineRdo = new Button(textComp, SWT.RADIO);
        // nonMarineRdo.setLayoutData(gd);
        // nonMarineRdo.setSelection(true);
        // nonMarineRdo.setText("Non-Marine Station");
        
        /*
         * Create the Use Decimal label.
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Label useDecimalLbl = new Label(textComp, SWT.CENTER);
        useDecimalLbl.setText("Use Decimal Degrees, West Longitude negative");
        useDecimalLbl.setLayoutData(gd);
    }
    
    /**
     * Create the bottom Add and Close buttons.
     */
    private void createBottomButtons()
    {        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);
        
        // Add a separator label.
        Label sepLbl = new Label(mainButtonComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);
        
        gd = new GridData(100, SWT.DEFAULT);
        Button addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent event)
            {
            	String latString = centroidLatTF.getText();
                String lonString = centroidLonTF.getText();

                if (((latString != null) && (latString.length() > 0)) && 
                        (lonString != null) && (lonString.length() > 0)) {
                    double lat;
                    double lon;
                    try {
                        lat = Double.parseDouble(latString);
                        lon = Double.parseDouble(lonString);

                        handleAddNewAction(lat, lon);
                    } catch (NumberFormatException e) {
                        MessageBox messageBox = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
                        messageBox.setText("Invalid Lat/Lon");
                        messageBox.setMessage("Invalid Lat/Lon entered.  Please enter correctly formatted Lat/Lon values");
                        messageBox.open();
                    }

                    //shell.dispose();
                } else {
                    MessageBox messageBox = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
                    messageBox.setText("Invalid Lat/Lon");
                    messageBox.setMessage("Invalid Lat/Lon entered.  Please enter correctly formatted Lat/Lon values");
                    messageBox.open();
                }
            }
        });
        
        gd = new GridData(100, SWT.DEFAULT);
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
    
    private void handleAddNewAction(double lat, double lon) {
        String areaId = idTF.getText();
        if ( areaId.length() != 6 || (areaId.charAt(2) != 'C' && areaId.charAt(2) != 'Z') ) {
        	displayInputErrorMsg("Invalid Area ID entered. Please enter a correctly formatted Area ID"); 
        	return;
        }
        if ( macDlg.isExistingZone(areaId) ) {
        	displayInputErrorMsg("The Area ID, " + areaId + ", is already in your Monitoring Area or among your Additional Zones"); 
        	return;
        }
        
        ZoneType type = ZoneType.REGULAR;
        if (marineRdo.getSelection()) {
            type = ZoneType.MARITIME;
        }

        MonitorConfigurationManager configManager = null; 
        if ( appName == CommonConfig.AppName.FOG ) {
        	configManager = FogMonitorConfigurationManager.getInstance();
        } else if ( appName == CommonConfig.AppName.SAFESEAS ) {
        	configManager = SSMonitorConfigurationManager.getInstance();
        } else if ( appName == CommonConfig.AppName.SNOW ) {
        	configManager = SnowMonitorConfigurationManager.getInstance();
        } else {
        	return;
        }
        	configManager.addArea(areaId, lat, lon, type, false);
            macDlg.addNewZoneAction(areaId, centroidLatTF.getText(), centroidLonTF.getText());
                
    }

	private void displayInputErrorMsg(String msg) {
        MessageBox messageBox = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
        messageBox.setText("Invalid input");
        messageBox.setMessage(msg);
        messageBox.open();
	}
}
