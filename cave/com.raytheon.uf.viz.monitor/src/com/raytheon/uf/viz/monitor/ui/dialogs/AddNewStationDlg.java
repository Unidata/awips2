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

import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.xml.StationIdXML;

/**
 * 
 * Add New Station dialog.
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
public class AddNewStationDlg extends Dialog
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
     * METAR radio button.
     */
    private Button metarRdo;
    
    /**
     * Maritime button.
     */
    private Button maritimeRdo;
    
    /**
     * Mesonet button;
     */
    private Button mesonetRdo;
    
    /**
     * Station label.
     */
    private Label stationLbl;
    
    /**
     * Station text control.
     */
    private Text stationTF;
    
    private String area;
    
    private INewZoneStnAction macDlg; 
            
    /**
     * Constructor.
     * @param parent Parent shell.
     * @param appName Application name.
     */
    public AddNewStationDlg(Shell parent, CommonConfig.AppName appName, String area, INewZoneStnAction macDlg)
    {    
        super(parent, 0);
        
        this.appName = appName;       
        this.area = area;
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
        shell.setText(appName.toString() + ": Add a New Station");
        
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
        createTopLabelRadioControls();
        
        createTextControls();
        
        createBottomButtons();
        
        setStationLabel();
    }
    
    /**
     * Create the top radio controls.
     */
    private void createTopLabelRadioControls()
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
        
        metarRdo = new Button(radioComp, SWT.RADIO);
        metarRdo.setText("Metar");
        metarRdo.setSelection(true);
        metarRdo.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent event)
            {
                setStationLabel();
            }
        });
        
        if (appName != CommonConfig.AppName.SNOW)
        {
            maritimeRdo = new Button(radioComp, SWT.RADIO);
            maritimeRdo.setText("Maritime");
            maritimeRdo.addSelectionListener(new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent event)
                {
                    setStationLabel();
                }
            });
        }
        
        mesonetRdo = new Button(radioComp, SWT.RADIO);
        mesonetRdo.setText("Mesonet");
        mesonetRdo.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent event)
            {
                setStationLabel();
            }
        });
    }
    
    /**
     * Create the text controls.
     */
    private void createTextControls()
    {
        Composite textComp = new Composite(shell, SWT.NONE);
        textComp.setLayout(new GridLayout(1, true));
        textComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        
        stationLbl = new Label(textComp, SWT.NONE);
        stationLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        
        stationTF = new Text(textComp, SWT.BORDER);
        stationTF.setLayoutData(new GridData(250, SWT.DEFAULT));
    }
    
    /**
     * Create the bottom buttons.
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
                String stationType = StationIdXML.METAR;
                FogMonitorConfigurationManager configManager = FogMonitorConfigurationManager.getInstance();
                if (metarRdo.getSelection()) {
                    stationType = StationIdXML.METAR;
                } else if (mesonetRdo.getSelection()) {
                    String s = stationTF.getText();
                    s.substring(s.indexOf("#"), s.length() - 1);
                    stationType = s.toUpperCase();
                    // TODO need to verifyu the stationType exists.
                    // was in SSmesonetStationInfo.txt in AWIPS1.
                } else {
                    stationType = StationIdXML.MARITIME;
                }
                
                configManager.addStation(area, stationTF.getText(), stationType, false);
                
                /**
                 * for DR #7854: add new station to Monitor Area Config GUI 
                 */
                handleAddNewStation();
                
                //shell.dispose();
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
    
    /**
     * Set the station label.
     */
    private void setStationLabel()
    {
        if (mesonetRdo.getSelection() == true)
        {
            stationLbl.setText("StationID#Provider:");
        }
        else
        {
            stationLbl.setText("StationID:");
        }
    }

	private void handleAddNewStation() {
		
		if ( !isValidStation() ) {
			displayInputErrorMsg("Invalid Station ID entered: Please enter a valid Station ID for the selected Station Type");
			return;
		}
		String stn = stationTF.getText();
		if ( metarRdo.getSelection() ) {
			stn = stn + "#METAR";
		} else if ( maritimeRdo.getSelection() ) {
			stn = stn + "#MARITIME";
		} else {
			//Mesonet
		}
		if ( macDlg.isExistingStation(stn) ) {
			displayInputErrorMsg("The Station, " + stn + ", is already in your Monitoring Area or among your Additional Stations");
			return;
		}
		macDlg.addNewStationAction(stn);
	}
	
	private boolean isValidStation() {
        
		String stnId = stationTF.getText(); 
		if ( stnId.contains("#") && !mesonetRdo.getSelection() ) {
			return false;
		}
		String catalogtypePhrase = ""; 
		if ( metarRdo.getSelection() ) {
			catalogtypePhrase = "catalogtype = 1"; // METAR
		} else if ( maritimeRdo.getSelection() ) {
			catalogtypePhrase = "catalogtype = 33 or catalogtype = 32"; // MARITIME
		} else {
			//TODO need code for handling Mesonet
		}
		try {
			String sql = "select stationid, catalogtype from common_obs_spatial where ( " + catalogtypePhrase + " ) and stationid = '" + stnId + "'";
			ISpatialQuery sq = SpatialQueryFactory.create();
			Object[] results = sq.dbRequest(sql, "metadata");
			if ( results == null ) {
				return false;
			}
			if ( results.length != 2 ) {
				return false;
			}
			return true;
			
			/**
			 * TODO: need to add code for handling Mesonet station type 
			 */
			
		} catch ( Exception e ) {
	        e.printStackTrace();
	    }
	
    	return false;
	}
	
	private void displayInputErrorMsg(String msg) {
        MessageBox messageBox = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
        messageBox.setText("Invalid input");
        messageBox.setMessage(msg);
        messageBox.open();
	}
}
