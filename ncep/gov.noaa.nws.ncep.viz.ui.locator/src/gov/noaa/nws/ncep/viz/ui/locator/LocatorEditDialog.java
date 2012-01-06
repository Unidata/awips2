package gov.noaa.nws.ncep.viz.ui.locator;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.ui.locator.resource.Locator;
import gov.noaa.nws.ncep.viz.ui.locator.resource.*;
import gov.noaa.nws.ncep.viz.ui.locator.util.LocatorInfo;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;


/**
 * This class displays the Locator Edit dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/2009  	64        	M. Li    	Initial creation 
 * 03/2009		65			M. Li		Add multiple locators
 * 11/2009      138         Greg Hull   location Options
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */

public class LocatorEditDialog extends AbstractEditResourceAttrsDialog{//Dialog {

	/**
     * Dialog shell;
     */
    private Shell shell;
    
    /**
     * SWT display component.
     */
    private Display display;

    /**
     * Label font.
     */
    private Font labelFont;
    
    private Combo roundingCombo, distanceCombo;

	private Combo positionCombo,locatorCombo;	
	private Button positionBtn;	
	public static final java.util.Map<Integer, Boolean> POS_ONOFF_MAP= new java.util.HashMap<Integer, Boolean>(); 
	public static final int NUM_OF_LOCATORS = 5;
	private static final String[] POS_ITEMS = new String[NUM_OF_LOCATORS];
	public static final Locator[] LOCATOR_LIST = new Locator[NUM_OF_LOCATORS];
	private static final int COMBO_INIT_SELECTION = 0; 

	private Combo directionCombo, displayCombo, latlonCombo;
    
    private Label roundingLabel, distanceLabel, directionLabel;
    
    private Label displayLabel, latlonLabel;
    
    private List<Locator> locatorList = null;
    
    private List<Locator> updateLocList;
    
    private int selectedLocIndex;
    
    private Label locatorNameLabel;
	/*
	 * pre-load all needed here    
	 */
	static {
		
		for(int i=0; i<NUM_OF_LOCATORS; i++){
			POS_ONOFF_MAP.put(i,false);
			LOCATOR_LIST[i] = null;
			POS_ITEMS[i] = ""+(i+1);
		}
			
		POS_ONOFF_MAP.put(0,true);
	//	POS_ONOFF_MAP.put(1,false);
	//	POS_ONOFF_MAP.put(2,false);
	//	POS_ONOFF_MAP.put(3,false);
	//	POS_ONOFF_MAP.put(4,false);
		
		LOCATOR_LIST[0] = LocatorInfo.NAME_LOCATOR_MAP.get("LATLON");
	//	LOCATOR_LIST[1] = null;
	//	LOCATOR_LIST[2] = null;
	//	LOCATOR_LIST[3] = null;
	//	LOCATOR_LIST[4] = null;
		//TODO: List<Locator> ?
		
	}

	//----------------------------created for pop-up this dialog: 2011-04-18    
	public LocatorEditDialog(Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
		super(parentShell,r,apply); 
	rd = ((LocatorResourceData)r).getLocatorResource();//2011-05-02	TODO
	//if(list==null){ 	list = LocatorInfo.testList; }
	
		if (locatorList == null) locatorList = LocatorInfo.testList;;//list;
		
		if (updateLocList == null) {
			updateLocList = new ArrayList<Locator>();
			for (Locator loc : LocatorInfo.testList){//list) {
				updateLocList.add(loc.copy());
			}
		}
	//	selectedLocIndex = index;
	//	System.out.println("________________list.size(): "+list.size());
	}
    
	//----------------------------created for pop-up this dialog: 2011-04-12    
    public LocatorEditDialog(Shell parentShell, INatlCntrsResourceData r,List<Locator> list, int index) {
    	super(parentShell,r,true); 
    	rd = ((LocatorResourceData)r).getLocatorResource();//2011-05-02	    	
    	if(list==null){ 	list = LocatorInfo.testList; }

		if (locatorList == null) locatorList = list;
		
		if (updateLocList == null) {
			updateLocList = new ArrayList<Locator>();
			for (Locator loc : list) {
				updateLocList.add(loc.copy());
			}
		}
		selectedLocIndex = index;
//    	System.out.println("___________list.size(): "+list.size());
    }
    
	public LocatorEditDialog(Shell parentShell, List<Locator> list, int index) {
		super(parentShell,null,true);
		// TODO Auto-generated constructor stub
		if (locatorList == null) locatorList = list;
			
		if (updateLocList == null) {
			updateLocList = new ArrayList<Locator>();
			for (Locator loc : list) {
				updateLocList.add(loc.copy());
			}
		}
		selectedLocIndex = index;
	}

    /**
     * Open method used to display the locator edit dialog. *
     * 
     * @return Return object (can be null).
     */
    public Object open() {

//rd = ;//LocatorInfo.getActiveDisplay(); 
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("Locator Edit Window");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        shell.setLayout(mainLayout);
        Rectangle rect = parent.getBounds();
        Point pt = new Point(0, rect.y + rect.height - 100);
        // align the bottom left of the dialog with the bottom left of Cave.
        shell.setLocation( rect.x+20, rect.y + rect.height - shell.getSize().y-40 );

        labelFont = new Font(shell.getDisplay(), "Arial", 10, SWT.BOLD);

        // Initialize all of the menus, controls, and layouts
        initializeComponents();
        updateLocator(false);
        setCombosEnabled(DISP_POSONOFF_MAP.get(rd).get(COMBO_INIT_SELECTION));//POS_ONOFF_MAP.get(COMBO_INIT_SELECTION));    // positionCombo's first always ON    

        // Pack the controls and open the display.
        shell.pack();
        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        labelFont.dispose();

        return null;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
    	initDisplayData(rd);
    	createLocatorPostionControls();    	addSeparator();	
		createLocatorSelectionControls();
    	addSeparator();
    	createRoundingControls();
    	createDistanceUnitControls();
    	createDirectionUnitControls();
    	createDisplayControls();
    	createLatLonUnitControls();		//also in setCombosEnabled() & updateLocator() for latlon parts
    	addSeparator();
    	createCloseButton();
   	
    }
    
    /**
     * Create the locatorSelection controls.
     */
    private void createLocatorSelectionControls() {
        Composite locatorSelectionComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        locatorSelectionComp.setLayout(gl);

        GridData gd = new GridData(150, SWT.DEFAULT);
        /*Label label*/locatorNameLabel = new Label(locatorSelectionComp, SWT.NONE);
        /*label*/locatorNameLabel.setText("LOCATOR");
        /*label*/locatorNameLabel.setLayoutData(gd);

        GridData comboGd = new GridData();
        ///*
        locatorCombo = new Combo(locatorSelectionComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        locatorCombo.setLayoutData(comboGd);
        
        for(Locator itm : locatorList){
        	locatorCombo.add(itm.getLocatorName());
        }
        locatorCombo.select(0); 
        int i = positionCombo.getSelectionIndex(); 
        if(getLocators()[i]!=null)
        	locatorCombo.setText(getLocators()[i].getLocatorName().trim());//*/       
//        locatorNameLabel = new Label(locatorSelectionComp, SWT.BORDER|SWT.SHADOW_OUT);
//        Font font = new Font(shell.getDisplay(), "Arial", 11, SWT.BOLD);
//        locatorNameLabel.setFont(font);
//        locatorNameLabel.setLayoutData(new GridData(120, 20));
        
        locatorCombo.addSelectionListener(new SelectionListener() {
        	@Override	
        	public void widgetDefaultSelected(SelectionEvent e) {	}

        	@Override	
        	public void widgetSelected(SelectionEvent e) {
        		updateLocatorList();					
					//		int i = positionCombo.getSelectionIndex();
					//System.out.println("--------------: listener: ");				
					//		if(POS_ONOFF_MAP.get(i))
					//		LOCATOR_LIST[i] = LocatorInfo.NAME_LOCATOR_MAP.get(locatorCombo.getText().trim());
					//		else			LOCATOR_LIST[i] = null;
        		boolean flag = "LATLON".equalsIgnoreCase(locatorCombo.getText().trim());
        		setOptionsCombosEnabled( ! flag);
        		setLatlonComboEnabled(flag);
        		setOptionsCombos();		
        	}        	
        });        
       
    }

    /**
     * Create the distance rounding controls.
     */
    private void createRoundingControls() {
        Composite roundingComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        roundingComp.setLayout(gl);

        GridData gd = new GridData(150, SWT.DEFAULT);
        roundingLabel = new Label(roundingComp, SWT.NONE);
        roundingLabel.setText("ROUND TO NEAREST");
        roundingLabel.setLayoutData(gd);

        GridData comboGd = new GridData();
        roundingCombo = new Combo(roundingComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        roundingCombo.setLayoutData(comboGd);
        roundingCombo.setItems(LocatorTool.ROUNDING_OPTIONS);
        
        roundingCombo.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				if (roundingCombo.getSelectionIndex() != -1) {
					updateLocList.get(selectedLocIndex).getDisplayOptions().setRoundingToNearest(
							Integer.valueOf(roundingCombo.getText()));
					getLocators()[positionCombo.getSelectionIndex()].getDisplayOptions().setRoundingToNearest(Integer.valueOf(roundingCombo.getText()));
				}
			}        	
        });
    }

    /**
     * Create the distance unit controls.
     */
    private void createDistanceUnitControls() {
        Composite distanceComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        distanceComp.setLayout(gl);

        GridData gd = new GridData(150, SWT.DEFAULT);
        distanceLabel = new Label(distanceComp, SWT.NONE);
        distanceLabel.setText("DISTANCE UNIT");
        distanceLabel.setLayoutData(gd);

        GridData comboGd = new GridData();
        distanceCombo = new Combo(distanceComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        distanceCombo.setLayoutData(comboGd);
        distanceCombo.setItems(LocatorTool.DISTANCEUNIT_OPTIONS);
       
        distanceCombo.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				if (distanceCombo.getSelectionIndex() != -1) {
					updateLocList.get(selectedLocIndex).getDisplayOptions().setDistanceUnit(distanceCombo.getText());
					getLocators()[positionCombo.getSelectionIndex()].getDisplayOptions().setDistanceUnit(distanceCombo.getText());
				}
			}        	
        });
    }

    /**
     * Create the direction unit controls.
     */
    private void createDirectionUnitControls() {
        Composite directionComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        directionComp.setLayout(gl);

        GridData gd = new GridData(150, SWT.DEFAULT);
        directionLabel = new Label(directionComp, SWT.NONE);
        directionLabel.setText("DIRECTION UNIT");
        directionLabel.setLayoutData(gd);

        GridData comboGd = new GridData();
        directionCombo = new Combo(directionComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        directionCombo.setLayoutData(comboGd);
        directionCombo.setItems(LocatorTool.DIRECTIONUNIT_OPTIONS);
     
        directionCombo.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				if (directionCombo.getSelectionIndex() != -1) {
					updateLocList.get(selectedLocIndex).getDisplayOptions().setDirectionUnit(directionCombo.getText());
					getLocators()[positionCombo.getSelectionIndex()].getDisplayOptions().setDirectionUnit(directionCombo.getText());
				}
			}        	
        });
    }

    /**
     * Create the station display option controls.
     */
    private void createDisplayControls() {
        Composite displayComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        displayComp.setLayout(gl);

        GridData gd = new GridData(150, SWT.DEFAULT);
        displayLabel = new Label(displayComp, SWT.NONE);
        displayLabel.setText("DISPLAY");
        displayLabel.setLayoutData(gd);

        GridData comboGd = new GridData();
        displayCombo = new Combo(displayComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        displayCombo.setLayoutData(comboGd);
        displayCombo.setItems(LocatorTool.STATIONDISPLAY_OPTIONS);

        //setCombos();
       
        // selection action
        displayCombo.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				if (displayCombo.getSelectionIndex() != -1) {
					updateLocList.get(selectedLocIndex).getDisplayOptions().setDisplayAttribute(displayCombo.getText());
					
					getLocators()[positionCombo.getSelectionIndex()].getDisplayOptions().setDisplayAttribute(displayCombo.getText());
				}
			}
        	
        });
    }

    /**
     * Create the latlon unit controls.
     */
    private void createLatLonUnitControls() {
        Composite latlonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        latlonComp.setLayout(gl);

        GridData gd = new GridData(150, SWT.DEFAULT);
        latlonLabel = new Label(latlonComp, SWT.NONE);
        latlonLabel.setText("LATLON UNIT");
        latlonLabel.setLayoutData(gd);

        GridData comboGd = new GridData();
        latlonCombo = new Combo(latlonComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        latlonCombo.setLayoutData(comboGd);
        latlonCombo.setItems(LocatorTool.LATLONUNIT_OPTIONS);
        latlonCombo.addSelectionListener(new SelectionListener() {
        	@Override
        	public void widgetDefaultSelected(SelectionEvent e) {
        	// TODO Auto-generated method stub
        	
        	}        	

			@Override
			public void widgetSelected(SelectionEvent e) {
				if (latlonCombo.getSelectionIndex() != -1) {
					getLocators()[positionCombo.getSelectionIndex()].getDisplayOptions().setLatLonUnit(latlonCombo.getText());					updateLocList.get(selectedLocIndex).getDisplayOptions().setLatLonUnit(latlonCombo.getText());
				}
			}
        	
        } );
    }

 
    /**
     * Add a horizontal separator to the display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }
    
    /**
     * Create the default and close button.
     */
    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        
        Button defaultBtn = new Button(centeredComp, SWT.NONE);
        defaultBtn.setText("Default");
        defaultBtn.setLayoutData(gd);
        defaultBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                //updateLocator(true); // disabled 2011-05-09
            }
        });
        
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("OK");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	// NOT used anymore
//            	MultipleLocatorsDialog multipleLocators = LocatorDisplay.getInstance().getMultipleLocators();
//            	if (multipleLocators != null) multipleLocators.setUpdateLocList(updateLocList);
                shell.dispose();
            }
        });
    }
    
    private void updateLocator(boolean isDefault) {
    	Locator currentLoc = new Locator();
    	if (isDefault) {
    		currentLoc = locatorList.get(selectedLocIndex).copy();
    		updateLocList.set(selectedLocIndex, locatorList.get(selectedLocIndex).copy());
    	} else {
    		currentLoc = updateLocList.get(selectedLocIndex).copy();
    	}
    	
    	// set locator name
    	if (!isDefault) {
//    		locatorNameLabel.setText(currentLoc.getLocatorName());
//    		locatorNameLabel.pack();
    	}	
    	
    	// set rounding to nearest
    	
    	if (currentLoc.getDisplayOptions().getRoundingToNearest() != null) {
    		int roundingIndex = 0;
    		for (int ii = 0; ii < LocatorTool.ROUNDING_OPTIONS.length; ii++) {
    			if (currentLoc.getDisplayOptions().getRoundingToNearest().toString().
    					equalsIgnoreCase(LocatorTool.ROUNDING_OPTIONS[ii])) {
    				roundingIndex = ii;
    				break;
    			}
    		}
    		
    		if (!roundingLabel.isEnabled()) roundingLabel.setEnabled(true);
    		if (!roundingCombo.isEnabled()) roundingCombo.setEnabled(true);
    		roundingCombo.select(roundingIndex);
    	} 
    	else {
    		if (roundingLabel.isEnabled()) roundingLabel.setEnabled(false);
    		if (roundingCombo.isEnabled())roundingCombo.setEnabled(false);
    	}
    	
    	// set distance unit
    	if (currentLoc.getDisplayOptions().getDistanceUnit() != null) {
    		int distanceIndex = 0;
    		for (int ii = 0; ii < LocatorTool.DISTANCEUNIT_OPTIONS.length; ii++) {
    			if (currentLoc.getDisplayOptions().getDistanceUnit().
    					equalsIgnoreCase(LocatorTool.DISTANCEUNIT_OPTIONS[ii])) {
    				distanceIndex = ii;
    				break;
    			}
    		}
    		
    		if (!distanceLabel.isEnabled()) distanceLabel.setEnabled(true);
    		if (!distanceCombo.isEnabled()) distanceCombo.setEnabled(true);
    		distanceCombo.select(distanceIndex);
    	}
    	else {
    		if (distanceLabel.isEnabled()) distanceLabel.setEnabled(false);
    		if (distanceCombo.isEnabled()) distanceCombo.setEnabled(false);
    	}
    	
    	// set direction unit
    	if (currentLoc.getDisplayOptions().getDirectionUnit() != null) {
    		int directionIndex = 0;
    		for (int ii = 0; ii < LocatorTool.DIRECTIONUNIT_OPTIONS.length; ii++) {
    			if (currentLoc.getDisplayOptions().getDirectionUnit().
    					equalsIgnoreCase(LocatorTool.DIRECTIONUNIT_OPTIONS[ii])) {
    				directionIndex = ii;
    				break;
    			}
    		}
    		
    		if (!directionLabel.isEnabled()) directionLabel.setEnabled(true);
    		if (!directionCombo.isEnabled()) directionCombo.setEnabled(true);
    		directionCombo.select(directionIndex);
    	}
    	else {
    		if (directionLabel.isEnabled()) directionLabel.setEnabled(false);
    		if (directionCombo.isEnabled()) directionCombo.setEnabled(false);
    	}
    	
    	// set display attribute
    	if (locatorList.get(selectedLocIndex).getAttributeName() == null &&
    			locatorList.get(selectedLocIndex).getAttributeID() == null)  {
    		displayLabel.setEnabled(false);
    		displayCombo.setEnabled(false);	
    	}
    	else if	(locatorList.get(selectedLocIndex).getAttributeName() == null ||
    			locatorList.get(selectedLocIndex).getAttributeID() == null) {
    		if (locatorList.get(selectedLocIndex).getAttributeName() == null){
    			displayCombo.select(1);
    		} 
    		else {
    			displayCombo.select(0);
    		}
    		
    		displayLabel.setEnabled(true);
    		displayCombo.setEnabled(false);
    		
    	}
    	else {
    	
    		if (currentLoc.getDisplayOptions().getDisplayAttribute() != null) {
    			int displayIndex = 0;
    			for (int ii = 0; ii < LocatorTool.STATIONDISPLAY_OPTIONS.length; ii++) {
    				if (currentLoc.getDisplayOptions().getDisplayAttribute().
    						equalsIgnoreCase(LocatorTool.STATIONDISPLAY_OPTIONS[ii])) {
    					displayIndex = ii;
    					break;
    				}
    			}
    		
    			if (!displayLabel.isEnabled()) displayLabel.setEnabled(true);
    			if (!displayCombo.isEnabled()) displayCombo.setEnabled(true);
    			displayCombo.select(displayIndex);
    		}
    		else {
    			if (displayLabel.isEnabled()) displayLabel.setEnabled(false);
    			if (displayCombo.isEnabled()) displayCombo.setEnabled(false);
    		}
    		
    	}
    	
    	// set lat/lon unit
    	if (currentLoc.getDisplayOptions().getLatLonUnit() != null) {
    		int latlonIndex = 0;
    		for (int ii = 0; ii < LocatorTool.LATLONUNIT_OPTIONS.length; ii++) {
    			if (currentLoc.getDisplayOptions().getLatLonUnit().
    					equalsIgnoreCase(LocatorTool.LATLONUNIT_OPTIONS[ii])) {
    				latlonIndex = ii;
    				break;
    			}
    		}
    		
    		if (!latlonLabel.isEnabled()) latlonLabel.setEnabled(true);
    		if (!latlonCombo.isEnabled()) latlonCombo.setEnabled(true);
    		latlonCombo.select(latlonIndex);
    	}
    	else {
    		if (latlonLabel.isEnabled()) latlonLabel.setEnabled(false);
    		if (latlonCombo.isEnabled()) latlonCombo.setEnabled(false);
    	}
    	
    }
    
    public List<Locator> getUpdateLocList() {
    	return updateLocList;
    }
    
    public void setLocatorSelection(int index) {
    	selectedLocIndex = index;
    	if (isOpen()) updateLocator(false);
    }
    
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }

    public void setLocatorDfltsList( List<Locator> dfltsList ) {
    	locatorList = dfltsList;
    }

	@Override
	public Composite createDialog(Composite topComp) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void initWidgets() {
		// TODO Auto-generated method stub
		
	}
	
	/**
     * Create the locatorPostion controls.
     */
    private void createLocatorPostionControls() {
    	
        Composite locatorPositionComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        locatorPositionComp.setLayout(gl);

        GridData gd = new GridData(150, SWT.DEFAULT);
        Label label = new Label(locatorPositionComp, SWT.NONE);
        label.setText("POSITION ");
        label.setLayoutData(gd);

        GridData comboGd = new GridData();
        
        
        positionCombo = new Combo(locatorPositionComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        positionCombo.setLayoutData(comboGd);        
        positionCombo.setItems(POS_ITEMS);
        positionCombo.select(COMBO_INIT_SELECTION);
        
        positionBtn = new Button(locatorPositionComp, SWT.CHECK);
        positionBtn.setText(" ON/OFF ");      
        positionBtn.setSelection(DISP_POSONOFF_MAP.get(rd).get(COMBO_INIT_SELECTION));
        
        //set combos for the first position
        //setCombosEnabled(POS_ONOFF_MAP.get(initSelection));
        
        /*
         * listener for position combo
         */
        positionCombo.addSelectionListener(new SelectionListener() {
			
        	@Override	public void widgetDefaultSelected(SelectionEvent e) {	}

			@Override	public void widgetSelected(SelectionEvent e) {
							
				int i = positionCombo.getSelectionIndex();
				boolean preState = DISP_POSONOFF_MAP.get(rd).get(i);
				positionBtn.setSelection(preState);
				setCombosEnabled(preState);
				
				if(preState && locatorCombo.isEnabled()){
					locatorCombo.setText(getLocators()[i].getLocatorName().trim());
					setOptionsCombos();
				}
				setCombosEnabled(preState);	//	locatorCombo text is needed after re-set		
			}        	
        });
        
        /*
         * listener for button
         */
        positionBtn.addSelectionListener(new SelectionListener() {
			
        	@Override	public void widgetDefaultSelected(SelectionEvent e) {	}

			@Override	public void widgetSelected(SelectionEvent e) {
														
				DISP_POSONOFF_MAP.get(rd)/*POS_ONOFF_MAP*/.put(positionCombo.getSelectionIndex(), positionBtn.getSelection());				
				setCombosEnabled(positionBtn.getSelection());//true);
				updateLocatorList();					
			}        	
        });
    }
    
    private void setCombosEnabled(boolean enabled){
//    	this.locatorCombo.setEnabled(enabled);
//    	this.locatorNameLabel.setEnabled(enabled);       	
    	
//	    this.directionCombo.setEnabled(enabled);
//	    this.directionLabel.setEnabled(enabled);
//	    	
//	    this.displayCombo.setEnabled(enabled);
//	    this.displayLabel.setEnabled(enabled);
//	    	
//	    this.distanceCombo.setEnabled(enabled);
//	    this.distanceLabel.setEnabled(enabled);
//	    	
//	    this.roundingCombo.setEnabled(enabled);
//	    this.roundingLabel.setEnabled(enabled); 
	    
    	setLocatorComboEnabled(enabled);
    	
    	if(enabled){
    		boolean flag = "LATLON".equalsIgnoreCase(locatorCombo.getText().trim() );
	    	setOptionsCombosEnabled( ! flag);
	    	setLatlonComboEnabled(flag);
    	}else{	    	
		    setOptionsCombosEnabled(enabled);
		    setLatlonComboEnabled(enabled);
    	}    	
  	
    	//this.latlonCombo.setEnabled(enabled);    	this.latlonLabel.setEnabled(enabled);
    }
    
    private void updateLocatorList(){
    	if(locatorCombo.isEnabled()){
    		int i = positionCombo.getSelectionIndex();
			getLocators()[i] = LocatorInfo.NAME_LOCATOR_MAP.get(locatorCombo.getText().trim());
		}
			
    }
    
    private void setLocatorComboEnabled(boolean enabled){
    	this.locatorCombo.setEnabled(enabled);
    	this.locatorNameLabel.setEnabled(enabled);     	
    }
    
    //Every time the dialog opens, the position Combo always shows 1
    //and the Locator its represent is always enabled
    private void setOptionsCombosEnabled(boolean enabled){
	    this.directionCombo.setEnabled(enabled);
	    this.directionLabel.setEnabled(enabled);
	    	
	    this.displayCombo.setEnabled(enabled);
	    this.displayLabel.setEnabled(enabled);
	    	
	    this.distanceCombo.setEnabled(enabled);
	    this.distanceLabel.setEnabled(enabled);
	    	
	    this.roundingCombo.setEnabled(enabled);
	    this.roundingLabel.setEnabled(enabled);    
	    
	    if(enabled) 
	    	setOptionsCombos();  
    }
    
    private void setLatlonComboEnabled(boolean enabled){
    	this.latlonCombo.setEnabled(enabled);    	
    	this.latlonLabel.setEnabled(enabled);
    	
    	if(enabled)
    		setLatlonCombos();
    	    		
    }
    
    private void setOptionsCombos(){
    	if(getLocators()[positionCombo.getSelectionIndex()] == null)
    		return;
    	
    	gov.noaa.nws.ncep.viz.ui.locator.resource.DisplayOptions disop = 
    		getLocators()[positionCombo.getSelectionIndex()].getDisplayOptions();
    	
    	String rounding = ""+disop.getRoundingToNearest();
    	if(Arrays.asList(LocatorTool.ROUNDING_OPTIONS).contains(rounding))
    		roundingCombo.setText(rounding);
    	
    	String disUnit = disop.getDistanceUnit();
    	if(disUnit != null &&  Arrays.asList(LocatorTool.DISTANCEUNIT_OPTIONS).contains(disUnit.trim()) )
    		distanceCombo.setText(disUnit);
    	
    	String dirUnit = disop.getDirectionUnit();
    	if(dirUnit != null &&  Arrays.asList(LocatorTool.DIRECTIONUNIT_OPTIONS).contains(dirUnit.trim()) )
    		directionCombo.setText(dirUnit);
    	
    	String attr = disop.getDisplayAttribute();        
    	if(attr != null && Arrays.asList(LocatorTool.STATIONDISPLAY_OPTIONS).contains(attr.trim()) ) 
    		displayCombo.setText(attr.trim());
    }
    
    private void setLatlonCombos(){
    	if(getLocators()[positionCombo.getSelectionIndex()] == null)
    		return;
    	
    	gov.noaa.nws.ncep.viz.ui.locator.resource.DisplayOptions disop = 
    		getLocators()[positionCombo.getSelectionIndex()].getDisplayOptions();
    	
    	String latlon = disop.getLatLonUnit();
    	if(Arrays.asList(LocatorTool.LATLONUNIT_OPTIONS).contains(latlon))
    		this.latlonCombo.setText(latlon);
    }
    
    public static final List<LocatorResource> DISP_LIST = new ArrayList<LocatorResource>();
    
    public static final Map<LocatorResource,Map<Integer, Boolean> > DISP_POSONOFF_MAP= new HashMap<LocatorResource,Map<Integer, Boolean> >(); 

    private static final Map<LocatorResource, String[]> DISP_POSITEMS = new HashMap<LocatorResource, String[]>();

    public static final Map<LocatorResource, Locator[]> DISP_LOCATORLIST_MAP = new HashMap<LocatorResource, Locator[]>();
    
    private LocatorResource rd = null;   //initialized first line in open()
    
    public static void initDisplayData(LocatorResource rd){
    	   	
    	if( ! DISP_LIST.contains(rd)){    		
    		
    		Map<Integer, Boolean> onoff = new HashMap<Integer, Boolean>();
    		Locator[] locators = new Locator[NUM_OF_LOCATORS];
    		String[]  items = new String[NUM_OF_LOCATORS];    		
    		
    		for(int i=0; i<NUM_OF_LOCATORS; i++){
    			onoff.put(i,false);
    			locators[i] = null;
    			items[i] = ""+(i+1);
    		}    			
    		onoff.put(0,true);    		
    		locators[0] = LocatorInfo.NAME_LOCATOR_MAP.get("LATLON");
    		
    		DISP_LIST.add(rd);
    		DISP_POSONOFF_MAP.put(rd, onoff);
    		DISP_LOCATORLIST_MAP.put(rd, locators);
    		DISP_POSITEMS.put(rd,items);
    		
    	}    	
    	
    }
    
    public Locator[] getLocators(){
    	return DISP_LOCATORLIST_MAP.get(rd);
    }

}
