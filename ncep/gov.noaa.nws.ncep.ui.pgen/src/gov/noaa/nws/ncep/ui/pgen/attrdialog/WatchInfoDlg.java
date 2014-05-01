/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.WatchInfoDlg
 * 
 * 20 September 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.common.staticdata.SPCCounty;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Singleton attribute dialog for a watch info dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09		#159		B. Yin   	Initial Creation.
 * 11/09		#159		B. Yin		Added all widgets
 * 12/09		#159		B. Yin		Added functions for specifications and county list.
 * 03/10		#159		B. Yin		Added FormatWatch, WatchStatus and WatchCancel
 * 02/11		?			B. Yin		Removed WatchCancel	
 * 11/13		?			B. Yin		Disable fmtBtn and status buttons when del/adding cnty.
 * 12/13		TTR800		B. Yin		Disable two status buttons before watch is issued		
 * 12/13		TTR904		B. Yin		Set the list title font and made the font smaller			
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class WatchInfoDlg  extends CaveJFACEDialog  {
	
	//instance of watch info dialog
	static private WatchInfoDlg INSTANCE = null;
	static String specLbl = "LAT       LON      Anchor Relative   Vor Relative     ";
	static String cntyLbl = " UGC  State   County Name       Lat/Lon     FIPS  WFO ";
	static String inactLbl = "=====Inactive counties INSIDE the watch area=====\n";
	static String actLbl = "=====Active counties OUTSIDE the watch area=====\n";
	
	//county table
	//static private List<County> allCounties;
	

    //instance of the FormatWatch Dialog
    private WatchFormatDlg fmtDlg;
    
    //instance of the WatchStatus Dialog
    private WatchStatusDlg statusDlg;
    
    //Text font for the county list
    static private Font txtFt;
    
    //Font for CWA
    static private Font cwaFt;
    
    //Font for CWA buttons
    static private Font cwaBtnFt;
		
	//instance of the watch box attribute dialog
	private WatchBoxAttrDlg wbDlg;
	
	//top level container of all widgets
	private Composite top;
	
	//label of the text area(Specifications/QC/County list)
	private Label textLabel;
	
	//text area for Specifications/QC/County List
	private Text text;
	
	//Specification radio button
	private Button specBtn;
	
	//QC radio button
	private Button qcBtn;
	
	//County List button
	private Button countyListBtn;
	private Button createBtn;
	private Button addBtn;
	private Button clearBtn;
	private Button clusteringOnBtn;
	private Button clusteringOffBtn;
	private Button wccBtn;
	private Button fmtBtn;
	private Button statusLineBtn;
	private Button statusBtn;
	
	//county lock On radio button
	private Button lockOnBtn;
	
	//county lock off radio button
	private Button lockOffBtn;
	
	//WFO group
	private Composite wfoGrp;
	
	//WFO text 
	private Text wfo;
	
	//State List group, holding the "States" label and state pane.
	private Composite stGrp;
	
	//State list pane, holding all state check boxes
	private Composite stPane;
	
	//State check boxes
	private List<Button> stBtns;
	
	//CWA group, holding the "CWA" label and the cwa pane.
	private Composite cwaGrp;
	
	//CWA pane, holding CWAs  
	private Composite cwaPane;
	
	//Bottom buttons
	private Composite wccGrp;
	
	//List of CWAs.Each CwaComposite contains a label, and in/out buttons
	private List<CwaComposite>  cwaComp;

	/**
	 * Protected constructor
	 * @param parentShell
	 */
	protected WatchInfoDlg(Shell parentShell, WatchBoxAttrDlg wbDlg) {
		
		super(parentShell);
		this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE );
		
		this.wbDlg = wbDlg;
		stBtns = new ArrayList<Button>();
		cwaComp = new ArrayList<CwaComposite>();
	}
	
	/**
	 * Return the instance of the singleton.
	 * @param parShell
	 * @return
	 */
	public static WatchInfoDlg getInstance( Shell parShell, WatchBoxAttrDlg wbDlg){

		if ( INSTANCE == null ){

			INSTANCE = new WatchInfoDlg( parShell, wbDlg );
			
		}

		return INSTANCE;

	} 
	
	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
			top = (Composite) super.createDialogArea(parent);

	        // Create the main layout for the shell.
	        GridLayout mainLayout = new GridLayout(1, false);
	        mainLayout.marginHeight = 3;
	        mainLayout.marginWidth = 3;
	        mainLayout.verticalSpacing = 3;
	        top.setLayout(mainLayout);

	        // Initialize all of the menus, controls, and layouts
	        this.initializeComponents();

	        return top;
	        
	}
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 */
	protected void initializeComponents() {
	
		// set title
        this.getShell().setText("Watch Specifications and County List");
        
        //create "Specification", "QC counties" and "County List" radio buttons.
        createRadioButtons();
        
        AttrDlg.addSeparator(top);
        
        //create text area and labels.
        if ( txtFt == null ) this.createFonts();
		
        textLabel = new Label(top, SWT.NONE);
        textLabel.setFont(txtFt);
		textLabel.setText(specLbl);

		text = new Text( top,  SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);                        
		text.setLayoutData( new GridData( 503, 100) );
		text.setEditable( false );
		text.setFont( txtFt );

        AttrDlg.addSeparator(top);

        // create "Create" "Add/DEl", and other buttons for counties 
        createCountyCtrls();
        
        AttrDlg.addSeparator(top);
        
        // create lists of WFOs
        wfoGrp = new Composite(top, SWT.NONE);
        
		GridLayout wfoGl = new GridLayout(2, false);
		wfoGl.verticalSpacing = 0;
		wfoGl.marginHeight = 2;
		wfoGl.marginBottom = 0;
		
		wfoGrp.setLayout(wfoGl);
		
        Label wfoLabel = new Label(wfoGrp, SWT.NONE);
    	wfoLabel.setText("WFOs:");
    	wfo = new Text(wfoGrp, SWT.MULTI);
    	wfo.setEditable(false);
		wfo.setFont( cwaFt );

		//create list of states with check boxes
    	stGrp = new Composite(top, SWT.NONE);
 		stGrp.setLayout(wfoGl); 	
    	Label stLabel = new Label(stGrp, SWT.NONE);
     	stLabel.setText("States:");
     	
     	stPane = new Composite(stGrp, SWT.NONE);
     	GridLayout stGl = new GridLayout(8, false);
     	stGl.verticalSpacing = 0;
     	stGl.marginHeight = 3;
     	stPane.setLayout(stGl);
     	
     	//create list of CWAs and in/out buttons
    	cwaGrp = new Composite(top, SWT.NONE);
 		cwaGrp.setLayout(wfoGl); 	
     	Label cwaLabel = new Label(cwaGrp, SWT.NONE);
     	cwaLabel.setText("CWAs:");
     	
     	cwaPane = new Composite(cwaGrp, SWT.NONE);
     	GridLayout cwaGl = new GridLayout(4, false);
     	cwaGl.verticalSpacing = 0;
     	cwaGl.marginHeight = 1;
     	cwaPane.setLayout(cwaGl);
     
        AttrDlg.addSeparator(top);

        // create the Anchor points toggle button
//        toggleAnchorsBtn = new Button(top, SWT.PUSH);
//        toggleAnchorsBtn.setText("Toggle Anchor Points");
//        AttrDlg.addSeparator(top);
        
        wccGrp = new Composite(top, SWT.None);
    	GridLayout wccGl = new GridLayout(4, false);
     	wccGl.marginLeft = 15;
        wccGrp.setLayout(wccGl);
        
        wccBtn = new Button(wccGrp, SWT.PUSH);
        wccBtn.setText("WCC/WCL");
		wccBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {

				boolean openCoordDlg = true;
				if ( wbDlg.getWatchBox().getCountyList().isEmpty() ){
					String msg = "Watch has no counties! Set default counties?";
				
					MessageDialog confirmDlg = new MessageDialog( 
			        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
			        		"Confirm Delete", null, msg,
			        		MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
			        confirmDlg.open();
			        
			        if ( confirmDlg.getReturnCode() == MessageDialog.OK ) {
			    		createCounties();
			        }
			        else {
			        	openCoordDlg = false; 
			        }
				}
				
				if ( openCoordDlg ){
					WatchCoordDlg coordDlg = WatchCoordDlg.getInstance(WatchInfoDlg.this.getParentShell(),wbDlg);
					coordDlg.setBlockOnOpen(false);
					coordDlg.open();
				}
			}
			
		});

		//Wathcformat WatchStatus WatchCancel butttons
		//AttrDlg.addSeparator(top);
		//Composite fmtPane = new Composite(top, SWT.NONE);
     	//GridLayout fmtGl = new GridLayout(2, false);
     	//fmtGl.marginLeft = 160;
     	//fmtPane.setLayout(fmtGl);
     	
     	//Create 'WatchFormat' button
     	fmtBtn = new Button(wccGrp, SWT.PUSH);
        fmtBtn.setText("Watch Format");
    	fmtBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {

				boolean openFmtDlg = true;

				if (wbDlg.getWatchBox().getCountyList().isEmpty() ){
					String msg = "Watch has no counties! Set default counties?";

					MessageDialog confirmDlg = new MessageDialog( 
							PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
							"Set Watch Counties", null, msg,
							MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
					confirmDlg.open();
					if ( confirmDlg.getReturnCode() == MessageDialog.OK ) {
						createCounties();
					}
					else {
						openFmtDlg = false;
					}
				}

				if ( openFmtDlg ) {
					fmtDlg = WatchFormatDlg.getInstance(WatchInfoDlg.this.getParentShell(), wbDlg);
					fmtDlg.setWatchBox(wbDlg.getWatchBox());
					fmtDlg.setBlockOnOpen(false);
					fmtDlg.open();
				}

			}

    	});
		
		statusLineBtn = new Button(wccGrp, SWT.PUSH);
        statusLineBtn.setText("Add Status Line");
        statusLineBtn.setEnabled(false);
       	statusLineBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				if ( wbDlg.getWatchBox().getIssueFlag()  == 0 ){
					String msg = "Please issue the watch first!";
					
					MessageDialog confirmDlg = new MessageDialog( 
			        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
			        		"Warning!", null, msg,
			        		MessageDialog.INFORMATION, new String[]{"OK"}, 0);
			        confirmDlg.open();
				}
				else {
					PgenUtil.setDrawingStatusLineMode(wbDlg.getWatchBox());
				}
			}
			
		});
				
		//create 'WatchStatus' button
        statusBtn = new Button(wccGrp, SWT.PUSH);
        statusBtn.setText("Watch Status"); 
        statusBtn.setEnabled(false);
    	statusBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				if (wbDlg.getWatchBox().getIssueFlag()  == 0 ){
					String msg = "Watch has not been issued!";

					MessageDialog infoDlg = new MessageDialog( 
							PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
							"Warning!", null, msg,
							MessageDialog.INFORMATION, new String[]{"OK"}, 0);
					infoDlg.open();
				}
				else {
					statusDlg = new WatchStatusDlg(WatchInfoDlg.this.getParentShell(), wbDlg.getWatchBox());
					statusDlg.setBlockOnOpen(false);
					statusDlg.open();
				}
			}
			
		});
    	
    	//Create 'WatchCancel' button
   /*     Button cancelBtn = new Button(fmtPane, SWT.PUSH);
        cancelBtn.setText("Watch Cancel");
        cancelBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
					String msg = String.format("Are you sure you want to cancel watch %1$04d ?",
									wbDlg.getWatchBox().getWatchNumber());
					
					MessageDialog confirmDlg = new MessageDialog( 
							PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
							"Confirm Watch Cancel", null, msg,
							MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
					confirmDlg.open();
					
					//Set the issue flag is 'Ok' is pressed.
					if ( confirmDlg.getReturnCode() == MessageDialog.OK ) {
						wbDlg.getWatchBox().setIssueFlag(-1);
						String fname = String.format("WW%1$04d.xml", wbDlg.getWatchBox().getWatchNumber());
						wbDlg.getWatchBox().saveToFile(fname);
					}
			}
			
		});
	 */
	}
	
	/**
	 * create "Specification", "QC counties" and "County List" radio buttons.
	 */
	private void createRadioButtons(){
		
		Composite btnGrp = new Composite(top, SWT.NONE);
		GridLayout btnGl = new GridLayout(3, false);
		btnGrp.setLayout(btnGl);

		specBtn = new Button(btnGrp, SWT.RADIO);
		specBtn.setText("Specifications");
		specBtn.setSelection(true);
		
		specBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				textLabel.setText(specLbl);
		    	text.setText(wbDlg.getWatchBox().getSpec());
			}
			
			
		});
		
		qcBtn = new Button(btnGrp, SWT.RADIO);
		qcBtn.setText("QC Counties");
		qcBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
		    	textLabel.setText(cntyLbl);
		   
		    	text.setText(inactLbl + wbDlg.getWatchBox().formatCountyInfo(wbDlg.getWatchBox().getInactiveCountiesInWB())+ "\n" 
						 + actLbl + wbDlg.getWatchBox().formatCountyInfo(wbDlg.getWatchBox().getActiveCountiesOutsideWB()));
			}
			
			
		});	
		
		countyListBtn = new Button(btnGrp, SWT.RADIO);
		countyListBtn.setText("County List");
		
		countyListBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
		    	textLabel.setText(cntyLbl);
		    	text.setText(wbDlg.getWatchBox().formatCountyInfo(wbDlg.getWatchBox().getCountyList()));

			}
			
			
		});		
	}

	/**
	 *  create "Create" "Add/DEl", and other buttons for counties
	 */
	private void createCountyCtrls(){
		
		//create county group including create/add/clear and clustering buttons
		Composite cntyGrp = new Composite(top, SWT.None);
		GridLayout gl = new GridLayout(5, false);
		cntyGrp.setLayout(gl);
		
		Label cLabel = new Label(cntyGrp, SWT.NONE);
	    cLabel.setText("Counties:");
	    
	    createBtn = new Button(cntyGrp, SWT.PUSH);
	    createBtn.setText("Create");
	    
	    createBtn.addListener(SWT.MouseDown, new Listener(){
	    	
	    	@Override
	    	public void handleEvent(Event event) {
	    		createCounties();

	    	}
	    });
	    
	    addBtn = new Button(cntyGrp, SWT.PUSH);
	    addBtn.setText("Add/Del");
	    addBtn.addListener(SWT.MouseDown, new Listener(){
	    	
	    	@Override
			public void handleEvent(Event event) {
	    		WatchInfoDlg.this.enableAllButtons(false);
	    		wbDlg.enableDspBtn(false);
	    		wbDlg.buttonBar.setEnabled(false);
	    		wbDlg.getWbTool().setAddDelCntyHandler();
	    	}
	    });
   
	    clearBtn = new Button(cntyGrp, SWT.PUSH);
	    clearBtn.setText("Clear");
	    
	    clearBtn.addListener(SWT.MouseDown, new Listener(){
	    	
	    	@Override
			public void handleEvent(Event event) {
	    		WatchBox newWb = (WatchBox)wbDlg.getWatchBox().copy();
	    		newWb.clearCntyList();
	    		wbDlg.drawingLayer.replaceElement(wbDlg.getWatchBox(), newWb);
	    		wbDlg.setWatchBox(newWb);
	    		wbDlg.drawingLayer.setSelected(newWb);

	    		clearCwaPane();
	    		setStatesWFOs();
	    		wbDlg.mapEditor.refresh();
	    	}
	    });

	    //Off/On radio buttons for Clustering.
		Composite grp11 = new Composite(cntyGrp, SWT.NONE);
		GridLayout gl1 = new GridLayout(3, false);
		grp11.setLayout(gl1);
		
		Label clstrLabel = new Label(grp11, SWT.NONE);
		clstrLabel.setText("Clustering:");
		
		clusteringOffBtn = new Button(grp11, SWT.RADIO);
		clusteringOffBtn.setText("Off");
		
		clusteringOnBtn = new Button(grp11, SWT.RADIO);
		clusteringOnBtn.setText("On");
		clusteringOnBtn.setSelection(true);
		
		//create a group that contains county lock radio buttons
		Composite lockGrp = new Composite(top, SWT.NONE);

		GridLayout gl2 = new GridLayout(3, false);
		lockGrp.setLayout(gl2);
		gl2.marginHeight = 2;
		
		Label lockLabel = new Label(lockGrp, SWT.NONE);
		lockLabel.setText("County Lock:");
		
		lockOffBtn = new Button(lockGrp, SWT.RADIO);
		lockOffBtn.setText("Off");
		lockOffBtn.setSelection(true);
		lockOffBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				createBtn.setEnabled(true);
				addBtn.setEnabled(true);
				clearBtn.setEnabled(true);
				clusteringOnBtn.setEnabled(true);
				clusteringOffBtn.setEnabled(true);
				if (stBtns != null ){
					for ( Button btn : stBtns ){
						btn.setEnabled(true);
					}
				}
				enableCWAComp(true);
			}
		});
		
		lockOnBtn = new Button(lockGrp, SWT.RADIO);
		lockOnBtn.setText("On");
		
		lockOnBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				createBtn.setEnabled(false);
				addBtn.setEnabled(false);
				clearBtn.setEnabled(false);
				clusteringOnBtn.setEnabled(false);
				clusteringOffBtn.setEnabled(false);
				if (stBtns != null ){
					for ( Button btn : stBtns ){
						btn.setEnabled(false);
					}
				}
				enableCWAComp(false);
			}
		});
	}
	
	@Override
	/**
	 * No Ok/Cancel buttons.
	 */
	public Control createButtonBar(Composite parent){
		return null;
	}
	
	@Override
	/**
	 * Set the location of the dialog
	 */
	public int open(){

		if ( this.getShell() == null ){
			this.create();
		}
		
		if ( wbDlg != null && wbDlg.getWatchBox() != null && wbDlg.getWatchBox().getIssueFlag() != 0 ){
			this.statusBtn.setEnabled(true);
			this.statusLineBtn.setEnabled(true);
			this.createBtn.setEnabled(false);
		}
		
		
   	    this.getShell().setLocation(this.getShell().getParent().getLocation());
   	    return super.open();
		
	}

	/**
	 * Set the text field for Specifications/County QC/County List
	 */
	private void setTextField(){
		if ( specBtn.getSelection()){
			text.setText(wbDlg.getWatchBox().getSpec());
		}
		else if (countyListBtn.getSelection()){
			text.setText(wbDlg.getWatchBox().formatCountyInfo(wbDlg.getWatchBox().getCountyList()));
		}
		else if ( qcBtn.getSelection() ){
			text.setText(inactLbl + wbDlg.getWatchBox().formatCountyInfo(wbDlg.getWatchBox().getInactiveCountiesInWB())+ "\n" 
						 + actLbl + wbDlg.getWatchBox().formatCountyInfo(wbDlg.getWatchBox().getActiveCountiesOutsideWB()));

		}
	}
	
	/**
	 * Set the text field, WFO list and state check boxes
	 */
	public void setStatesWFOs(){
		setTextField();
		wfo.setText(formatWfoStr(wbDlg.getWatchBox().getWFOs()));
		createStateChkBoxes(wbDlg.getWatchBox().getStates());
		
		stPane.layout();
		stGrp.pack(true);
		stGrp.layout();
		
		cwaPane.layout();
		cwaGrp.pack(true);
		cwaGrp.layout();
		
		wccGrp.layout();
		wccGrp.pack(true);
		wccGrp.layout();
		
		top.pack(true);
		top.layout();
		
		WatchInfoDlg.this.getShell().pack(true);
		WatchInfoDlg.this.getShell().layout();
		WatchInfoDlg.this.getShell().redraw(0, 0, WatchInfoDlg.this.getShell().getSize().x,
				WatchInfoDlg.this.getShell().getSize().y, true);
	}

	/**
	 * Clear all CWAs
	 */
	public void clearCwaPane(){
		Iterator<CwaComposite> it = cwaComp.iterator();
		while(it.hasNext() ){
			CwaComposite cwa = it.next();
			cwa.comp.dispose();
			it.remove();
		}

		cwaPane.layout();
		cwaGrp.pack(true);
		cwaGrp.layout();
	}

	/**
	 * create CWA list and in/out buttons
	 * @param cwas
	 */
	public void createCWAs(List<String> cwas ){

		if (cwas != null && !cwas.isEmpty() ){
	

			for ( final String aCWA : cwas ){

				cwaComp.add(new CwaComposite(aCWA));
				cwaPane.layout();
				cwaPane.pack(true);

			}
			
			setCwaBtns();
		}

		cwaPane.layout();
		cwaGrp.pack(true);
		cwaGrp.layout();
	}
	
	/**
	 * Create state list and check boxes
	 * @param states
	 */
	private void createStateChkBoxes(List<String> states ){

		//delete state check boxes that are not in the list
		if ( stBtns != null ){
			Iterator<Button> it = stBtns.iterator();
			while(it.hasNext() ){
				Button btn = it.next();
				if ( !states.contains(btn.getText())){
					btn.dispose();
					it.remove();

				}
			}
		}

		//create state check boxes for the watch box
		if ( states != null && !states.isEmpty()) {
			for ( String st : states ){

				//find out if the state check box is already in place
				boolean notIn = true;
				for ( Button btn : stBtns ){
					if ( st.equalsIgnoreCase(btn.getText())){
						notIn = false;
						break;
					}
				}

				//if not in, create the state check box
				if ( notIn ){
					Button stBtn = new Button(stPane, SWT.CHECK);
					stBtn.setText(st);
					stBtn.setSelection(true);

					//add listener to refresh WFO/CWA and county list 
					stBtn.addSelectionListener( new SelectionListener(){

						@Override
						public void widgetDefaultSelected(SelectionEvent e) {
							// TODO Auto-generated method stub

						}

						@Override
						public void widgetSelected(SelectionEvent e) {
							WatchBox newWb = (WatchBox)wbDlg.getWatchBox().copy();
							newWb.setCountyList(wbDlg.getWatchBox().getCountyList());
							newWb.removeState(((Button)e.widget).getText());
							wbDlg.drawingLayer.replaceElement(wbDlg.getWatchBox(), newWb);
							wbDlg.setWatchBox(newWb);
				    		wbDlg.drawingLayer.setSelected(newWb);

							setStatesWFOs();	
							setCwaBtns();
							wbDlg.mapEditor.refresh();

						}

					});

					stBtns.add(stBtn);
					stPane.layout();
					stPane.pack(true);
				}

			}
		}
		stPane.layout();
		stGrp.pack(true);
		stGrp.layout();
	}
	
	@Override
	public boolean close(){
		
		//State check boxes are disposed when the dialog is closed.
		//So they need to be cleared. If not, an exception would occur
		//when they are used in createStateChkBoxes().
		stBtns.clear();
		if ( fmtDlg != null ) fmtDlg.close();
		WatchCoordDlg.getInstance(WatchInfoDlg.this.getParentShell(),wbDlg).close();
		if ( wbDlg != null) {
			if (wbDlg.getWbTool() != null )  wbDlg.getWbTool().resetMouseHandler();
			wbDlg.enableDspBtn(true);
		}
		
		return super.close();
	}
	
	/**
	 * Format the string of WFO list
	 * @param wfos
	 * @return
	 */
	static public String formatWfoStr(List<String> wfos){
		
		//initialize the string
		String wfoStr = "";
		for ( int ii = 0; ii < 42; ii++ ){
			wfoStr +=" ";
		}
		
		if ( wfos != null && !wfos.isEmpty()){
			
			wfoStr ="";
			int nWfo = 0;
			for(String aWfo : wfos ){

				if ( aWfo != null && !wfoStr.contains(aWfo)){
					
					//one line contains 7 WFOs at most
					if ( nWfo != 0 && nWfo%7 == 0 ) wfoStr += "\n";
					wfoStr += "...";
					wfoStr += aWfo;
					nWfo++;
				}

			}
		}

		return wfoStr;
	}
	
	/**
	 * Enable/disable CWA in/out buttons
	 * @param flag
	 */
	private void enableCWAComp( boolean flag ){
		for ( CwaComposite cwa : cwaComp ){
			cwa.comp.setEnabled(flag);
			cwa.lbl.setEnabled(flag);
			cwa.inBtn.setEnabled(flag);
			cwa.outBtn.setEnabled(flag);
		}
	}
	
	/**
	 * Check if the county lock radio button is set.
	 * @return true if county lock is set
	 */
	public boolean isCountyLock(){
		return lockOnBtn.getSelection();
	}
	
	/**
	 * Check if the county lock radio button is set.
	 * @return true if county lock is set
	 */
	public boolean isClusteringOn(){
		return this.clusteringOnBtn.getSelection();
	}
	
	/**
	 * Go through all CWAs in the list and check if all counties of each cwa
	 *  are in the watch box. If yes, set the IN radio button. If all counties of 
	 *  a CWA are not in the watch box, set the OUT radio button. Otherwise, do
	 *  not set either button.
	 */
	public void setCwaBtns(){
		List<SPCCounty> cntyInWb = wbDlg.getWatchBox().getCountyList();
		for ( CwaComposite cwa : cwaComp){
			
			//get number of counties of a CWA, which are inside of the watch box
			int countiesIn = getCountiesOfCwa(cwa.lbl.getText(), cntyInWb);
			
			if ( countiesIn == 0 ){
				cwa.outBtn.setSelection(true);
				cwa.inBtn.setSelection(false);

			}
			else {
				//get the total county number of the CWA
				int totalCounties = getCountiesOfCwa(cwa.lbl.getText(), PgenStaticDataProvider.getProvider().getSPCCounties());
				if ( totalCounties == countiesIn ){
					cwa.inBtn.setSelection(true);
					cwa.outBtn.setSelection(false);

				}
				else {
					cwa.inBtn.setSelection(false);
					cwa.outBtn.setSelection(false);
				}
			}
		}
	}
	
	/**
	 * Get total number of counties of the input CWA, which are in the list
	 * @param cwa
	 * @param counties
	 * @return - number of counties
	 */
	private int getCountiesOfCwa(String cwa, List<SPCCounty> counties){
		int total = 0;
		for ( SPCCounty cnty : counties ){
			if (cnty.getWfo() != null && cwa.equalsIgnoreCase(cnty.getWfo())){
				total++;
			}
		}
		return total;
	}
	
	/**
	 * A class that hold a CWA composite, which contains a CWA label,
	 * and IN/OUT radio buttons.
	 * @author bingfan
	 *
	 */
	private class CwaComposite{
		
		private Composite comp;
		
		//CWA label 
		private Label lbl;
		
		// IN/OUT radio buttons
		private Button inBtn;
		private Button outBtn;
		
		/**
		 * private constructor
		 * @param cwa
		 * @param cwaFt - label font
		 * @param btnFt - button font
		 */
		private CwaComposite(final String cwa){
			
			//create the composite holding the label and the radio buttons
			comp = new Composite(cwaPane, SWT.BORDER);
			
			GridLayout cwaGl = new GridLayout(2, false);
			cwaGl.marginHeight = 2;
			cwaGl.marginWidth = 4;
			comp.setLayout(cwaGl);
			
			//create the radio button group
			Composite ioBtnComp = new Composite(comp, SWT.NONE);
			
			GridLayout btnGl = new GridLayout(1, false);
			btnGl.marginHeight = 0;
			ioBtnComp.setLayout(btnGl);
			
			inBtn = new Button(ioBtnComp, SWT.RADIO);
			inBtn.setFont(cwaBtnFt);
			inBtn.setText("IN");
			
			inBtn.addSelectionListener(new SelectionListener(){

				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public void widgetSelected(SelectionEvent e) {
					WatchBox newWb = (WatchBox)wbDlg.getWatchBox().copy();
					newWb.setCountyList(wbDlg.getWatchBox().getCountyList());
					newWb.addCwa(cwa);
					wbDlg.drawingLayer.replaceElement(wbDlg.getWatchBox(), newWb);
					wbDlg.setWatchBox(newWb);
		    		wbDlg.drawingLayer.setSelected(newWb);

					setStatesWFOs();
					wbDlg.mapEditor.refresh();
				}
				
			});
			
			outBtn = new Button(ioBtnComp, SWT.RADIO);
			outBtn.setFont(cwaBtnFt);
			outBtn.setText("OUT");
			
			//create the CWA label
			lbl = new Label(comp, SWT.NONE);
			lbl.setText(cwa);
			lbl.setFont(cwaFt);
			
			outBtn.addSelectionListener(new SelectionListener(){

				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public void widgetSelected(SelectionEvent e) {
					WatchBox newWb = (WatchBox)wbDlg.getWatchBox().copy();
					newWb.setCountyList(wbDlg.getWatchBox().getCountyList());
					newWb.removeCwa(cwa);
					wbDlg.drawingLayer.replaceElement(wbDlg.getWatchBox(), newWb);
					wbDlg.setWatchBox(newWb);
		    		wbDlg.drawingLayer.setSelected(newWb);

					setStatesWFOs();
					wbDlg.mapEditor.refresh();
				}
				
			});
		}
	}

	/**
	 * Enable/disable all widgets in the dialog
	 * @param flag
	 */
	public void enableAllButtons( boolean flag ){
		
		if ( wbDlg != null && wbDlg.getWatchBox() != null && wbDlg.getWatchBox().getIssueFlag() != 0 ){
			//for issued watch, disable "create" button
			this.statusBtn.setEnabled(flag);
			this.statusLineBtn.setEnabled(flag);
			this.createBtn.setEnabled(false);
		}
		else {
			//for not issued watch, disable 'add status line' button and 'watch status' button.
			createBtn.setEnabled(flag);
			statusBtn.setEnabled(false);
			statusLineBtn.setEnabled(false);
		}
		
		addBtn.setEnabled(flag);
		clearBtn.setEnabled(flag);
		clusteringOnBtn.setEnabled(flag);
		clusteringOffBtn.setEnabled(flag);
		lockOnBtn.setEnabled(flag);
		lockOffBtn.setEnabled(flag);
		specBtn.setEnabled(flag);
		qcBtn.setEnabled(flag);
		countyListBtn.setEnabled(flag);
//		toggleAnchorsBtn.setEnabled(flag);
		wccBtn.setEnabled(flag);
		fmtBtn.setEnabled(flag);
	
		
		if (stBtns != null ){
			for ( Button btn : stBtns ){
				btn.setEnabled(flag);
			}
		}
		enableCWAComp(flag);
	}

 
    /**
     * Create county list
     */
    private void createCounties(){

		wbDlg.getWatchBox().clearCntyList();

		//create watch box polygon
		GeometryFactory gf = new GeometryFactory();
		Coordinate pts[] = new Coordinate[7];
		pts[0] = wbDlg.getWatchBox().getPoints().get(1);
		pts[1] = wbDlg.getWatchBox().getPoints().get(2);
		pts[2] = wbDlg.getWatchBox().getPoints().get(3);
		pts[3] = wbDlg.getWatchBox().getPoints().get(5);
		pts[4] = wbDlg.getWatchBox().getPoints().get(6);
		pts[5] = wbDlg.getWatchBox().getPoints().get(7);
		pts[6] = wbDlg.getWatchBox().getPoints().get(1);

		Polygon watchGeo = new Polygon(gf.createLinearRing(pts), null, gf);

		WatchBox newWb = (WatchBox)wbDlg.getWatchBox().copy();
	
		List<SPCCounty> counties = PgenStaticDataProvider.getProvider().getCountiesInGeometry(watchGeo);
		
		for ( SPCCounty county : counties ){
	    	if(isClusteringOn()){
	    		newWb.addClstCnty(county);
	    	}
	    	else {
	    		newWb.addCounty(county);
	    	}
		}
		
		clearCwaPane();
		top.pack(true);
		top.layout();
		
		WatchInfoDlg.this.getShell().pack(true);
		WatchInfoDlg.this.getShell().layout();
		
		newWb.update(wbDlg);
		wbDlg.drawingLayer.replaceElement(wbDlg.getWatchBox(), newWb);
		wbDlg.drawingLayer.setSelected(newWb);
		
		wbDlg.setWatchBox(newWb);
		createCWAs(newWb.getWFOs() );
		setStatesWFOs();
		
		wbDlg.mapEditor.refresh();

    }
    
    /**
     * Create static fonts used in the dialog
     */
    private void createFonts(){
		txtFt = new Font(this.getShell().getDisplay(), "Courier New", 11, SWT.NORMAL);
		cwaBtnFt = new Font(this.getShell().getDisplay(), "Courier New", 9, SWT.NORMAL);
		cwaFt = new Font(this.getShell().getDisplay(), "Courier New", 12, SWT.NORMAL);
    }
    
    public void setAddDelCountyMode(){
		enableAllButtons(false);
		wbDlg.enableDspBtn(false);
		wbDlg.buttonBar.setEnabled(false);
		if ( wbDlg.getWbTool() != null ) {
			wbDlg.getWbTool().setAddDelCntyHandler();
		}
    }
}
