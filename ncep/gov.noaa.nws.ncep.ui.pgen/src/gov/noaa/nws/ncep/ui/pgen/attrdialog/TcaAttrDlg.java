/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.TcaAttrDlg
 * 
 * 03 June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.tca.Basin;
import gov.noaa.nws.ncep.ui.pgen.tca.BreakpointPair;
import gov.noaa.nws.ncep.ui.pgen.tca.ITca;
import gov.noaa.nws.ncep.ui.pgen.tca.StormAdvisoryNumber;
import gov.noaa.nws.ncep.ui.pgen.tca.TropicalCycloneAdvisory;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenTcaTool;

/**
 * Singleton attribute dialog for text.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09					S. Gilbert   	Initial Creation.
 * 05/12		769			B. Yin			Move the creation of UTC time and 
 *											isTimeValid, getInitialTime to PgenUtil
 *											so that Watch can also use them.
 *
 * </pre>
 * 
 * @author	S. Gilbert
 */

public class TcaAttrDlg extends AttrDlg implements ITca, SelectionListener {
	
	static TcaAttrDlg INSTANCE = null;
	private static TcaAttrInfo info=null;
    private PgenTcaTool tcaTool = null;
	
	private static final int SAVE_ID = IDialogConstants.CLIENT_ID + 7586;
	private static final String SAVE_LABEL = "Save TCA";
	private static final int CREATE_TEXT_ID = IDialogConstants.CLIENT_ID + 7587;
	private static final String CREATE_TEXT_LABEL = "Create Text";
	private static final int CANCEL_ID = IDialogConstants.CLIENT_ID + 7588;
	private static final String CANCEL_LABEL = "Cancel All";
	public static final String PGEN_TCA_ATTR_INFO   = "TCAinfo.xml";
	
	private final String APPLY = "Apply";
	private final String DELETE_SEGMENT = "Delete Segment";
	private final String NEW_SEGMENT = "New Segment";
	private final String SEV_TROPICAL_STORM = "Tropical Storm";
	private final String ADVISORY_WATCH = "Watch";
	private final String BREAKPOINT_OFFICIAL = "Official";
	private final String GEOG_TYPE_NONE = "None";
	
	private Composite top = null;
	private Combo statusItems = null;
	private Combo stormTypes = null;
	private Combo basinTypes = null;
	private Combo specialGeogTypes = null;
    private Text advisoryNumber = null;
    private Text stormNameField = null;
    private Spinner stormNumber = null;
    private Combo timeZoneTypes = null;
    private Combo severityTypes = null;
    private Combo advisoryTypes = null;
    private Combo breakpointTypes = null;
    private Text validTime = null;
    private DateTime validDate = null;
    
    private Coordinate textLocation = new Coordinate(-80.4,25.8);
    
    private Button apply;
    private Button deleteSegment;
    private Text bkpt1Field;
    private Text bkpt2Field;
    private List breakpointList = null;
    private ArrayList<TropicalCycloneAdvisory> advisories;
    
    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private TcaAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);
        
        advisories = new java.util.ArrayList<TropicalCycloneAdvisory>();

    }
	
	/**
	 * Creates a TCA attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static TcaAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new TcaAttrDlg( parShell );
				
			} catch (VizException e) {
				
				e.printStackTrace();
				
			}	
		}
		
		readOptions();
		
		return INSTANCE;
		
	} 
	
	/*
	 * Clear out advisories list each time attribute dialog is opened
	 * @see gov.noaa.nws.ncep.ui.pgen.attrDialog.AttrDlg#open()
	 */
    @Override
	public int open() {
    
    	int ret = super.open();
    	advisories.clear();
    	return ret;
    }
    
    /*
     *  Add buttons to the button bar.
     */
    @Override
    public void createButtonsForButtonBar( Composite parent ) {   
    	
    	//  used to save the current TCA element to a file
    	createButton(parent, SAVE_ID, SAVE_LABEL,	true);
    	getButton(SAVE_ID).setEnabled(false);
    	
    	//  used to create a TCV (vtec) message for the current advisory
    	createButton(parent, CREATE_TEXT_ID, CREATE_TEXT_LABEL,	true);
    	getButton(CREATE_TEXT_ID).setEnabled(false);
    	
    	//  used to cancel all current watches and warnings
    	createButton(parent, CANCEL_ID, CANCEL_LABEL,	true);
    	getButton(CANCEL_ID).setEnabled(false);
    	
    	//  close dialog button
    	createButton(parent, IDialogConstants.CLOSE_ID, IDialogConstants.CLOSE_LABEL,	true);

    }

	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
	        top = (Composite) super.createDialogArea(parent);
	        this.getShell().setText("TCA Attributes");

	        // Create the main layout for the dialog.
	        GridLayout mainLayout = new GridLayout(1, true);
	        mainLayout.marginHeight = 3;
	        mainLayout.marginWidth = 3;
	        top.setLayout(mainLayout);

	        /*
	         *  Initialize all of the Storm Information Section
	         */
			Group g1 = new Group(top,SWT.SHADOW_ETCHED_IN);
			g1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
	        createStormInfoArea(g1);
			
	        /*
	         * Initialize section that allows users to add/modify breakpoints
	         * for each advisory
	         */
			Group g2 = new Group(top,SWT.SHADOW_ETCHED_IN);
			g2.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			createBreakpointTools(g2);
			
			/*
			 * Initialize breakpoint list section
			 */
			Group g3 = new Group(top,SWT.SHADOW_ETCHED_IN);
			g3.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			createBreakpointList(g3);
			
			//top.pack();
	        return top;
	        
	}
	
	/*
	 * creates the widgets in the storm information section of the dialog
	 */
	private void createStormInfoArea(Group g1) {

        FormLayout layout = new FormLayout();
        layout.marginHeight = 3;
        layout.marginWidth = 3;
		g1.setLayout(layout);

		/*
		 * Issuing Status label
		 */
		Label statusLabel = new Label(g1,SWT.NONE);
		statusLabel.setText("Issuing Status:");
		FormData fd = new FormData();
		fd.left = new FormAttachment(0,10);
		fd.top = new FormAttachment(0,10);
		statusLabel.setLayoutData(fd);
		
		/*
		 * Issuing status pulldown
		 */
        statusItems = new Combo( g1, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getStatusList() ) {
            statusItems.add( st );
        }
        statusItems.setText(statusItems.getItem(0));
		fd = new FormData();
		fd.left = new FormAttachment(0,10);
		fd.top = new FormAttachment(statusLabel, 5, SWT.BOTTOM);
		statusItems.setLayoutData(fd);
		int idx = statusItems.indexOf("Operational");
		if ( idx != -1 ) statusItems.select(idx);
		
		/*
		 * Storm Type label
		 */
		Label typeLabel = new Label(g1,SWT.NONE);
		typeLabel.setText("Storm Type:");
		fd = new FormData();
		fd.top = new FormAttachment(0,10);
		//fd.left = new FormAttachment(stormTypes, 0, SWT.LEFT);
		//fd.bottom = new FormAttachment(stormTypes, 5, SWT.TOP);
		typeLabel.setLayoutData(fd);
		
		/*
		 * Storm Type pulldown
		 */
        stormTypes = new Combo( g1, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getTypeList() ) {
            stormTypes.add( st );
        }
        stormTypes.setText(stormTypes.getItem(0));
		fd = new FormData();
		fd.left = new FormAttachment(statusItems, 50, SWT.RIGHT);
		fd.right = new FormAttachment(100, -10);
		fd.top = new FormAttachment(typeLabel, 5, SWT.BOTTOM);
		stormTypes.setLayoutData(fd);
		
		// reposition Storm Type label
		fd = (FormData)typeLabel.getLayoutData();
		fd.left = new FormAttachment(stormTypes, 0, SWT.LEFT);
		
		/*
		 * Basin label
		 */
		Label basinLabel = new Label(g1,SWT.NONE);
		basinLabel.setText("Basin:");
		fd = new FormData();
		//fd.top = new FormAttachment(0,10);
		fd.left = new FormAttachment(0,10);
		fd.top = new FormAttachment(statusItems, 15, SWT.BOTTOM);
		basinLabel.setLayoutData(fd);
		
		/*
		 * Basin pulldown
		 */
        basinTypes = new Combo( g1, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getBasinList() ) {
            basinTypes.add( st );
        }
        basinTypes.setText(basinTypes.getItem(0));
		fd = new FormData();
		fd.left = new FormAttachment(basinLabel, 30, SWT.RIGHT);
		//fd.left = new FormAttachment(50, 0);
		fd.top = new FormAttachment(statusItems, 10, SWT.BOTTOM);
		fd.right = new FormAttachment(statusItems, 0, SWT.RIGHT);
		basinTypes.setLayoutData(fd);
		
		/*
		 * Storm Name label
		 */
		Label stormNameLabel = new Label(g1,SWT.NONE);
		stormNameLabel.setText("Name:");
		fd = new FormData();
		fd.top = new FormAttachment(stormTypes,15, SWT.BOTTOM);
		fd.left = new FormAttachment(stormTypes, 0, SWT.LEFT);
		stormNameLabel.setLayoutData(fd);
		
		/*
		 * Storm Name text field
		 */
		stormNameField = new Text(g1,SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.top = new FormAttachment(stormTypes,10, SWT.BOTTOM);
		fd.left = new FormAttachment(stormNameLabel, 10, SWT.RIGHT);
		fd.right = new FormAttachment(stormTypes, 0, SWT.RIGHT);
		stormNameField.setLayoutData(fd);
		
		/*
		 * Storm number label
		 */
		Label stormNumberLabel = new Label(g1,SWT.NONE);
		stormNumberLabel.setText("Storm#:");
		fd = new FormData();
		//fd.top = new FormAttachment(0,10);
		fd.left = new FormAttachment(0,10);
		fd.top = new FormAttachment(basinTypes, 15, SWT.BOTTOM);
		stormNumberLabel.setLayoutData(fd);
		
		/*
		 * Storm Number spinner
		 */
		stormNumber = new Spinner(g1, SWT.BORDER);
		fd = new FormData();
		fd.left = new FormAttachment(stormNumberLabel, 35, SWT.RIGHT);
		fd.top = new FormAttachment(basinTypes, 10, SWT.BOTTOM);
		fd.right = new FormAttachment(statusItems,0, SWT.RIGHT);
		stormNumber.setLayoutData(fd);
		stormNumber.setMinimum(1);
		
		/*
		 * Valid Time label
		 */
		Label validTimeLabel = new Label(g1,SWT.NONE);
		validTimeLabel.setText("Valid Time:");
		fd = new FormData();
		fd.top = new FormAttachment(stormNameField,15, SWT.BOTTOM);
		fd.left = new FormAttachment(stormTypes, 0, SWT.LEFT);
		validTimeLabel.setLayoutData(fd);

		validDate = new DateTime(g1, SWT.BORDER | SWT.DATE );
		fd = new FormData();
		fd.top = new FormAttachment(stormNameField,10, SWT.BOTTOM);
		fd.left = new FormAttachment(validTimeLabel, 10, SWT.RIGHT);
		validDate.setLayoutData(fd);
		
		/*
		validTime = new DateTime(g1, SWT.BORDER | SWT.TIME | SWT.SHORT);
		fd = new FormData();
		fd.top = new FormAttachment(stormNameField,10, SWT.BOTTOM);
		fd.left = new FormAttachment(validDate, 0, SWT.RIGHT);
		fd.right = new FormAttachment(stormTypes, 0, SWT.RIGHT);
		validTime.setLayoutData(fd);
		*/
		
		/*
		 * valid Time text field ----  REPLACED WITH DateTime WIDGETS ABOVE
		 */
	
		validTime = new Text(g1, SWT.SINGLE | SWT.BORDER | SWT.CENTER);
		fd = new FormData();
		fd.top = new FormAttachment(stormNameField,10, SWT.BOTTOM);
		fd.left = new FormAttachment(validDate, 10, SWT.RIGHT);
		//fd.right = new FormAttachment(stormTypes, 0, SWT.RIGHT);
		validTime.setLayoutData(fd);
		PgenUtil.setUTCTimeTextField(g1, validTime, Calendar.getInstance( TimeZone.getTimeZone("GMT")), 
				stormNameField, 15);
		
		/*
		 * Advisory number label
		 */
		Label advisoryNumberLabel = new Label(g1,SWT.NONE);
		advisoryNumberLabel.setText("Advisory#:");
		fd = new FormData();
		//fd.top = new FormAttachment(0,10);
		fd.left = new FormAttachment(0,10);
		fd.top = new FormAttachment(stormNumber, 15, SWT.BOTTOM);
		advisoryNumberLabel.setLayoutData(fd);

		/*
		 * Advisory number text field
		 */
		advisoryNumber = new Text(g1, SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.left = new FormAttachment(advisoryNumberLabel, 25, SWT.RIGHT);
		fd.top = new FormAttachment(stormNumber, 10, SWT.BOTTOM);
		fd.bottom = new FormAttachment(100,-10);
		fd.width = 50;
		advisoryNumber.setLayoutData(fd);
		
		/*
		 * Add verify listener to make sure advisory number is valid
		 */
		advisoryNumber.addVerifyListener(new VerifyListener(){
			@Override
			public void verifyText(VerifyEvent e) {
				e.doit = validateAdvisoryNumber(e);
				if ( ! e.doit ) Display.getCurrent().beep();
		}});
		
		/*
		 * Up arrow button used to increment storm advisory number in text field
		 */
		Button upArrow = new Button(g1, SWT.ARROW | SWT.UP);
		fd = new FormData();
		fd.left = new FormAttachment(advisoryNumber, 0, SWT.RIGHT);
		fd.top = new FormAttachment(stormNumber, 10, SWT.BOTTOM);
		fd.bottom = new FormAttachment(advisoryNumber, 0, SWT.BOTTOM);
		upArrow.setLayoutData(fd);
		upArrow.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
			/*
			 * Increment advisory number when up arrow button is pressed
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				int num = StormAdvisoryNumber.getRegularAdvisory(advisoryNumber.getText());
				if ( num < 999 ) num++;
				advisoryNumber.setText(Integer.toString(num));
		    	if ( ! advisories.isEmpty() ) {
		    		getButton(CANCEL_ID).setEnabled(true);
		    		getButton(SAVE_ID).setEnabled(true);
		    		getButton(CREATE_TEXT_ID).setEnabled(false);
		    	}
			}
			
		});
		
		/*
		 * Down arrow button used to decrement storm advisory number in text field
		 */
		Button downArrow = new Button(g1, SWT.ARROW | SWT.DOWN);
		fd = new FormData();
		fd.left = new FormAttachment(upArrow, 0, SWT.RIGHT);
		fd.top = new FormAttachment(stormNumber, 10, SWT.BOTTOM);
		fd.bottom = new FormAttachment(advisoryNumber, 0, SWT.BOTTOM);
		fd.right = new FormAttachment(statusItems,0, SWT.RIGHT);
		downArrow.setLayoutData(fd);
		downArrow.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
			/*
			 * Decrement advisory number when down arrow button is pressed
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				int num = StormAdvisoryNumber.getRegularAdvisory(advisoryNumber.getText());
				if ( (num > 1) &&  ! StormAdvisoryNumber.isIntermediate(advisoryNumber.getText())) num--;
				advisoryNumber.setText(Integer.toString(num));
			}
			
		});

		((FormData)upArrow.getLayoutData()).width = 20;
		((FormData)downArrow.getLayoutData()).width = 20;
		advisoryNumber.setText("1");        // initialize w/ default value

		/*
		 * Time Zone label
		 */
		Label timeZoneLabel = new Label(g1,SWT.NONE);
		timeZoneLabel.setText("Time Zone for TCV product:");
		fd = new FormData();
		fd.top = new FormAttachment(validTime,15, SWT.BOTTOM);
		fd.left = new FormAttachment(stormTypes, 0, SWT.LEFT);
		timeZoneLabel.setLayoutData(fd);
		
		/*
		 * Time Zone Pulldown
		 */
        timeZoneTypes = new Combo( g1, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getTimezones() ) {
            timeZoneTypes.add( st );
        }
        timeZoneTypes.setText(timeZoneTypes.getItem(0));
		fd = new FormData();
		fd.top = new FormAttachment(validTime,10, SWT.BOTTOM);
		fd.left = new FormAttachment(timeZoneLabel, 10, SWT.RIGHT);
		fd.right = new FormAttachment(stormTypes, 0, SWT.RIGHT);
		timeZoneTypes.setLayoutData(fd);

	}
	
	/*
	 * Create widgets used in the breakpoint modification section of the dialog
	 */
	private void createBreakpointTools(Group g2) {

        FormLayout layout = new FormLayout();
        layout.marginHeight = 3;
        layout.marginWidth = 3;
		g2.setLayout(layout);

		/*
		 * Composite used to group Breakpoint Attributes
		 */
		Composite bkptInfo = new Composite(g2, SWT.NONE);
		FormData fd = new FormData();
		fd.left = new FormAttachment(0, 10);
		fd.right = new FormAttachment(100, -10);
		fd.top = new FormAttachment(0,10);
		//fd.bottom = new FormAttachment( 100, -10);
		bkptInfo.setLayoutData(fd);

		GridLayout grid = new GridLayout(3,true);
		grid.horizontalSpacing=30;
		bkptInfo.setLayout(grid);
		
		/*
		 *  watch/warning Severity label
		 */
		Label severityLabel = new Label(bkptInfo,SWT.NONE);
		severityLabel.setText("Severity:");
		
		/*
		 *   Advisory Type label
		 */
		Label advisoryLabel = new Label(bkptInfo,SWT.NONE);
		advisoryLabel.setText("Advisory Type:");
		
		/*
		 *  Breakpoint Type label
		 */
		Label breakpointTypeLabel = new Label(bkptInfo,SWT.NONE);
		breakpointTypeLabel.setText("Breakpoint Type:");
		
		/*
		 * watch/warning severity pulldown
		 */
		severityTypes = new Combo( bkptInfo, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getSeverityList() ) {
            severityTypes.add( st );
        }
        severityTypes.setText(severityTypes.getItem(0));
        GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
        severityTypes.setLayoutData(gd);
		
		/*
		 *  advisory type pulldown
		 */
        advisoryTypes = new Combo( bkptInfo, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getAdvisoryList() ) {
            advisoryTypes.add( st );
        }
        advisoryTypes.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL));
        advisoryTypes.setText(advisoryTypes.getItem(0));
		
		/*
		 * breakpoint Types pulldown
		 */
        breakpointTypes = new Combo( bkptInfo, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getBreakpointTypeList() ) {
            breakpointTypes.add( st );
        }
        breakpointTypes.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL));
        breakpointTypes.setText(breakpointTypes.getItem(0));
		
		/*
		 *  Breakpoint 1 label
		 */
		Label bkpt1Label = new Label(bkptInfo,SWT.NONE);
		bkpt1Label.setText("Break Point 1:");
		
		/*
		 *  Breakpoint 2 label
		 */
		Label bkpt2Label = new Label(bkptInfo,SWT.NONE);
		bkpt2Label.setText("Break Point 2:");
		
		/*
		 *  Special geography label
		 */
		Label specialGeogLabel = new Label(bkptInfo,SWT.NONE);
		specialGeogLabel.setText("Special Geography:");
		
		/*
		 * Breakpoint 1 text field
		 */
        bkpt1Field = new Text(bkptInfo, SWT.SINGLE | SWT.BORDER);       
        bkpt1Field.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL));
		
		/*
		 * breakpoint 2 text field.
		 */
        bkpt2Field = new Text(bkptInfo, SWT.SINGLE | SWT.BORDER);       
        bkpt2Field.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL));
		
		/*
		 * special Geography pulldown
		 */
        specialGeogTypes = new Combo( bkptInfo, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getGeographyTypeList() ) {
            specialGeogTypes.add( st );
        }
        specialGeogTypes.setText(specialGeogTypes.getItem(0));
        specialGeogTypes.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL));

		/*
		 * Composite used to group Apply, delete segment, and new segment buttons
		 */
		Composite forButtons = new Composite(g2, SWT.NONE);
		fd = new FormData();
		fd.left = new FormAttachment(0, 10);
		fd.right = new FormAttachment(100, -10);
		fd.top = new FormAttachment(bkptInfo, 15, SWT.BOTTOM);
		fd.bottom = new FormAttachment( 100, -10);
		forButtons.setLayoutData(fd);

		RowLayout row = new RowLayout(SWT.HORIZONTAL);
		row.pack = false;
		row.justify = true;
		forButtons.setLayout(row);

		/*
		 * Apply button used to save changes to the current advisory
		 */
		apply = new Button(forButtons, SWT.PUSH);
		apply.setText(APPLY);
		apply.setEnabled(false);
		apply.addSelectionListener(this);

		/*
		 * Delete Segment button used to delete the currently selected advisory
		 */
		deleteSegment = new Button(forButtons, SWT.PUSH);
		deleteSegment.setText(DELETE_SEGMENT);
		deleteSegment.setEnabled(false);
		deleteSegment.addSelectionListener(this);

		/*
		 * used to notify drawing tool that a new advisory is to be created
		 */
		Button newSegment = new Button(forButtons, SWT.PUSH);
		newSegment.setText(NEW_SEGMENT);
		newSegment.addSelectionListener(this);
		
	}
	
	/*
	 * create widgets for the section of the dialog that lists current breakpoints
	 */
	private void createBreakpointList(Group g3) {

        FillLayout layout = new FillLayout();
        layout.marginHeight = 3;
        layout.marginWidth = 3;
		g3.setLayout(layout);

		/*
		 * contains list of current watch/warning advisories
		 */
		breakpointList =  new List( g3, SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		GridData gd = (GridData)g3.getLayoutData();
		gd.heightHint = 6 * breakpointList.getItemHeight();
		
		breakpointList.addSelectionListener( new SelectionListener(){

			private int lastSelected=-1;

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// no-op
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				//System.out.println(breakpointList.getSelectionIndex()+breakpointList.getSelection().toString());
				/*
				 * If same item in list is selected twice in a row, assume the second
				 * select is actually a deselect
				 */
				if ( breakpointList.getSelectionIndex() == lastSelected ) {
					breakpointList.deselect(breakpointList.getSelectionIndex());
					lastSelected = -1;
					resetAdvisoryInfo();  // reset advisory info section of dlg to defaults
					tcaTool.deselectAdvisory();   // Notify TCA drawing tool that no adv is selected
					return;
				}

				/*
				 * item is selected.  update info in dialog for this advisory,
				 * and notify drawing tool which advisory is selected
				 */
				lastSelected = breakpointList.getSelectionIndex();
				updateAdvisoryInfo(lastSelected);
				tcaTool.selectAdvisory(lastSelected);
			}
		});
		
	}
	
	/*
	 * Resets the advisory information section of the TCA dialog to
	 * default values.  Usually called when no advisory is selected
	 */
	protected void resetAdvisoryInfo() {

		//  Apply and Delete buttons are not available when no advisory is selected
		apply.setEnabled(false);
		deleteSegment.setEnabled(false);
		
		//  update widgets
		severityTypes.setText( SEV_TROPICAL_STORM );
		advisoryTypes.setText( ADVISORY_WATCH );
		breakpointTypes.setText(BREAKPOINT_OFFICIAL);
		bkpt1Field.setText("");
		bkpt2Field.setText("");
		specialGeogTypes.setText( GEOG_TYPE_NONE );
		specialGeogTypes.setEnabled(true);
	}

	/*
	 * Update the advisory info section of the dialog with values
	 * in the currently selected advisory
	 */
	protected void updateAdvisoryInfo(int index) {

		// Apply and Delete buttons are active when an advisory is selected 
		apply.setEnabled(true);
		deleteSegment.setEnabled(true);
		
		// Update widgets values
		TropicalCycloneAdvisory tca = advisories.get(index);
		severityTypes.setText( tca.getSeverity() );
		advisoryTypes.setText( tca.getAdvisoryType() );
		specialGeogTypes.setText( tca.getGeographyType() );
		specialGeogTypes.setEnabled(false);
		
		if ( tca.getSegment() instanceof BreakpointPair ) {
			bkpt1Field.setText( tca.getSegment().getBreakpoints().get(0).getName() );
			bkpt2Field.setText( tca.getSegment().getBreakpoints().get(1).getName() );
		}
		else {
			bkpt1Field.setText("");
			bkpt2Field.setText("");
		}
		
	}
	
	/**
	 * Called when an advisory is selected.  Ensures that the advisory is
	 * selected in the breakpoint list, and that the advisory info is updated in the dialog
	 */
	public void selectAdvisory(int index) {
		breakpointList.select(index);
		updateAdvisoryInfo(index);
	}

	/**
	 * Called when no advisory is selected.  Ensures that nothing is
	 * selected in the breakpoint list, and that default values are set
	 * in the dialog
	 */
	public void deselectAdvisory() {
		breakpointList.deselectAll();
		resetAdvisoryInfo();
	}

	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		
		/*
		 * Save TCA button pressed.  Save TCA element to a vgf.xml file
		 */
		if ( buttonId == SAVE_ID ) {
			
			//  Prompt for storm name, if field is empty
			if ( stormNameField.getText().isEmpty() ) {
				String msg = "Please provide the storm name.";
		    	MessageDialog messageDlg = new MessageDialog( 
		        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
		        		"Warning", null, msg,
		        		MessageDialog.ERROR, new String[]{"OK"}, 0);
		        messageDlg.open();
			}
			else if ( ! PgenUtil.isTimeValid( validTime.getText() ) ) {
				StringBuilder msg = new StringBuilder("The Product Time ");
				msg.append('"');
				msg.append( validTime.getText() );
				msg.append(" UTC");
				msg.append('"');
				msg.append(" is invalid.");
		    	MessageDialog messageDlg = new MessageDialog( 
		        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
		        		"Warning", null, msg.toString(),
		        		MessageDialog.ERROR, new String[]{"OK"}, 0);
		        messageDlg.open();
			}
			else {
				if ( tcaTool.saveAdvisory() ) {
					getButton(SAVE_ID).setEnabled(false);
					getButton(CREATE_TEXT_ID).setEnabled(true);
				}
			}
		}
		
		/*
		 * Generate a TCV (vtec) message for this advisory number
		 */
		else if ( buttonId == CREATE_TEXT_ID ) {
			//System.out.println("Create Text PRESSED~!~~!~~");
			TcaTextMessageDlg textMessage = new TcaTextMessageDlg(this.getShell());
			
			textMessage.setOutputFilename(generateVTECFilename());
			
			String tcvMessage = tcaTool.createTCV();
			textMessage.setMessage(tcvMessage);
			
			textMessage.open();
			//System.out.println("TCV RETURN = "+textMessage.getReturnCode());
			if ( textMessage.getReturnCode() == IDialogConstants.OK_ID ) {
				getButton(CREATE_TEXT_ID).setEnabled(false);
			}
			
		}
		
		/*
		 * Cancel all watches/warnings in the current advisory number
		 */
		else if ( buttonId == CANCEL_ID ) {
			
			// display confirmation dialog
			String msg = "Cancel all current watches and warnings\nAnd create TCV message?";
	    	MessageDialog confirmDlg = new MessageDialog( 
	        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
	        		"Confirm", null, msg,
	        		MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
	        confirmDlg.open();
	        
	        if ( confirmDlg.getReturnCode() == MessageDialog.OK ) {
				if ( ! advisories.isEmpty() ) 
					setTextLocation( advisories.get(0).getSegment().getBreakpoints().get(0).getLocation());
	        	breakpointList.removeAll();
	        	advisories.clear();
	        	getButton(SAVE_ID).setEnabled(false);
	        	getButton(CREATE_TEXT_ID).setEnabled(false);
	        	tcaTool.advisoryDeleted();
	        	
				TcaTextMessageDlg textMessage = new TcaTextMessageDlg(this.getShell());
				textMessage.setOutputFilename(generateVTECFilename());
				String tcvMessage = tcaTool.createTCV();
				textMessage.setMessage(tcvMessage);
				textMessage.open();
	        }
		}
		
		/*
		 * Close TCA Tool by loading PGEN Select tool
		 */
		else if ( buttonId == IDialogConstants.CLOSE_ID ) {
			//System.out.println("Close PRESSED~!~~!~~");
			PgenUtil.setSelectingMode();
		}
		
	}
	
	/*
	 * override to do nothing
	 */
	@Override
	public void enableButtons(){
		// do nothing
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute attr ){
	
		if ( attr instanceof ITca ) {
			
			ITca tca = (ITca)attr;
			this.setIssuingStatus(tca.getIssuingStatus() );           
			this.setStormType( ((ITca) attr).getStormType() );
			this.setBasin( tca.getBasin() );
			this.setStormName( tca.getStormName() );
			this.setStormNumber( tca.getStormNumber() );
			this.setAdvisoryNumber( tca.getAdvisoryNumber() );
			this.setAdvisoryTime( tca.getAdvisoryTime() );
			this.setTimeZone( tca.getTimeZone() );
			this.setTextLocation( tca.getTextLocation() );
			
			advisories.clear();
			breakpointList.removeAll();
			for ( TropicalCycloneAdvisory adv : tca.getAdvisories() ) {
				this.addAdvisory(adv.copy());
			}
		}
	}

	/*
	 * read in all possible selections for the pulldown menus from the tcainfo.xml file
	 */
	private static void readOptions() {
		
		File tcainfoFile = 	PgenStaticDataProvider.getProvider().getStaticFile( 
				 PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + PGEN_TCA_ATTR_INFO);   	    

		try {
			info = (TcaAttrInfo)SerializationUtil.jaxbUnmarshalFromXmlFile(tcainfoFile.getAbsoluteFile() );
		}
		catch ( Exception e) {
			e.printStackTrace();
		}
		
	}
	
	/*
	 * creates a new string with user input changes for a storm advisory
	 * number; then checks to see if it is a valid advisory number.
	 */
	private boolean validateAdvisoryNumber( VerifyEvent ve ) {
		
		boolean stat = false;
		
		if ( ve.widget instanceof Text ) {
			Text advnum = (Text)ve.widget;
    		StringBuffer str = new StringBuffer(advnum.getText());
    		str.replace(ve.start, ve.end, ve.text);

    		if ( str.toString().isEmpty() ) return true;
    		stat = StormAdvisoryNumber.isValid(str.toString());
		}
		
		return stat;
	}

	/*
	 * generate the name of the file used to store the TCV message based on the storm info
	 */
	private String generateVTECFilename() {
		String basin = Basin.getBasinAbbrev(getBasin()).toUpperCase();
		if ( basin.equals("AL") ) basin = new String("AT");
		int num = getStormNumber() % 5;
		if ( num == 0 ) num = 5;
		String name = String.format("%sTCV%2s%1d", PgenUtil.getCurrentOffice(), basin, num );
		return PgenUtil.getPgenActivityTextProdPath() + File.separator + name;
	}
	
	/*
	 * Returns list of watch/warning advisories
	 * @see gov.noaa.nws.ncep.ui.pgen.tca.ITca#getAdvisories()
	 */
	@Override
	public ArrayList<TropicalCycloneAdvisory> getAdvisories() {
		return advisories;
	}
	
	/*
	 * Add given advisory to the watch/warning list and the breakpoint list
	 */
	public void addAdvisory(TropicalCycloneAdvisory advisory) {
		advisories.add(advisory);
		breakpointList.add( toListString(advisory) );
		getButton(SAVE_ID).setEnabled(true);
	}

	/*
	 * Replace an advisory with the given advisory in the watch/warning list and the breakpoint 
	 * list.  Update the advisory info section of the dialog
	 */
	public void replaceAdvisory(int index, TropicalCycloneAdvisory advisory) {
		advisories.set(index, advisory);
		breakpointList.setItem(index, toListString(advisory) );
		updateAdvisoryInfo(index);
	}

	/*
	 * creates a string to display in the breakpoint list for this advisory
	 */
	private String toListString(TropicalCycloneAdvisory advisory) {

		StringBuilder sb = new StringBuilder(advisory.getSeverity() + "\t" + advisory.getAdvisoryType()+"\t");
		sb.append( advisory.getSegment().getBreakpoints().get(0).getName() );
		if (advisory.getSegment() instanceof BreakpointPair ) sb.append( "\t" + advisory.getSegment().getBreakpoints().get(1).getName() );
		return sb.toString();
	}

	/*
	 * Return advisory number
	 * @see gov.noaa.nws.ncep.ui.pgen.tca.ITca#getAdvisoryNumber()
	 */
	@Override
	public String getAdvisoryNumber() {
		return advisoryNumber.getText();
	}
	
	/*
	 * Set the advisory number
	 */
	private void setAdvisoryNumber(String adnum) {
		advisoryNumber.setText(adnum);
	}

	@Override
	public String getStormName() {
		return stormNameField.getText();
	}
	
	private void setStormName(String name) {
		stormNameField.setText(name);
	}

	@Override
	public int getStormNumber() {
		return stormNumber.getSelection();
	}
	
	private void setStormNumber( int num) {
		stormNumber.setSelection(num);
	}

	@Override
	public String getStormType() {
		return stormTypes.getText();
	}
	
	private void setStormType(String type) {
		stormTypes.setText(type);
	}

	@Override
	public Coordinate getTextLocation() {
		return textLocation;     // this attribute does not appear in Attribute Dialog
	}
	
	public void setTextLocation(Coordinate loc) {
		this.textLocation = loc;
	}

	@Override
	public String getTimeZone() {
		return timeZoneTypes.getText();
	}
	
	private void setTimeZone(String zone) {
		timeZoneTypes.setText(zone);
	}

	@Override
	public Calendar getAdvisoryTime() {
		int time = Integer.parseInt( validTime.getText() );
		int hours = time / 100;
		int minutes = time % 100;
		
		Calendar advTime = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		advTime.set(validDate.getYear(), validDate.getMonth(), validDate.getDay(), 
				    hours, minutes, 0);
		advTime.set(Calendar.MILLISECOND, 0);
		return advTime;
	}
	
	private void setAdvisoryTime(Calendar time) {
		validDate.setYear( time.get(Calendar.YEAR));
		validDate.setMonth( time.get(Calendar.MONTH));
		validDate.setDay( time.get(Calendar.DAY_OF_MONTH));
		
		validTime.setText( String.format("%02d%02d", time.get(Calendar.HOUR_OF_DAY), time.get(Calendar.MINUTE) ));
		//validTime.setHours( time.get(Calendar.HOUR_OF_DAY));
		//validTime.setMinutes( time.get(Calendar.MINUTE));
		//validTime.setSeconds(0);
	}

	@Override
	public String getIssuingStatus() {
		return statusItems.getText();
	}

	private void setIssuingStatus(String status){
		statusItems.setText(status);
	}
	
	@Override
	public String getBasin() {
		return basinTypes.getText();
	}
	
	private void setBasin(String basin) {
		basinTypes.setText(basin);
	}

	@Override
	public void widgetDefaultSelected(SelectionEvent e) {
		// TODO Auto-generated method stub
	}

	/*
	 * Called when Apply, Delete Segment, or New Segment button is pressed
	 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
	 */
	@Override
	public void widgetSelected(SelectionEvent e) {
		
		if (e.widget instanceof Button ) {
			Button b = (Button)e.widget;
			
			/*
			 * Notify the Drawing Tool to allow new breakpoints to be selected
			 */
			if ( b.getText().equals(NEW_SEGMENT) ) {
				if ( getGeogType().equals(GEOG_TYPE_NONE) )
					tcaTool.setPairMode();
				else
					tcaTool.setSingleMode();
			}
			
			/*
			 * Remove the selected advisory from the list of advisories, breakpoint list,
			 * and the notify the drawing tool.
			 */
			else if ( b.getText().equals(DELETE_SEGMENT) ) {
				int idx = breakpointList.getSelectionIndex();
				if ( idx != -1 ) {
					setTextLocation( advisories.get(idx).getSegment().getBreakpoints().get(0).getLocation());
					breakpointList.remove(idx);
					advisories.remove(idx);
					if ( advisories.isEmpty() ) getButton(SAVE_ID).setEnabled(false);
					else getButton(SAVE_ID).setEnabled(true);
				}
				resetAdvisoryInfo();
				tcaTool.advisoryDeleted();
			}
				
			/*
			 * Apply changes to the selected advisory.  notify drawing tool
			 */
			else if ( b.getText().equals(APPLY) ) {
				int idx = breakpointList.getSelectionIndex();
				TropicalCycloneAdvisory tca = advisories.get(idx);
				tca.setSeverity( severityTypes.getText() );
				tca.setAdvisoryType( advisoryTypes.getText() );
				//advisories.set(idx, tca);
				breakpointList.setItem(idx, toListString(tca) );
				if ( advisories.isEmpty() ) getButton(SAVE_ID).setEnabled(false);
				else getButton(SAVE_ID).setEnabled(true);
				tcaTool.updateTcaElement();
				tcaTool.selectAdvisory(idx);
			}

		}
		
	}
	
	public String getGeogType() {
		return specialGeogTypes.getText();
	}
	
	public String getSeverity() {
		return severityTypes.getText();
	}
	
	public String getAdvisoryType() {
		return advisoryTypes.getText();
	}
	
	public String getBreakpointType() {
		return breakpointTypes.getText();
	}
	
	/**
	 * Registers the drawing tool, so that it can be notified of changes in the attr dialog.
	 * @param tcaTool the tcaTool to set
	 */
	public void setTcaTool(PgenTcaTool tcaTool) {
		this.tcaTool = tcaTool;
	}
	
}
