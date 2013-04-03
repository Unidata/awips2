package gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources.ManageResourceControl.IEditResourceComposite;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimeMatchMethod;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimelineGenMethod;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;
import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroup;
import gov.noaa.nws.ncep.viz.resources.manager.AttributeSet;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.viz.core.exception.VizException;


/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09/10		  #273		Greg Hull	 Created
 * 09/02/10       #307      Greg Hull    Change time interval spinner to combo
 * 09/07/10       #307      Greg Hull    add default time range
 * 10/05/10       #307      Greg Hull    handle 'event-type' resources
 * 11/27/10       #365      Greg Hull    dynamically generated resource types and sub-types
 * 01/24/10                 Greg Hull    Change frame Interval to frame span
 * 03/01/11       #408      Greg Hull    add filter labels
 * 07/22/11       #450      Greg Hull    Save to User Localization
 * 07/05/12       #821      Greg Hull    up the maxValue for numFrames spinner 
 * 08/25/12       #         Greg Hull    allow to enable/disable resources
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */
class EditResourceTypeComp extends Composite implements IEditResourceComposite {
	ResourceDefnsMngr rscDefnMngr;
	ManageResourceControl mngrControl; // the parent composite 
	
	ResourceName       seldRscName=null;
	ResourceDefinition seldRscDefn;
	
	Button enableRscTypeBtn = null;

	Text    rscTypeTxt;	
	Combo   timeMatchMethodCombo;
	Spinner dfltNumFramesSpnr;
	Spinner dfltTimeRangeDaysSpnr;
	Spinner dfltTimeRangeHrsSpnr;
//	Spinner timeIntervalSpnr;
	Label   frameIntLbl; // either 'Frame Span' or 'Default Frame Interval'
	Combo   frameSpanCombo;
	Label   minsLbl2, minsLbl3;
	Group   timelineGenMthdGrp;
	Button  useFrameIntrvlBtn;
	Button  useDataTimesBtn;
	Button  useManualTimelineBtn;
	Button  binDataBtn;
	Spinner binStartIntrvlSpnr;
	Spinner binEndIntrvlSpnr;
	Label   startLbl, endLbl;
	Text    editParamsTxt;
	Label   editParamsLbl;
	
	Text    filterLabelsTxt;
	
	Text    subTypeGenTxt;
	Label   subTypeGenLbl;
	
	Button  newTypeBtn;
	Button  saveTypeBtn;
	Button  cancelBtn;
	
	String availFrameSpanStrings[] = { //"N/A",
			"1 min", "2 mins", "5 mins", "10 mins", "15 mins", "20 mins", "30 mins",
			"1 hr", "90 mins", "2 hrs",  "3 hrs",  "6 hrs",  "12 hrs",  "24 hrs" };
	
	int availFrameSpanMins[] = { //0,
			1, 2, 5, 10, 15, 20, 30, 60, 90, 120, 180, 360, 720, 1440 };  
	
	public EditResourceTypeComp( Composite parent, int style, 
			ManageResourceControl mgrCtl) {
		super( parent, style );
		Composite top_form = this;      

		FormData fd = new FormData();
    	fd.top = new FormAttachment( 0, 12 );  // offset from sash so the title shows up
    	fd.left = new FormAttachment( 0, 0 );
    	fd.right = new FormAttachment( 100, 0 );
    	fd.bottom = new FormAttachment( 100, 0 );
    	top_form.setLayoutData(fd);
		
		setLayoutData( fd );

		top_form.setLayout( new FormLayout() );

		mngrControl = mgrCtl;
		rscDefnMngr = mngrControl.getRscDefnMngr();
	
		rscTypeTxt = new Text( top_form, SWT.SINGLE | SWT.BORDER );
		rscTypeTxt.setText( "" );

    	fd = new FormData();
    	fd.width = 150;
    	fd.top = new FormAttachment( 0, 35 );
    	fd.left = new FormAttachment( 0, 15 );
    	rscTypeTxt.setLayoutData( fd );

		Label rscTypeLbl = new Label( top_form, SWT.NONE );
		rscTypeLbl.setText("Resource Type");
		fd = new FormData();
    	fd.bottom = new FormAttachment( rscTypeTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( rscTypeTxt, 0, SWT.LEFT );
    	rscTypeLbl.setLayoutData( fd );
	
    	dfltNumFramesSpnr = new Spinner( top_form, SWT.BORDER );
    	fd = new FormData();
    	fd.top = new FormAttachment( rscTypeTxt, 0, SWT.TOP );
    	fd.left = new FormAttachment( rscTypeTxt, 20, SWT.RIGHT );
    	dfltNumFramesSpnr.setLayoutData( fd );

		Label dfltNumFramesLbl = new Label( top_form, SWT.NONE );
		dfltNumFramesLbl.setText("Num Frames");//Default Num\nFrames");
		fd = new FormData();
    	fd.bottom = new FormAttachment( dfltNumFramesSpnr, -3, SWT.TOP );
    	fd.left = new FormAttachment( dfltNumFramesSpnr, 0, SWT.LEFT ); //rscTypeTxt, 20, SWT.RIGHT );
    	dfltNumFramesLbl.setLayoutData( fd );

    	dfltNumFramesSpnr.setMinimum(1);
    	dfltNumFramesSpnr.setMaximum(999);
    	dfltNumFramesSpnr.setDigits(0);
    	dfltNumFramesSpnr.setIncrement(1);
    	dfltNumFramesSpnr.setTextLimit(4);
    	
		enableRscTypeBtn = new Button( top_form, SWT.CHECK );
		enableRscTypeBtn.setText( "Enabled" );
    	
		fd = new FormData();
    	fd.top = new FormAttachment( dfltNumFramesSpnr, -20, SWT.TOP );
    	fd.left = new FormAttachment( 50, 0);
    	enableRscTypeBtn.setLayoutData( fd );
    	
    	editParamsTxt = new Text( top_form, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData();
    	fd.height = 100;
    	fd.left = new FormAttachment( enableRscTypeBtn, 0, SWT.LEFT );
    	fd.top = new FormAttachment( enableRscTypeBtn, 35, SWT.BOTTOM );
    	fd.right = new FormAttachment( 100, -10 );
//    	fd.bottom = new FormAttachment( 65, 0 );
    	editParamsTxt.setLayoutData( fd );

		editParamsLbl = new Label( top_form, SWT.NONE );
		editParamsLbl.setText("Edit Resource Parameters");
		fd = new FormData();
    	fd.bottom = new FormAttachment( editParamsTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( editParamsTxt, 0, SWT.LEFT );
    	editParamsLbl.setLayoutData( fd );
    	
    	
    	filterLabelsTxt = new Text( top_form, SWT.SINGLE | SWT.BORDER ); //| SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData();
    	fd.height = 20;
    	fd.left = new FormAttachment( editParamsTxt, 0, SWT.LEFT );
    	fd.right = new FormAttachment( editParamsTxt, 0, SWT.RIGHT );
    	fd.top = new FormAttachment( editParamsTxt, 40, SWT.BOTTOM );
    	filterLabelsTxt.setLayoutData( fd );
    	filterLabelsTxt.setToolTipText( "comma-separated labels with no spaces" );
    	
		
    	Label lblLbl = new Label( top_form, SWT.NONE );
    	lblLbl.setText("Filterable Labels");
		fd = new FormData();
    	fd.bottom = new FormAttachment( filterLabelsTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( filterLabelsTxt, 0, SWT.LEFT );
    	lblLbl.setLayoutData( fd );

		
    	
    	subTypeGenTxt = new Text( top_form, SWT.SINGLE | SWT.BORDER );
    	fd = new FormData();
    	fd.width = 190;
    	fd.left = new FormAttachment( filterLabelsTxt, 0, SWT.LEFT );
    	fd.top = new FormAttachment( filterLabelsTxt, 40, SWT.BOTTOM );
    	subTypeGenTxt.setLayoutData( fd );
    	subTypeGenTxt.setToolTipText( subTypeGenToolTipText );
		subTypeGenTxt.setVisible( false );
		subTypeGenTxt.setBackground( top_form.getBackground() );
		subTypeGenTxt.setEditable(false);

    	subTypeGenLbl = new Label( top_form, SWT.NONE );
    	subTypeGenLbl.setText("Sub-Type Generator");
		fd = new FormData();
    	fd.bottom = new FormAttachment( subTypeGenTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( subTypeGenTxt, 0, SWT.LEFT );
    	subTypeGenLbl.setLayoutData( fd );
    	
    	
    	frameSpanCombo = new Combo( top_form, SWT.DROP_DOWN | SWT.READ_ONLY );
    	fd = new FormData();
    	fd.top = new FormAttachment( rscTypeTxt, 40, SWT.BOTTOM );
    	fd.left = new FormAttachment( rscTypeTxt, 0, SWT.LEFT );
    	frameSpanCombo.setLayoutData( fd );
    	//frameIntervalCombo.setToolTipText( frameIntervalToolTipText );
    	frameSpanCombo.setItems( availFrameSpanStrings );

		frameIntLbl = new Label( top_form, SWT.NONE );
		frameIntLbl.setText("Frame Span");
		fd = new FormData();
    	fd.bottom = new FormAttachment( frameSpanCombo, -3, SWT.TOP );
    	fd.left = new FormAttachment( frameSpanCombo, 0, SWT.LEFT ); 
    	frameIntLbl.setLayoutData( fd );


    	dfltTimeRangeDaysSpnr = new Spinner( top_form, SWT.BORDER );
    	fd = new FormData();
    	fd.top = new FormAttachment( frameSpanCombo, 0, SWT.TOP );
    	fd.left = new FormAttachment( frameSpanCombo, 70, SWT.RIGHT );
    	dfltTimeRangeDaysSpnr.setLayoutData( fd );

		Label dfltTimeRangeLbl = new Label( top_form, SWT.NONE );
		dfltTimeRangeLbl.setText("Range (Days/Hrs)");
		fd = new FormData();
    	fd.bottom = new FormAttachment( dfltTimeRangeDaysSpnr, -3, SWT.TOP );
    	fd.left = new FormAttachment( dfltTimeRangeDaysSpnr, 0, SWT.LEFT ); 
    	dfltTimeRangeLbl.setLayoutData( fd );

    	dfltTimeRangeDaysSpnr.setMinimum(0);
    	dfltTimeRangeDaysSpnr.setMaximum(999);
    	dfltTimeRangeDaysSpnr.setDigits(0);
    	dfltTimeRangeDaysSpnr.setIncrement(1);
    	dfltTimeRangeDaysSpnr.setTextLimit(4);
    	dfltTimeRangeDaysSpnr.setPageIncrement(30);
    	
//		Label timeRangeDaysLbl = new Label( top_form, SWT.NONE );
//		timeRangeDaysLbl.setText("Days");
//		fd = new FormData();
//    	fd.top = new FormAttachment( dfltTimeRangeDaysSpnr, 3, SWT.TOP );
//    	fd.left = new FormAttachment( dfltTimeRangeDaysSpnr, 3, SWT.RIGHT ); 
//    	timeRangeDaysLbl.setLayoutData( fd );

    	dfltTimeRangeHrsSpnr = new Spinner( top_form, SWT.BORDER );
    	fd = new FormData();
    	fd.top = new FormAttachment( dfltTimeRangeDaysSpnr, 0, SWT.TOP );
    	fd.left = new FormAttachment( dfltTimeRangeDaysSpnr, 8, SWT.RIGHT );
    	dfltTimeRangeHrsSpnr.setLayoutData( fd );

    	dfltTimeRangeHrsSpnr.setMinimum(0);
    	dfltTimeRangeHrsSpnr.setMaximum(23);
    	dfltTimeRangeHrsSpnr.setDigits(0);
    	dfltTimeRangeHrsSpnr.setIncrement(1);
    	dfltTimeRangeHrsSpnr.setTextLimit(2);
    	
    	// a group composite to force the radio behaviour
    	timelineGenMthdGrp = new Group( top_form, SWT.SHADOW_NONE );
    	timelineGenMthdGrp.setText("Generate Timeline from:");
    	fd = new FormData();
    	fd.top = new FormAttachment( frameSpanCombo, 25, SWT.BOTTOM );
    	fd.left = new FormAttachment( frameSpanCombo, 0, SWT.LEFT );
    	timelineGenMthdGrp.setLayoutData( fd );
    	
    	GridLayout gl = new GridLayout(2, false);
    	gl.horizontalSpacing = 10;
    	gl.marginWidth = 15;
    	gl.marginHeight = 10;
    	timelineGenMthdGrp.setLayout( gl);

    	useFrameIntrvlBtn = new Button(timelineGenMthdGrp, SWT.RADIO );
    	useFrameIntrvlBtn.setText( "Frame Interval" );//Time Interval of" );
    	useFrameIntrvlBtn.setLayoutData( new GridData() );

    	useDataTimesBtn = new Button(timelineGenMthdGrp, SWT.RADIO );
    	useDataTimesBtn.setText( "Data Times" );
    	fd = new FormData();
    	useDataTimesBtn.setLayoutData( new GridData() );
    	
    	useManualTimelineBtn = new Button(timelineGenMthdGrp, SWT.RADIO );
    	useManualTimelineBtn.setText( "Manual Timeline" );
    	fd = new FormData();
    	useManualTimelineBtn.setLayoutData( new GridData() );
    	
    	timeMatchMethodCombo = new Combo( top_form, SWT.DROP_DOWN | SWT.READ_ONLY );
    	fd = new FormData();
    	fd.top = new FormAttachment( timelineGenMthdGrp, 40, SWT.BOTTOM );
    	fd.left = new FormAttachment( timelineGenMthdGrp, 0, SWT.LEFT );
    	timeMatchMethodCombo.setLayoutData( fd );

		Label timeMatchMthdLbl = new Label( top_form, SWT.NONE );
		timeMatchMthdLbl.setText("Time Matching Method");
		fd = new FormData();
    	fd.bottom = new FormAttachment( timeMatchMethodCombo, -2, SWT.TOP );
    	fd.left = new FormAttachment( timeMatchMethodCombo, 0, SWT.LEFT );
    	timeMatchMthdLbl.setLayoutData( fd );


    	
    	binDataBtn = new Button(top_form, SWT.CHECK );
    	binDataBtn.setText( "Enable Binning" );
    	fd = new FormData();
    	fd.top = new FormAttachment( subTypeGenTxt, 25, SWT.BOTTOM );
    	fd.left = new FormAttachment( subTypeGenTxt, 0, SWT.LEFT );
    	binDataBtn.setLayoutData( fd );
    	binDataBtn.setEnabled( false ); // not implemented yet

    	// TODO: Remove the bin offset widgets if the frame Span is sufficient to replace it    	
    	binDataBtn.setVisible( false );
		
    	binStartIntrvlSpnr = new Spinner( top_form, SWT.BORDER );
    	fd = new FormData();
    	fd.top = new FormAttachment( binDataBtn, 30, SWT.BOTTOM );
    	fd.left = new FormAttachment( binDataBtn, 20, SWT.LEFT );
    	binStartIntrvlSpnr.setLayoutData( fd );

    	binStartIntrvlSpnr.setMinimum(0);
    	binStartIntrvlSpnr.setMaximum(60*3); // 3hrs : Is this reasonable?
    	binStartIntrvlSpnr.setDigits(0);
    	binStartIntrvlSpnr.setIncrement(1);
    	binStartIntrvlSpnr.setTextLimit(2);
    	
		startLbl = new Label( top_form, SWT.NONE );
		startLbl.setText("Before");
		fd = new FormData();
    	fd.bottom = new FormAttachment( binStartIntrvlSpnr, -2, SWT.TOP );
    	fd.left = new FormAttachment( binStartIntrvlSpnr, 0, SWT.LEFT );
    	startLbl.setLayoutData( fd );

		minsLbl2 = new Label( top_form, SWT.NONE );
		minsLbl2.setText("mins");
		fd = new FormData();
    	fd.top = new FormAttachment( binStartIntrvlSpnr, 3, SWT.TOP );
    	fd.left = new FormAttachment( binStartIntrvlSpnr, 5, SWT.RIGHT );
    	minsLbl2.setLayoutData( fd );

    	binEndIntrvlSpnr = new Spinner( top_form, SWT.BORDER );
    	fd = new FormData();
    	fd.top = new FormAttachment( binStartIntrvlSpnr, 0, SWT.TOP );
    	fd.left = new FormAttachment( binStartIntrvlSpnr, 50, SWT.RIGHT );
    	binEndIntrvlSpnr.setLayoutData( fd );

    	binEndIntrvlSpnr.setMinimum(0);
    	binEndIntrvlSpnr.setMaximum(60*3); // 3hrs : Is this reasonable?
    	binEndIntrvlSpnr.setDigits(0);
    	binEndIntrvlSpnr.setIncrement(1);
    	binEndIntrvlSpnr.setTextLimit(2);
    	
    	endLbl = new Label( top_form, SWT.NONE );
		endLbl.setText("After");
		fd = new FormData();
    	fd.bottom = new FormAttachment( binEndIntrvlSpnr, -2, SWT.TOP );
    	fd.left = new FormAttachment( binEndIntrvlSpnr, 0, SWT.LEFT );
    	endLbl.setLayoutData( fd );

		minsLbl3 = new Label( top_form, SWT.NONE );
		minsLbl3.setText("mins");
		fd = new FormData();
    	fd.top = new FormAttachment( binEndIntrvlSpnr, 3, SWT.TOP );
    	fd.left = new FormAttachment( binEndIntrvlSpnr, 5, SWT.RIGHT );
    	minsLbl3.setLayoutData( fd );

    	// Only one of these will be visible at a time
    	saveTypeBtn = new Button( top_form, SWT.PUSH );
    	saveTypeBtn.setText("Save");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( 100, -30 );
    	saveTypeBtn.setLayoutData( fd );
    	
    	newTypeBtn = new Button( top_form, SWT.PUSH );
    	newTypeBtn.setText("Create");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( 100, -30 );
    	newTypeBtn.setLayoutData( fd );

    	cancelBtn = new Button( top_form, SWT.PUSH );
    	cancelBtn.setText( "Cancel");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( saveTypeBtn, -20, SWT.LEFT );
    	cancelBtn.setLayoutData( fd );


    	// initialize the combo boxes with selectable items.
    	//    	
		for( TimeMatchMethod tmm : TimeMatchMethod.values() ) {
			timeMatchMethodCombo.add( tmm.toString() );
		}
		
		ArrayList<String> availRscImplementations = 
			ResourceExtPointMngr.getInstance().getAvailResources();
		
		// Add listeners
		//
		enableRscTypeBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		
        	}
		});
		
		rscTypeTxt.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				
				String newTextStr = rscTypeTxt.getText().trim();
				
				int indx = newTextStr.indexOf( "${" );
				
				if( indx != -1 ) {
					newTextStr = newTextStr.substring(0,indx-1);
				}
				
				if( newTextStr.isEmpty() ) {
					saveTypeBtn.setEnabled( false );
					newTypeBtn.setEnabled( false );
				}
				else {
					saveTypeBtn.setEnabled( true );

					// if the name has been changed, the 'save' button acts as a 'Rename' or Save As
					//					
					// disable the New button if the name hasn't been changed.
					//
					if( seldRscDefn.getResourceDefnName().equals( newTextStr ) ) {
						saveTypeBtn.setText("Save" );
						
						newTypeBtn.setEnabled( false );
					}
					else {
						saveTypeBtn.setText("Save As" );
						newTypeBtn.setEnabled( true );
						
						// disable the Save button if the new name already exists 
						String rscType = newTextStr;
						
						//ResourceDefinition.getResourceType( rscTypeTxt.getText().trim() );
						
						if( rscDefnMngr.findResourceDefinition( rscType ) ) {
							saveTypeBtn.setEnabled( false );
						}
					}
				}
			}
    	});		

		useFrameIntrvlBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
//        		frameIntervalCombo.setEnabled( useTimeIntrvlBtn.getSelection() );
//        		timeIntervalCombo.setVisible( useTimeIntrvlBtn.getSelection() );
//        		minsLbl1.setVisible( useTimeIntrvlBtn.getSelection() );
        	}
		});
		
		binDataBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		binStartIntrvlSpnr.setVisible( binDataBtn.getSelection() );
        		binEndIntrvlSpnr.setVisible( binDataBtn.getSelection() );
        		startLbl.setVisible( binDataBtn.getSelection() );
        		endLbl.setVisible( binDataBtn.getSelection() );
        		minsLbl2.setVisible( binDataBtn.getSelection() );
        		minsLbl3.setVisible( binDataBtn.getSelection() );        	}
		});
		
		editParamsTxt.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
			}
    	});	
		
//		subTypeGenTxt.addVerifyListener( new VerifyListener() {
//			@Override
//			public void verifyText(VerifyEvent event) {
//				String text = ((Text) event.widget).getText();
//				String newText = event.text;
//				
//				if( !text.startsWith("${") ||
//					!text.endsWith("}" ) ) {
//			        event.doit = false;
//			        return;
//				} 		
//			}
//		});
		
		saveTypeBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		saveResourceType();
        	}
		});

		newTypeBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		createResourceType();
        	}
		});
		
		cancelBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		mngrControl.editActionCanceled();
        	}
		});
	}

	@Override
	public ResourceName getSelectedResourceName( ) {
		return seldRscName;
	}
	
	// TODO : not implemented
	@Override
	public boolean isModified( ) {
		return false; 
	}
	
	@Override
	public void activate() {
		setVisible( true );
		if( getParent() instanceof Group ) {
			((Group)getParent()).setText( getTitle() );
		}
	}

	public void copySelectedResource( ResourceName rscName ) {
		setSelectedResource( rscName );
		newTypeBtn.setVisible( true );
		saveTypeBtn.setVisible( false );
		rscTypeTxt.setEditable( true );
		rscTypeTxt.setBackground( editParamsTxt.getBackground() ); // set white to indicate editable
		
		rscTypeTxt.setText( "CopyOf"+rscTypeTxt.getText() );
		rscTypeTxt.setSelection(0, rscTypeTxt.getText().length() );
		rscTypeTxt.setFocus();
	}
	
	public void editSelectedResource( ResourceName rscName ) {
		setSelectedResource( rscName );
		newTypeBtn.setVisible( false );
		saveTypeBtn.setVisible( true );
		
		rscTypeTxt.setEditable( false );
		rscTypeTxt.setBackground( getParent().getBackground() );
	}
	
	public void setSelectedResource( ResourceName rscName ) {

		seldRscName = rscName;

		if( seldRscName.getRscType().isEmpty() ) {
			rscTypeTxt.setText("");
			saveTypeBtn.setEnabled( false );
		}
		else {
			saveTypeBtn.setEnabled( false );
			
			seldRscDefn = new ResourceDefinition( rscDefnMngr.getResourceDefinition( seldRscName ) );

			if( seldRscDefn == null ) { // ????
				System.out.println("Unable to get Resource Defn for:"+seldRscName );
				return;
			}

			enableRscTypeBtn.setSelection( seldRscDefn.getIsEnabled() );

			String rscTypeGen = seldRscDefn.getRscTypeGenerator();
			
			if( rscTypeGen != null && !rscTypeGen.isEmpty() ) {
				rscTypeTxt.setText( seldRscName.getRscType() +
						":${"+ rscTypeGen + "}");
			}
			else {
				rscTypeTxt.setText( seldRscName.getRscType() );
			}

			// set the frame count
			int numFrames = seldRscDefn.getDfltFrameCount();
			dfltNumFramesSpnr.setSelection( numFrames );
				
			// if we let any resource select Manual timeline then we need another way to 
			// specify 'Event-type' resources ....
			TimelineGenMethod timelineMthd = seldRscDefn.getTimelineGenMethod();

			if( timelineMthd == TimelineGenMethod.USE_DATA_TIMES ) {
//				timelineGenMthdGrp.setEnabled( true );
				useDataTimesBtn.setSelection( true );
				useFrameIntrvlBtn.setSelection( false );
				useManualTimelineBtn.setSelection( false );
			}
			else if( timelineMthd == TimelineGenMethod.USE_FRAME_INTERVAL ) {
//				timelineGenMthdGrp.setEnabled( true );
				useDataTimesBtn.setSelection( false );
				useFrameIntrvlBtn.setSelection( true );				
				useManualTimelineBtn.setSelection( false );
			}
			else if( timelineMthd == TimelineGenMethod.USE_MANUAL_TIMELINE ) {
//				timelineGenMthdGrp.setEnabled( false );
				useDataTimesBtn.setSelection( false );
				useFrameIntrvlBtn.setSelection( false );
				useManualTimelineBtn.setSelection( true );
			}

			// if this is an event type resource; modify the 
			//     timeMatchMthd, frameInterval and timelineGeneration selections
			timeMatchMethodCombo.removeAll();
			frameSpanCombo.removeAll();
			
			if( seldRscDefn.isEventResource() || 
				seldRscDefn.isPgenResource() ) {
				useDataTimesBtn.setSelection( false );  // sanity check since
				useFrameIntrvlBtn.setSelection( false ); //  all event resources should
				useManualTimelineBtn.setSelection( true ); // be set to MANUAL anyway
				
				useDataTimesBtn.setEnabled( false );
				useFrameIntrvlBtn.setEnabled( false );
				
		    	// For event resources, only EXACT is meaningful
				timeMatchMethodCombo.add( TimeMatchMethod.EXACT.toString() );
				timeMatchMethodCombo.select(0);
				
				// for Event resources the frameSpan is the default Frame Interval				
				frameIntLbl.setText("Dflt Frame Interval");
//				frameSpanCombo.add( "N/A" );
//				frameSpanCombo.select(0);
				frameSpanCombo.setItems( availFrameSpanStrings );
				frameSpanCombo.setToolTipText("For this Event-Type resource the Default Frame Interval is \n"+
						                          "used to set the initial Frame Interval for a Manual timeline." );
				
				int frameSpan = seldRscDefn.getFrameSpan();				
				frameSpanCombo.deselectAll();

				for( int i=0; i<availFrameSpanMins.length ; i++ ) {				
					if( availFrameSpanMins[i] == frameSpan ) {
						frameSpanCombo.select(i);
						break;
					}
				}
				
				binDataBtn.setEnabled( false );
			}
			else {
				useDataTimesBtn.setEnabled( true );
				useFrameIntrvlBtn.setEnabled( true );

				for( TimeMatchMethod tmm : TimeMatchMethod.values() ) {
					timeMatchMethodCombo.add( tmm.toString() );
				}

				TimeMatchMethod timeMatchMthd = seldRscDefn.getTimeMatchMethod();
				int comboIndx=0;

				for( comboIndx=0 ; comboIndx < timeMatchMethodCombo.getItemCount() ; comboIndx++ ) {
					if( timeMatchMthd.toString().equals( timeMatchMethodCombo.getItem( comboIndx ) ) ) {
						timeMatchMethodCombo.select( comboIndx );
						break;
					}
				}
				if( comboIndx < timeMatchMethodCombo.getItemCount() ) {
				}
				
				frameIntLbl.setText("Frame Span");
				frameSpanCombo.setItems( availFrameSpanStrings );
				frameSpanCombo.setToolTipText("The Frame Span for a resource is the maximum range of times for "+
				                                  "data displayed in the frame." );

				int frameSpan = seldRscDefn.getFrameSpan();				
				frameSpanCombo.deselectAll();

				for( int i=0; i<availFrameSpanMins.length ; i++ ) {				
					if( availFrameSpanMins[i] == frameSpan ) {
						frameSpanCombo.select(i);
						break;
					}
				}
				
				binDataBtn.setEnabled( true );
			}
			
			
			int timeRangeHrs = seldRscDefn.getDfltTimeRange(); // in hours
			int timeRangeDays = timeRangeHrs / 24;
			timeRangeHrs %= 24;
			
			dfltTimeRangeDaysSpnr.setSelection( timeRangeDays );
			dfltTimeRangeHrsSpnr.setSelection( timeRangeHrs );
			

			if( seldRscDefn.getResourceParametersAsString().isEmpty() ) {
				editParamsTxt.setVisible( false );
				editParamsLbl.setVisible( false );
			}
			else {
				editParamsTxt.setVisible( true );
				editParamsTxt.setText( seldRscDefn.getResourceParametersAsString() ); 
				editParamsLbl.setVisible( true );
			}

			String filtLabelsStr="";
			for( String filtLbl : seldRscDefn.getFilterLabels() ) {
				if( filtLabelsStr.isEmpty() ) {
					filtLabelsStr = filtLbl;
				}
				else {
					filtLabelsStr = filtLabelsStr + "," + filtLbl;
				}
			}
			
			filterLabelsTxt.setText( filtLabelsStr );
			
//			if( seldRscDefn.getResourceCategory().equals( ResourceName.RadarRscCategory ) ||
//				seldRscDefn.getResourceCategory().equals( ResourceName.SatelliteRscCategory ) ) {
			if( !seldRscDefn.getSubTypeGenerator().isEmpty() ) {
//				subTypeGenTxt.setEnabled( true );
				subTypeGenTxt.setVisible( true );
				subTypeGenLbl.setVisible( true );
				subTypeGenTxt.setText( seldRscDefn.getSubTypeGenerator() );				
			}
			else {
//				subTypeGenTxt.setEnabled( false );
				subTypeGenTxt.setVisible( false );
				subTypeGenLbl.setVisible( false );
				subTypeGenTxt.setText( "" );				
			}
//			if( seldRscDefn.applyAttrSetGroups() ) {
//				subTypeGenLbl.setText( "Attribute Set Group Table:" );
//				subTypeGenTxt.setEditable( false );
//				subTypeGenTxt.setEnabled( false );
//				subTypeGenTxt.setText("");
//			}
//			else {
//				subTypeGenLbl.setText( "Sub-Type Generator:");
//				subTypeGenTxt.setEditable( true );
//				subTypeGenTxt.setEnabled( true );
//				subTypeGenTxt.setText( "${" + seldRscDefn.getSubTypeGenerator() + "}" );
//				subTypeGenTxt.setSelection(2, 
//						seldRscDefn.getSubTypeGenerator().length()+2 );
//				subTypeGenTxt.setText( seldRscDefn.getSubTypeGenerator() );
//			}
			
//			BinOffset binOffset = seldRscDefn.getBinOffset();
			
//			binDataBtn.setSelection( binOffset != null );
			binStartIntrvlSpnr.setVisible( false );//binDataBtn.getSelection() );
			binEndIntrvlSpnr.setVisible( false );//binDataBtn.getSelection() );
			startLbl.setVisible( false );//binDataBtn.getSelection() );
			endLbl.setVisible( false );//binDataBtn.getSelection() );
			
			minsLbl2.setVisible( false );//binDataBtn.getSelection() );
			minsLbl3.setVisible( false );//binDataBtn.getSelection() );
		}
	}

	@Override
	public void deactivate() {
		setVisible( false );
	}

	@Override
	public String getTitle() {
		return "Edit Resource Type";			
	}	
	
	protected void createResourceType( ) {
    	ResourceDefinition newRscDefn = new ResourceDefinition( seldRscDefn );
		    	
		String rscTypeName = rscTypeTxt.getText().trim();
		String rscTypeGen = "";
		
		int indx = rscTypeName.indexOf(":${");

		// if there is a type generator then parse the rscType name and the name of the appending generator parameter
		if( indx != -1 ) {
			rscTypeGen  = rscTypeName.substring(indx+2);
			rscTypeName = rscTypeName.substring(0,indx);
			indx = rscTypeGen.indexOf("}");
			if( indx != -1 ) {
				rscTypeGen = rscTypeGen.substring(0,indx);
			}
		}
		
		if( rscDefnMngr.getResourceDefinition( rscTypeName ) != null ) {
			
    		MessageDialog confirmDlg = new MessageDialog( getShell(), 
    				"Resource Exist", null, 
    				"The Resource Type " +rscTypeName + " already exists.\n\n"+
    				"Enter a different name.",
    				MessageDialog.INFORMATION, new String[]{"OK"}, 0);
    		confirmDlg.open();
    		return;
		}

		newRscDefn.setResourceDefnName( rscTypeName );
		
    	// this will copy the localizationFile and the sub-types and attrSets which we do not want.
//    	if( !newRscDefn.applyAttrSetGroups() ) {
//    		newRscDefn.getSubTypesMap().clear();	
//    	}
    	
		setSelectedRscDefnFromGUI( newRscDefn );

		
		// if attrSetGroups apply then we will just create 1 standard attrSetGroup with nothing in 
		// it. The user will need to edit this later.
		// TODO : allow the user to copy all (or some?) of the attributeSetGroups from the 
		// original rscDefn.
		if( newRscDefn.applyAttrSetGroups() && 
		   !newRscDefn.isPgenResource() ) {

//			newRscDefn.setAttrSetGroupNames( new ArrayList<String>() );
//			newRscDefn.addAttrSetGroupName("standard");
		}
		
    	String newLocFilename = seldRscDefn.getLocalizationFile().getName();
    	
    	// peal off the filename and last directory of the same name
    	// and replace with a path and filename based on the new name
    	newLocFilename = newLocFilename.substring( 0, 
    			newLocFilename.lastIndexOf(File.separator) );
    	newLocFilename = newLocFilename.substring( 0, 
    			newLocFilename.lastIndexOf(File.separator)+1 ) + 
    			rscTypeName + File.separator + rscTypeName + ".xml";

		newRscDefn.setLocalizationFile( null );
		newRscDefn.setLocalizationName( newLocFilename );

		//seldRscDefn.setResourceDefnName( rscTypeName );
//		String origRscType = seldRscDefn.getResourceDefnName();

		try {
			rscDefnMngr.saveResourceDefn( newRscDefn );

//			create a new attrSetGroup
			if( newRscDefn.applyAttrSetGroups() ) {
				if( !newRscDefn.isPgenResource() ) {
					AttrSetGroup asg = new AttrSetGroup();
					asg.setResource( newRscDefn.getResourceDefnName() );
					asg.setAttrSetGroupName("standard");
					
					rscDefnMngr.saveAttrSetGroup( asg );
				}
			}
			// or a new 'default' attrSet so the user will have something to edit.
    		else {
    			List<String> availAttrSets = rscDefnMngr.getAvailAttrSets( seldRscDefn );

    			// find an attrSet from the original to copy over for the new resource.
    			// priority is 'default', 'standard' and then just the first one in the list.
    			AttributeSet dfltAttrSet = null;

    			for( String attrSetName : availAttrSets ) {
    				if( dfltAttrSet == null ) {
    					dfltAttrSet = rscDefnMngr.getAttrSet(seldRscDefn, attrSetName );    						
    				}
    				else if( attrSetName.equals("standard" ) ) {
    					dfltAttrSet = rscDefnMngr.getAttrSet(seldRscDefn, attrSetName );    						    						
    				}
    				else if( attrSetName.equals("default" ) ) {
    					dfltAttrSet = rscDefnMngr.getAttrSet(seldRscDefn, attrSetName );
    					break;
    				}    					 
    			}
    			if( dfltAttrSet == null ) {
    				return;
    			}
    			
    			try {
    				FileReader fr = new FileReader( dfltAttrSet.getFile().getFile() );
    				char[] attrsSetStr = new char[(int) dfltAttrSet.getFile().getFile().length()];
    				fr.read(attrsSetStr);
    				fr.close();
    				
        			rscDefnMngr.saveAttrSet(newRscDefn, dfltAttrSet.getName(), new String( attrsSetStr ) );
        			
    			} catch (FileNotFoundException fnf ) {
    				throw new VizException( "file not found for default attr set.");
    			} catch (IOException ioe ) {
    				throw new VizException( "i/o error copying default attr set file.");
    			}
    		}			
			seldRscDefn = newRscDefn;

			ResourceName newSeldRscName = new ResourceName();
			newSeldRscName.setRscCategory( seldRscDefn.getResourceCategory() );
			newSeldRscName.setRscType( seldRscDefn.getResourceDefnName() );		

			mngrControl.updateResourceSelections( newSeldRscName );

		} catch (VizException e) {
			MessageDialog errDlg = new MessageDialog(getShell(), 
					"New Resource Type", null, 
					"Error Creating new Type " +rscTypeName + "\n\n"+
					e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
			return;
		}
	}
	
	// The name of the resource type hasn't changed
	//
	protected void saveResourceType( ) {
		
		setSelectedRscDefnFromGUI( seldRscDefn );
		
//		String origRscType = seldRscDefn.getResourceDefnName();
		
		String rscTypeName = rscTypeTxt.getText().trim();
		String rscTypeGen = "";
		
		int indx = rscTypeName.indexOf(":${");

		// if there is a type generator then parse the rscType name and the name of the appending generator parameter
		if( indx != -1 ) {
			rscTypeGen  = rscTypeName.substring(indx+2);
			rscTypeName = rscTypeName.substring(0,indx);
			indx = rscTypeGen.indexOf("}");
			if( indx != -1 ) {
				rscTypeGen = rscTypeGen.substring(0,indx);
			}
		}
	
		LocalizationFile lFile = seldRscDefn.getLocalizationFile();

		if( lFile.getContext().getLocalizationLevel() == LocalizationLevel.USER ) {
			MessageDialog confirmDlg = new MessageDialog( getShell(), 
					"Confirm", null, 
					"This will overwrite the current User-Level Resource Type\n"+
					"Are you sure you want to do this?",
					MessageDialog.CONFIRM, new String[]{"Yes", "No"}, 0);
			confirmDlg.open();
			
			if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
				return;
			}			
		}
//		else { // create a new user-level lFile
//			LocalizationContext newContext = NcPathManager.getInstance().getContext(
//					lFile.getContext().getLocalizationType(),
//					   LocalizationLevel.USER );
//			lFile = NcPathManager.getInstance().getLocalizationFile( newContext,
//										lFile.getName() );
//			seldRscDefn.setLocalizationFile(lFile);
//		}
				
		try {			
			rscDefnMngr.saveResourceDefn( seldRscDefn );
		
    		MessageDialog msgDlg = new MessageDialog( getShell(), 
    				"Saved", null, 
    				"The Resource " + rscTypeName + " has been saved.\n",
    				MessageDialog.INFORMATION, new String[]{"OK"}, 0);
    		msgDlg.open();

		
		} catch (VizException e) {
			MessageDialog confirmDlg = new MessageDialog( getShell(), 
					"Save Resource Type", null, 
					"Error Writing new Resource Definitions Table\n\n"+
					e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			confirmDlg.open();
		}
		
		mngrControl.updateResourceSelections( null );
		
		return;
	}
	
	// update the seldRscDefn with the GUI selections
	//
	private void setSelectedRscDefnFromGUI( ResourceDefinition rscDefn ) {
		
		rscDefn.setEnabled( enableRscTypeBtn.getSelection() );
		
		rscDefn.setDfltFrameCount( dfltNumFramesSpnr.getSelection() );
		
		for( TimeMatchMethod tmm : TimeMatchMethod.values() ) {
			if( timeMatchMethodCombo.getText().equals( tmm.toString() ) ) {
				rscDefn.setTimeMatchMethod( tmm );
				break;
			}
		}		
				
		if( frameSpanCombo.getText().equals("N/A") ) {
			rscDefn.setFrameSpan( 0 );
		}
		else {
			for( int i=0; i<availFrameSpanStrings.length ; i++ ) {				
				if( availFrameSpanStrings[i].equals( frameSpanCombo.getText() ) ) {
					rscDefn.setFrameSpan( availFrameSpanMins[i] );
					break;
				}
			}
		}
 
		if( useFrameIntrvlBtn.getSelection() ) {
			rscDefn.setTimelineGenMethod( TimelineGenMethod.USE_FRAME_INTERVAL );
		}
		else if( useDataTimesBtn.getSelection() ) {
			rscDefn.setTimelineGenMethod( TimelineGenMethod.USE_DATA_TIMES );			
		}
		else if( useManualTimelineBtn.getSelection() ) {
			rscDefn.setTimelineGenMethod( TimelineGenMethod.USE_MANUAL_TIMELINE );			
		}

		int timeRangeHrs = dfltTimeRangeDaysSpnr.getSelection() * 24 + 
		                                dfltTimeRangeHrsSpnr.getSelection();
		rscDefn.setDfltTimeRange( timeRangeHrs );
		
		// The GUI is in minutes and binOffset is in seconds
//		if( binDataBtn.getSelection() ) {
//			rscDefn.setBinOffset( 
//					new BinOffset( binEndIntrvlSpnr.getSelection()*60,
//								   binStartIntrvlSpnr.getSelection()*60 ) );
//		}
//		else {
//			rscDefn.setBinOffset( null );
//		}
		
		if( editParamsTxt.isVisible() ) {			
			if( !rscDefn.getResourceParametersAsString().equals( 
					editParamsTxt.getText() ) ) {
				rscDefn.setResourceParamsModified( true );
				 
				rscDefn.setResourceParametersFromString( editParamsTxt.getText() );
			}
		}

		String filtLabelsStr = filterLabelsTxt.getText();
//		filtLabelsStr.replace('\n', "");
		String filtLabelsArray[] = filtLabelsStr.split(",");
		
		rscDefn.setFilterLabels( new ArrayList<String>() );
		
		for( String filtLabel : filtLabelsArray ) {
			rscDefn.addFilterLabel( filtLabel.trim() );
		}
		
		// don't save the ${
		if( !rscDefn.applyAttrSetGroups() ) {
			String subTypeGenStr = subTypeGenTxt.getText();
//			int indx = subTypeGenStr.indexOf( "${" );
//			if( indx != -1 ) {
//				subTypeGenStr = subTypeGenStr.substring( indx );
//				indx = subTypeGenStr.indexOf("}");
//				if( indx != -1 ) {
//					subTypeGenStr = subTypeGenStr.substring(0, indx);
//				}
//			}
			rscDefn.setSubTypeGenerator( subTypeGenStr );
			
			// 
			//rscDefn.getGeneratedSubTypesList().clear();
		}		
	}
	
	private String subTypeGenToolTipText = "This is the name of a DB column used to generate "
		+ "the Sub-Types for this resource type.";
}