/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.WatchFormatDlg
 * 
 * 20 January 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;

import java.awt.Color;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Singleton for a watch format dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#159		B. Yin   	Initial Creation.
 * 03/06/11     #707        Q.Zhou      Changed FORECASTER text to combo. Load from forecaster.xml.
 * 03/12		#703		B. Yin		Create SEL, SAW, WOU, etc.
 * 05/12		#776, 769	B. Yin		Added UTC time. Carry ove info from WCC/WCL dialogs.
 * 08/12        #770        Q. Zhou     added continuing Watch.
 * 08/13		TTR 796		B. Yin		Added drop down list for expiration time.
 * 12/13		TTR 800		B. Yin		USe UTC time class.
 * </pre>
 * 
 * @author	B. Yin
 */

public class WatchFormatDlg  extends CaveJFACEDialog  {

	//instance of text dialog
	private WatchFormatMsgDlg msgDlg;
	
	//instance of watch format dialog
	static private WatchFormatDlg INSTANCE = null;
	
	//instance of the watch box 
	private WatchBox wb;
	
	//instance of watch box attr dialog
	private WatchBoxAttrDlg wbDlg;  
	
	//top level container of all widgets
	private Composite top;

	//issue status buttons
	private Button testBtn;
	private Button actvBtn;
	
	//weather type buttons
	private Button tornadoBtn;
	private Button stormBtn;
	
	//time zone buttons
	private Button estBtn, cstBtn, mstBtn, pstBtn, edtBtn, cdtBtn, mdtBtn, pdtBtn;
	
	//severity buttons
	private Button normalBtn;
	private Button pdsBtn;
	
	//watch number
	private Text   watchNumber;
	
	//forecaster name
	private Combo   forecasterCombo;
	private static String FORECASTERS[] = null;
	
	//expiration date and time
	private DateTime validDate;
	private Combo validTime;
	public static String HOURS[] =  { "0000", "0100", "0200", "0300", "0400", "0500", "0600", "0700", "0800",
									  "0900", "1000", "1100", "1200", "1300", "1400", "1500", "1600", "1700",
									  "1800", "1900", "2000", "2100", "2200", "2300"};

	//hail size
    private Combo hailCombo;
	public static String HAILSIZE[] =  { "0.5", "1.0", "1.5", "2.0", "2.5", "3.0", "3.5", "4.0" };
	
	//wind gust
	private Combo windCombo;
	public static String WINDGUST[] =  { "50", "60", "70", "80", "90", "100" };
	
	//top level
	private Combo topCombo;
	public static String TOPLEVEL[] =  { "300", "350", "400", "450", "500", "550","600", "650", "700" };
		
	//motion combo, direction and speed
	private Combo dirCombo;
	public static String MOVEDIR[] =  { "180", "190", "200", "210", "220", "230", "240", "250", "260",
				"270", "280", "290", "300", "310", "320", "330", "340", "350", "360"};
	private Combo spdCombo;
	public static String MOVESPD[] =  { "10", "15", "20", "25", "30", "35","40", "45", "50","55","60","65","70" };

	public static final String S_STORM = "SEVERE THUNDERSTORM";
	public static final String TORNADO = "TORNADO";
	
	private static final Color STORM_COLOR = Color.CYAN;
	private static final Color STORM_FILL_COLOR = new Color(255,191,201);

	private static final Color TORNADO_COLOR = Color.RED;
	private static final Color TORNADO_FILL_COLOR = new Color(255,215,0);

	//replace watch number
	private Text rText;
	
	//continue watch number
	private Text cText;
	
	//states text 
	private Text states;

	/**
	 * Protected constructor
	 * @param parentShell
	 */
	protected WatchFormatDlg(Shell parentShell, WatchBoxAttrDlg wbDlg ) {
		
		super(parentShell);
		this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE );
		
		this.setWbDlg(wbDlg);
		
		WatchCoordDlg.readForecasterTbl();
		FORECASTERS = WatchCoordDlg.getForecasters();
	}
	
	/**
	 * Return the instance of the singleton.
	 * @param parShell
	 * @return
	 */
	public static WatchFormatDlg getInstance( Shell parShell, WatchBoxAttrDlg wbDlg ){

		if ( INSTANCE == null ){

			INSTANCE = new WatchFormatDlg( parShell, wbDlg );
			
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
	        GridLayout mainLayout = new GridLayout(2, false);
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
		this.getShell().setText("Format Watch");
	
		//Create issue status
		Label statusLbl = new Label(top, SWT.LEFT);
		statusLbl.setText("Issue Status:");

		Composite status = new Composite(top, SWT.NONE);
		GridLayout btnGl = new GridLayout(2, false);
		status.setLayout(btnGl);

		testBtn = new Button(status, SWT.RADIO);
		testBtn.setText("Test");
		actvBtn = new Button(status, SWT.RADIO);
		actvBtn.setText("Active");
		actvBtn.setSelection(true);
		
		//Create watch number
		Label wnLbl = new Label(top, SWT.LEFT);
		wnLbl.setText("Watch Number:");
		watchNumber = new Text(top, SWT.SINGLE | SWT.RIGHT | SWT.BORDER );
		
		watchNumber.addVerifyListener(new VerifyListener(){

			@Override
			public void verifyText(VerifyEvent e) {
				e.doit = PgenUtil.validatePositiveInteger(e);
				if ( ! e.doit ) Display.getCurrent().beep();
			}
		});

		//Create expiration time
		Label timeLbl = new Label(top, SWT.LEFT);
		timeLbl.setText("Expiration Time:");

		Composite dt = new Composite(top, SWT.NONE);
		GridLayout dtGl = new GridLayout(2, false);
		dt.setLayout(dtGl);
		validDate = new DateTime(dt, SWT.BORDER | SWT.DATE );

		Calendar expTime = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		expTime.add(Calendar.HOUR, 8);

		validDate.setYear( expTime.get(Calendar.YEAR));
		validDate.setMonth( expTime.get(Calendar.MONTH));
		validDate.setDay( expTime.get(Calendar.DAY_OF_MONTH));
		
		Composite c1 = new Composite(dt, SWT.NONE);
		c1.setLayout( new FormLayout());

		validTime = new Combo( c1, SWT.DROP_DOWN | SWT.MULTI );;

		FormData fd = new FormData();
		fd.top = new FormAttachment(watchNumber,2, SWT.BOTTOM);
		fd.left = new FormAttachment(validDate, 5, SWT.RIGHT);
		fd.width = 80;;
		validTime.setLayoutData(fd);

		for ( String hrStr : HOURS ) {
			validTime.add( hrStr );
		}
		
		setUTCTimeTextField(c1, validTime,  expTime, watchNumber, 5);

		//create watch type
		Label typeLbl = new Label(top, SWT.LEFT);
		typeLbl.setText("Watch Type:");

		Composite wType = new Composite(top, SWT.NONE);
		wType.setLayout(btnGl);

		stormBtn = new Button(wType, SWT.RADIO);
		stormBtn.setText("Svr T'STORM");
		
		tornadoBtn = new Button(wType, SWT.RADIO);
		tornadoBtn.setText("Tornado");
		
		//create severity
		Label sevLbl = new Label(top, SWT.LEFT);
		sevLbl.setText("Severity:");

		Composite sev = new Composite(top, SWT.NONE);
		sev.setLayout(btnGl);

		normalBtn = new Button(sev, SWT.RADIO);
		normalBtn.setText("Normal");
		normalBtn.setSelection(true);
		pdsBtn = new Button(sev, SWT.RADIO);
		pdsBtn.setText("PDS");
		
		//create time zone
		Label zoneLbl = new Label(top, SWT.LEFT);
		zoneLbl.setText("Time Zone:");

		Composite zone = new Composite(top, SWT.NONE);
		GridLayout zoneGl = new GridLayout(4, false);
		zone.setLayout(zoneGl);
		
		estBtn = new Button(zone, SWT.RADIO);
		estBtn.setText("EST");
		cstBtn = new Button(zone, SWT.RADIO);
		cstBtn.setText("CST");
		mstBtn = new Button(zone, SWT.RADIO);
		mstBtn.setText("MST");
		pstBtn = new Button(zone, SWT.RADIO);
		pstBtn.setText("PST");
		
		edtBtn = new Button(zone, SWT.RADIO);
		edtBtn.setText("EDT");
		cdtBtn = new Button(zone, SWT.RADIO);
		cdtBtn.setText("CDT");
		mdtBtn = new Button(zone, SWT.RADIO);
		mdtBtn.setText("MDT");
		pdtBtn = new Button(zone, SWT.RADIO);
		pdtBtn.setText("PDT");
		
		//Create hail size
		Label hailLbl = new Label(top, SWT.LEFT);
		hailLbl.setText("Max Hail Size (in.):");

		hailCombo = new Combo( top, SWT.DROP_DOWN | SWT.MULTI );
		hailCombo.setLayoutData(new GridData(80,30));

		for ( String hailSize : HAILSIZE ) {
			hailCombo.add( hailSize );
		}

		hailCombo.select( 3 );
		
		
		//Create wind gust
		Label windLbl = new Label(top, SWT.LEFT);
		windLbl.setText("Max Wind Gust (kts):");

		windCombo = new Combo( top, SWT.DROP_DOWN | SWT.MULTI );
		windCombo.setLayoutData(new GridData(80,30));
		
		for ( String wind : WINDGUST ) {
			windCombo.add( wind );
		}

		windCombo.select( 1 );	

		//Create top level
		Label topLbl = new Label(top, SWT.LEFT);
		topLbl.setText("Max Top (100 feet):");

		topCombo = new Combo( top, SWT.DROP_DOWN | SWT.MULTI );
		topCombo.setLayoutData(new GridData(80,30));

		for ( String top : TOPLEVEL ) {
			topCombo.add( top );
		}

		topCombo.select( 4 );		
		
		//create motion combo
		Label moveLbl = new Label(top, SWT.LEFT);
		moveLbl.setText("Motion Vector(deg):");

		Composite moveVec = new Composite(top, SWT.NONE);
		GridLayout moveLayout = new GridLayout(3, false);
		moveVec.setLayout(moveLayout);
		
		dirCombo = new Combo( moveVec, SWT.DROP_DOWN | SWT.MULTI ); 
		dirCombo.setLayoutData(new GridData(80,30));
		for ( String st : MOVEDIR ) {
			dirCombo.add( st );
		}

		dirCombo.select( 6 );
		
		Label spdLbl = new Label(moveVec, SWT.LEFT);
		spdLbl.setText("(kts):");
		
		spdCombo = new Combo( moveVec, SWT.DROP_DOWN | SWT.MULTI ); 
		spdCombo.setLayoutData(new GridData(80,30));
		for ( String st : MOVESPD ) {
			spdCombo.add( st );
		}

		spdCombo.select( 5 );
		
		//create States included
		Label stLabel = new Label(top, SWT.NONE);
		stLabel.setText("States Included:");
		states = new Text(top, SWT.MULTI);
		states.setEditable(false);
		
		String stStr ="";
		for ( String str : wb.getStates() ){
			stStr += str + " ";
		}
	
		states.setText(stStr);
		
		//Create replace watch number text
		Label replaceLbl = new Label(top, SWT.LEFT);
		replaceLbl.setText("Replace Watch#:");
		rText = new Text(top, SWT.SINGLE | SWT.RIGHT | SWT.BORDER );
		
		rText.addVerifyListener(new VerifyListener(){

			@Override
			public void verifyText(VerifyEvent e) {
				e.doit = PgenUtil.validatePositiveInteger(e);
				if ( ! e.doit ) Display.getCurrent().beep();
			}
		});
		
		//Create continue watch number text
		List<String> contWatch = PgenStaticDataProvider.getProvider().loadContWatchNum();//loadContWatchNum();
		String cont = "";
		if (contWatch != null && !contWatch.isEmpty()) {
			for (int i=0; i< contWatch.size(); i++)
				cont += contWatch.get(i) +" ";
			
			if (cont.endsWith(" "))
				cont = cont.substring(0, cont.length()-1);
					
		}
		else {
			cont = "0000";
		}
		
		Label continueLbl = new Label(top, SWT.LEFT);
		continueLbl.setText("Continue Watch#:");
		cText = new Text(top, SWT.SINGLE | SWT.RIGHT | SWT.BORDER );
		
		cText.setText(cont);
		cText.setEditable(true);
		
	// Continue watch number can be a string like "510,512".
	//	cText.addVerifyListener(new VerifyListener(){

	//		@Override
	//		public void verifyText(VerifyEvent e) {
	//			e.doit = PgenUtil.validatePositiveInteger(e);
	//			if ( ! e.doit ) Display.getCurrent().beep();
	//		}
	//	});

		//Create forecaster list
		Label forecasterLbl = new Label(top, SWT.LEFT);
		forecasterLbl.setText("Forecaster:");
		forecasterCombo = new Combo(top, SWT.DROP_DOWN );
		for ( String str : FORECASTERS ){
        	forecasterCombo.add(str);
        }
				
		AttrDlg.addSeparator(top);

	}

	@Override
	/**
	 * Set the location of the dialog and initialize the widgets
	 */
	public int open(){

		if ( this.getShell() == null ){
			this.create();
		}
		
   	    this.getShell().setLocation(this.getShell().getParent().getLocation());
   	    
	   	initDlg();

   	    return super.open();
		
	}
	
	/**
	 * Get expiration time
	 * @return
	 */
	private Calendar getExpirationTime(){
		Calendar expiration = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		expiration.set(validDate.getYear(), validDate.getMonth(), validDate.getDay(), 
				getExpHour(), getExpMinute(), 0);
		expiration.set(Calendar.MILLISECOND, 0);
		return expiration;
	}
	

	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	public void okPressed(){
		
		String err= checkErr();
		if ( !err.isEmpty() ){
			MessageDialog infoDlg = new MessageDialog( 
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
					"Warning!", null, err,
					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
			infoDlg.open();
		}
		else {
			applyOnWB(wb);
			wbDlg.drawingLayer.resetElement(wb);
			wbDlg.mapEditor.refresh();
		}
	}
	
	/**
	 * Create buttons on the button bar
	 */
	@Override
	public void createButtonsForButtonBar(Composite parent){

		GridLayout barGl = new GridLayout(3, false);
		parent.setLayout(barGl);

		Button contBtn = new Button( parent, SWT.PUSH);

		super.createButtonsForButtonBar(parent);

		contBtn.setText("Continue");
		contBtn.setLayoutData(getButton(IDialogConstants.CANCEL_ID).getLayoutData());
		contBtn.addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {

				String err = checkErr();
				if ( !(err.isEmpty()) ){
					MessageDialog infoDlg = new MessageDialog( 
							PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
							"Warning!", null, err,
							MessageDialog.INFORMATION, new String[]{"OK"}, 0);
					infoDlg.open();
				}
				else {
					applyOnWB(wb);
					wbDlg.drawingLayer.resetElement(wb);
					wbDlg.mapEditor.refresh();
					msgDlg = new WatchFormatMsgDlg(WatchFormatDlg.this.getParentShell(),
							WatchFormatDlg.this);
					msgDlg.setBlockOnOpen(false);
					//msgDlg.setMessage(wb.convert2Text());
					msgDlg.open();
				}
			}

		});

		getButton(IDialogConstants.OK_ID).setText("Apply");
	}

	/**
	 * Check if all required info are filled in.
	 * @return string of the error message
	 */
	private String checkErr(){
		boolean showErr = false;
		String err = "The following Problems have been identified:\n\n";
		if ( watchNumber.getText().isEmpty() ){
			showErr = true;
			err += "\tWatch number is invalid.\n";

		}
		if ( !stormBtn.getSelection() && !tornadoBtn.getSelection()){
			showErr = true;
			err += "\tWeather type is not selected.\n";
		}
		if ( getTimeZone().isEmpty() ){
			showErr = true;
			err += "\tTime zone is not selected.\n";
		}
		if (forecasterCombo.getText().isEmpty()){
			showErr = true;
			err += "\tForecaster is not selected.";
		}
		
		if ( showErr )
		return err;
		else 
			return "";
		
	}
	
	/**
	 * get time zone
	 * @return time zone
	 */
	private String getTimeZone(){
		if ( estBtn.getSelection() ){
			return estBtn.getText();
		}
		else if ( cstBtn.getSelection() ){
			return cstBtn.getText();
		}
		else if ( mstBtn.getSelection() ){
			return mstBtn.getText();
		}
		else if ( pstBtn.getSelection() ){
			return pstBtn.getText();
		}
		else if ( edtBtn.getSelection() ){
			return edtBtn.getText();
		}
		else if ( cdtBtn.getSelection() ){
			return cdtBtn.getText();
		}
		else if ( mdtBtn.getSelection() ){
			return mdtBtn.getText();
		}
		else if ( pdtBtn.getSelection() ){
			return pdtBtn.getText();
		}
		else {
			return "";
		}
	}
	
	/**
	 * Set the time zone
	 * @param zone - time zone
	 */
	private void setTimeZone(String zone ){
		
		if ( zone == null ) return;
		
		if ( zone.equalsIgnoreCase("est")){
			estBtn.setSelection(true);
		}
		if ( zone.equalsIgnoreCase("cst")){
			cstBtn.setSelection(true);
		}	
		if ( zone.equalsIgnoreCase("mst")){
			mstBtn.setSelection(true);
		}	
		if ( zone.equalsIgnoreCase("pst")){
			pstBtn.setSelection(true);
		}	
		if ( zone.equalsIgnoreCase("edt")){
			edtBtn.setSelection(true);
		}
		if ( zone.equalsIgnoreCase("cdt")){
			cdtBtn.setSelection(true);
		}	
		if ( zone.equalsIgnoreCase("mdt")){
			mdtBtn.setSelection(true);
		}	
		if ( zone.equalsIgnoreCase("pdt")){
			pdtBtn.setSelection(true);
		}	
	}
	
	/**
	 * Get Watch type
	 * @return string - watch type
	 */
	private String getWatchType(){
		String type ="";
		if (stormBtn.getSelection()){
			type = S_STORM;
		}
		else if (tornadoBtn.getSelection()){
			type = TORNADO;
		}
		return type;
	}
	
	/**
	 * Get issue status
	 * @return string - issue status
	 */
	private String getStatus(){
		String type ="";
		if (testBtn.getSelection()){
			type = testBtn.getText();
		}
		else if (actvBtn.getSelection()){
			type =actvBtn.getText();
		}
		return type;
	}
	
	/**
	 * Get PDS type
	 * @return string - PDS type
	 */
	private String getPdsType(){
		String type ="";
		if (pdsBtn.getSelection()){
			type = "PDS";
		}
		else if (normalBtn.getSelection()){
			type ="NORMAL";
  		}
		return type;
	}
	
	/**
	 * Get watch number
	 * @return int - watch number
	 */
	public int getWatchNumber(){
		if ( watchNumber.getText() == null || watchNumber.getText().isEmpty() ){
			return 0;
		}
		else {
			return Integer.valueOf(watchNumber.getText());
		}
	}
	
	private String getContNumber(){
		if ( cText.getText() == null || cText.getText().isEmpty() ){
			return "";
		}
		else {
			return cText.getText();
		}
	}
	
	private int getReplaceNumber(){
		if ( rText.getText() == null || rText.getText().isEmpty() ){
			return 0;
		}
		else {
			return Integer.valueOf(rText.getText());
		}
	}
	/**
	 * Apply attributes from the format dialog to the watch box element 
	 * @param wb
	 */
	private void applyOnWB(WatchBox wb){
		wb.setIssueStatus(this.getStatus());
		wb.setIssueTime(Calendar.getInstance( TimeZone.getTimeZone("GMT") ));
		wb.setExpTime(getExpirationTime());
		wb.setSeverity(getPdsType());
		wb.setTimeZone(getTimeZone());
		wb.setHailSize(Float.valueOf(hailCombo.getText()));
		wb.setGust(Integer.valueOf(windCombo.getText()));
		wb.setTop(Integer.valueOf(topCombo.getText()));
		wb.setMoveDir(Integer.valueOf(dirCombo.getText()));
		wb.setMoveSpeed(Integer.valueOf(spdCombo.getText()));
		wb.setStatesIncl(states.getText());
		wb.setAdjAreas("");
		wb.setIssueFlag(1);
		wb.setWatchType(getWatchType());
		wb.setWatchNumber(Integer.valueOf(watchNumber.getText()));
		wb.setForecaster(forecasterCombo.getText());
		wb.setEndPointAnc(String.format("%1$s - %2$s", wb.getRelative(wb.getPoints().get(0),wb.getAnchors()[0]),
				wb.getRelative(wb.getPoints().get(4), wb.getAnchors()[1])));
		wb.setEndPointVor(String.format("%1$s - %2$s", wb.getRelative(wb.getPoints().get(0),wb.getNearestVor(wb.getPoints().get(0))),
				wb.getRelative(wb.getPoints().get(4),wb.getNearestVor(wb.getPoints().get(4)))));
		wb.setHalfWidthSm((int)Math.round(wb.getHalfWidth()/PgenUtil.SM2M));
		wb.setHalfWidthNm((int)Math.round(wb.getHalfWidth()/PgenUtil.NM2M/5.0)*5);
		wb.setWathcAreaNm((int)Math.round(wb.getWatchArea()));
		wb.setCntyInfo(wb.formatCountyInfo(wb.getCountyList()));
		
		if ( wb.getWatchType().equalsIgnoreCase(S_STORM)){
			wb.setColors(new Color[]{STORM_COLOR, STORM_FILL_COLOR});
		}
		else if ( wb.getWatchType().equalsIgnoreCase(TORNADO)){
			wb.setColors(new Color[]{TORNADO_COLOR, TORNADO_FILL_COLOR});
		}
		
		wb.setContWatch( this.getContNumber());
		wb.setReplWatch(this.getReplaceNumber());
	}

	/**
	 * Get info from the watch element and set the values of widgets
	 */
	private void initDlg(){
		
		if ( wb.getIssueFlag() != 0){

			if ( wb.getIssueStatus() != null ){
				if( wb.getIssueStatus().equalsIgnoreCase("Test") ){
					testBtn.setSelection(true);
				}
				else if ( wb.getIssueStatus().equalsIgnoreCase("Active")){
					actvBtn.setSelection(true);
				}
			}

			watchNumber.setText(String.valueOf(wb.getWatchNumber()));
			
			setTimeZone(wb.getTimeZone());
			
			hailCombo.setText(String.valueOf(wb.getHailSize()));
			windCombo.setText(String.valueOf(wb.getGust()));
			topCombo.setText(String.valueOf(wb.getTop()));
			dirCombo.setText(String.valueOf(wb.getMoveDir()));
			spdCombo.setText(String.valueOf(wb.getMoveSpeed()));
			
			cText.setText(String.valueOf(wb.getContWatch()));

   	    }
   	    
		Calendar exp = wb.getExpTime();
		if ( exp != null ){
			validDate.setDate(exp.get(Calendar.YEAR), exp.get(Calendar.MONTH), exp.get(Calendar.DAY_OF_MONTH));
			validTime.setText(getInitialTime( exp ));

		}

		if ( wb.getWatchType() != null ){
			if (wb.getWatchType().equalsIgnoreCase("Tornado")){
				tornadoBtn.setSelection(true);
			}
			else if (wb.getWatchType().equalsIgnoreCase("SEVERE THUNDERSTORM")){
				stormBtn.setSelection(true);
			}
		}

		
		rText.setText(String.valueOf(wb.getReplWatch()));
		
		if ( wb.getForecaster() != null )
			forecasterCombo.setText(wb.getForecaster());
	}

	/**
	 * Get watch element
	 * @return WatchBox - watch element
	 */
	public WatchBox getWatchBox(){
		return wb;
	}
	
	/**
	 * Set Watch element
	 * @param wb - watch element
	 */
	public void setWatchBox( WatchBox wb ){
		this.wb = wb;
	}

	/**
	 * Set the WatchBox attribute dialog
	 * @param wbDlg
	 */
	public void setWbDlg(WatchBoxAttrDlg wbDlg) {
		this.wbDlg = wbDlg;
	}

	/**
	 * Get the watch box attribute dialog
	 * @return watch box attribute dialog
	 */
	public WatchBoxAttrDlg getWbDlg() {
		return wbDlg;
	}
	
	/**
	 * Close the format dialog and the message dialog
	 */
	@Override
	public boolean close(){
		if ( msgDlg != null ) msgDlg.close(); 
		return super.close();
	}
	
	/**
	 *		Sets the UTC time text widget
	 */
	public static void setUTCTimeTextField( Composite parent, final Combo validTime, Calendar cal,  Control topWidget, int offset ){
		
		validTime.setTextLimit(4);
		validTime.setText( String.format("%02d00", cal.get(Calendar.HOUR_OF_DAY)));
		validTime.addVerifyListener( new VerifyListener(){

			@Override
			public void verifyText(VerifyEvent ve) {
				final char BACKSPACE = 0x08;
				final char DELETE = 0x7F;
				
				if ( Character.isDigit(ve.character) || Character.UNASSIGNED == ve.character ||
					  ve.character == BACKSPACE || ve.character == DELETE ) ve.doit = true;
				else {
					ve.doit = false;
					Display.getCurrent().beep();
				}
			}} );
		
		validTime.addModifyListener( new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				if ( !validTime.getText().isEmpty() ){
					if ( isTimeValid( validTime.getText() ) )
						validTime.setBackground( Display.getCurrent().getSystemColor( SWT.COLOR_WHITE));
					else
						validTime.setBackground( Display.getCurrent().getSystemColor( SWT.COLOR_RED));
				}
			}
			
		});
		
	}
	/**
 	 *  check if the input string is a valid time
     */
	private static boolean isTimeValid( String text) {
		int time = Integer.parseInt(text);
		int hour = time / 100;
		int minute = time % 100;
		
		if ( hour >= 0 && hour <= 23 &&
				minute >= 00 && minute <=59 ) return true;
		
		return false;
	}
	
	/**
     *	Get the time string in the format HH00 from the input calendar time.
	 */
	private String getInitialTime(Calendar now ) {
		
		int minute = now.get(Calendar.MINUTE);
		if ( minute >= 15 ) now.add(Calendar.HOUR_OF_DAY, 1);
		int hour = now.get(Calendar.HOUR_OF_DAY);

		return String.format("%02d00", hour);
	}
	
	/**
 	 * Get the expiration hour from the validTime text widget
     */
	private int getExpHour( ){
		int ret =0;
		try {
			String hm = validTime.getText();
			ret = Integer.parseInt(hm.substring(0, hm.length()== 4 ? 2:1 ));
		}
		catch (Exception e ){
			
		}
		return ret;
	}
	
	/**
 	 * Get the expiration minute from the validTime text widget
     */
	private int getExpMinute(){
		int ret =0;
		try {
			String hm = validTime.getText();
			ret = Integer.parseInt(hm.substring(hm.length()== 4 ? 2:1 ), hm.length()-1);
		}
		catch (Exception e ){
			
		}
		return ret;
	}
}


