/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.WatchStatusDlg
 * 
 * 20 January 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenToolUtils;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implementation of a watch status dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10		#159		B. Yin   	Initial Creation.
 * 12/13		TTR 800		B. Yin		Use UTC time class.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class WatchStatusDlg  extends CaveJFACEDialog{
	
	//minutes to round issue time 
	private static final int ITIME_RND = 5;        

	//instance of the Watch element working on
	private WatchBox wb;
	
	//top level container of all widgets
	private Composite top;
	
	//Watch number
	private Text watchNumber;
	
	//discussion reference number
	private Button refBtn;
	private Text refTxt;
	
	//Status Message Dialog
	private WatchStatusMsgDlg statusMsgDlg;
	
	//Direction combo
	private Combo dirCombo;
	private static String dir[] =  { "RIGHT", "LEFT", "NORTH", "SOUTH", "EAST", "WEST", 
		"NORTHEAST", "SOUTHEAST", "NORTHWEST", "SOUTHWEST", 
		"NORTH AND EAST", "SOUTH AND EAST", "NORTH AND WEST", "SOUTH AND WEST" };
	
	//valid date and time
	private DateTime validDate;
	private UTCTimeText validTime;
	
	//expiration check box and date/time widgets
	private Button expBtn;
	private DateTime expDate;
	private UTCTimeText expTime;
	private Text lnTxt;
	
	//forecaster name
	private Text forecaster;
	
	//issue time
	private Calendar issueTime;
	
	/**
	 * Protected constructor
	 * @param parentShell
	 */
	protected WatchStatusDlg(Shell parentShell, WatchBox wb ) {
		
		super(parentShell);
		this.wb = wb;
		
	}
	
	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {

		//top level container
		top = (Composite) super.createDialogArea(parent);

		// set title
		this.getShell().setText("Watch Status");

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(1, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		mainLayout.verticalSpacing = 3;
		top.setLayout(mainLayout);

		Composite wnComp = new Composite(top, SWT.NONE);
		wnComp.setLayout(new GridLayout(4, false));
		
		//Create watch number
		Label wnLbl = new Label(wnComp, SWT.LEFT);
		wnLbl.setText("Watch:");
		watchNumber = new Text(wnComp, SWT.SINGLE | SWT.RIGHT | SWT.BORDER );

		watchNumber.addVerifyListener(new VerifyListener(){

			@Override
			public void verifyText(VerifyEvent e) {
				e.doit = PgenUtil.validatePositiveInteger(e);
				if ( ! e.doit ) Display.getCurrent().beep();
			}

		});

		//Create FromLine
		if ( wb != null && wb.hasStatusLine() ) {
			Label dirLbl = new Label(wnComp, SWT.LEFT);
			dirLbl.setText("Continues To The");

			dirCombo = new Combo( wnComp, SWT.DROP_DOWN | SWT.READ_ONLY );

			for ( String str : dir ) {
				dirCombo.add( str );
			}

			dirCombo.select( 0 );

			Composite lnComp = new Composite(top, SWT.NONE);
			lnComp.setLayout(new GridLayout(2, false));

			Label lnLbl = new Label(lnComp, SWT.LEFT);
			lnLbl.setText("Of The Line");
			lnTxt = new Text(lnComp, SWT.SINGLE | SWT.LEFT | SWT.BORDER );
			lnTxt.setLayoutData(new GridData(350,20));
		}
		else {
			Label dirLbl = new Label(wnComp, SWT.LEFT);
			dirLbl.setText("Continues Across Entire Area");
		}

		AttrDlg.addSeparator(top);

		//create reference discussion number
		Composite refComp = new Composite(top, SWT.NONE);
		refComp.setLayout(new GridLayout(2, false));
		refBtn = new Button( refComp, SWT.CHECK);
		refBtn.setText("Reference Mesoscale Discussion#:");
		refTxt = new Text(refComp, SWT.SINGLE | SWT.RIGHT | SWT.BORDER );
		refTxt.addVerifyListener(new VerifyListener(){

			@Override
			public void verifyText(VerifyEvent e) {
				e.doit = PgenUtil.validatePositiveInteger(e);
				if ( ! e.doit ) Display.getCurrent().beep();
			}

		});

		//Expiration date/time
		Composite expDt = new Composite(top, SWT.NONE);
		expDt.setLayout( new GridLayout(3, false) );
		expBtn = new Button( expDt, SWT.CHECK);
		expBtn.setText("Final Status - Expiration Time:");

		expDate = new DateTime(expDt, SWT.BORDER | SWT.DATE );
		
		//expTime = new DateTime(expDt, SWT.BORDER | SWT.TIME | SWT.SHORT );
		expTime = new UTCTimeText(expDt, SWT.SINGLE | SWT.BORDER | SWT.CENTER);

		//FormData fd2 = new FormData();
		//fd2.top = new FormAttachment(refComp, 2, SWT.BOTTOM);
		//fd2.left = new FormAttachment(expDate, 5, SWT.RIGHT);
		//expTime.setLayoutData(fd2);
		
		expTime.setUTCTimeTextField(expDt, Calendar.getInstance(), null, 5, false);
		
		//Valid date/time
		Label validLbl = new Label( expDt, SWT.NONE);
		validLbl.setText("Status Valid Until:");

		validDate = new DateTime(expDt, SWT.BORDER | SWT.DATE );
		validTime = new UTCTimeText(expDt, SWT.BORDER | SWT.TIME | SWT.SHORT );
		validTime.setUTCTimeTextField(expDt, Calendar.getInstance(), null, 5, false);

		//forecaster
		Composite fcstComp = new Composite(top, SWT.NONE);
		fcstComp.setLayout( new GridLayout(2, false) );
		Label fcstLbl = new Label( fcstComp, SWT.NONE);
		fcstLbl.setText("Forecaster:");
		forecaster = new Text(fcstComp, SWT.SINGLE | SWT.LEFT | SWT.BORDER );
		forecaster.setLayoutData( new GridData( 150, 15 ) );
		AttrDlg.addSeparator(top);

		return top;

	}
	
	@Override
	/**
	 * Set the location of the dialog and initialize the widgets
	 */
	public int open(){

		if ( this.getShell() == null ){
			this.create();
		}
		
   	  //  this.getShell().setLocation(this.getShell().getParent().getLocation());
		
		this.getButton(IDialogConstants.OK_ID).setText("Format");
				
		if ( wb.getIssueFlag() != 0 )
			initDlg();

   	    return super.open();
		
	}
	
	/**
	 * Initialize the dialog (set values of watch number, etc.)
	 */
	private void initDlg(){
		watchNumber.setText(String.valueOf(wb.getWatchNumber()));
		if ( lnTxt != null ) lnTxt.setText(generateFromLine());
		
		if ( wb.getStatusForecaster() != null ){
			forecaster.setText(wb.getStatusForecaster());
		}
		else {
			forecaster.setText(wb.getForecaster());
		}
		
		Calendar exp = wb.getExpTime();
		if ( exp != null ){
			expDate.setDate(exp.get(Calendar.YEAR), exp.get(Calendar.MONTH), exp.get(Calendar.DAY_OF_MONTH));
			expTime.setTime(exp.get(Calendar.HOUR_OF_DAY), exp.get(Calendar.MINUTE));
		}
		
		Calendar valid = this.generateIssueAndValidTime();
		validDate.setDate(valid.get(Calendar.YEAR), valid.get(Calendar.MONTH), valid.get(Calendar.DAY_OF_MONTH));
		validTime.setTime(valid.get(Calendar.HOUR_OF_DAY), valid.get(Calendar.MINUTE));
	}
	
	/**
	 * Loop through all status lines and construct the from line text
	 * @return - FromLine text
	 */
	private String generateFromLine(){

		String ln = "";
		
		//get the Watch collection and look for status lines
		AbstractDrawableComponent adc = wb.getParent();
		if ( adc instanceof DECollection && adc.getName().equalsIgnoreCase("Watch")){
			
			Iterator<AbstractDrawableComponent> it = ((DECollection)adc).getComponentIterator();
			
			//get anchor point list
			List<Station> anchors = PgenStaticDataProvider.getProvider().getAnchorTbl().getStationList();
			
			//loop through status lines and construct from lines.
			while ( it.hasNext() ){
				AbstractDrawableComponent elem = it.next();
				if ( elem instanceof Line && elem.getPgenType().equalsIgnoreCase("POINTED_ARROW")){
					
					//add "AND" for additional lines
					if ( !ln.isEmpty() ) ln += " AND ";
					
					for(Coordinate pt : elem.getPoints()){
						Station st = WatchBox.getNearestAnchorPt(pt, anchors);
						
						GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

						gc.setStartingGeographicPoint(st.getLongitude(), st.getLatitude() );
						gc.setDestinationGeographicPoint(pt.x, pt.y  );

						long dist = Math.round( gc.getOrthodromicDistance()/PgenUtil.SM2M);
						long dir = Math.round(gc.getAzimuth());
						if ( dir < 0 ) dir += 360;
						ln += dist + " " + WatchBox.dirs[(int)Math.round(dir/22.5)]+ " " + st.getStid()+ " TO ";
					}
					
					//remove the last " TO "
					if (ln.length() >= 4 ) ln = ln.substring(0, ln.length() - 4 );
				}
			}
			
		}
		
		return ln;
		
	}
	
	/**
	 * Get the watch number from the dialog
	 * @return - watch number
	 */
	public int getWatchNumber(){
		if ( watchNumber.getText() == null ){
			return 0;
		}
		else {
			return Integer.valueOf(watchNumber.getText());
		}
	}
	
	/**
	 * Get the discussion reference number
	 * @return - reference number
	 */
	public int getDiscussionNumber(){
		if ( refTxt.getText() == null || refTxt.getText().isEmpty()){
			return 0;
		}
		else {
			return Integer.valueOf(refTxt.getText());
		}
	}
	
	/**
	 * Get the working watch element
	 * @return - watch box element
	 */
	public WatchBox getWatchBox(){
		return wb;
	}
	
	/**
	 * Get expiration time
	 * @return - Calendar expiration time
	 */
	private Calendar getExpirationTime(){
		
		if ( expBtn.getSelection() ){
			Calendar expiration = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
			expiration.set(expDate.getYear(), expDate.getMonth(), expDate.getDay(), 
					expTime.getHours(), expTime.getMinutes(), 0);
			expiration.set(Calendar.MILLISECOND, 0);
			return expiration;
		}
		else {
			return getValidTime();
		}
	}
	
	/**
	 * Get expiration time from dialog
	 * @return
	 */
	private Calendar getValidTime(){
		Calendar expiration = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		expiration.set(validDate.getYear(), validDate.getMonth(), validDate.getDay(), 
				    validTime.getHours(), validTime.getMinutes(), 0);
		expiration.set(Calendar.MILLISECOND, 0);
		return expiration;
	}
	
	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void okPressed() {
		
		//apply values from the dialog to watch box element 
		applyOnWB();
		
		//open status message window
		statusMsgDlg = new WatchStatusMsgDlg(this.getParentShell(), this );
		statusMsgDlg.setBlockOnOpen(false);
		statusMsgDlg.setMessage(generateStatusMsg());
		statusMsgDlg.open();
	}
	
	/**
	 * Apply values from the dialog to the watch box element
	 */
	public void applyOnWB(){
		
		wb.setWatchNumber(getWatchNumber());
		
		int dNum = 0;
		if ( refBtn.getSelection() ){
			dNum = getDiscussionNumber();
		}
		
		Calendar eTime = null;
		if ( expBtn.getSelection() ){
			eTime = getExpirationTime();
		}
		
		wb.setIssueTime( issueTime );
		wb.addStatus(getContinueText(), dNum, getValidTime(), eTime, forecaster.getText());
		
	}
	
	/**
	 * Generate watch status message
	 * @return - status message
	 */
	private String generateStatusMsg(){
		String msg ="";
		Calendar exp;
		if ( expBtn.getSelection() ){
			exp = this.getExpirationTime();
		}
		else {
			exp = this.getValidTime();
		}
		
		
		msg += "WOUS20 KWNS " +  String.format("%1$td%1$tH%1$tM", issueTime) +"\n" +
						"WWASPC" + "\n" +
						"SPC WW-A " + String.format("%1$td%1$tH%1$tM\n", exp) +"\n";
		
		String stateLine = "";
		for ( String st : wb.getStates() ){
			stateLine += st + "Z000-";
		}
		
		stateLine += String.format("%1$td%1$tH%1$tM", exp) + "-" + "\n";
		
		msg += stateLine + "\n" + 
				"STATUS REPORT ON WW " + getWatchNumber() + "\n\n" +
				PgenToolUtils.wrapWatchText("THE SEVERE WEATHER THREAT " + getContinueText(), 65) + "\n\n";
		
		if ( expBtn.getSelection() ){
			msg += "WW " + wb.getWatchNumber() +" WILL BE ALLOWED TO EXPIRE AT " +
					String.format("%1$td%1$tH%1$tM", exp) + "Z." + "\n\n";
		}
		
		if ( refBtn.getSelection() ){
			msg += "FOR ADDITIONAL INFORMATION SEE MESOSCALE DISCUSSION " +
					String.format("%1$04d", this.getDiscussionNumber()) +"\n\n" +
					".." + forecaster.getText().toUpperCase() + ".." + 
					String.format("%1$tm/%1$td/%1$ty", exp) +
					"\n\n";
		}
		
		msg += "ATTN...WFO...";
		
		for ( String wfo : wb.getWFOs() ){
			msg += wfo + "...";
		}
		
		msg += "\n\n&&\n\n" + "STATUS REPORT FOR WT " + wb.getWatchNumber() +"\n\n";
		
		msg += "SEVERE WEATHER THREAT CONTINUES FOR THE FOLLOWING AREAS\n\n";
		
		for ( String state : wb.getStates()){
			msg += wb.createCountyInfo(state, exp);
			msg += "$$\n\n\n";
		}
		
		msg += "THE WATCH STATUS MESSAGE IS FOR GUIDANCE PURPOSES ONLY. PLEASE\n" +
				"REFER TO WATCH COUNTY NOTIFICATION STATEMENTS FOR OFFICIAL\n" +
				"INFORMATION ON COUNTIES...INDEPENDENT CITIES AND MARINE ZONES\n" +
				"CLEARED FROM SEVERE THUNDERSTORM AND TORNADO WATCHES.\n" +
				"$$\n";
			
		return msg;
	}
	
	/**
	 * Construct the "Continues ..." text for the watch status message
	 * @return - String "continues..." text
	 */
	private String getContinueText(){
		
		String fromLn = "";
		if ( lnTxt != null ){
			fromLn ="CONTINUES TO THE " + dirCombo.getText() + " OF THE LINE " +
					lnTxt.getText();
		}
		else {
			fromLn ="CONTINUES ACROSS ENTIRE AREA";
		}
		
		return fromLn;
		
	}
	
	private Calendar generateIssueAndValidTime(){
		
		//get current time
		Calendar currTime = Calendar.getInstance( TimeZone.getTimeZone("GMT") );

		issueTime = (Calendar) currTime.clone();
		int min = currTime.get(Calendar.MINUTE);
		if ( min != 0 ){
			int remainder = min % ITIME_RND;
			int plusmin = ITIME_RND - remainder;
			issueTime.add(Calendar.MINUTE, plusmin);
		}
		
		Calendar validTime = (Calendar) issueTime.clone();
		validTime.add(Calendar.MINUTE, 60 - issueTime.get(Calendar.MINUTE) + 40);
		
		return validTime;

	}
	
}
