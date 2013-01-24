/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.WatchCoordDlg
 * 
 * 20 January 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.productmanage.ProductConfigureDialog;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProductType;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenToolUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Singleton for a watch coordination dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#159		B. Yin   	Initial Creation.
 * 03/06/11     #707        Q.Zhou      Changed FORECASTER text to combo. Load from forecaster.xml.
 * 03/12		#703		B. Yin		Generate product text from style sheet
 * 05/12		#769, 776	B. Yin		Added UTC time. Carry ove info to WatchFormat dialog.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class WatchCoordDlg  extends CaveJFACEDialog  {

	static final int LINE_LEN = 65;
    public static final String PGEN_FORECASTER      = "forecasters.xml";

	//instance of watch info dialog
	static private WatchCoordDlg INSTANCE = null;
	
	//Preliminary ID list
	private static final String[] ID_LIST = new String[] { "A","B","C","D",
		"E", "F", "G", "H","I","J" };
	
	//phone number list
	private static final String[] PHONE_LIST = new String[] { 
		"1-800-5551111","1-800-5552222","1-800-5553333","1-800-5554444",
		"1-800-5555555","1-800-5556666","1-800-5557777","1-800-5558888","1-800-5559999" };
		
	//instance of the watch box attribute dialog
	private WatchBoxAttrDlg wbDlg;
	
	//top level container of all widgets
	private Composite top;

	//weather type buttons
	private Button tornadoBtn;
	private Button stormBtn;
	
	//forecaster name
	private Combo   forecasterCombo;
	private static Document forecasterTbl = null;
	private static String FORECASTERS[] = null;
	private static String FORECASTER_XPATH = "/forecasters/forecaster";
	
	//expiration date and time
	private DateTime validDate;
	private Text validTime;
	
	//phone number combo
	private Combo phoneCombo;
	
	//preliminary Id combo
	private Combo idCombo;

	//proposed WFO text 
	private Text proposedWFOs;
	
	//WFO list pane, holding nearby wfo check boxes
	private Composite wfoPane;
	
	//WFO check boxes
	private List<Button> wfoBtns;
	
	private String dirPath;
	private Text rText ;
	/**
	 * Protected constructor
	 * @param parentShell
	 */
	protected WatchCoordDlg(Shell parentShell, WatchBoxAttrDlg wbDlg) {
		
		super(parentShell);
		this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE );
		
		this.wbDlg = wbDlg;
		wfoBtns = new ArrayList<Button>();
		
		forecasterTbl = readForecasterTbl();
		FORECASTERS = getForecasters();
        
	}
	
	/**
	 * Return the instance of the singleton.
	 * @param parShell
	 * @return
	 */
	public static WatchCoordDlg getInstance( Shell parShell, WatchBoxAttrDlg wbDlg){

		if ( INSTANCE == null ){

			INSTANCE = new WatchCoordDlg( parShell, wbDlg );
			
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
		this.getShell().setText("Watch Coordination");
		
		Composite panel1 = new Composite(top, SWT.NONE);
		GridLayout pLayout = new GridLayout(2, false);
		pLayout.marginHeight = 3;
		pLayout.marginWidth = 3;
		pLayout.verticalSpacing = 3;
		panel1.setLayout(pLayout);

		//create preliminary ID combo
		Label idLbl = new Label(panel1, SWT.LEFT);
		idLbl.setText("Preliminiary ID:");

		idCombo = new Combo( panel1, SWT.DROP_DOWN | SWT.READ_ONLY );       
		for ( String st : ID_LIST ) {
			idCombo.add( st );
		}

		idCombo.select( 0 );

		//create watch type
		Label typeLbl = new Label(panel1, SWT.LEFT);
		typeLbl.setText("Watch Type:");

		Composite wType = new Composite(panel1, SWT.NONE);
		GridLayout btnGl = new GridLayout(2, false);
		wType.setLayout(btnGl);

		stormBtn = new Button(wType, SWT.RADIO);
		stormBtn.setText("Svr T'Storm");
		tornadoBtn = new Button(wType, SWT.RADIO);
		tornadoBtn.setText("Tornado");

		//Create expiration time
		Label timeLbl = new Label(panel1, SWT.LEFT);
		timeLbl.setText("Expiration Time:");

		Composite dt = new Composite(panel1, SWT.NONE);
		GridLayout dtGl = new GridLayout(2, false);
		dt.setLayout(dtGl);
		validDate = new DateTime(dt, SWT.BORDER | SWT.DATE );
		//validTime = new DateTime(dt, SWT.BORDER | SWT.TIME | SWT.SHORT );

		Calendar expTime = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		validDate.setYear( expTime.get(Calendar.YEAR));
		validDate.setMonth( expTime.get(Calendar.MONTH));
		validDate.setDay( expTime.get(Calendar.DAY_OF_MONTH));

		Composite c1 = new Composite(dt, SWT.NONE);
		c1.setLayout( new FormLayout());

		validTime = new Text(c1, SWT.SINGLE | SWT.BORDER | SWT.CENTER);

		FormData fd = new FormData();
		//fd.top = new FormAttachment(watchNumber,2, SWT.BOTTOM);
		fd.left = new FormAttachment(validDate, 5, SWT.RIGHT);
		validTime.setLayoutData(fd);
		PgenUtil.setUTCTimeTextField(c1, validTime, expTime, wType, 5);
		//create phone list combo
		Label phoneLbl = new Label(panel1, SWT.LEFT);
		phoneLbl.setText("Phone Number:");

		phoneCombo = new Combo( panel1, SWT.DROP_DOWN | SWT.READ_ONLY );       
		for ( String st : PHONE_LIST ) {
			phoneCombo.add( st );
		}

		phoneCombo.select( 0 );

		//Create forecaster list
		Label forecasterLbl = new Label(panel1, SWT.LEFT);
		forecasterLbl.setText("Forecaster:");
		
		forecasterCombo = new Combo(panel1, SWT.DROP_DOWN );		
        for ( String str : FORECASTERS ){
        	forecasterCombo.add(str);
        }
        
		//Create replace watch number text
		Label replaceLbl = new Label(panel1, SWT.LEFT);
		replaceLbl.setText("Replace Watch#:");
		rText = new Text(panel1, SWT.SINGLE | SWT.RIGHT | SWT.BORDER );

		AttrDlg.addSeparator(top);

		Composite panel2 = new Composite(top, SWT.NONE);
		panel2.setLayout(pLayout);

		Label pwfoLabel = new Label(panel2, SWT.NONE);
		pwfoLabel.setText("Proposed WFOs:");
		proposedWFOs = new Text(panel2, SWT.MULTI);
		proposedWFOs.setEditable(false);
		proposedWFOs.setText(WatchInfoDlg.formatWfoStr(wbDlg.getWatchBox().getWFOs()));

		Label rwfoLabel = new Label(panel2, SWT.NONE);
		rwfoLabel.setText("Replaced WFOs:");
		Text replacedWFOs = new Text(panel2, SWT.MULTI);
		replacedWFOs.setEditable(false);

		Label nwfoLabel = new Label(panel2, SWT.NONE);
		nwfoLabel.setText("Nearby WFOs:");
		wfoPane = new Composite( panel2, SWT.None);
		GridLayout wfoGl = new GridLayout(4, false);
		wfoPane.setLayout(wfoGl);
		this.createWfoChkBoxes(wbDlg.getWatchBox().getNearbyWFOs());

		AttrDlg.addSeparator(top);

	}

	@Override
	/**
	 * Set the location of the dialog
	 */
	public int open(){

		if ( this.getShell() == null ){
			this.create();
		}
		
   	    this.getShell().setLocation(this.getShell().getParent().getLocation());
  	    this.getButton(IDialogConstants.OK_ID).setText("Format");
  	    this.getButtonBar().pack();
   	    return super.open();
		
	}


	/**
	 * Create wfo list and check boxes
	 * @param wfos
	 */
	private void createWfoChkBoxes(List<String> wfos ){

		//delete state check boxes that are not in the list
		if ( wfoBtns != null ){
			Iterator<Button> it = wfoBtns.iterator();
			while(it.hasNext() ){
				Button btn = it.next();
				btn.dispose();
				it.remove();
			}
		}

		//create WFO check boxes for the watch box
		if ( wfos != null && !wfos.isEmpty()) {
			for ( String st : wfos ){

				//if not in, create the state check box
				Button stBtn = new Button(wfoPane, SWT.CHECK);
				stBtn.setText(st);
				stBtn.setSelection(false);

				wfoBtns.add(stBtn);
				wfoPane.layout();
				wfoPane.pack(true);
			}

		}
	
		wfoPane.layout();
	}

	/**
	 * Check weather type and forecaster, then open WCC SAVE dialog
	 */
	@Override
	public void okPressed(){
		
		if ( !stormBtn.getSelection() && !tornadoBtn.getSelection() ){
			
			String msg = "Please select weather type!";

			MessageDialog confirmDlg = new MessageDialog( 
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
					"Watch Information", null, msg,
					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
			confirmDlg.open();
			
		}
		else if (forecasterCombo.getText().isEmpty()){
			String msg = "Please type in forecaster name!";

			MessageDialog confirmDlg = new MessageDialog( 
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
					"Watch Information", null, msg,
					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
			confirmDlg.open();
		}
		else {
			String pdName = wbDlg.drawingLayer.getActiveProduct().getType();
			ProductType pt = ProductConfigureDialog.getProductTypes().get( pdName);
			if ( pt != null ) pdName = pt.getType();
		
			String pd1 = pdName.replaceAll(" ", "_");
				
			dirPath = PgenUtil.getPgenOprDirectory() + 
								File.separator + pd1 +  File.separator + "prod" +
							File.separator + "text" + File.separator;
			updateWatch();
			openWCCDlg();
		}
	}

	private void updateWatch(){
		WatchBox wb = wbDlg.getWatchBox();
		
		wb.setWatchType(this.getWatchType());
		wb.setForecaster(forecasterCombo.getText());
		try {
			wb.setReplWatch(Integer.parseInt(rText.getText()));
		}
		catch( Exception e){
			//if anything wrong, don't set the watch replace number
		}
		wb.setExpTime(this.getExpirationTime());
	}
	
	
	/**
	 * Get weather type string
	 * @return
	 */
	private String getWeatherType(){
		String weatherType = "";
		if ( stormBtn.getSelection() ) weatherType = "SEVERE THUNDERSTORM";
		else if ( tornadoBtn.getSelection() ) weatherType = "TORNADO";
		return weatherType;
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
	
	/**
	 * Open WCC SAVE dialog
	 */
	private void openWCCDlg(){

		//get meet-me time
		Calendar meetMe = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		meetMe.add(Calendar.MINUTE, 4);
		
		//get expiration time
		Calendar expiration = getExpirationTime();
		
		//wcc text
		String wccText = String.format("THERE WILL BE A MEET-ME CONFERENCE CALL WITH THE STORM PREDICTION CENTER AT %1$02d%2$02d UTC %3$02d %4$s %5$4d TO COORDINATE A POTENTIAL %6$s WATCH VALID THROUGH %7$02d%8$02d UTC %9$02d %10$s %11$04d.", 
				meetMe.get(Calendar.HOUR_OF_DAY), meetMe.get(Calendar.MINUTE), meetMe.get(Calendar.DAY_OF_MONTH), 
				meetMe.getDisplayName(Calendar.MONTH, Calendar.LONG, Locale.US).toUpperCase(), meetMe.get(Calendar.YEAR),
				getWeatherType(), expiration.get(Calendar.HOUR_OF_DAY),expiration.get(Calendar.MINUTE), expiration.get(Calendar.DAY_OF_MONTH),
				expiration.getDisplayName(Calendar.MONTH,Calendar.LONG, Locale.US).toUpperCase(), expiration.get(Calendar.YEAR) );
		
		wccText += "\n\nTHE FOLLOWING NATIONAL WEATHER SERVICE FORECAST OFFICES ARE NEEDED ON THE CONFERENCE CALL:\n\n";
		String wfoList =  WatchInfoDlg.formatWfoStr(wbDlg.getWatchBox().getWFOs()).substring(3).replaceAll("\n","");
		wccText += "   " + wfoList;
		
		//get nearby WFOs
		boolean nearby = false;
		String nearbyWFOs = "";
		for ( Button btn : wfoBtns ){
			if (btn.getSelection() ){
				if (!nearby) nearby = true;
				if (nearbyWFOs.isEmpty() ) nearbyWFOs += btn.getText();
				else nearbyWFOs += "..."+btn.getText();
			}
		}
		
		if( nearby ){
			wccText += "\n\nTHE FOLLOWING NWS WFOS NEAR THE PROPOSED WATCH AREA ARE BEING REQUESTED TO PARTICIPATE ON THE CONFERENCE CALL:";
			wccText += "\n\n   " + nearbyWFOs;
		}
		
		wccText += "\n\nPhone Number: " + phoneCombo.getText();
		wccText += "\nPassword:     -     ";
		wccText += "\n\nIF PASSWORD IS NOT AVAILABLE, CONTACT SPC LEAD FORECASTER.";
		wccText += "\n\nATTN..." + wfoList + "...WNAW...WNAR";
		wccText += "\n\n" + forecasterCombo.getText();
		
		
		String msg = wccText + "\n\n------------------------------------\n";
		msg += "\nSave To:   KNCFNIMNAT";
		
		String wfoLaunch = "None";
		if ( !wbDlg.getWatchBox().getWFOs().isEmpty()){
			wfoLaunch =  WatchInfoDlg.formatWfoStr(wbDlg.getWatchBox().getWFOs()).substring(3).replaceAll("\n","");
			wfoLaunch = wfoLaunch.replaceAll("\\.\\.\\.", ",")+ ",WNAW,WNAR";
		}
		
		String wccLaunch =   "#!/bin/csh\nlaunch_prod text_" + wfoLaunch + " KNCFNIMNAT\n";
		
		
		WatchWCCDlg wccDlg = new WatchWCCDlg(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
		wccDlg.setMessage(PgenToolUtils.wrapWatchText(wccText, LINE_LEN));
		wccDlg.setWCCLaunchText( wccLaunch);
		wccDlg.setBlockOnOpen(true);
		wccDlg.setOutputPath(dirPath);
		wccDlg.open();
			
		if (wccDlg.getReturnCode() == MessageDialog.OK){
			openWCLDlg();
		}
	}
	
	/**
	 * Open WCL SAVE dialog
	 */
	private void openWCLDlg(){
		
		Calendar utc = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

		String wclText = String.format("NWUS64 KWNS %1$td%1$tH%1$tM", utc);
		wclText += "\nWCL" + idCombo.getText();
		
		wclText += "\n\n." + getWeatherType() + " WATCH " +  idCombo.getText();
		
		wclText += "\nCOORDINATION COUNTY LIST FROM THE NWS STORM PREDICTION CENTER\n";
		wclText += String.format("EFFECTIVE UNTIL %1$tH%1$tM UTC.\n\n\n", getExpirationTime());		
		
		for ( String state : wbDlg.getWatchBox().getStates()){
			wclText += PgenToolUtils.wrapWatchText(wbDlg.getWatchBox().createCountyInfo(state, getExpirationTime()),
					LINE_LEN);
			wclText += "$$\n\n\n";
		}
		String attn = PgenToolUtils.wrapWatchText("ATTN...WFO" +  WatchInfoDlg.formatWfoStr(wbDlg.getWatchBox().getWFOs()).replaceAll("\n", ""),
				LINE_LEN) + "\n";
		wclText += attn;
		
		String msg = wclText;
		
		WatchWCLDlg wclDlg = new WatchWCLDlg(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
		wclDlg.setWCLFileNmae("KWNSWCL"+idCombo.getText());
		wclDlg.setMessage(msg);
		wclDlg.setBlockOnOpen(true);
		wclDlg.setOutputPath(dirPath);
		wclDlg.open();
		
		if ( wclDlg.getReturnCode() == OK){
			close();
		}
		
	}
	
	public static Document readForecasterTbl() {
		
		if (FORECASTERS == null) {
			try {
				String forecasterFile = PgenStaticDataProvider.getProvider().getFileAbsolutePath(
						   PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + PGEN_FORECASTER );
				
				SAXReader reader = new SAXReader();
				forecasterTbl = reader.read(forecasterFile);
			} catch (Exception e) {
				e.printStackTrace();				
			}
		}
		
		return forecasterTbl;
	}
	
	public static String[] getForecasters() {
		if (forecasterTbl == null)
			FORECASTERS = new String[] {"BALDWIN", "BIRCH", "EVANS", "GALLINA"};
		else {
			List<String> list = new ArrayList<String>();		
			List<Node> nodes = forecasterTbl.selectNodes(FORECASTER_XPATH);
		
			for (Node node : nodes) {
				list.add( node.valueOf("@name").toString());
			}
		
			FORECASTERS = new String[list.size()];
			FORECASTERS = list.toArray(FORECASTERS);
		}
		
		return FORECASTERS;
	}
	
	/**
	 * Get Watch type
	 * @return string - watch type
	 */
	private String getWatchType(){
		String type ="";
		if (stormBtn.getSelection()){
			type = WatchFormatDlg.S_STORM;
		}
		else if (tornadoBtn.getSelection()){
			type = WatchFormatDlg.TORNADO;
		}
		return type;
	}
	
	/**
 	 * Get the expiration hour from the validTime text widget
     */
	private int getExpHour(){
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
 	 * Get the expiration minutes from the validTime text widget
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

