/*
 * SpenesFormatDlg
 * 
 * Date created: June 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Spenes;

import java.util.ArrayList;
import java.util.List;

import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
* Singleton for a Spenes format dialog.
* 
* <pre>
* SOFTWARE HISTORY
* Date       	Ticket#		Engineer	Description
* ------------	----------	-----------	--------------------------
* 05/12		    #734		J. Zeng   	Initial Creation.
* </pre>
* 
* @author	J. Zeng
*/

public class SpenesFormatDlg extends CaveJFACEDialog { 
	/**
	 * instance of text dialog
	 */
	private SpenesFormatMsgDlg sfMsgDlg = null;
	
	/**
	 *  instance of Spenes format dialog
	 */
	private static SpenesFormatDlg INSTANCE = null;
	
	/**
	 *  instance of spenes
	 */
	private static Spenes spenes = null;

	/**
	 * instance of spenes attr dialog
	 */
	private SpenesAttrDlg spDlg = null;
	
	/**
	 * top level container of all widgets
	 */
	private Composite top;
	
	/**
	 *  id for format button
	 */
	private static int FORMAT_ID = 20120508+0;
	
	/**
	 * id of reset button
	 */	
	private static int RESET_ID = 20120508+1;
	
	/**
	 * Combo widget for latestDate
	 */
	private Combo comboLatestData = null;
	
	/**
	 * Combo widget for forecaster name
	 */
	private Combo comboForecaster = null;
	
	/**
	 * Combo widget for Outlook level
	 */
	private Combo comboOutlookLevel = null;
		
	/**
	 * text area for state abbreviation
	 */
	private Text txtAbrv;
	
	/**
	 * text area for state information
	 */
	private Text txtLocation = null;
	
	/**
	 * text area for AttnWFOs
	 */
	private Text txtAttnWFOs = null;
	
	/**
	 * text area for AttnRFCs
	 */
	private Text txtAttnRFCs = null;
	
	/**
	 * text area for Event title
	 */
	private Text txtEvent = null;
	
	/**
	 * text area for analysis
	 */
	private Text txtSatAnaTre = null;
	
	/**
	 * text area for addionation precipitation
	 */
	private Text txtAddlInfo = null;
	
	/**
	 * date time for observation hour
	 */
	private DateTime dtObsHr;
	
	/**
	 * gridData for layout of this dialog parts
	 */
	private GridData singleTxtGridData = new GridData(300,36);
	
	/**
	 * button for Satellite Analysis and Trend
	 */
	private Button btnSatAnaTre = null;
	/**
	 * button for additional precipitation
	 */
	private Button btnAddlInfo = null;

	/**
	 * hour for the beginning of the short term 
	 */
	private DateTime dtTermsHr;

	/**
	 * hour for the ending of the short term 
	 */
	private DateTime dtTerms2Hr;
	/**
	 * DataType name
	 */
	private static String DATATYPEARRAY[] = new String[] {"GOES-11", "GOES-12", "GOES-13", "GOES-15", "DPD", "IND", "Meteosat", "VAAC"};
	/**
	 * Variables to retrieve forecasters
	 */
	private static Document forecasterTbl = null;
	private static String FORECASTERS[] = null;
	private static String FORECASTER_XPATH = "/forecasters/forecaster";
	private static final String PGEN_FORECASTER = "forecasters.xml";
	
	/**
	 * Array for outlook level
	 */
	private static String OUTLOOKLEVEL[] = {"HIGH", "MEDIUM", "LOW"};
	
	
	/**
	 * Protector constructor for this class
	 * @param Shell: parent shell for this dialog
	 * @throws VizException
	 */
	private SpenesFormatDlg(Shell parentShell, SpenesAttrDlg spDlg) throws VizException{
		super(parentShell);
		this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
		
		this.setSpDlg(spDlg);
		readForecasterTbl();
		FORECASTERS = getForecasters();
	}
	
	/**
	 * Return the instance of the singleton.
	 * @param parShell
	 * @return
	 */
	public static SpenesFormatDlg getInstance( Shell parentShell, SpenesAttrDlg spenesDlg, Spenes sp){
		if ( INSTANCE == null) {
			try {
				INSTANCE = new SpenesFormatDlg(parentShell, spenesDlg);
				
			} catch (VizException e){
				e.printStackTrace();
			}
		} 
		spenes = sp;
		
		return INSTANCE;
	}
	
	/**
	 * Creates the dialog area
	 */
	//@Override
	public Control createDialogArea(Composite parent){
		top = (Composite) super.createDialogArea(parent);
		
		this.getShell().setText( "SPENES FORMAT");
		
		//Create the main layout for the shell
		GridLayout mainLayout = new GridLayout(2, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		mainLayout.verticalSpacing = 3;
		top.setLayout(mainLayout);
		

		// Initialize all of the menus, controls, and layouts
		//this.initializeComponents();
		
		Group top1 = new Group(top,SWT.LEFT);
        top1.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,2,1));
        top1.setLayout(new GridLayout(2,false));			
		createArea1(top1);
		
		Group top2 = new Group(top,SWT.LEFT);
        top2.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,2,1));
        top2.setLayout(new GridLayout(2,false));			
		createArea2(top2);
		
		Group top3 = new Group(top,SWT.LEFT);
		top3.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,2,1));
		top3.setLayout(new GridLayout(2,false));			
		createArea3(top3);
		
		Group top4 = new Group(top,SWT.LEFT);
		top4.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,2,1));
		top4.setLayout(new GridLayout(2,false));			
		createArea4(top4);		
		
		
		Group top5 = new Group(top,SWT.LEFT);
		top5.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,2,1));
		top5.setLayout(new GridLayout(2,false));			
		createArea5(top5);		
		
		return top;
	}
	
	/**
	 * Open the dialog and initialize the widgets
	 */
	@Override
	public int open(){
		
		if ( this.getShell() == null ){
			this.create();
		}
		/*if(shellLocation == null){
	   	    this.getShell().setLocation(this.getShell().getParent().getLocation());
		} else {
			getShell().setLocation(shellLocation);
		}*/
		
   	 	return super.open();
		
	}
	
	/**
	 * get spenes
	 * @return
	 */
	public Spenes getSpenes(){
		return spenes;
	}
	
	/**
	 * set spenes
	 * @param sp
	 */
	public void setSpenes(Spenes sp) {
		spenes = sp;
	}
	
	/**
	 * button listener method override from super class.
	 * @param int button id for button.
	 */
	@Override
	protected void buttonPressed(int buttonId){
		if(IDialogConstants.OK_ID == buttonId) {
			okPressed();
		} else if (CANCEL == buttonId){
			cancelPressed();
		} else if (FORMAT_ID == buttonId) {
			formatPressed();
		} else if (RESET_ID == buttonId){
			resetPressed();
		} 
	}
	
	/**
	 * button listener for "Format"
	 */
	public void formatPressed(){
		copyAttrToSpenes();

		SpenesFormatMsgDlg sfmDlg = new SpenesFormatMsgDlg(this.getParentShell(), SpenesFormatDlg.this);
		sfmDlg.setBlockOnOpen(false);
		sfmDlg.open();
	}
	
	/**
	 * button listener for "Apply"
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
			copyAttrToSpenes();
			spDlg.drawingLayer.resetElement(spenes);
			spDlg.mapEditor.refresh();
		}
	}
	
	/**
	 * Check if all required info are filled in.
	 * @return string of the error message
	 */
	private String checkErr(){
		boolean showErr = false;
		String err = "The following Problems have not been identified:\n\n";
		System.out.println("Latest Data Used: " + comboLatestData.getText());
		if (comboLatestData.getText().isEmpty()) {
			showErr = true;
			err += "\nEntry of Latest Data is empty.\n";
		}
				
		if (comboForecaster.getText().isEmpty()) {
			showErr = true;
			err += "\nEntry of Forecaster is empty.\n";
		}
		if ( txtEvent.getText().isEmpty() ){
			showErr = true;
			err += "\nEvent is empty.\n";

		}
        
		if ( txtSatAnaTre.getText().isEmpty() ) {
			err += "\nSatellite Analysis and Trends is empty.\n";
		}
		
		if ( showErr )
		return err;
		else 
			return "";
		
	}
	
	/**
	 * button listener helper for "Reset"
	 */
	public void resetPressed(){
		txtEvent.setText("");
		txtSatAnaTre.setText("");
		txtAddlInfo.setText("");
		
		comboLatestData.deselectAll();
		comboForecaster.deselectAll();
		comboOutlookLevel.deselectAll();
	}
	
	/**
	 * Create the button bar for this dialog
	 * override from the super class
	 * @param Composite: parent of the dialog
	 */
	@Override
	public void createButtonsForButtonBar(Composite parent){

		createButton(parent, IDialogConstants.OK_ID, "Apply", true);
		createButton(parent, FORMAT_ID, "Format SPENES", true);
		createButton(parent, RESET_ID, "Reset", true);
		createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", true);
		
		getButton(IDialogConstants.OK_ID).setEnabled(true);
		getButton(FORMAT_ID).setEnabled(true);
		getButton(RESET_ID).setEnabled(true);
		getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
	}
	
	/**
	 * helper method for creating dialog area 1
	 */
	private void createArea1(Group top) {
		
		Label lblTime = new Label(top,SWT.LEFT);
		lblTime.setText("ESTIMATE...DATA/TIME ");
		
		Label lblTimeTxt = new Label(top, SWT.LEFT);
		if (spenes.getInitDateTime() == null ) spenes.setInitTime();
		lblTimeTxt.setText(spenes.getInitDateTime());
		
		Label lblLtsData = new Label(top,SWT.LEFT | SWT.SCROLL_LINE);
		lblLtsData.setText("LATEST DATA USED: ");
		
		comboLatestData = new Combo(top, SWT.DROP_DOWN);
		if(DATATYPEARRAY != null) {
			for(String str : DATATYPEARRAY){
				comboLatestData.add(str);
			}
			comboLatestData.select(1);
		}
		if (spenes.getLatestDataUsed() != null )
			comboLatestData.setText(spenes.getLatestDataUsed());
				
		comboLatestData.addListener(SWT.Selection, new Listener(){

			@Override
			public void handleEvent(Event e) {
			}
			
		});
			
		Label lblObsHr = new Label(top,SWT.LEFT); 
		lblObsHr.setText("Observation Time: ");
		Composite dt = new Composite(top, SWT.NONE);
		GridLayout dtGL = new GridLayout(1,false);
		dt.setLayout(dtGL);
		dtObsHr = new DateTime(dt, SWT.BORDER | SWT.TIME | SWT.SHORT); 
		if (spenes.getObsHr() >= 0 )
			dtObsHr.setHours(spenes.getObsHr());
		dtObsHr.setMinutes(0);
		dtObsHr.addListener(SWT.Selection, new Listener(){
			@Override
			public void handleEvent(Event event) {
				
		}});
		
		Label lblForecaster = new Label(top, SWT.LEFT);
		lblForecaster.setText("Forecaster: ");
		comboForecaster = new Combo(top, SWT.DROP_DOWN);
		if (FORECASTERS != null) {
			for (String str : FORECASTERS) {
				comboForecaster.add(str);
			}
		}
		if (spenes.getForecasters() != null)
			comboForecaster.setText(spenes.getForecasters());
		
		comboForecaster.addListener(SWT.Selection, new Listener(){

			@Override
			public void handleEvent(Event e) {
				//String fcst = comboForecaster.getText().trim();
			}
			
		});
	}
	
	
	/**
	 * helper method for creating dialog area 2
	 */
	private void createArea2(Group top){
		
		Label lblAbrv = new Label(top, SWT.LEFT);
		lblAbrv.setText("States Abrv: ");
		
		txtAbrv = new Text(top, SWT.LEFT | SWT.BORDER | SWT.WRAP | SWT.V_SCROLL);
		txtAbrv.setLayoutData(singleTxtGridData);
		//if ( spenes.getStateZ000() != null )
		System.out.println("State Z000: " + spenes.getStateZ000());
		txtAbrv.setText(spenes.getStateZ000());
		txtAbrv.addModifyListener(new TxtModifyListener());
		
		Label lblLocation = new Label(top, SWT.LEFT);
		lblLocation.setText("Location: ");
		
		txtLocation = new Text(top, SWT.LEFT | SWT.BORDER | SWT.WRAP | SWT.V_SCROLL);
		txtLocation.setLayoutData(singleTxtGridData);		
		txtLocation.setText(spenes.getLocation());
		txtLocation.addModifyListener(new TxtModifyListener());
		
		Label lblAttnWFOs = new Label(top, SWT.LEFT);
		lblAttnWFOs.setText("Attn WFOs: ");		
		txtAttnWFOs = new Text(top, SWT.LEFT | SWT.BORDER | SWT.WRAP | SWT.V_SCROLL);
		txtAttnWFOs.setLayoutData(singleTxtGridData);
		txtAttnWFOs.setText(spenes.getAttnWFOs());
		txtAttnWFOs.addModifyListener(new TxtModifyListener());
		
		Label lblAttnRFCs = new Label(top, SWT.LEFT);
		lblAttnRFCs.setText("Attn RFCs: ");
		txtAttnRFCs = new Text(top, SWT.LEFT | SWT.BORDER | SWT.WRAP | SWT.V_SCROLL);
		txtAttnRFCs.setLayoutData(singleTxtGridData);
		txtAttnRFCs.setText(spenes.getAttnRFCs());
		txtAttnRFCs.addModifyListener(new TxtModifyListener());
	}
	
	/**
	 * helper method for creating dialog area 3
	 */
	private void createArea3(Group top) {
		Label lblEvent = new Label(top, SWT.LEFT);
		lblEvent.setText("Event:     ");
		
		//txtEvent.setText("");//spenes.getEvent());
		txtEvent = new Text(top,SWT.LEFT | SWT.BORDER | SWT.WRAP | SWT.V_SCROLL);
		txtEvent.setLayoutData(singleTxtGridData);
		if (spenes.getEvent() != null) 
			txtEvent.setText(spenes.getEvent());
		txtEvent.addModifyListener(new TxtModifyListener());
		
		btnSatAnaTre = new Button( top, SWT.PUSH);
		btnSatAnaTre.setText("S.A.T.");
		//btnSatAnaTre.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false,2,1));
		btnSatAnaTre.setLayoutData(new GridData(60,26));
		btnSatAnaTre.addListener(SWT.Selection, new Listener(){

			@Override
			public void handleEvent(Event event) {
				try {
					SpenesSATDlg SATDlg = new SpenesSATDlg(SpenesFormatDlg.this.getShell());
					SATDlg.setBlockOnOpen(false);
					SATDlg.open();
				} catch (VizException e) {
					e.printStackTrace();
				}				
			}
			
		});
		
		txtSatAnaTre = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP | SWT.V_SCROLL);		
		GridData gData1 = new GridData(300,96);
		gData1.horizontalAlignment = 7;
		txtSatAnaTre.setLayoutData(gData1);
		if (spenes.getSatAnalysisTrend() != null)
			txtSatAnaTre.setText(spenes.getSatAnalysisTrend());
		txtSatAnaTre.addModifyListener(new TxtModifyListener());
	}
	
	/**
	 * helper method for creating dialog area 4
	 */
	private void createArea4(Group top) {
		
		Composite dt = new Composite(top, SWT.NONE);
		GridLayout dtGL = new GridLayout(3,false);
		dt.setLayout(dtGL);
		
		Label lblShortTermFrom = new Label(dt,SWT.NONE); 
		lblShortTermFrom.setText("ShortTerm: ");
		
		
		dtTermsHr = new DateTime(dt, SWT.BORDER | SWT.TIME | SWT.SHORT); 
		if(spenes.getShortTermBegin() >= 0 )
			dtTermsHr.setHours(spenes.getShortTermBegin());
		dtTermsHr.setMinutes(0);
		dtTermsHr.addListener(SWT.Selection, new Listener(){
			@Override
			public void handleEvent(Event event) {		
			}});
		dtTerms2Hr = new DateTime(dt, SWT.BORDER | SWT.TIME | SWT.SHORT); 
		if(spenes.getShortTermEnd() >= 0 )
			dtTerms2Hr.setHours(spenes.getShortTermEnd());
		dtTerms2Hr.setMinutes(0);
		dtTerms2Hr.addListener(SWT.Selection, new Listener(){
			@Override
			public void handleEvent(Event event) {		
			}});
	}
	
	/**
	 * helper method for creating dialog area 5
	 */
	private void createArea5(Group top) {
		Label lblOutlookLevel = new Label(top, SWT.LEFT);
		lblOutlookLevel.setText("Confidence: ");

		comboOutlookLevel = new Combo(top, SWT.DROP_DOWN);
		if (spenes.getOutlookLevel() != null ) 
			comboOutlookLevel.setText(spenes.getOutlookLevel());
		if (OUTLOOKLEVEL != null) {
			for(String str : OUTLOOKLEVEL){
				comboOutlookLevel.add(str);
			}
			comboOutlookLevel.select(2);
		}

		if (spenes.getOutlookLevel() != null )
			comboOutlookLevel.setText(spenes.getOutlookLevel());
		comboOutlookLevel.addListener(SWT.Selection, new Listener(){

			@Override
			public void handleEvent(Event e) {
				
			}			
		});
		
		btnAddlInfo = new Button( top, SWT.PUSH);
		btnAddlInfo.setText("H.P.");
		btnAddlInfo.setLayoutData(new GridData(60,26));
		btnAddlInfo.addListener(SWT.Selection, new Listener(){

			@Override
			public void handleEvent(Event event) {
				
				try {
					SpenesHPDlg HPDlg = new SpenesHPDlg(SpenesFormatDlg.this.getShell());
					HPDlg.setBlockOnOpen(false);
					HPDlg.open();
				} catch (VizException e) {
					
					e.printStackTrace();
				}
			}
			
		});
		
		//txtAddlInfo.setText(spenes.getAddlInfo());
		txtAddlInfo = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP | SWT.V_SCROLL);
		GridData gData2 = new GridData(300,96);
		gData2.horizontalAlignment = 7;
		txtAddlInfo.setLayoutData(gData2);
		if (spenes.getAddlInfo() != null )
			txtAddlInfo.setText(spenes.getAddlInfo());
		txtAddlInfo.addModifyListener(new TxtModifyListener()); 
		
	}
		
	/**
	 * listener for modifying text
	 */	
	public class TxtModifyListener implements ModifyListener{
		@Override
		public void modifyText(ModifyEvent e){
			
			if(e.widget instanceof Text){
				
			}else if(e.widget instanceof Combo){
				
			}
		}
	}
	
	/**
	 * read Forecaster table
	 * @return
	 */
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
	
    /**
     * get forecaster name
     * @return
     */
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
     * get spenesAttrDlg
     * @return
     */
	public SpenesAttrDlg getSpDlg(){
		return spDlg;
	}
	
	/**
	 * set spenesAttrDlg
	 * @param spDlg
	 */
	public void setSpDlg(SpenesAttrDlg spDlg){
		this.spDlg = spDlg;
	}
	
	
	
	/**
	 * Close the format dialog and the message dialog
	 */
	@Override
	public boolean close(){
		if (sfMsgDlg != null ) sfMsgDlg.close();
		return super.close();
	} 
	
	/**
	 * method to copy dialog attributes to the Spenes
	 */
	public void copyAttrToSpenes(){
		spenes.setInitDateTime(spenes.getInitDateTime());
		spenes.setStateZ000(txtAbrv.getText());
		spenes.setLatestData(comboLatestData.getText());
		spenes.setObsHr(dtObsHr.getHours());
		spenes.setForecasters(comboForecaster.getText());
		spenes.setLocation(txtLocation.getText());
		spenes.setAttnWFOs(txtAttnWFOs.getText());
		spenes.setAttnRFCs(txtAttnRFCs.getText());
		spenes.setEvent(txtEvent.getText());
		spenes.setSatAnalysisTrend(txtSatAnaTre.getText());
		spenes.setShortTermBegin(dtTermsHr.getHours());
		spenes.setShortTermEnd(dtTerms2Hr.getHours());
		spenes.setOutlookLevel(comboOutlookLevel.getText());
		spenes.setAddlInfo(txtAddlInfo.getText());
	}
	
	/**
	 * Dialog to edit satellite analysis and trends
	 * @author jzeng
	 *
	 */
	private class SpenesSATDlg extends CaveJFACEDialog {
		private int RESETSAT_ID = 2012062601;
		
		/*
		 * the single instance of this type
		 */
		private Composite top = null;
		
		private Text txtSAT;
		private String message = "";
		
		//SpenesFormatDlg sfDlg = null; 
		
		protected SpenesSATDlg(Shell parShell) throws VizException {
			super(parShell);
		}

		/**
		 * create the dialog area for this dialog type
		 */
		@Override
		public Control createDialogArea(Composite parent) {
			getShell().setText("Edit Satellite Analysis and Trends ...");
			
			top = (Composite) super.createDialogArea(parent);
			
			/**
			 *  Create the main layout for the dialog area.
		     */
			GridLayout mainLayout = new GridLayout(1, true);
			mainLayout.marginHeight = 3;
			mainLayout.marginWidth = 3;
			top.setLayout(mainLayout);
			
			
			txtSAT = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP | SWT.V_SCROLL);
			txtSAT.setFont( new Font(txtSAT.getDisplay(), "Courier", 12, SWT.NORMAL));
			txtSAT.setLayoutData(new GridData(500,600));
			txtSAT.setText(txtSatAnaTre.getText());
			txtSAT.addModifyListener(new TxtModifyListener());
			
			return top;			
		}
		
		/*@Override
		public void setAttrForDlg(IAttribute ia) {
			
		}*/
		
		/**
		 * Create the button bar for this dialog
		 * override from the super class
		 * @param Composite: parent of the dialog
		 */
		@Override
		public void createButtonsForButtonBar(Composite parent){
			createButton(parent, IDialogConstants.OK_ID, "Ok", true);
			createButton(parent, RESETSAT_ID, "Reset", true);
			createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", true);
		
			this.getButton(IDialogConstants.OK_ID).setEnabled(true);
			this.getButton(RESETSAT_ID).setEnabled(true);
			this.getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
			

		}
		
		/**
		 * button listener method override from super class.
		 * @param int button id for button.
		 */
		@Override
		protected void buttonPressed(int buttonId){
			if(IDialogConstants.OK_ID == buttonId) {
				okPressed();
			} else if(RESETSAT_ID == buttonId){
				resetPressed();
			} else if (CANCEL == buttonId){
				cancelPressed();
			} 
			
			
		}
			
		/**
		 * clean all the texts for next open up
		 */
		private void resetSAT(){
			txtSAT.setText("");
			message = "";
		}
			
		/*
		 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
		 */
		@Override
		public void okPressed(){
			message = txtSAT.getText();
			if (message != null){
				txtSatAnaTre.setText(message);				
			}
			close();
		}
		
		@Override 
		public void cancelPressed(){
			super.cancelPressed();
		}
		
		/**
		 * Reset the text
		 */
		public void resetPressed(){
			resetSAT();			
		}
		
		@Override
		/**
		 * Set the location of the dialog
		 */
		public int open(){
			
			if ( this.getShell() == null ){
				this.create();
			}
			// this.getShell().setLocation(this.getShell().getParent().getLocation());
			
			this.getButtonBar().pack();
			return super.open();
				
		}
	}
	
	/**
	 * dialog for heavy precipitation 
	 * @author jzeng
	 *
	 */
	private class SpenesHPDlg extends CaveJFACEDialog {
		
		private int RESETHP_ID = 2012062602;
	
		/*
		 * the single instance of this type
		 */
		private Composite top = null;

		private Text txtHP;
		private String message = "";
		
		//SpenesFormatDlg sfDlg = null; 
		
		protected SpenesHPDlg(Shell parShell) throws VizException {
			super(parShell);
		}

		/**
		 * create the dialog area for this dialog type
		 */
		@Override
		public Control createDialogArea(Composite parent) {
			getShell().setText("Edit Heavy Precipitation...");
			
			top = (Composite) super.createDialogArea(parent);
			
			/**
			 *  Create the main layout for the dialog area.
		     */
			GridLayout mainLayout = new GridLayout(1, true);
			mainLayout.marginHeight = 3;
			mainLayout.marginWidth = 3;
			top.setLayout(mainLayout);
			
			
			txtHP = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP | SWT.V_SCROLL);
			txtHP.setFont( new Font(txtHP.getDisplay(), "Courier", 12, SWT.NORMAL));
			txtHP.setLayoutData(new GridData(500,600));
			txtHP.setText(txtAddlInfo.getText());
			txtHP.addModifyListener(new TxtModifyListener());
			
			return top;			
		}		
		
		/**
		 * Create the button bar for this dialog
		 * override from the super class
		 * @param Composite: parent of the dialog
		 */
		@Override
		public void createButtonsForButtonBar(Composite parent){
			createButton(parent, IDialogConstants.OK_ID, "Ok", true);
			createButton(parent, RESETHP_ID, "Reset", true);
			createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", true);
			
			getButton(IDialogConstants.OK_ID).setEnabled(true);
			getButton(RESETHP_ID).setEnabled(true);
			getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
	
		}
		
		/**
		 * button listener method override from super class.
		 * @param int button id for button.
		 */
		@Override
		protected void buttonPressed(int buttonId){
			if(IDialogConstants.OK_ID == buttonId) {
				okPressed();
			} else if(RESETHP_ID == buttonId){
				resetPressed();
			} else if (CANCEL == buttonId){
				cancelPressed();
			} 
		}
		/**
		 * clean all the texts for next open up
		 */
		private void resetHP(){
			txtHP.setText("");
			message = "";
		}
			
		/*
		 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
		 */
		@Override
		public void okPressed(){
			message = txtHP.getText();
			if (message != null){
				txtAddlInfo.setText(message);				
			}
			close();
		}
		
		/**
		 * Reset Heavy Precipitation
		 */
		public void resetPressed(){
			resetHP();
		}
		
		@Override
		/**
		 * Set the location of the dialog
		 */
		public int open(){
			
			if ( this.getShell() == null ){
				this.create();
			}
			this.getButtonBar().pack();
			return super.open();
				
		}
	}

	
}
