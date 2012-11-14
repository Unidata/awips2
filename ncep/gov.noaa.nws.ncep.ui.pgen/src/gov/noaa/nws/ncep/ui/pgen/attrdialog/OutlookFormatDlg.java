/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.OutlookFormatDlg
 * 
 * 5 April 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;

import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Implementation of outlook format dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/10			?		B. Yin   	Initial Creation.
 * 07/11        #450        G. Hull     NcPathManager
 * 03/12		$703		B. Yin		Generate product text from style sheet
 * 05/12		#710		B. Yin		Format HAIL outlook first
 * 07/12		#789		B. Yin		Change all time to UTC.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class OutlookFormatDlg  extends CaveJFACEDialog{

	//Outlook initial time and expiration time document
	static private Document otlkTimesTbl;
	
	//instance of the outlook attribute dialog
	private OutlookAttrDlg otlkDlg;
	
	//instance of the outlook message dialog
	private OutlookFormatMsgDlg msgDlg;

	//instance of the outlook element working on
	private Outlook otlk;
	
	//top level container of all widgets
	private Composite top;
	
	//day radio buttons
	private Button dayBtn[];
	private static String days[] =  { "Day1", "Day2", "Day3",  "Day4-8",
		 "Enh00", "Enh04", "Enh12", "Enh16", "Enh20","Day1 Fire", "Day2 Fire", "Day3-8 Fire" }; 
	
	//initial date and time
	private DateTime initDate;
	private Text initTime;
	
	//expiration check box and date/time widgets
	private DateTime expDate;
	private Text expTime;
	
	//forecaster name
	private Text forecaster;

	/**
	 * Protected constructor
	 */
	public OutlookFormatDlg(Shell parentShell, OutlookAttrDlg otlkDlg, Outlook otlk ) {
		
		super(parentShell);        
		this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE );
		this.otlk = otlk;
		this.otlkDlg = otlkDlg;
		
	}
	
	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {

		//top level container
		top = (Composite) super.createDialogArea(parent);

		// set title
		this.getShell().setText("Format Outlook");

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(2, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		mainLayout.verticalSpacing = 3;
		top.setLayout(mainLayout);

		//Create day radio boxes
		Label dayLbl = new Label(top, SWT.LEFT);
		dayLbl.setText("Day/Prod:");
		
		Group dayGrp = new Group(top, SWT.RADIO);
		dayGrp.setLayout(new GridLayout(3, false));
		
		dayBtn = new Button[days.length];
		
		for ( int ii = 0; ii < days.length; ii++ ){
			dayBtn[ii] = new Button(dayGrp, SWT.RADIO);
			dayBtn[ii].setText(days[ii]);
			dayBtn[ii].addSelectionListener(new SelectionAdapter(){

				@Override
				public void widgetSelected(SelectionEvent e) {
					if ( ((Button)e.widget).getSelection() ) {
						setInitDt(getDefaultInitDT(((Button)e.widget).getText().replaceAll(" Fire", "")));
						setExpDt(getDefaultExpDT(((Button)e.widget).getText().replaceAll(" Fire", "")));
					}
					
				}
				
			});
		}
		dayBtn[0].setSelection(true);
		
		//Initial date/time
	
		Label initLbl = new Label(top, SWT.NONE);
		initLbl.setText("Initial Time:");
		
		Composite initDt = new Composite(top, SWT.NONE);
		initDt.setLayout( new GridLayout(2, false) );
		initDate = new DateTime(initDt, SWT.BORDER | SWT.DATE );
		initTime = new Text(initDt, SWT.SINGLE | SWT.BORDER | SWT.CENTER);

		FormData fd = new FormData();
		fd.top = new FormAttachment(dayGrp,2, SWT.BOTTOM);
		fd.left = new FormAttachment(initDate, 5, SWT.RIGHT);
		initTime.setLayoutData(fd);
		PgenUtil.setUTCTimeTextField(initDt, initTime,  
				this.getDefaultInitDT(this.getDays().replaceAll(" Fire", "")),dayGrp, 5);
		
		setInitDt(this.getDefaultInitDT(this.getDays().replaceAll(" Fire", "")));

		//expiration date/time
		Label expLbl = new Label( top, SWT.NONE);
		expLbl.setText("Expiration Time:");
		Composite expDt = new Composite(top, SWT.NONE);
		expDt.setLayout( new GridLayout(2, false) );
		expDate = new DateTime(expDt, SWT.BORDER | SWT.DATE );
		expTime = new Text(expDt, SWT.SINGLE | SWT.BORDER | SWT.CENTER);

		FormData fd2 = new FormData();
		fd2.top = new FormAttachment(initTime, 2, SWT.BOTTOM);
		fd2.left = new FormAttachment(expDate, 5, SWT.RIGHT);
		expTime.setLayoutData(fd2);
		
		PgenUtil.setUTCTimeTextField(expDt, expTime,  
				this.getDefaultExpDT(this.getDays().replaceAll(" Fire", "")),dayGrp, 5);
		
		setExpDt(this.getDefaultExpDT(this.getDays().replaceAll(" Fire", "")));

		//forecaster
		Label fcstLbl = new Label( top, SWT.NONE);
		fcstLbl.setText("Forecaster:");
		forecaster = new Text(top, SWT.SINGLE | SWT.RIGHT | SWT.BORDER );
		
		//outlook type
		Label typeLbl = new Label(top, SWT.None);
		typeLbl.setText("Outlook Type:");
		
		Text typeTxt = new Text(top, SWT.SINGLE | SWT.RIGHT  );
		typeTxt.setText(otlk.getOutlookType());
		typeTxt.setEditable(false);
		
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
		
   	    this.getShell().setLocation(this.getShell().getParent().getLocation());
		this.getButton(IDialogConstants.OK_ID).setText("OtlkAll");
				
   	    return super.open();
		
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
		contBtn.addSelectionListener(new SelectionAdapter(){
			
			public void widgetSelected(SelectionEvent e) {
					
				long mins = (getExpTime().getTime().getTime() - getInitTime().getTime().getTime())/(1000*60);
				String msg = "The duration of your outlook will be " + (int)Math.floor(mins/60) + "h " +
								mins%60 +"m";
				msg += "\n";
				if ( getInitTime().before(Calendar.getInstance()) ){
					long dMins= (Calendar.getInstance().getTime().getTime() - getInitTime().getTime().getTime())/(1000*60);
					msg += "The outlook became valid " + (int)Math.floor(dMins/60) + "h " +
							dMins%60 +"m" + " ago.";
				}
				else {
					long dMins= (getInitTime().getTime().getTime() - Calendar.getInstance().getTime().getTime())/(1000*60) ;
					msg += "The outlook will become valid in " + (int)Math.floor(dMins/60) + "h " +
							dMins%60  +"m.";
				}
				
				
				MessageDialog confirmDlg = new MessageDialog( 
						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
						"Warning", null, msg,
						MessageDialog.QUESTION, new String[]{"OK","Cancel"}, 0);
				confirmDlg.open();

				if ( confirmDlg.getReturnCode() == MessageDialog.OK){
					msgDlg = new OutlookFormatMsgDlg(OutlookFormatDlg.this.getParentShell(),
							OutlookFormatDlg.this, otlk, otlkDlg.drawingLayer.getActiveLayer());
					msgDlg.setBlockOnOpen(false);
					msgDlg.setMessage(formatOtlk(otlk, otlkDlg.drawingLayer.getActiveLayer()));
					msgDlg.open();
				}
			}
		});
	}
	
	/**
	 * Get the working Outlook element
	 * @return - outlook element
	 */
	public Outlook getOutlook(){
		return otlk;
	}
	
	/**
	 * Get expiration time
	 * @return - Calendar expiration time
	 */
	public Calendar getExpTime(){

		Calendar expiration = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		expiration.set(expDate.getYear(), expDate.getMonth(), expDate.getDay(), 
			    this.getHourFromTextField( expTime ), 
			    this.getMinuteFromTextField( expTime ), 0); 
		expiration.set(Calendar.MILLISECOND, 0);
		return expiration;
	}
	
	/**
	 * Get initial time
	 * @return
	 */
	public Calendar getInitTime(){
		Calendar init = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		init.set(initDate.getYear(), initDate.getMonth(), initDate.getDay(), 
				    this.getHourFromTextField(initTime), 
				    this.getMinuteFromTextField(initTime), 0); 

		init.set(Calendar.MILLISECOND, 0);
		
		return init;
	}
	
	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void okPressed() {
			otlkAll();
		//otlkDlg.showContLines(otlk);
		//msgDlg.setMessage(formatOtlk())generateOutlookMsg(otlk, otlkDlg.drawingLayer.getActiveLayer());
		return;
	}
	
	private void otlkAll(){
		//format hail first
		for ( Product pd : otlkDlg.drawingLayer.getProducts()){
			for ( Layer layer : pd.getLayers() ){
				Iterator<AbstractDrawableComponent> it = layer.getComponentIterator();
				while ( it.hasNext() ){
					AbstractDrawableComponent adc = it.next();
					if ( adc.getName().equalsIgnoreCase("Outlook") &&
							adc.getPgenType().equalsIgnoreCase("hail") ){
						
						if ( msgDlg != null ){
							msgDlg.close();
						}
							msgDlg = new OutlookFormatMsgDlg(OutlookFormatDlg.this.getParentShell(),
									OutlookFormatDlg.this, (Outlook)adc, layer);
							msgDlg.setBlockOnOpen(true);
							msgDlg.setMessage(formatOtlk((Outlook)adc, layer));

							int rt = msgDlg.open();
							
							if ( rt == Dialog.CANCEL ) return;
						
						/*else {
							msgDlg.setOtlk((Outlook)adc);
							msgDlg.setLayer( layer);
						}
						*/
						
						
						break;
					}
				}
			}
		}
		
		//format other outlooks
		for ( Product pd : otlkDlg.drawingLayer.getProducts()){
			for ( Layer layer : pd.getLayers() ){
				Iterator<AbstractDrawableComponent> it = layer.getComponentIterator();
				while ( it.hasNext() ){
					AbstractDrawableComponent adc = it.next();
					if ( adc.getName().equalsIgnoreCase("Outlook") &&
							!adc.getPgenType().equalsIgnoreCase("hail") ){
						
						if ( msgDlg != null ){
							msgDlg.close();
						}
							msgDlg = new OutlookFormatMsgDlg(OutlookFormatDlg.this.getParentShell(),
									OutlookFormatDlg.this, (Outlook)adc, layer);
							msgDlg.setBlockOnOpen(true);
							msgDlg.setMessage(formatOtlk((Outlook)adc, layer));

							int rt = msgDlg.open();
							
							if ( rt == Dialog.CANCEL ) return;
						
						/*else {
							msgDlg.setOtlk((Outlook)adc);
							msgDlg.setLayer( layer);
						}
						*/
						
						
						break;
					}
				}
			}
		}
	}

	/**
	 * Re-order outlook lines
	 */
/*	public void reorderOtlkLines(){
		
		otlk.reorderLines();
		
	}
	*/
	
	/**
	 * Generate formatted outlook message
	 * @return - outlook message
	 */
	private String generateOutlookMsg( Outlook ol, Layer layer){
		String msg ="";
	/*	
		if ( !ol.getOutlookType().equalsIgnoreCase("EXCE_RAIN")) {

			//days
			msg += formatDays().toUpperCase() + "\n";

			//forecaster
			msg += forecaster.getText().toUpperCase() + "\n";

			//initial time - expiration time
			msg += String.format("%1$td%1$tH%1$tMZ", getInitTime()) +" - " +
			String.format("%1$td%1$tH%1$tMZ", getExpTime()) +"\n";
		}
		
		//get line info for all outlooks
		//msg += generateLineInfo( ol, "\n");
		msg += ol.generateLineInfo("\n");
	*/
		Layer defaultLayer = new Layer();
		//add watch collection(box and status line)
		defaultLayer.addElement(this.issueOutlook(ol));

		Product defaultProduct = new Product();
		defaultProduct.addLayer(defaultLayer);

		ArrayList<Product> prds = new ArrayList<Product>();
		prds.add( defaultProduct );
		Products fileProduct = ProductConverter.convert( prds );
		
		org.w3c.dom.Document sw = null;
    	
    	try{
    		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
    		dbf.setNamespaceAware(true);
    		DocumentBuilder db = dbf.newDocumentBuilder();
    		sw = db.newDocument();
    		Marshaller mar =  SerializationUtil.getJaxbContext().createMarshaller();
    		mar.marshal( fileProduct, sw );
    	}catch(Exception e){
    		e.printStackTrace();
    	}

    	DOMSource ds = new DOMSource(sw);
    	
    	//get style sheet file path
    	String xsltPath =  PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + File.separator + "xslt" + File.separator + "outlook" + File.separator + "Outlook.xlt";

		LocalizationFile lFile = PgenStaticDataProvider.getProvider().getStaticLocalizationFile(xsltPath);
		
		if ( lFile != null ){
			msg = PgenUtil.applyStyleSheet( ds, lFile.getFile().getAbsolutePath());
		}
		
		//show warning if there are different types of outlook in the same layer
		Iterator<AbstractDrawableComponent> it = layer.getComponentIterator();
		while ( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			if ( adc instanceof Outlook ){
				if ( !((Outlook)adc).getOutlookType().equalsIgnoreCase(ol.getOutlookType())){
					MessageDialog warningDlg = new MessageDialog( 
							PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
							"Warning!", null, "More than one outlook types are found in one layer!",
							MessageDialog.INFORMATION, new String[]{"OK"}, 0);
					warningDlg.open();
					break;
				}
			}
		}
		
		return msg;
	}
	
	/**
	 * Format day string
	 * @return
	 */
	public String formatDays(){
		
		String dayStr = "";
		
		for ( Button btn : dayBtn ){
			if (btn.getSelection() ){
				dayStr = btn.getText();
				if ( dayStr.contains("Enh")){
					dayStr = "Day1";
				}
				else {
					dayStr = dayStr.replace("-", "").replace("Fire", "");
				}
				break;
			}
		}
		
		return dayStr;
	}

	/**
	 * Format outlook message
	 * @return
	 */
	public String formatOtlk(Outlook ol, Layer layer){
		otlkDlg.showContLines(ol);
		return generateOutlookMsg(ol, layer);
	}
	
	/**
	 * Get forecaster name
	 * @return
	 */
	public String getForecaster(){
		return forecaster.getText();
	}
	
	/**
	 * get Day
	 * @return
	 */
	public String getDays(){
		for ( Button btn : dayBtn ){
			if (btn.getSelection() ){
				return btn.getText();
			}
		}
		return "";
	}
	
	/**
	 * Read outlook default init/exp time document
	 * @return
	 */
	private Document readOtlkTimesTbl() {
		
		if ( otlkTimesTbl == null) {
			try {
				String outlookTimesFile =  		PgenStaticDataProvider.getProvider().getFileAbsolutePath(
						   PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "outlooktimes.xml" );
				
				SAXReader reader = new SAXReader();
				otlkTimesTbl = reader.read(outlookTimesFile);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		return otlkTimesTbl;
	}
	
	/**
	 * Get the default initial date/time for the input day period
	 */
	private Calendar getDefaultInitDT( String days ) {

		String xpath = "/root/days[@name='" + days.toUpperCase() +"']";
		
		Node day = readOtlkTimesTbl().selectSingleNode(xpath);
		List<Node> nodes = day.selectNodes("range");
		
		Calendar curDt = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		int minutes = curDt.get(Calendar.HOUR_OF_DAY) * 60 + curDt.get(Calendar.MINUTE);
		int initAdjDay = 0;
		int initHH = 0;
		int initMM = 0;
		
		for (Node node : nodes) {
			try {
				int from = Integer.valueOf(node.valueOf("@from").substring(0,2)) * 60 +
							Integer.valueOf(node.valueOf("@from").substring(2));
				int to = Integer.valueOf(node.valueOf("@to").substring(0,2)) * 60 +
							Integer.valueOf(node.valueOf("@to").substring(2));
				if ( minutes > from && minutes <= to ){
					initHH = Integer.valueOf(node.valueOf("@init").substring(0, 2));
					initMM = Integer.valueOf(node.valueOf("@init").substring(2));
					initAdjDay = Integer.valueOf(node.valueOf("@initAdj"));
					break;
				}
			}
			catch ( Exception e ){
				continue;
			}
		}

		curDt.add(Calendar.DAY_OF_MONTH, initAdjDay);
		curDt.set(Calendar.HOUR_OF_DAY, initHH);
		curDt.set(Calendar.MINUTE, initMM);
		
		return curDt;
	}

	/**
	 * Get the expiration date/time for the input day period
	 * @param days
	 * @return
	 */
	private Calendar getDefaultExpDT( String days ) {

		String xpath = "/root/days[@name='" + days.toUpperCase() +"']";
		
		Node day = readOtlkTimesTbl().selectSingleNode(xpath);
		List<Node> nodes = day.selectNodes("range");
		
		Calendar curDt = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		int minutes = curDt.get(Calendar.HOUR_OF_DAY) * 60 + curDt.get(Calendar.MINUTE);
		int expAdjDay = 0;
		int expHH = 0;
		int expMM = 0;
		
		for (Node node : nodes) {
			try {
				int from = Integer.valueOf(node.valueOf("@from").substring(0,2)) * 60 +
							Integer.valueOf(node.valueOf("@from").substring(2));
				int to = Integer.valueOf(node.valueOf("@to").substring(0,2)) * 60 +
							Integer.valueOf(node.valueOf("@to").substring(2));
				if ( minutes > from && minutes <= to ){
					expHH = Integer.valueOf(node.valueOf("@exp").substring(0, 2));
					expMM = Integer.valueOf(node.valueOf("@exp").substring(2));
					expAdjDay = Integer.valueOf(node.valueOf("@expAdj"));
					break;
				}
			}
			catch ( Exception e ){
				continue;
			}
			
		}
		
		curDt.add(Calendar.DAY_OF_MONTH, expAdjDay);
		curDt.set(Calendar.HOUR_OF_DAY, expHH);
		curDt.set(Calendar.MINUTE, expMM);
		
		return curDt;
	}
	
	/**
	 * Set the initial date/time 
	 * @param cal - Calendar initial time
	 */
	private void setInitDt( Calendar cal ){
		initDate.setYear(cal.get(Calendar.YEAR));
		initDate.setMonth(cal.get(Calendar.MONTH));
		initDate.setDay(cal.get(Calendar.DAY_OF_MONTH));
		initTime.setText(String.format("%1$tH%1$tM", cal));
	}
	
	/**
	 * Set the expiration date/time
	 * @param cal - Calendar expiration time 
	 */
	private void setExpDt( Calendar cal ){
		expDate.setYear(cal.get(Calendar.YEAR));
		expDate.setMonth(cal.get(Calendar.MONTH));
		expDate.setDay(cal.get(Calendar.DAY_OF_MONTH));
		expTime.setText(String.format("%1$tH%1$tM", cal));
	}
	
	/**
	 * Save all information into the outlook
	 * @param ol
	 */
	public Outlook issueOutlook( Outlook ol ){
		ol.setForecaster(getForecaster().toUpperCase());
		ol.setDays(getDays().toUpperCase());
		ol.setIssueTime(getInitTime());
		ol.setExpirationTime(getExpTime());
		ol.setLineInfo(ol.generateLineInfo("new_line"));
		return ol;
	}

	public OutlookAttrDlg getOtlkDlg() {
		return otlkDlg;
	}

	public void setOtlkDlg(OutlookAttrDlg otlkDlg) {
		this.otlkDlg = otlkDlg;
	}
	
	
	/**
 	 * Get the expiration hour from the validTime text widget
     */
	private int getHourFromTextField( Text txt ){
		int ret =0;
		try {
			String hm = txt.getText();
			ret = Integer.parseInt(hm.substring(0, hm.length()== 4 ? 2:1 ));
		}
		catch (Exception e ){
			
		}
		return ret;
	}
	
	/**
 	 * Get the expiration minute from the validTime text widget
     */
	private int getMinuteFromTextField( Text txt ){
		int ret =0;
		try {
			String hm = txt.getText();
			ret = Integer.parseInt(hm.substring(hm.length()== 4 ? 2:1 ), hm.length()-1);
		}
		catch (Exception e ){
			
		}
		return ret;
	}
}
