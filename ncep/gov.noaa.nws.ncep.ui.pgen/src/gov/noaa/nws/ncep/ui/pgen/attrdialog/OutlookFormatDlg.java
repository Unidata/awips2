/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.OutlookFormatDlg
 * 
 * 5 April 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.clipper.ClipProduct;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.productmanage.ProductConfigureDialog;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProductType;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;

import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Polygon;

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
 * 11/12		?			B. Yin		Fixed the otlkAll exception.
 * 01/13		#966		B. Yin		Added clipping functions.
 * 08/13		TTR783,773	B. Yin		Order outlook lines when formatting
 * 										Added forecaster drop-down
 * 11/13		#1049		B. Yin		Handle outlook type defined in layer.
 * 12/13		TTR800		B. Yin		Use UTC time class.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class OutlookFormatDlg  extends CaveJFACEDialog{

	//Outlook initial time and expiration time document
	private static Document otlkTimesTbl;
	
	private static HashMap<String,Polygon> bounds;
	
	//Order of labels in the outlook text message
	private String[] orderedLabels = {"HIGH", "MDT", "SLGT", "TSTM", "NONE", "2%", "5%", "10%", "15%",
			"25%", "30%", "35%", "40%", "45%", "50%", "60%", "70%", "75%", "HATCHED", "AREA",
			"ISODRYT", "SCTDRYT", "DRY-TSTM", "ELEVATED", "CRITICAL", "EXTREME", "D3", "D4", "D5",
			"D6", "D7", "D8", "D3-4", "D3-5", "D3-6", "D3-7", "D3-8", "D4-5", "D4-6", "D4-7", "D4-8",
			"D5-6", "D5-7", "D5-8", "D6-6", "D6-8", "D7-8"};
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
	private UTCTimeText initTime;
	
	//expiration check box and date/time widgets
	private DateTime expDate;
	private UTCTimeText expTime;
	
	//forecaster name
	private Combo   forecasterCombo;
	private static String lastForecaster = "";
	
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
		initTime = new UTCTimeText(initDt, SWT.SINGLE | SWT.BORDER | SWT.CENTER);

		FormData fd = new FormData();
		fd.top = new FormAttachment(dayGrp,2, SWT.BOTTOM);
		fd.left = new FormAttachment(initDate, 5, SWT.RIGHT);
		initTime.setLayoutData(fd);
		initTime.setUTCTimeTextField(initDt, 
				this.getDefaultInitDT(this.getDays().replaceAll(" Fire", "")),dayGrp, 5, true);
		
		setInitDt(this.getDefaultInitDT(this.getDays().replaceAll(" Fire", "")));

		//expiration date/time
		Label expLbl = new Label( top, SWT.NONE);
		expLbl.setText("Expiration Time:");
		Composite expDt = new Composite(top, SWT.NONE);
		expDt.setLayout( new GridLayout(2, false) );
		expDate = new DateTime(expDt, SWT.BORDER | SWT.DATE );
		expTime = new UTCTimeText(expDt, SWT.SINGLE | SWT.BORDER | SWT.CENTER);

		FormData fd2 = new FormData();
		fd2.top = new FormAttachment(initTime.getTextWidget(), 2, SWT.BOTTOM);
		fd2.left = new FormAttachment(expDate, 5, SWT.RIGHT);
		expTime.setLayoutData(fd2);
		
		expTime.setUTCTimeTextField(expDt, 
				this.getDefaultExpDT(this.getDays().replaceAll(" Fire", "")),dayGrp, 5, true);
			
		setExpDt(this.getDefaultExpDT(this.getDays().replaceAll(" Fire", "")));

		//forecaster
		Label fcstLbl = new Label( top, SWT.NONE);
		fcstLbl.setText("Forecaster:");
		forecasterCombo = new Combo(top, SWT.DROP_DOWN );
		WatchCoordDlg.readForecasterTbl();
		for ( String str : WatchCoordDlg.getForecasters() ){ //forecaster table should be put in a common place
        	forecasterCombo.add(str);
        }
		
		if ( !lastForecaster.isEmpty() ){
			forecasterCombo.setText(lastForecaster);
		}
		
		forecasterCombo.addModifyListener( new ModifyListener(){

			@Override
			public void modifyText(ModifyEvent e) {
				lastForecaster = ((Combo)e.widget).getText();
			}
		});
			
		
		//outlook type
		Label typeLbl = new Label(top, SWT.None);
		typeLbl.setText("Outlook Type:");
		
		Text typeTxt = new Text(top, SWT.SINGLE | SWT.RIGHT  );
		typeTxt.setText(getOutlookType());
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
				Layer layer = otlkDlg.drawingLayer.getActiveLayer();
				String fmtFlag = layer.getMetaInfoFromKey(OutlookAttrDlg.OTLK_FORMAT_FLAG_IN_LAYER_META);
				if ( fmtFlag.equalsIgnoreCase("false")){
					MessageDialog infoDlg = new MessageDialog( 
							PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
							"Information", null, "This layer is configured not to be formatted.",
							MessageDialog.INFORMATION, new String[]{"OK"}, 0);
					infoDlg.open();
					return;
				}
					
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
					msgDlg.setBlockOnOpen(true);
					msgDlg.setMessage(formatOtlk(otlk, otlkDlg.drawingLayer.getActiveLayer()));
					int rt = msgDlg.open();
					if ( rt == Dialog.OK )cleanup();
				}
			}
		});
	}
	
	/**
	 * Get the working Outlook element
	 * @return - outlook element
	 */
//	public Outlook getOutlook(){
//		return otlk;
//	}
	
	/**
	 * Get expiration time
	 * @return - Calendar expiration time
	 */
	public Calendar getExpTime(){

		Calendar expiration = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		expiration.set(expDate.getYear(), expDate.getMonth(), expDate.getDay(), 
				expTime.getHours( ), 
				expTime.getMinutes(), 0); 
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
				    initTime.getHours(), 
				    initTime.getMinutes()); 

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
		//only format active product
		//format order: from current layer goes down then goes to the top if necessary
		Product pd = otlkDlg.drawingLayer.getActiveProduct();
		int currentLayerIdx = pd.getLayers().indexOf(otlkDlg.drawingLayer.getActiveLayer() );
		
		for ( int ii = 0; ii < pd.getLayers().size(); ii++ ){
			//int jj = (ii+currentLayerIdx)%pd.getLayers().size();
			Layer fmtLayer = pd.getLayers().get( (ii+currentLayerIdx)%pd.getLayers().size() );
			String fmtFlag = fmtLayer.getMetaInfoFromKey(OutlookAttrDlg.OTLK_FORMAT_FLAG_IN_LAYER_META);
			if ( fmtFlag.equalsIgnoreCase("false") ) continue;
			
			String otlkType = fmtLayer.getMetaInfoFromKey(OutlookAttrDlg.OTLK_TYPE_IN_LAYER_META);
			if ( otlkType == null || otlkType.isEmpty() ){
				otlkType = "OUTLOOK";
			}
					
			//find outlook
			Iterator<AbstractDrawableComponent> it = fmtLayer.getComponentIterator();
			Outlook lk = null;

			while ( it.hasNext() ){
				AbstractDrawableComponent adc = it.next();
				if ( adc.getName().equalsIgnoreCase("Outlook") &&
						((Outlook)adc).getOutlookType().equalsIgnoreCase( otlkType ) ){
					lk = (Outlook)adc;
					break;
				}
			} // end while loop
			
			if ( msgDlg != null ){
				msgDlg.close();
			}
			msgDlg = new OutlookFormatMsgDlg(OutlookFormatDlg.this.getParentShell(),
					OutlookFormatDlg.this, lk, fmtLayer);
			msgDlg.setBlockOnOpen(true);
			msgDlg.setMessage(formatOtlk(lk, fmtLayer));

			int rt = msgDlg.open();

			if ( rt == Dialog.CANCEL ) return;
		}   //end layer loop
		
		cleanup();

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

		if ( ol == null ) {

			//days
			msg += formatDays().toUpperCase() + "\n";

			//forecaster
			msg += getForecaster().toUpperCase() + "\n";

			//initial time - expiration time
			msg += String.format("%1$td%1$tH%1$tMZ", getInitTime()) +" - " +
			String.format("%1$td%1$tH%1$tMZ", getExpTime()) +"\n";
		}
		else {
			Layer defaultLayer = otlkDlg.drawingLayer.getActiveLayer().copy();
			defaultLayer.clear();

			Product defaultProduct = new Product();
			defaultProduct.addLayer(defaultLayer);

			String pdName = otlkDlg.drawingLayer.getActiveProduct().getType();
			ProductType pt = ProductConfigureDialog.getProductTypes().get( pdName);

			Polygon boundsPoly = null;

			if ( pt != null && pt.getClipFlag() != null &&  pt.getClipFlag() ) {
				boundsPoly = getBoundsPoly(pt.getClipBoundsTable(), pt.getClipBoundsName());
				if ( boundsPoly != null ){
					processClip( ol, defaultLayer, boundsPoly); 
				}
			}

			if (pt ==null || pt.getClipFlag() == null || !pt.getClipFlag() || boundsPoly == null ) {
				//add watch collection(box and status line)
				defaultLayer.addElement(this.issueOutlook(ol));
			}


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
		if ( ol != null ) otlkDlg.showContLines(ol);
		return generateOutlookMsg(ol, layer);
	}
	
	/**
	 * Get forecaster name
	 * @return
	 */
	public String getForecaster(){
		return forecasterCombo.getText();
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
		if ( ol != null ) {
			reorder(ol);
			ol.setForecaster(getForecaster().toUpperCase());
			ol.setDays(getDays().toUpperCase());
			ol.setIssueTime(getInitTime());
			ol.setExpirationTime(getExpTime());
			ol.setLineInfo(generateLineInfo(ol, "new_line"));
		}
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
			ret = Integer.parseInt(hm.substring(hm.length()== 4 ? 2:1 ));
		}
		catch (Exception e ){
			
		}
		return ret;
	}
	
	/**
	 * Removes selected element. Refresh editor. Set PGEN selecting mode.
	 */
	private void cleanup(){
		this.close();
		this.getOtlkDlg().drawingLayer.removeSelected();
		this.getOtlkDlg().mapEditor.refresh();
		this.getOtlkDlg().close();
		PgenUtil.setSelectingMode();
	}
	
	/**
	 * Clip the input outlook and put it in the layer.
	 * @param ol
	 * @param layer
	 * @param boundsPoly
	 */
	private void processClip(Outlook ol, Layer layer, Polygon boundsPoly ){
	
		//Plot the bounds polygon.
		//Line border = new Line(null, new Color[]{Color.MAGENTA},.5f,.5,true,
		//		false, Arrays.asList(boundsPoly.getCoordinates()), 0,
		//		FillPattern.FILL_PATTERN_6,"Lines","LINE_SOLID");
		//otlkDlg.drawingLayer.addElement( border );
		
		//clip
		Outlook clipped = new ClipProduct( boundsPoly, true).clipOutlook(this.issueOutlook(ol) );
		
		//Put it in layer
		layer.addElement(clipped);
		this.issueOutlook(clipped);

		///Put it in the map editor. 
		otlkDlg.drawingLayer.replaceElement(ol, clipped);
		otlk = clipped;
		
		//Clean up. Remove ghost. Re-set selected element.
		if ( !clipped.isEmpty() && clipped.getPrimaryDE() instanceof Line){
			otlkDlg.setDrawableElement( clipped.getPrimaryDE() );
			if ( otlkDlg.drawingLayer.getSelectedComp() != null &&  !clipped.isEmpty()) {
				otlkDlg.drawingLayer.setSelected( clipped.getPrimaryDE());
			}
			otlkDlg.drawingLayer.removeGhostLine();
			otlkDlg.mapEditor.refresh();
		}
		else if (clipped.isEmpty()){
			otlkDlg.drawingLayer.removeSelected();
			otlkDlg.drawingLayer.removeGhostLine();
			otlkDlg.mapEditor.refresh();
		}
		
	}
	
	/**
	 * Gets the bounds polygon.
	 * @param boundsTbl
	 * @param boundsName
	 * @return
	 */
	private Polygon getBoundsPoly(String boundsTbl, String boundsName){
		if ( bounds == null ){
			bounds = new HashMap<String, Polygon>();
		}
		
		//check if the polygon is still in memory.
		Polygon boundsPoly = bounds.get( boundsTbl + boundsName);
		
		//load the bounds polygon.
		if ( boundsPoly == null){
			boundsPoly = PgenStaticDataProvider.getProvider().loadBounds(boundsTbl, boundsName);
			if ( boundsPoly != null ){ 
				//only keep one polygon in memory
				bounds.clear();	
				bounds.put( boundsTbl + boundsName, boundsPoly);
			}
		}
		
		return boundsPoly;
	}
	
	
	/**
	 * Generate from line information for the outlook
	 * @param ol - outlook element
	 * @param lineBreaker - new line character
	 * @return
	 */
	private String generateLineInfo( Outlook ol, String lineBreaker ){
		
		if ( ol.getOutlookType().equalsIgnoreCase("EXCE_RAIN")) return excessiveRain(ol, lineBreaker);
			
		String lnInfo = "";
		
		List<Station> anchors = PgenStaticDataProvider.getProvider().getAnchorTbl().getStationList();

		Iterator<AbstractDrawableComponent> it = ol.getComponentIterator();
		while( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			if ( adc.getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)){
				Iterator<DrawableElement> itDe = ((DECollection)adc).createDEIterator();
				List<Line> lines = new ArrayList<Line>();
				gov.noaa.nws.ncep.ui.pgen.elements.Text txt = null;
				while ( itDe.hasNext() ){
					DrawableElement de = itDe.next();
					if ( de instanceof gov.noaa.nws.ncep.ui.pgen.elements.Text ) txt = (gov.noaa.nws.ncep.ui.pgen.elements.Text) de;
					else if ( de instanceof Line ) lines.add((Line)de);
				}
				
				String lblInfo = "";
				if ( txt == null ){
					lblInfo += "LABEL -1 -1" + lineBreaker;
				}
				else {
				//	lblInfo += txt.getText()[0];
					lblInfo = otlkDlg.getTextForLabel(ol.getOutlookType(), txt.getText()[0]);
				//	if ( this.outlookType.equalsIgnoreCase("FIREOUTL") ) {
				//		lblInfo += " AREA FIRE WEATHER";
				//	}
					lblInfo += lineBreaker;
					lblInfo += "LABEL " + String.format("%1$5.2f %2$5.2f", txt.getLocation().y, txt.getLocation().x);
					lblInfo += lineBreaker;;
				}
				
				if ( !lines.isEmpty() ){
					for ( Line ln : lines ){
						lnInfo += lblInfo;
						
						ArrayList<Coordinate> points = new ArrayList<Coordinate>();
						points.addAll(ln.getPoints());
						
						if ( ln.isClosedLine() ){
							points.add( points.get(0));
						}
						
						for (Coordinate pt : points ){
							Station st = WatchBox.getNearestAnchorPt(pt, anchors);
							GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

							gc.setStartingGeographicPoint(st.getLongitude(), st.getLatitude() );
							gc.setDestinationGeographicPoint(pt.x, pt.y  );

							long dist = Math.round( gc.getOrthodromicDistance()/PgenUtil.SM2M);
							long dir = Math.round(gc.getAzimuth());
							if ( dir < 0 ) dir += 360;

							lnInfo += String.format("%1$5.2f %2$7.2f%3$4d %4$-3s%5$4s", pt.y, pt.x, dist, 
									WatchBox.dirs[(int)Math.round(dir/22.5)], st.getStid());
							lnInfo += lineBreaker;

						}
						lnInfo += "$$" + lineBreaker;
					}
				}
			}
			else if ( adc.getName().equalsIgnoreCase( Outlook.OUTLOOK_LINE_GROUP )){
				Iterator<DrawableElement> itDe = ((DECollection)adc).createDEIterator();
				ArrayList<Line> lns =  new ArrayList<Line>();
				gov.noaa.nws.ncep.ui.pgen.elements.Text txt = null;
				while ( itDe.hasNext() ){
					DrawableElement de = itDe.next();
					if ( de instanceof gov.noaa.nws.ncep.ui.pgen.elements.Text ) txt = (gov.noaa.nws.ncep.ui.pgen.elements.Text) de;
					else if ( de instanceof Line ) lns.add((Line)de);
				}
				
				if ( txt == null ){
					lnInfo += "LABEL -1 -1" + lineBreaker;
				}
				else {
					lnInfo += txt.getText()[0] + lineBreaker;
					lnInfo += "LABEL " + String.format("%1$5.2f %2$5.2f", txt.getLocation().y, txt.getLocation().x);
					lnInfo += lineBreaker;
				}
				
				int iLines = 0;
				for (Line ln : lns ){
					iLines++;
					for (Coordinate pt : ln.getPoints() ){
						Station st = WatchBox.getNearestAnchorPt(pt, anchors);
						GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

						gc.setStartingGeographicPoint(st.getLongitude(), st.getLatitude() );
						gc.setDestinationGeographicPoint(pt.x, pt.y  );

						long dist = Math.round( gc.getOrthodromicDistance()/PgenUtil.SM2M);
						long dir = Math.round(gc.getAzimuth());
						if ( dir < 0 ) dir += 360;
						
						lnInfo += String.format("%1$5.2f %2$7.2f%3$4d %4$-3s%5$4s", pt.y, pt.x, dist, 
								WatchBox.dirs[(int)Math.round(dir/22.5)], st.getStid());
						lnInfo += lineBreaker;
						
					}
					if ( iLines < lns.size()) lnInfo += "...CONT..." + lineBreaker;
				}
				
				lnInfo += "$$" + lineBreaker;
			}
		}
		
		return lnInfo;
	}
	
	private String excessiveRain( Outlook ol, String lineBreaker ){
		String ret = "";
		
		String rainTxt = "RISK OF RAINFALL EXCEEDING FFG TO THE RIGHT OF A LINE FROM";
		String fiveInch = "TOTAL RAINFALL AMOUNTS OF FIVE INCHES WILL BE POSSIBLE TO THE RIGHT OF A LINE FROM";
		
		List<Station> sfstns = PgenStaticDataProvider.getProvider().getSfStnTbl().getStationList();

		Iterator<AbstractDrawableComponent> it = ol.getComponentIterator();
		while( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			if ( adc.getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)){
				Iterator<DrawableElement> itDe = ((DECollection)adc).createDEIterator();
				List<Line> lines = new ArrayList<Line>();
				gov.noaa.nws.ncep.ui.pgen.elements.Text txt = null;
				while ( itDe.hasNext() ){
					DrawableElement de = itDe.next();
					if ( de instanceof gov.noaa.nws.ncep.ui.pgen.elements.Text ) txt = (gov.noaa.nws.ncep.ui.pgen.elements.Text) de;
					else if ( de instanceof Line ) lines.add((Line)de);
				}
				
				String lblInfo = "";
				if ( txt == null ){
					break;
				}
				else if ( txt.getString()[0].equalsIgnoreCase("5 INCH")){
					ret += fiveInch;
				}
				else if ( txt.getString()[0].equalsIgnoreCase("SLGT")){
					lblInfo += "SLIGHT ";
				}
				else if ( txt.getString()[0].equalsIgnoreCase("MDT")){
					lblInfo += "MODERATE ";
				}
				else if ( txt.getString()[0].equalsIgnoreCase("HIGH")){
					lblInfo += "HIGH ";
				}
				
				if ( !lblInfo.isEmpty() ) ret += lblInfo + rainTxt;
				
				if ( !lines.isEmpty() ){
					for ( Line ln : lines ){
						ArrayList<Coordinate> pts = ln.getPoints();
						if ( ln.isClosedLine() ) pts.add( pts.get(0));
						for (Coordinate pt : pts ){
							Station st = WatchBox.getNearestAnchorPt(pt, sfstns);
							GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

							gc.setStartingGeographicPoint(st.getLongitude(), st.getLatitude() );
							gc.setDestinationGeographicPoint(pt.x, pt.y  );

							long dist = Math.round( gc.getOrthodromicDistance()/PgenUtil.SM2M);
							long dir = Math.round(gc.getAzimuth());
							if ( dir < 0 ) dir += 360;

							ret += String.format(" %1$d %2$s %3$s", (int)(dist/5*5), 
									WatchBox.dirs[(int)Math.round(dir/22.5)], st.getStid());

						}
					}
					ret += "\n";
				}
			}
			ret += "\n";
		}
		
		return PgenUtil.wrap(ret, 65, "\n", false);

	}

	/**
	 * Order outlook lines by label
	 * @param otlk
	 */
	private void reorder( Outlook otlk ){
		List<AbstractDrawableComponent> ordered = new ArrayList<AbstractDrawableComponent>();

		for ( String str : orderedLabels ){
			int ii = 0;
			Iterator<AbstractDrawableComponent>  it = otlk.getComponentIterator();
			while ( it.hasNext() ){
				boolean found = false;
				AbstractDrawableComponent adc = it.next();
				Iterator<AbstractDrawableComponent>  it1 = null;
				if (adc.getName().equalsIgnoreCase(
						Outlook.OUTLOOK_LABELED_LINE)) {
					it1 = ((DECollection)adc).getComponentIterator();
				}
				else if (adc.getName().equalsIgnoreCase(
						Outlook.OUTLOOK_LINE_GROUP)) {
					if ( ((DECollection)adc).getItemAt(0).getName().equalsIgnoreCase(
							Outlook.OUTLOOK_LABELED_LINE)) {
						it1 = ((DECollection)((DECollection)adc).getItemAt(0)).getComponentIterator(); 
					}
				}
				
				if ( it1 == null ) continue;
				
				while ( it1.hasNext() ){ 
					AbstractDrawableComponent adcInside = it1.next();

					if ( adcInside instanceof gov.noaa.nws.ncep.ui.pgen.elements.Text ) { 
						if ( ((gov.noaa.nws.ncep.ui.pgen.elements.Text)adcInside).getText()[0].equalsIgnoreCase(str)){
							found = true;
							break;
						}
					}
				}

				if ( found ){
					ordered.add(adc);
					it.remove();
					ii++;
				}

			}
		}

		if ( !ordered.isEmpty() ){
			otlk.add( ordered );
		}
	}
	
	private String getOutlookType(){
		if ( otlk != null ){
			return otlk.getOutlookType(); 
		}
		else {
			return otlkDlg.getOutlookType();
		}
		
	}
}
