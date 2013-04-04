/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog
 * 
 * Janurary 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.StringReader;
import java.text.*;
import java.util.*;

import javax.xml.bind.JAXBException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.jface.dialogs.*;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.events.*;
import org.eclipse.ui.PlatformUI;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.*;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.sigmet.*;
import gov.noaa.nws.ncep.ui.pgen.*;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;

import com.vividsolutions.jts.geom.Coordinate;


/**
 * The class for Volcano Attribute Dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#165		G. Zhang   	Initial Creation.
 *
  * 04/11		#?			B. Yin		Re-factor IAttribute
 *  07/11        #450        G. Hull     NcPathManager
 * 11/12		#889		B. Yin		Don't save XML file before saving text file.
 * 11/12		#890		B. Yin		Allow lower cases and numbers in the correction text field.
 * 
 * </pre>
 * 
 * @author	G. Zhang
 */

public class VolcanoVaaAttrDlg extends AttrDlg implements ISigmet{
	
	/**
	 * singleton instance of this class
	 */
	private static VolcanoVaaAttrDlg INSTANCE;
	
	/**
	 * id for "Format VAA" button.
	 */
	private static int FORMAT_ID = 20100201+0;
	
	/**
	 * id for "Reset" button.
	 */
	private static int RESET_ID = 20100201+1;
	
    private static final String PGEN_VAA_XSLT       = "xslt"+File.separator+
	"vaa"+File.separator+"vaaXml2Txt.xslt";
	/**
	 * top Composite for this dialog.
	 */
	protected Composite top = null;
	
	/**
	 * the Volcano this dialog representing for.
	 */
	private Volcano volcano = null;	
	
	/**
	 * Volcano location text, depends on the Volcano
	 */
	Label lblLocText = null; 

	/**
	 * Volcano area text, depends on the Volcano
	 */
	Label lblAreaText = null; 

	/**
	 * Volcano location elevation, depends on the Volcano
	 */
	Label lblElevText = null;
	
	/**
	 * Combo widget for VAAC station name.
	 */
	Combo comboStn = null;
	
	/**
	 * Combo widget for VAAC station id.
	 */
	Combo comboId = null; 

	/**
	 * Combo widget for VAAC station header.
	 */
	Combo comboHdr = null;

	/**
	 * Combo widget for the VAA product type.
	 */
	Combo comboType = null;
	
	/**
	 * Text field for the eruption year.
	 */
	Text  txtYear=null; 

	/**
	 * Text field for the eruption advisory no.
	 */
	Text  txtAdNo=null; 

	/**
	 * Text field for the correction no.
	 */
	Text  txtCorr=null;			

	/**
	 * Text field for the eruption info source.
	 */
	Text  txtInfoSour=null; 

	/**
	 * Text field for the eruption addition source.
	 */
	Text  txtAddInfoSour=null; 

	/**
	 * Text field for the eruption details.
	 */
	Text  txtErup=null; 

	/**
	 * Text field for the eruption Date.
	 */
	Text  txtAshDate=null; 

	/**
	 * Text field for the eruption time.
	 */
	Text  txtAshTime=null; 

	/**
	 * Text field for the remarks for the eruption.
	 */
	Text  txtRemark=null;
	
	/**
	 * Combo widget for aviation color code
	 */
	Combo comboAviColoCode = null; 

	/**
	 * Combo widget for next advisory time.
	 */
	Combo comboNextAdv = null; 

	/**
	 * Combo widget for forecaster name.
	 */
	Combo comboForecaster = null;
	
	/**
	 * list for Volcano info sources.
	 */
	org.eclipse.swt.widgets.List listInfoSour = null;
	
	/**
	 * flag indicating if it is from this dialog.
	 */
	private boolean fromEditDlg = true;

	/**
	 * flag indicating if it is from selection.
	 */
	private boolean fromSelection = true;
	
	/**
	 * Radio Button for NIL of Date/time.
	 */
	private Button btnNil = null;

	/**
	 * Radio Button for ash clouds info.
	 */
	private Button btnCloudInfo = null;

	/**
	 * Radio Button for correction.
	 */
	private Button btnCorr = null;
	
	/**
	 * product types from this dialog: NORMAL, QUICK, etc
	 */
	private String[] type = VaaInfo.ProductInfo.getProduct(VaaInfo.LOCS[0]);
	
	/**
	 * Volcano elevation text.
	 */
	private String elevFootMeterTxt = "";
	
	/**
	 * xml file name for the Volcano.
	 */
	private String volXMLFileName = "";
	
	/**
	 * list of ash clouds
	 */
	private List<VolcanoAshCloud> vacList = new ArrayList<VolcanoAshCloud>();
	
	/**
	 * GridData for layout of this dialog parts
	 */
	private GridData singleTxtGridData = new GridData(58,16);
	
	/**
	 * helper class for verifying input texts.
	 */
	private TxtVerifyListener tvl = new TxtVerifyListener();
	
	/**
	 * Fhr 6 Ash Cloud info of user manual inputs	
	 */
	private String vacInfoFhr6 = null;

	/**
	 * Fhr 12 Ash Cloud info of user manual inputs	
	 */
	private String vacInfoFhr12 = null;

	/**
	 * Fhr 18 Ash Cloud info of user manual inputs	
	 */
	private String vacInfoFhr18 = null;
	
	/**
	 * constructor for this class.
	 * @param Shell: parent Shell for this dialog.
	 * @throws VizException
	 */
	public VolcanoVaaAttrDlg(Shell parShell) throws VizException {		
        super(parShell);                   
	}
	
	/**
	 * singleton creation method for this class.
	 * @param Shell: parent Shell for this dialog.
	 * @return
	 */
	public static VolcanoVaaAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){					
			try {
				INSTANCE = new VolcanoVaaAttrDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}			
		}		
		return INSTANCE;		
	}
	
	public HashMap<String, Object> getAttrFromDlg(){ return new HashMap<String,Object>(); }
	
	/**
	 * when the attrDlg opens after selecting, 
	 * the selected elem is NOT set yet.
	 * 
	 * This method is called after it opened, 
	 * so some fields can be set here.
	 * @param IAttribute: the element this dialog for.
	 */	
	public void setAttrForDlg(IAttribute ia){
		
		//---title
		
		this.getShell().setText( getDlgTitle() );
		
		//---location, area, elevation
		
		lblLocText.setText(volcano.getTxtLoc());
		lblAreaText.setText(volcano.getArea());
		lblElevText.setText(volcano.getElev());//getElevText());
		
		//---year, advisorNo, correction
		
		txtYear.setText(getYear());
		txtAdNo.setText(getAdvisoryNo());
		/*String corr = volcano.getCorr();//-----------nmap2 does NOT do this, so comment out
		if(corr != null){
			btnCorr.setSelection(true);
			txtCorr.setEnabled(true);
			txtCorr.removeVerifyListener(tvl);//tvl added already during creation
			txtCorr.setText(corr);
			txtCorr.addVerifyListener(tvl);
			setTxtChange(txtAdNo, false);
		}*/
		
		//---product type: from Edit
		
		comboType.setEnabled( true );
		comboType.setText(volcano.getProduct()); //in legacy, only type is NOT reflected 2010-03-25
		
		//--- set text fields
		
		txtInfoSour.setText(Volcano.getUserInputPart(volcano.getInfoSource()));//.getNoNullTxt(volcano.getInfoSource()));
		txtAddInfoSour.setText(volcano.getNoNullTxt(volcano.getAddInfoSource()));
		txtErup.setText(Volcano.getUserInputPart(volcano.getErupDetails()));//.getNoNullTxt(volcano.getErupDetails()));
		txtAshDate.setText(volcano.getNoNullTxt(volcano.getObsAshDate()));
		txtAshTime.setText(volcano.getNoNullTxt(volcano.getObsAshTime()));
		txtRemark.setText(Volcano.getUserInputPart(volcano.getRemarks()));//.getNoNullTxt(volcano.getRemarks()));
		
		//--- set combo fields
		
		setComboItem(comboForecaster, volcano.getForecasters(), true);
		setComboItem(comboNextAdv,volcano.getNextAdv(), false);
		
		//--- TODO: obs & fcst
		
	}
	
	public void setVolcano(DrawableElement de){
		volcano = (Volcano)de;//2010-03-25VaaInfo.getVolcano();//2010-03-17//(Volcano)de;
	}
	
	public Volcano getVolcano(){
		return volcano;
	}
	
	/**
	 * button listener method overridden from super class.
	 * @param int: button id for buttons.
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (IDialogConstants.OK_ID == buttonId) {
			okPressed();
		}else if (IDialogConstants.CANCEL_ID == buttonId) {
			cancelPressed();
		}else if (this.FORMAT_ID == buttonId){
			formatPressed();
		}else if (this.RESET_ID == buttonId){
			resetPressed();
		}
		
	}

	/**
	 * button listener helper for "Apply", overridden from super class.
	 */
	@Override
	public void okPressed(){//Apply button
		//TODO: undo/redo ???
		//--- 2010-03-23
		this.copyAttr2Vol();//.copyAttrToVolcano();
	}	
	
	/**
	 * button listener helper for "Cancel", overridden from super class.
	 */
	@Override
	public void cancelPressed(){
		if(drawingLayer == null){
			setReturnCode(CANCEL);
			close();
		}else{
			super.cancelPressed();
		}
	}
	
	/**
	 * button listener helper for "Format Vaa".
	 */
	public void formatPressed(){	
		copyAttr2Vol();
		
		SaveMsgDlg smDlg = SaveMsgDlg.getInstance(this.getParentShell());
		smDlg.setVolAttrDlgInstance(VolcanoVaaAttrDlg.INSTANCE);
				
		smDlg.setVolcano(volcano);	
		
		String xsltFile = PgenStaticDataProvider.getProvider().getFileAbsolutePath(
				   PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + PGEN_VAA_XSLT );
		
		
		ArrayList<Product> prds = new ArrayList<Product>();
		Product volProd = getVaaProduct( volcano );

		String xml = "";
		if(volProd != null){
			prds.add(volProd);
			Products filePrds = ProductConverter.convert( prds );
			try {
				xml = SerializationUtil.marshalToXml( filePrds );
			} catch (JAXBException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		
		smDlg.setTxtFileContent( convertXml2Txt(xml, xsltFile));	
		
		smDlg.setBlockOnOpen(true);
		smDlg.open();

		if ( smDlg.getReturnCode() == OK ){ 
			saveVolcano();
		}
	}
	
	/**
	 * button listener helper for "Reset".
	 */
	public void resetPressed(){
		
		txtYear.setText("");
		txtAdNo.setText("");
		txtInfoSour.setText("");
		txtAddInfoSour.setText("");
		txtErup.setText("");
		txtAshDate.setText("");
		txtAshTime.setText("");
		txtRemark.setText("");
		
		comboForecaster.deselectAll();
		comboHdr.deselectAll();
		comboId.deselectAll();
		comboNextAdv.deselectAll();
		comboStn.deselectAll();	
		
	}
	
	/**
	 * method for creating the Button bar for this dialog;
	 * overridden form the super class.
	 * @param Composite: parent of the dialog.
	 */
	@Override
	public void createButtonsForButtonBar(Composite parent){		 		
		
		createButton(parent, IDialogConstants.OK_ID, "Apply", true);
    	createButton(parent, IDialogConstants.CANCEL_ID, "Cancel",true);
    	createButton(parent, FORMAT_ID, "Format VAA",	true);    
    	createButton(parent, RESET_ID, "Reset Form",	true);    	
    	
    	getButton(IDialogConstants.OK_ID).setEnabled(true);
  		getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
  		getButton(FORMAT_ID).setEnabled(true);
  		getButton(RESET_ID).setEnabled(true); 
  		this.mouseHandlerName = null;//--- 2010-03-10 for VaaCloudDlg not showing buttons when creating
  	}
	
	/**
	 * method for creating the dialog area, 
	 * overridden from the super class.
	 * @param Composite: parent Composite of the dialog.
	 * @return:
	 */
	@Override
	public Control createDialogArea(Composite parent) {			
		this.top = (Composite) super.createDialogArea(parent);
		
		this.getShell().setText( getDlgTitle() );
		
		GridLayout mainLayout = new GridLayout(8, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);
        
        Group top1 = new Group(top,SWT.LEFT);
        top1.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
        top1.setLayout(new GridLayout(8,false));			
		createArea1(top1);
		
		Group top2 = new Group(top,SWT.LEFT);
        top2.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
        top2.setLayout(new GridLayout(8,false));			
		createArea2(top2);
		
		Group top3 = new Group(top,SWT.LEFT);
		top3.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
		top3.setLayout(new GridLayout(8,false));			
		createArea3(top3);
		
		Group top4 = new Group(top,SWT.LEFT);
		top4.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
		top4.setLayout(new GridLayout(8,false));			
		createArea4(top4);			
		
		return top;
	}
	
	/**
	 * helper method for creating dialog area.
	 * @param Group: top container for the widgets.
	 */
	private void createArea1(Group top){
		Label lblLoc = new Label(top, SWT.LEFT);
		lblLoc.setText("Location: ");
		
		lblLocText = new Label(top, SWT.LEFT);
		lblLocText.setText( getLocText() );
		
		Label lblArea = new Label(top, SWT.LEFT);
		lblArea.setText("Area: ");
		
		lblAreaText = new Label(top, SWT.LEFT);
		lblAreaText.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,5,1));
		lblAreaText.setText( getAreaText() );
		
		
		Label lblElev = new Label(top, SWT.LEFT);
		lblElev.setText("Elevation: ");
		
		lblElevText = new Label(top, SWT.LEFT);
		lblElevText.setText( getElevText() );
	}
	
	/**
	 * helper method for creating dialog area.
	 * @param Group: top container for the widgets.
	 */
	private void createArea2(Group top){
		Label lblOrig = new Label(top, SWT.LEFT);
		lblOrig.setText("Orig Stn/VAAC: ");
		
		comboStn = new Combo(top, SWT.READ_ONLY);
		comboStn.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,5,1));
		comboStn.setItems(this.getStnIdNumArray(null, null));
		comboStn.select(1);
		
		Label lblDummy1 = new Label(top,SWT.LEFT), lblDummy2 = new Label(top, SWT.LEFT);
		
		Label lblId = new Label(top, SWT.LEFT);
		lblId.setText("WMO ID: ");
		
		comboId = new Combo(top, SWT.READ_ONLY);
		comboId.setItems( this.getStnIdNumArray(true, comboStn.getText().trim()) );
		comboId.select(0);
		
		Label lblHdr = new Label(top, SWT.LEFT);
		lblHdr.setText("Hdr Number: ");
		
		comboHdr = new Combo(top, SWT.READ_ONLY);
		comboHdr.setItems(this.getStnIdNumArray(false, comboStn.getText().trim()) );
		comboHdr.select(0);
		
		comboType = new Combo(top, SWT.READ_ONLY);
		comboType.setItems(type);
		comboType.select(0);
		comboType.setLayoutData(new GridData(SWT.RIGHT,SWT.CENTER,true,false,2,1));
		comboType.setEnabled( isFromSelection() );
		
		comboStn.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				String stn = comboStn.getText().trim();
				
				comboId.setItems( getStnIdNumArray(true, stn));
				comboId.select(0);
				
				comboHdr.setItems( getStnIdNumArray(false, stn));
				comboHdr.select(0);
			}
		});
		
	}
	
	/**
	 * helper method for creating dialog area.
	 * @param Group: top container for the widgets.
	 */
	private void createArea3(Group top){
		
		Label lblYear = new Label(top, SWT.LEFT);
		lblYear.setText("Year: ");
		
		txtYear = new Text(top, SWT.LEFT | SWT.BORDER);
		txtYear.setLayoutData(singleTxtGridData);
		
		Label lblAdNo = new Label(top, SWT.LEFT);
		lblAdNo.setText("Advisory No: ");
		
		txtAdNo = new Text(top, SWT.LEFT | SWT.BORDER);
		txtAdNo.setLayoutData(singleTxtGridData);
		
		btnCorr = new Button(top, SWT.CHECK);
		btnCorr.setText("Correction: ");
		
		txtCorr = new Text(top, SWT.LEFT | SWT.BORDER);
		txtCorr.setEnabled(false);		
		txtCorr.addVerifyListener(tvl);		
		
		btnCorr.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				boolean flag = ((Button) e.widget).getSelection();				
				if( ! flag ){ 
					String corr = txtCorr.getText();					
					if( corr!=null && corr.length()>0){				
						setTxtChange(txtAdNo, true);
						
						// listener removed to allow reset the field
						txtCorr.removeVerifyListener(tvl);
						txtCorr.setText("");
						txtCorr.addVerifyListener(tvl);
					}
					
				}				
				txtCorr.setEnabled(flag);
			}
		});
		
		txtYear.setText(getYear());// always set year and advisory no		
		txtAdNo.setText(getAdvisoryNo());
	}
	
	/**
	 * helper method for creating dialog area.
	 * @param Group: top container for the widgets.
	 */
	public void createArea4(Group top){
		/*
		Label lblInfoSour = new Label(top, SWT.LEFT);
		lblInfoSour.setText("Information Source: ");
		lblInfoSour.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));
	
		listInfoSour = new org.eclipse.swt.widgets.List(top, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);
		GridData gData = new GridData(300,96);
		gData.horizontalSpan = 7;
		listInfoSour.setLayoutData( gData);		
		listInfoSour.setItems( this.getInfoSourItems() );//Multi-selecting: ctrl+click / shift+click
		*/
		//------------------ Info Source
		
		Button btnInfoSour = new Button(top, SWT.PUSH);
		btnInfoSour.setText("Information Source:");
		btnInfoSour.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));
		
		txtInfoSour = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP);
		GridData gData = new GridData(300,96);
		gData.horizontalSpan = 7;
		txtInfoSour.setLayoutData(gData);
		txtInfoSour.setEditable(false);// only get info from the dialog			
		
		btnInfoSour.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				AshCloudInfoSourceDlg  isDlg = 
					AshCloudInfoSourceDlg.getInstance(VolcanoVaaAttrDlg.this.getParentShell());
				
				isDlg.open();
			}
		});
		
		//------------------ Addition
		
		Label lblAddInfoSour = new Label(top, SWT.LEFT);
		lblAddInfoSour.setText("Add'l Info Source: ");
		lblAddInfoSour.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));
		
		txtAddInfoSour = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP);
		GridData gData2 = new GridData(300,96);
		gData2.horizontalSpan = 7;
		txtAddInfoSour.setLayoutData(gData2);
		txtAddInfoSour.addModifyListener(new TxtModifyListener());
		
		//------------------ Aviation
		
		Label lblAviColoCode = new Label(top,SWT.LEFT);
		lblAviColoCode.setText("Aviation Color Code: ");
		
		comboAviColoCode = new Combo(top,SWT.READ_ONLY);
		comboAviColoCode.setItems(getAviColoCode());
		comboAviColoCode.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,7,1));
		comboAviColoCode.setEnabled(false);//TODO: ONLY for Washington VAAC ???
		
		//------------------ Eruption
		
		Label lblErup = new Label(top, SWT.LEFT);
		lblErup.setText("Eruption Details: ");
		lblErup.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));
		
		txtErup = new Text(top, SWT.BORDER | SWT.MULTI);
		GridData gData3 = new GridData(300,96);
		gData3.horizontalSpan = 7;
		txtErup.setLayoutData(gData3);
		txtErup.addModifyListener(new TxtModifyListener());
		
		//------------------ Ash Date / Time
		
		Label lblAshDate = new Label(top, SWT.LEFT);
		lblAshDate.setText("Obs Ash Date(DD): ");		
		
		txtAshDate = new Text(top, SWT.BORDER);				
		txtAshDate.setLayoutData(singleTxtGridData);
		
		Label lblAshTime = new Label(top, SWT.LEFT);
		lblAshTime.setText("Time(HHHH):");		
		
		txtAshTime = new Text(top, SWT.BORDER);			
		txtAshTime.setLayoutData(singleTxtGridData);
		
		Label lblZ = new Label(top, SWT.LEFT);
		lblZ.setText("Z");		
		
		btnNil = new Button(top, SWT.CHECK);
		btnNil.setText("NIL");			
		btnNil.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				String nil = volcano.NIL_STRING, clear = "";
				
				if( ((Button)e.widget).getSelection() ){
					txtAshDate.setText(nil);
					txtAshTime.setText(nil);
					txtAshDate.setEditable(false);
					txtAshTime.setEditable(false);
				}else{
					txtAshDate.setText(clear);
					txtAshTime.setText(clear);
					txtAshDate.setEditable(true);
					txtAshTime.setEditable(true);
				}
			}
		});
		
		Label lblDummyNil = new Label(top, SWT.LEFT), lblDummyNil2 = new Label(top, SWT.LEFT);
		
		//------------------ Cloud Info
		
		btnCloudInfo = new Button(top, SWT.PUSH);
		btnCloudInfo.setText("Observed and Forecast Ash Cloud Inormation...");
		btnCloudInfo.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,8,1));
		btnCloudInfo.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				
				AshCloudInfoDlg aciDlg = AshCloudInfoDlg.getInstance(VolcanoVaaAttrDlg.this.getParentShell());				
						
				aciDlg.setDateTimeForDlg(txtAshDate.getText().trim(), txtAshTime.getText().trim());
				aciDlg.setVaaAttrDlg(VolcanoVaaAttrDlg.this);
				aciDlg.open();
				//TODO: check vacList, get FHR00/06/12/18 info, then put in related fields of aciDlg
			}
		});
		//------------------ Remarks
		
		Label lblRemarks = new Label(top, SWT.LEFT);
		lblRemarks.setText("Remarks: ");
		lblRemarks.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));
		
		txtRemark = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP);
		GridData gData4 = new GridData(300,96);
		gData4.horizontalSpan = 7;
		txtRemark.setLayoutData(gData4);
		txtRemark.addModifyListener(new TxtModifyListener());
		
		//------------------ Advisory
		
		Label lblNextAdv = new Label(top, SWT.LEFT);
		lblNextAdv.setText("Next Advisory: ");
		
		comboNextAdv = new Combo(top, SWT.DROP_DOWN);
		comboNextAdv.setItems(getNextAdvText());
		comboNextAdv.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,7,1));
		comboNextAdv.addModifyListener(new TxtModifyListener());
		
		//------------------ Forecasters
		Label lblForecaster = new Label(top, SWT.LEFT);
		lblForecaster.setText("Forecaster(s): ");
		
		comboForecaster =new Combo(top, SWT.READ_ONLY | SWT.DROP_DOWN);
		comboForecaster.setItems( getForecastersName());
		comboForecaster.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,7,1));
		
		//--- set the contents after finishing layout
		//--- TEST/RESUME have rmks without a volcano
		txtRemark.setText(getRemarks());	
		
		/*
		 * TEST/RESUME/BACKUP: set the Date/Time to NIL
		 */
		
		if( VaaInfo.isNonDrawableVol(volcano)){
			btnNil.setSelection(true);
			txtAshDate.setText(volcano.NIL_STRING);
			txtAshTime.setText(volcano.NIL_STRING);
			txtAshDate.setEditable(false);
			txtAshTime.setEditable(false);
		}
		
	}
	
	/**
	 * getter for the dialog's title, depending on 
	 * the Volcano this dialog representing for.
	 * @return String: this dialog's title.
	 */
	private String getDlgTitle(){
		StringBuilder sb = new StringBuilder("VAA - ");
		
		String num = volcano.getNumber();
		sb.append(volcano.getName()+"("+( num==null ? "" : num)+")" );
	
		return sb.toString().toUpperCase();
	}
	
	private String getLocText(){
		//initial value, to be replaced by setAttrForDlg(IAttribute)
		return volcano.getTxtLoc();//"N9999W99999"; 
	}
	
	private String getAreaText(){
		
		return volcano.getArea();//"TEST";
	}
	
	/**
	 * getter for Volcano elevation with foot or meter.
	 * @return String: text of elevation of the Volcano.
	 */
	private String getElevText(){
		if( volcano == null)
			return "";
		//return volcano.getElev();/*---2010-03-26 move to VolcanoCreateDlg TODO: clean up code		
		StringBuilder sb = new StringBuilder();
		
		int dMeter = 0;
		String sMeter = volcano.getElev();
		/*try{
			dMeter = Double.parseDouble(sMeter);
		}catch(Exception e){ }
		String feet = VaaInfo.getFootTxtFromMeter(dMeter,0);*/
		if(sMeter == null || sMeter.length() == 0){ 
			sb.append(dMeter).append("  ft (").append(dMeter).append("  m)");
			volcano.setElev(sb.toString());// reset elev. for Txt Msg
		}
		elevFootMeterTxt = volcano.getElev();//sb.toString();
		return elevFootMeterTxt;//*///TODO: elevFootMeterTxt needs to be assigned a value ??
	}
	
	public String getElevFootMeterTxt(){
		return elevFootMeterTxt;
	}
	
	/**
	 * Function for get Stn/VAAC, WMO ID, and Hdr No.
	 * @param isId, a Boolean used as a 3-way flag: null:Stn; true:ID; false:Hdr No. 
	 * @param stn, a String as the key to STN map.
	 */	
	public static String[] getStnIdNumArray(Boolean isId, String stn){
		Map<String,String[]> map = VaaInfo.VAA_INFO_STN_MAP;
	
		if(isId == null){
			return map.keySet().toArray(new String[]{});
		}else if(isId){
			return stn==null ? new String[]{} : map.get(stn)[0].substring(1).split(";");
		}else{
			return stn==null ? new String[]{} : map.get(stn)[1].substring(1).split(";");
		}		
	}
	
	/**
	 * static method using VaaInfo maps for info source.
	 * @return String[]: info source items.
	 */	
	public static String[] getInfoSourItems(){
		String[] s = VaaInfo.VAA_INFO_SINGLE_MAP.get("information-source");

		return s;
	}
	
	/**
	 * static method using VaaInfo maps for forecaster names.
	 * @return String[]: forecaster names.
	 */	
	public static String[] getForecastersName(){
		String[] s = VaaInfo.VAA_INFO_SINGLE_MAP.get("forecasters");

		 return s;
	}
	
	/**
	 * static method using VaaInfo maps for aviation color codes.
	 * @return String[]: aviation color codes.
	 */	
	public static String[] getAviColoCode(){
		String[] s = VaaInfo.VAA_INFO_SINGLE_MAP.get("aviation-color-code");

		return s;
	}
	
	/**
	 * static method using VaaInfo maps for next advisory.
	 * @return String[]: next advisory items.
	 */	
	public static String[] getNextAdvText(){
		String[] s = VaaInfo.VAA_INFO_SINGLE_MAP.get("next-advisory");

		 return s;
	}
	
	/**
	 * getter for remarks for the Volcano; 
	 * some with hard-wired text, others user input.
	 * @return String: remarks in text.
	 */
	public String getRemarks(){
		String rmk = Volcano.getNoNullTxt(volcano.getRemarks());	//rmk == null ? "" : rmk;
		String[] words = rmk.split(Volcano.WORD_SPLITTER);
		if(VaaInfo.isNonDrawableVol(volcano) && words.length > 1)
			return words[1];//TEST, RESUME, BACKUP
		return words[0];
	}
	
	/**
	 * getter for current year.
	 * @return String: current year text.
	 */
	public String getYear(){		
		return ""+(1900 + new Date().getYear());		
	}
	
	/**
	 * The Advisory No for ALL TEST/RESUME: 001 
	 * 
	 * for others, it's 1 + the advisory no of the latest
	 * text product of the same volcano.
	 *  
	 * 1. Get the latest text product of the volcano
	 *    (use the file name: zao_20100219_1741.txt    
	 *    
	 * 2. If 	no such file exist use 000.
	 *    else 	Get the advisory no
	 *    
	 * 3. return the advisory no + 1
	 * 
	 * NOTE: 2010-02-19
	 * NO txt products any more!
	 * xslt will be used to translate the element
	 * xml file to required format. 
	 */		
	public String getAdvisoryNo(){
		//TODO: see @5418 of nmap_pgvolw.c
		
		if(VaaInfo.isNonDrawableVol(volcano))
			return "001";		
			
		String no = VaaInfo.getLatestAdvNo(volcano.getName(), VaaInfo.FILE_EXTENSION_XML);		
		int noInt = Integer.parseInt(no);
		
		return  new DecimalFormat("000").format(++noInt);		
	}
	
	public boolean isFromSelection() {
		return fromSelection;
	}

	public void setFromSelection(boolean fromSelection) {
		this.fromSelection = fromSelection;
	}
	
	/**
	 * method for copying dialog attributes to the Volcano.
	 */
	public void copyAttrToVolcano(){
	
		volcano.setOrigStnVAAC(comboStn.getText());
		volcano.setWmoId(comboId.getText());
		volcano.setHdrNum(comboHdr.getText());
		if( this.fromSelection ) volcano.setProduct(comboType.getText()); // TEST/RESUME can be set already
		volcano.setYear(txtYear.getText());
		volcano.setAdvNum(txtAdNo.getText());
		volcano.setCorr(txtCorr.getText());
	
		//String[] info = this.listInfoSour.getSelection();
		//StringBuilder infoS = new StringBuilder();
		//for(String s : info) infoS.append(s).append(" ");		
		volcano.setInfoSource(this.txtInfoSour.getText());//infoS.toString());
		
		volcano.setAddInfoSource( getTxtNoRsrvWord(txtAddInfoSour.getText()) );
		volcano.setAviColorCode(comboAviColoCode.getText());
		volcano.setErupDetails( getTxtNoRsrvWord(txtErup.getText()) );
		
		volcano.setObsAshDate(txtAshDate.getText());
		volcano.setObsAshTime(txtAshTime.getText());
		volcano.setNil(""+btnNil.isEnabled());
		
		//TODO:	obs & fcst 00,6,12,18
		volcano.setObsFcstAshCloudInfo(VaaInfo.getAshCloudInfo("00")); //VaaInfo.LAYERS[1] "OBS" NOT F00	
		String[] fhrDT = VaaInfo.getFhrTimes(txtAshDate.getText().trim(), txtAshTime.getText().trim());
		volcano.setObsFcstAshCloudInfo6( getVacInfoFhr6(fhrDT) );//VaaInfo.getAshCloudInfo("06"));
		volcano.setObsFcstAshCloudInfo12( getVacInfoFhr12(fhrDT) );//VaaInfo.getAshCloudInfo("12"));
		volcano.setObsFcstAshCloudInfo18( getVacInfoFhr18(fhrDT) );//VaaInfo.getAshCloudInfo("18"));
		
		//volcano.setObsFcstAshCloudInfo()
	
		volcano.setRemarks( getTxtNoRsrvWord(txtRemark.getText()) );
		volcano.setNextAdv( getTxtNoRsrvWord(getNxtAdvTxt(comboNextAdv.getText())) );
		volcano.setForecasters(comboForecaster.getText());		
		
	}
	
	/**
	 * set the Volcano element's fields
	 * with different types:
	 * 
	 * TEST/RESUME, NORMAL, END/QUICK/NEAR
	 * 
	 */
	private void copyAttr2Vol(){
		
		copyFixedAttr2Vol();		
		
		if( VaaInfo.isNonDrawableVol(volcano)){
			/*
			 * for TEST/RESUME, all fields set already
			 * in VolcanoCreateDlg btnGo listener and
			 * copyFixedAttr2Vol()
			 */
			return;
			
		}else{		
				
			copyAttrToVolcano();
					
			/*
			 * reset certain fields using values
			 * from the Vaa.xml file
			 */
			VaaInfo.setVolcanoFields(volcano, volcano.getProduct(), true);			
				
		}
		
	}
	
	/**
	 * these attributes are needed for all product types
	 */
	private void copyFixedAttr2Vol(){		
		
		volcano.setWmoId(comboId.getText());
		volcano.setHdrNum(comboHdr.getText());
		volcano.setOrigStnVAAC(comboStn.getText());
		volcano.setCorr(txtCorr.getText());
		
		if( volcano.getProduct()==null || volcano.getProduct().length()==0) 
			volcano.setProduct(comboType.getText()); // TEST/RESUME can be set already			
		if("BACKUP".equals(volcano.getProduct()))//TODO: hard-code ?
			volcano.setRemarks( getTxtNoRsrvWord(txtRemark.getText()) );
	}
	
	/**
	 * set info source Text field.
	 * @param String[]: infor source items.
	 */
	public void setInfoSource(String[] source){
		
		if( source == null || source.length == 0) return ;
		
		StringBuilder sb = new StringBuilder();
		
		for(String s : source){
			sb.append(s).append(". ");
		}
		//set focus, or some lines will NOT display completely. TODO: investigate this
		txtInfoSour.setFocus();
		
		txtInfoSour.setText(sb.toString());		
	}

	private void init(){		
		
	}
	
	/**
	 * control a Text field's number increment/decrement by 1
	 * @param txtAdNo: the field that the number in it changes
	 * @param increFlag: increment true, decrement false.
	 */	
	private void setTxtChange(Text txtAdNo, boolean increFlag){
		if( VaaInfo.isNonDrawableVol(volcano))
			return;
		
		if(txtAdNo == null || txtAdNo.isDisposed())
			return;
		
		String adNo = txtAdNo.getText();
		if(adNo == null || adNo.length() == 0)
			return;
		
		int ano = 0, orig = 0;
		try{
			ano = Integer.parseInt(txtAdNo.getText());
			orig = Integer.parseInt(getAdvisoryNo());
		}catch(Exception e){
			return;
		}
		
        if(orig > 1){//ano > 1
        	  if(increFlag)
        		  ano++;
        	  else
        		  ano--;
        	  
        	  txtAdNo.setText(new DecimalFormat("000").format(ano));
        }
	}
	
	/**
	 * This class verifys the txtCorr content
	 * @author gzhang
	 */	
	private class TxtVerifyListener implements VerifyListener{
		
		/**
		 * only A-Z allowed for Correction field
		 */
		//TTR632: Numbers and lower cases are also allowed cor correction field
		
		
		public void verifyText(VerifyEvent event){
			// Assume we don't allow it
	        event.doit = false;

	        // Get the character typed
	        int myChar = event.character;
	        String text = ((Text) event.widget).getText();

	        // Allow A-Z as first char and decrement advNo
	        // Allow a-z and numbers for TTR632 
	        if (((myChar>=65 && myChar<=90) || 
	        	 (myChar>=97 && myChar<=122)||
	        	 Character.isDigit(myChar))
	        		&& text.length() == 0){	        	
	        	event.doit = true;	        	
	        	setTxtChange(txtAdNo, false);		          
	        }

	        //0-9? Grace Swanson: ONLY Upper Case letters //if(Character.isDigit(myChar)&&text.length()==0) event.doit = true;
	        
	        //Backspace: 8 and delete: 127 and increment advNo
	        if ((myChar==8 || myChar==127) && text.length() == 1){
		          event.doit = true;
		          setTxtChange(txtAdNo, true);				         
	        }
			
		}
	}
	
	/**
     * a safe way setting text for Combo Widget
     * 
     * @param Combo Widget
     * @param text to be with the Combo
     * @param isReadOnly flag
     */    
    public void setComboItem(Combo c, String s, boolean isReadOnly){
    	if(isReadOnly && Arrays.asList(c.getItems()).contains(s))
    		c.setText(s);    	
    	if( ! isReadOnly && s != null && s.length() > 0)
    		c.setText(Volcano.getUserInputPart(s));
    	
    }
    
    /**
     * a null field means AshCloudInfoDlg not opened
     * so set it with drawing layer's clouds info
     * same for fhr12/18
     */
	public String getVacInfoFhr6(String[] fhrDT) {
		return vacInfoFhr6==null 
				? fhrDT[0]+"  "+VaaInfo.getAshCloudInfo(VaaInfo.LAYERS[2])
				: vacInfoFhr6;
	}

	public void setVacInfoFhr6(String vacInfoFhr6) {
		this.vacInfoFhr6 = vacInfoFhr6;
	}

	public String getVacInfoFhr12(String[] fhrDT) {
		return vacInfoFhr12==null 
				? fhrDT[1]+"  "+VaaInfo.getAshCloudInfo(VaaInfo.LAYERS[3])
				: vacInfoFhr12;
	}

	public void setVacInfoFhr12(String vacInfoFhr12) {
		this.vacInfoFhr12 = vacInfoFhr12;
	}

	public String getVacInfoFhr18(String[] fhrDT) {
		return vacInfoFhr18==null 
				? fhrDT[2]+"  "+VaaInfo.getAshCloudInfo(VaaInfo.LAYERS[4])
				: vacInfoFhr18;
	}

	public void setVacInfoFhr18(String vacInfoFhr18) {
		this.vacInfoFhr18 = vacInfoFhr18;
	}
	
	/**	
	 *  2010-04-07/12 it has been decided to save
	 * 	Volcano into XML files when Format VAA 
	 *  is clicked, then using XSLT to transform
	 *  the XML into text 
	 *  
	 *  @return Product of Volcanos and VolcanoAshClouds
	 */
	private Product getVaaProduct(Volcano vol){
		/*
		List<Product> prods = PgenSession.getInstance().getPgenResource().getProducts();
		Product volProd = null;
		for(Product p : prods){
			List<Layer> lyrList = p.getLayers();
			for(Layer lyr : lyrList){
				List<AbstractDrawableComponent> list = lyr.getDrawables();
				for(AbstractDrawableComponent adc : list){
					
					if( adc instanceof Volcano 
							&& volcano.getName().equals( ((Volcano)adc).getName())){
						volProd = p;
					}
				}
			}			
		}
		*/
		return vol != null ? VaaInfo.VOL_PROD_MAP.get(vol) : null;//volProd;
	}
	
	/**
	 * getter for the to be saved file name.
	 * @return String: file name
	 */
	private String getFileName(){
		String connector = "_";
		
		StringBuilder sb = new StringBuilder();
		sb.append( volcano.getName() );
		sb.append(connector);
		sb.append(VaaInfo.getDateTime("yyyyMMdd"));
		sb.append(connector);
		sb.append(VaaInfo.getDateTime("HHmm"));
				
		sb.append(".xml");
		volXMLFileName = sb.toString();
		return volXMLFileName;
	}
	
	/**
	 * method for saving the Volcano in xml file.	 * 
	 * @return boolean: flag indicating if saving successful.
	 */
	private boolean saveVolcano() {		
		
		String filename = PgenUtil.getPgenActivityTextProdPath()+File.separator+getFileName();

		//--------------No need since the file name invisible to users
		//boolean canWrite = checkFileStatus(filename);
		//if ( ! canWrite ) return false;
		
		ArrayList<Product> prds = new ArrayList<Product>();
		Product volProd = getVaaProduct( volcano );

		if(volProd != null){
			prds.add(volProd);
			Products filePrds = ProductConverter.convert( prds );
	
			FileTools.write( filename, filePrds );
			
			//remove the Volcano Product after saving
			//removeVaaProduct(volProd);
			return true;
		}
			
		return false;		
	}
	
	/***
	 * Creates a formatted string comprising of the contents of the XML string, to which
	 * formatting information is applied from the style-sheet.
	 * @param xmlString - XML string
	 * @param xltFileName - Name of the style-sheet
	 * @return A <tt>String</tt> with the formatted contents of the XML file.  
	 */
	private String convertXml2Txt(String xmlString, String xltFileName){
        String res = "";
		
        Source xmlSource = new StreamSource( new StringReader( xmlString ) );
        Source xsltSource = new StreamSource(xltFileName);
       
        TransformerFactory transFact = TransformerFactory.newInstance();
        
        try {
        	Transformer trans = transFact.newTransformer(xsltSource);
        	ByteArrayOutputStream baos = new ByteArrayOutputStream();
        	trans.transform(xmlSource, new StreamResult(baos));
        	
        	res = new String(baos.toByteArray());
        }
        catch ( Exception e ) {        	
             System.out.println("Error: File is corrupt");
        }
        
        return getNormalizedTxt(res);
	}	
	
	/**
	 * use the current time for Next Advisory if it
	 * requires time and Not entered by the user.
	 * 
	 * @param String: Next Advisory String
	 * @return String: String containing time if needed.
	 */	
	private String getNxtAdvTxt(String nxtAdv){
		String word = "YYYYMMMDD/HHNNZ";//TODO: replace the hard-code 
		
		if(nxtAdv!=null && nxtAdv.trim().length()>0	&& nxtAdv.contains(word)){
			StringBuilder sb = new StringBuilder();			
			
			sb.append(VaaInfo.getDateTime("yyyyMMdd"));
			sb.append("/");
			sb.append(VaaInfo.getDateTime("HHmm"));
			sb.append("Z");
			
			return nxtAdv.replaceFirst(word, sb.toString());
		}			
		
		return nxtAdv;
	}
	
	/**
	 * Prohibiting use of Volcano.WORD_SPLITTER in
	 * some Text fields from user inputs.
	 * 
	 * @author gzhang
	 *
	 */	
	private class TxtModifyListener implements ModifyListener{
		
		public void modifyText(ModifyEvent e){
			String txt = "";
			if(e.widget instanceof Text){
				txt =((Text)e.widget).getText();
			}else if(e.widget instanceof Combo){
				txt = ((Combo)e.widget).getText();
			}
			
			if(txt != null && txt.length() > 0 && txt.contains(Volcano.WORD_SPLITTER)){				
				
				MessageDialog confirmDlg = new MessageDialog( 
						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
						"Warning", null, " ::: IS A RESERVED WORD AND WILL BE REMOVED FROM THE TEXT!",
						MessageDialog.WARNING, new String[]{"OK"}, 0);
				
				confirmDlg.open();
			}
		}
	}
	
	/**
	 * take off the reserved word from the text.
	 * @param String: to be checked with reserved word.
	 * @return String: empty text or text without reserved words.
	 */
	private String getTxtNoRsrvWord(String txt){
		if(txt == null || txt.length() == 0)
			return "";
		
		if(txt.contains(Volcano.WORD_SPLITTER)){
			return txt.replaceAll(Volcano.WORD_SPLITTER, "");
		}
		
		return txt;
	}
	
	/**
	 * 20100818 workshop issue:
	 * PGEN text NOT to be added to the final text product.	 *  
	 */
	private String getNormalizedTxt(String s){
		StringBuilder sb = new StringBuilder("");
		
		String[] txt = s.split("\n");
		int start = 0, end = 0; //VOLCANO Layer text added on top, others bottom
		for(int i=0; i<txt.length; i++){
			if(txt.length > 3){//TODO: remove the hard coded words, see vaaAllHeader.xslt
				if(txt[i].startsWith("FV")
						&& txt[i+1].startsWith("VA ADVISORY")
						&& txt[i+2].startsWith("DTG") ){
					
					start = i;
				}
				
				if("NNNN".equals(txt[i]) && (i > start)) {
					end = i;
					break;
				}
			}
		}
		
		for(int j=start; j<=end; j++){
			sb.append(txt[j]).append("\n");
		}
		
		return sb.toString();
	}

	@Override
	public Coordinate[] getLinePoints() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getPatternName() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getSmoothFactor() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public Boolean isClosedLine() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Boolean isFilled() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public FillPattern getFillPattern() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getLineType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public double getWidth() {
		// TODO Auto-generated method stub
		return 0;
	}
	
	@Override
	public boolean close() {
		drawingLayer.removeSelected();
		SaveMsgDlg.getInstance(this.getParentShell()).close();
		return super.close();
	}
}
