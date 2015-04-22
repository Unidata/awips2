/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog
 * 
 * Janurary 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;

import java.awt.Color;
import java.util.*;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.graphics.RGB;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.*;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.sigmet.*;
import gov.noaa.nws.ncep.ui.pgen.*;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import com.raytheon.uf.viz.core.exception.VizException;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * The class for Volcano Ash Cloud create/edit dialog 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#165		G. Zhang   	Initial Creation.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 10/11		#?			J. Wu		Link color/FHR with layer.
 * 02/12		#481,2,3	B. Yin		Fixed VAA text output issues.
 * 03/13		#928		B. Yin 		Made the button bar smaller. 										
 * </pre>
 * 
 * @author	G. Zhang
 */

public class VaaCloudDlg extends AttrDlg implements IVaaCloud{
	
	/**
	 * singleton instance of this class.
	 */
	private static VaaCloudDlg INSTANCE;
	
	/**
	 * the top Composite of this dialog.
	 */
	protected Composite top = null;
	
	/**
	 * the volcano that the Clouds belong to.
	 */
	private Volcano volcano = null;
	
	/**
	 * the element represent the vaa Cloud.
	 */
	private AbstractSigmet vaCloud = null;
	
	/**
	 * radio button for drawing area.
	 */
	Button btnArea=null;

	/**
	 * radio button for drawing ESOL.
	 */
	Button btnLine=null;

	/**
	 * radio button for drawing NotSeen text.
	 */
	Button btnNotSeen=null;

	/**
	 * radio button for drawing others-fcst text.
	 */
	Button btnFcst=null;		

	/**
	 * Text field for width of ESOL.
	 */
	Text txtWidth=null;

	/**
	 * Text field for starting FL cloud.
	 */
	Text txtFL=null;

	/**
	 * Text field for ending FL cloud.
	 */
	Text txtFL2=null;

	/**
	 * Text field for direction of cloud.
	 */
	Text txtDir=null;

	/**
	 * Text field for moving speed of cloud.
	 */
	Text txtSpd=null;
	
	/**
	 * Combo widget for others-fcst
	 */
	Combo comboFcst=null;
	
	/**
	 * Combo widget for Forecast hours
	 */
	Combo comboFHR=null;
	
	/**
	 * color selector for the cloud to be drawn
	 */
	protected ColorButtonSelector cs = null;	
	
	/**
	 * String representing the type of clouds
	 */
	private String type = "Area";
	
	/**
	 * flag indicating if attributes have been copied to the element.
	 */
	private boolean copiedToSigmet = false;
	
	/**
	 * words divider
	 */
	private static final String SEPERATER = VaaInfo.SEPERATER;//SigmetAttrDlg.LINE_SEPERATER;
	
	/**
	 * String variable for mouse handler name with pgen selecting tool.
	 */
	private String mouseHandlerName = null;
	
	/**
	 * constructor for this class.
	 * @param Shell: parent Shell of this dialog.
	 * @throws VizException
	 */
	public VaaCloudDlg(Shell parShell) throws VizException {		
        super(parShell);               
	}
	
	/**
	 * singleton creation method for this class.
	 * @param Shell: parent Shell of this dialog.
	 * @return
	 */
	public static VaaCloudDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				INSTANCE = new VaaCloudDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}
			
		}		
		
		return INSTANCE;		
	}
	
	public HashMap<String, Object> getAttrFromDlg(){ return new HashMap<String,Object>(); }
	
	/**
	 * set attributes for this dialog with the
	 * elements' fields.
	 * @param IAttribute: element this dialog represents for.
	 */
	public void setAttrForDlg(IAttribute ia){ 
		if(AttrDlg.mouseHandlerName==null) return;//20100810 testing TODO		
		vaCloud = (AbstractSigmet) ia;
		type = vaCloud.getType();
		
		Color clr = ia.getColors()[0];
		if ( clr != null ) 
			setColor(clr);
		
		txtWidth.setText(""+(vaCloud.getWidth()));
		
		if(type.contains("Text")&& ! type.contains("WINDS")){
			comboFcst.select( VaaInfo.getFcstItemIndexFromTxt(type.split(SEPERATER)[1]));
		}
		
		/*
		 * use RadioBtnGroup convenient class for enable/disable buttons
		 */
		RadioBtnGroup rBtnGrp = 
			new RadioBtnGroup( new Button[]{ btnArea,btnLine,btnNotSeen,btnFcst } );
		
		if(type.contains("Area")) 
			rBtnGrp.enableBtn(btnArea, null, new Control[]{ txtWidth, comboFcst});
		else if(type.contains("Line")) 
			rBtnGrp.enableBtn(btnLine, new Control[]{txtWidth}, new Control[]{comboFcst});
		else if(type.contains("Text")){
			if(type.contains("WINDS"))
				rBtnGrp.enableBtn(btnNotSeen, null, new Control[]{txtWidth,comboFcst});
			else
				rBtnGrp.enableBtn(btnFcst, new Control[]{comboFcst}, new Control[]{txtWidth});
		}
		
		setFhrFlDirSpdTxt(vaCloud);
		AttrDlg.mouseHandlerName = null;//20100810 testing TODO
		
		//Associate the color and forecast hour with the active layer.
		if ( "Volcano".equalsIgnoreCase( drawingLayer.getActiveProduct().getType() ) ) {
		    cs.setColorValue( getLayerColor() );
		    comboFHR.setText( drawingLayer.getActiveLayer().getName() );
		}
		
	}
	
	/**
	 * button listener overridden for "Apply" button
	 */
	@Override
	public void okPressed(){
		applyPressed();
	}
	
	/**
	 * method overridden from super class for buttons creation.
	 */
	@Override
	public void createButtonsForButtonBar(Composite parent){
		
		((GridLayout)parent.getLayout()).verticalSpacing = 0;
		((GridLayout)parent.getLayout()).marginHeight = 3;
		
		// if NOT from select, then no buttons show
		
		if(mouseHandlerName==null) return;
		
		createButton(parent, IDialogConstants.OK_ID, "Apply", true);
    	createButton(parent, IDialogConstants.CANCEL_ID, "Cancel",true);
    	
    	getButton(IDialogConstants.OK_ID).setEnabled(true);
  		getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
  		
  		// reset it so next create will have NO buttons
  		
  		mouseHandlerName = null;
  		
  		this.getButton(IDialogConstants.CANCEL_ID).setLayoutData( new GridData(ctrlBtnWidth,ctrlBtnHeight));
  		this.getButton(IDialogConstants.OK_ID).setLayoutData( new GridData(ctrlBtnWidth,ctrlBtnHeight));
	}
	
	/**
	 * method overridden from super class for this dialog
	 */
	@Override
	public void enableButtons(){
		
	}
	
	/**
	 * method overridden for creating this dialog area.	
	 * @param Composite: parent for this dialog
	 */
	@Override
	public Control createDialogArea(Composite parent) {		
		this.top = (Composite) super.createDialogArea(parent);
		this.getShell().setText("VAA Ash Cloud Create/Edit");
		
		GridLayout mainLayout = new GridLayout(8, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);
        
        Group top1 = new Group(top,SWT.LEFT);
        top1.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
        top1.setLayout(new GridLayout(10,false));			
		createArea1(top1);
		
		Group top2 = new Group(top,SWT.LEFT);
        top2.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,11,1));
        top2.setLayout(new GridLayout(12,false));			
		createArea2(top2);    
        
		init();//setBtnObservers(); 
		return top;
	}
	
	/**
	 * helper method for creating dialog area.
	 * @param Group: container for this area.
	 */
	private void createArea1(Group top){
		
		btnArea = new Button(top,SWT.RADIO);
		btnArea.setText("Area");
		btnArea.setSelection(true);
		type = "Area";
		
		btnLine = new Button(top,SWT.RADIO);
		btnLine.setText("Line");
				
		Label lblWidth = new Label(top,SWT.LEFT);
		lblWidth.setText("Width:");
		
		//Label lblDummy = new Label(top,SWT.LEFT);	lblDummy.setText(" ");
		
		txtWidth = new Text(top,SWT.BORDER);
		txtWidth.setEnabled(false);
				
		btnNotSeen = new Button(top,SWT.RADIO);
		btnNotSeen.setText("NotSeen");
		
		
		btnFcst = new Button(top,SWT.RADIO);
		btnFcst.setText("others-FCST");
				
		comboFcst = new Combo(top,SWT.READ_ONLY);
		comboFcst.setItems(VaaInfo.VAA_INFO_OTHERSFCST_MAP.get(VaaInfo.OTHERSFCST_DIALOG));//new String[]{"NOT AVBL", "ASH DSIPTG","ASH DSIPG(NO FL)","NO ASH EXP"});
		comboFcst.select(0);	
		comboFcst.setEnabled(false);
		
		comboFcst.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				type = getFcstTxt();//"Text:::"+comboFcst.getText().trim();
			}
		});
		
		btnArea.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				type = "Area";
				radioBtnEnable(false,false);				
			}
		});
		
		btnLine.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				type = "Line:::ESOL";
				radioBtnEnable(true,false);				
			}
		});
		
		btnNotSeen.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				type = getNotSeenTxt();//type = "Text:::NotSeen";
				radioBtnEnable(false,false);			
			}
		});
		
		btnFcst.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				type = getFcstTxt();//"Text:::"+comboFcst.getText().trim();
				radioBtnEnable(false,true);				
			}
		});
		/*
		Composite colorGroup = new Composite(top, SWT.NONE);        
        cs = new ColorMatrixSelector(colorGroup, true, true, 30, 30, 14, 16, 24, 24, 0, 2, 2);
        cs.setColorValue(new RGB(0,255,0));
        */
		
		cs = new ColorButtonSelector( top, 50, 20 );
		cs.setColorValue( getLayerColor());//FHR_COLOR_MAP.get(VaaInfo.getLayerIdx()));//new RGB(255,255,0) );
		cs.addListener(new IPropertyChangeListener() {			
			public void propertyChange( PropertyChangeEvent event ) {
				//rscAttrPointColor.setAttrValue( event.getNewValue() );
			}
		});		
	}
	
	/**
	 * helper method for creating dialog area.
	 * @param Group: container for this area.
	 */
	private void createArea2(Group top){
		Label lblFHR = new Label(top, SWT.LEFT);
        lblFHR.setText("FHR ");
      
        comboFHR = new Combo(top, SWT.READ_ONLY);
        comboFHR.setItems(new String[]{"F00","F06","F12","F18"});
        comboFHR.select(VaaInfo.getLayerIdx()-1);//0);
        comboFHR.addListener(SWT.Selection, new Listener(){
        	public void handleEvent(Event e){
        		if( isTxtType() )
        			type = getNotSeenTxt();
        	}
        });
        
        Label lblDummy = new Label(top,SWT.LEFT);	lblDummy.setText("   ");
        
        Label lblFL = new Label(top,SWT.LEFT);
        lblFL.setText("FL      ");
        
        txtFL = new Text(top, SWT.BORDER);
        txtFL.setText("SFC          ");
        txtFL.addListener(SWT.Modify, new Listener(){
        	public void handleEvent(Event e){
        		if( isTxtType() )
        			type = getDisplayTxt();
        	}
        });
        
        Label lblFLHyphen = new Label(top, SWT.LEFT);
        lblFLHyphen.setText("-");
        
        txtFL2 = new Text(top, SWT.BORDER);
        txtFL2.addListener(SWT.Modify, new Listener(){
        	public void handleEvent(Event e){
        		if( isTxtType() )
        			type = getDisplayTxt();
        	}
        });
        
        Label lblDir = new Label(top, SWT.LEFT);
        lblDir.setText("DIR ");
        
        txtDir = new Text(top, SWT.BORDER);
        txtDir.addListener(SWT.Modify, new Listener(){
        	public void handleEvent(Event e){
        		if( isTxtType() )
        			type = getDisplayTxt();
        	}
        });
        
        Label lblSpd = new Label(top, SWT.LEFT);
        lblSpd.setText("SPD ");
        
        txtSpd = new Text(top, SWT.BORDER);
        //txtSpd.setText("0");
        txtSpd.addListener(SWT.Modify, new Listener(){
        	public void handleEvent(Event e){
        		if( isTxtType() )
        			type = getDisplayTxt();
        	}
        });
        
        Label lblKts = new Label(top, SWT.BORDER);
        lblKts.setText(" kts");
	}
	
	public String getLineType(){
		 return type;
	}
	
	/**
	 * getter for ESOL width of the cloud.
	 * @return String: width in String, default 25.
	 */
	public String getCloudWidth(){
		String dWidth = "25";
		
		if(txtWidth==null || txtWidth.isDisposed()) 
			return dWidth;
		
		String w = txtWidth.getText();
		double wi = 0;
		
		try{
			wi = Double.parseDouble(w);
		}catch(Exception e){
			txtWidth.setText(dWidth);
		}
		
		if(w==null || w.length()==0 || wi < 10 ){
			txtWidth.setText(dWidth);			
		}
		
		return txtWidth.getText(); 
	}
	
	//public void setWidth(){	}
	
	/**
	 * colors of the cloud.
	 */
	public Color[] getColors(){		
  		
	    Color[] colors = new Color[2];          
        colors[0] =new java.awt.Color( cs.getColorValue().red,
				cs.getColorValue().green, cs.getColorValue().blue );          
        colors[1] = Color.green;

        return colors;  	
	}

	public void setColor( Color clr ){
		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
	}
	
	/**
	 * helper method for getting displayed text
	 * @return String: text to be displayed
	 */
	private String getDisplayTxt(){
		if(btnNotSeen.getSelection())
			return getNotSeenTxt();
		else if(btnFcst.getSelection())
			return getFcstTxt();
		else 
			return "";
	}
	
	/**
	 * helper method for getting NotSeen display text.
	 * @return String: text for NotSeen text.
	 */
	private String getNotSeenTxt(){
		StringBuilder sb = new StringBuilder(VaaInfo.TYPE_TEXT).append(VaaInfo.SEPERATER);//"Text:::");
	
		if("F00".equals( this.comboFHR.getText().trim()) ){
			sb.append(VaaInfo.VA_NOT_IDENTIFIABLE).append(SEPERATER);
		}
		
		sb.append("WINDS ");			
		
		String sFL=txtFL.getText().trim(), sFL2=txtFL2.getText().trim();
		if("SFC".equals(sFL) ){
			sb.append("SFC");
		}else{
			sb.append("FL").append(sFL);
		}
		sb.append("/FL").append(sFL2==null ? "" : sFL2);
		
		sb.append(" ");
		
		String sDir=txtDir.getText().trim(), sSpd=txtSpd.getText().trim();
		sb.append(sDir==null ? "" : sDir.toUpperCase());
		sb.append("/").append(sSpd==null ? "" : sSpd);
		
		return sb.append("KT").toString();
	}
	
	/**
	 * helper method for getting others-fcst displayed text.
	 * @return String: text for others-fcst text.
	 */
	private String getFcstTxt(){
		Map<String,String> map = new HashMap<String,String>();
		
		String[] 
		       fcstNames = VaaInfo.VAA_INFO_OTHERSFCST_MAP.get(VaaInfo.OTHERSFCST_DIALOG),
		       fcstValues = VaaInfo.VAA_INFO_OTHERSFCST_MAP.get(VaaInfo.OTHERSFCST_DISPLAY);
		
		for(int i=0; i<fcstNames.length; i++){
			map.put(fcstNames[i], fcstValues[i]);
		}
		
		StringBuilder sb = new StringBuilder(VaaInfo.TYPE_TEXT).append(VaaInfo.SEPERATER);
		
		String s = comboFcst.getText().trim(), sFL=txtFL.getText().trim(), sFL2=txtFL2.getText().trim();			
		String fl = ("SFC".equals(sFL) ? "SFC" : "FL"+sFL)+ "/FL" + sFL2+" ";			
		
		if("ASH DSIPTG".equals(s)){
			String word = map.get(s);
			int index = word.indexOf("ASH");	
			
			return sb.append(fl).append(word.substring(index)).toString();
		}else{
			if ( s.contains("WITH FL")){
				sb.append(fl);
				return sb.append(map.get(s).substring(7)).toString();
			}
			else return sb.append(map.get(s)).toString();
		}
					
	}

	
	public void setSigmet(DrawableElement sigmet){
		vaCloud = (AbstractSigmet)sigmet;
		
		//VaaInfo.addCloud2Layer(vaCloud);		
	}
	
	/**
	 * helper method for "Apply" button.
	 * it creates a new element as the copy of the
	 * old one and replace the old one in drawingLayer
	 */
	private void applyPressed(){
		
//copyEditableAttrToAbstractSigmet(this.vaCloud);//2010-02-17	

		ArrayList<AbstractDrawableComponent> adcList = null;
		ArrayList<AbstractDrawableComponent> newList = new ArrayList<AbstractDrawableComponent>() ;
		if ( drawingLayer != null ) {
			adcList = (ArrayList<AbstractDrawableComponent>) drawingLayer.getAllSelected();
		}		
		if ( adcList != null && !adcList.isEmpty() ){			
			for ( AbstractDrawableComponent adc : adcList){
				Sigmet el = (Sigmet)adc.getPrimaryDE();	
				if ( el != null ){					
					Sigmet newEl = (Sigmet)el.copy();
			
					attrUpdate();//neccesary?
					
					copyEditableAttrToAbstractSigmet(newEl);
					//this.setAbstractSigmet(newEl);					
					newList.add(newEl);
				}
			}			
			ArrayList<AbstractDrawableComponent> oldList = new ArrayList<AbstractDrawableComponent>(adcList);
			drawingLayer.replaceElements(oldList, newList);
		}		
		drawingLayer.removeSelected();
		for ( AbstractDrawableComponent adc : newList ){
			drawingLayer.addSelected(adc);
		}		
		if ( mapEditor != null ) 	mapEditor.refresh();
		
	}
	
	/**
	 * helper method copy attributes of the dialog to the element.
	 * @param AbstractSigmet: the element to accept attributes.
	 */
	private void copyEditableAttrToAbstractSigmet(AbstractSigmet ba){
		
		ba.setColors(getColors());
		((Sigmet)ba).setEditableAttrFreeText(getFhrFlDirSpdTxt());
		/*
        ba.setEditableAttrArea(this.getEditableAttrArea());
        ba.setEditableAttrFromLine(this.getEditableAttrFromLine());
        ba.setEditableAttrId(this.getEditableAttrId());
       
        ba.setEditableAttrSeqNum(this.getEditableAttrSequence());
        */
        ba.setType(getLineType()); 
        ba.setWidth(Double.parseDouble(getCloudWidth()));
        
        copiedToSigmet = true;
		
	}
	
	private void attrUpdate(){
		//this.setEditableAttrId(this.comboID.getText());
		//this.setEditableAttrSequence(this.spiSeq.getText());
	}
	
	private void setBtnObservers(){
		/*
		rBtnObs.addBtn(btnArea);
		rBtnObs.addBtn(btnLine);
		rBtnObs.addBtn(btnFcst);
		rBtnObs.addBtn(btnNotSeen);
		*/
	}
	
	private void init(){
		
		setBtnObservers();
		
	}
	
	/**
	 * Boolean parameters can be used as a 3-state flag
	 */	
	private void radioBtnEnable(Boolean txtW, Boolean comboF){
		if(txtW != null ){
			txtWidth.setEnabled(txtW);
			txtWidth.setText("25");
		}
		if(comboF != null)
			comboFcst.setEnabled(comboF);
	}
	
	/**
	 * use editableAttrFromLine for FHR,FL,DIR,and SPD info
	 */	
	public String getFhrFlDirSpdTxt(){
		StringBuilder sb = new StringBuilder();
		
		if( ! comboFHR.isDisposed() ){
			String 	fhr=comboFHR.getText().trim(), 
					fl=txtFL.getText(), 
					fl2=txtFL2.getText(),
					dir=txtDir.getText(), 
					spd=txtSpd.getText();
			
			sb.append(fhr).append(SEPERATER);
			sb.append(fl==null ||fl.length()==0 ?  " ":fl.trim().toUpperCase()).append(SEPERATER);
			sb.append(fl2==null||fl2.length()==0 ? " ":fl2.trim().toUpperCase()).append(SEPERATER);
			sb.append(dir==null||dir.length()==0 ? " ":dir.trim().toUpperCase()).append(SEPERATER);
			sb.append(spd==null||spd.length()==0 ? " ":spd.trim().toUpperCase());
		}
		
		return sb.toString();
	}
	
	/**
	 * set the dialog Text fields with element's fields values.
	 * @param AbstractSigmet: element with the attributes for this dialog.
	 */
	private void setFhrFlDirSpdTxt(AbstractSigmet aSig){
		String fromLine = ((Sigmet)aSig).getEditableAttrFreeText();
		if(fromLine == null ) return;
		
		String[] wArray = fromLine.split(SEPERATER);
		
		if(wArray.length > 0) comboFHR.setText(wArray[0]); 
		if(wArray.length > 1) txtFL.setText(wArray[1]); 
		if(wArray.length > 2) txtFL2.setText(wArray[2]); 
		if(wArray.length > 3) txtDir.setText(wArray[3]); 
		if(wArray.length > 4) txtSpd.setText(wArray[4]); 
		
	}
	
	/**
	 * helper method telling if a Text type is chosen.
	 * @return
	 */
	private boolean isTxtType(){
		return btnNotSeen.getSelection() || btnFcst.getSelection();
	}

	public Volcano getVolcano() {
		return volcano;
	}

	public void setVolcano(Volcano volcano) {
		this.volcano = volcano;
	}
	
	public RGB getFHRColor(int fhr){
		
		return new RGB(255,255,255);
	}
	
	/**
	 * using Product Center's pre-set color for each Layer.
	 * @return RGB: color for current active Layer.
	 */
	public RGB getLayerColor(){
		Color c = PgenSession.getInstance().getPgenResource().getActiveLayer().getColor();
		return new RGB(c.getRed(),c.getGreen(),c.getBlue());
	}
	
	@Override
	public void setMouseHandlerName(String mhName){
		AttrDlg.mouseHandlerName = mhName;
		mouseHandlerName = mhName;
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
	public double getWidth() {
		return Double.parseDouble(this.getCloudWidth());
	}
	
}

