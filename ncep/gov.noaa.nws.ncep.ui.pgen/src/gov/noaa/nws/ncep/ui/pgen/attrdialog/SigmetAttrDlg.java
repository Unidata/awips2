/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.SigmetAttrDlg
 * 
 * September 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import static gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo.AREA_MAP;
import static gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo.ID_MAP;
import static gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo.SIGMET_TYPES;
import static gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo.VOL_NAME_BUCKET_ARRAY;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.sigmet.ISigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo;
import gov.noaa.nws.ncep.viz.common.SnapUtil;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import java.awt.Color;
import java.beans.PropertyDescriptor;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.apache.commons.beanutils.BeanUtils;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Singleton attribute dialog for sigmet.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09		160			Gang Zhang 	Initial Creation. 
 * 03/10        231         Archana     Altered the dialog for sigmet 
 *                                      to display only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix.
 * 03/10		#223		M.Laryukhin	Refactored getVOR method to be used with gfa.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 07/11        #450        G. Hull     NcPathManager
 * 12/11		#526		B. Yin		Close dialog after text is saved.
 * 02/12        #597        S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 03/12        #612,#613   S. Gurung   Accept phenom Lat/Lon and convert them to prepended format.
 * 										Change KZOA to KZAK.
 * 03/12        #611        S. Gurung   Fixed ability to change SIGMET type (from Area to Line/Isolated and back and forth)
 * 03/12        #676        Q. Zhou     Added Issue Office dropdown list.
 * 08/12        #612		S. Gurung   Fixed issue related to conversion of phenom Lat/Lon to prepended format
 *  </pre>
 * 
 * @author	gzhang
 */

public class SigmetAttrDlg  extends AttrDlg implements ISigmet {
	
	private static SigmetAttrDlg INSTANCE = null;
	private static String mouseHandlerName = null;
	
	private HashMap<String, Object> attr = new HashMap<String, Object>( );
	
	public static final String AREA = "Area", LINE = "Line", ISOLATED = "Isolated";

	
	private String lineType = AREA; //default
	
	private String origLineType = lineType; 
	
	private static final String WIDTH = "10.00";
	private String widthStr = WIDTH;//default, nautical miles
	
	private static final String[] LINE_SIDES = new String[]{"ESOL","NOF","SOF","EOF","WOF"};	
	private String sideOfLine = LINE_SIDES[0]; //default
	
	private gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement sigmet = null;
	
    protected Composite top = null;
    protected ColorButtonSelector cs = null;
    
    private boolean withExpandedArea = false, copiedToSigmet = false, comboPhenomCalled = false;;
    private  boolean tropCycFlag = false;
    
    private Text txtInfo;
    private Control detailsArea;
	private Point cachedWindowSize;
	private String latLonFormatFlagAndText;//_fmtflag in C without text
	
	
	private String editableAttrArea;	//MWO
	private String editableAttrIssueOffice;	//IssueOffice
	private String editableAttrStatus;	//0=new,1=amend,2=cancel...
	private String editableAttrId;		//alfa,brave...
	private String editableAttrSeqNum;	//1,2,...,300
	private String editableAttrStartTime;	//start valid
	private String editableAttrEndTime;	//end valid
	private String editableAttrRemarks;	
	private String editableAttrPhenom;
	private String editableAttrPhenom2;
	private String editableAttrPhenomName;
	private String editableAttrPhenomLat;
	private String editableAttrPhenomLon;
	private String editableAttrPhenomPressure;
	private String editableAttrPhenomMaxWind;
	private String editableAttrFreeText;
	private String editableAttrTrend;
	private String editableAttrMovement;
	private String editableAttrPhenomSpeed;
	private String editableAttrPhenomDirection;
	private String editableAttrLevel;
	private String editableAttrLevelInfo1;
	private String editableAttrLevelInfo2;
	private String editableAttrLevelText1;
	private String editableAttrLevelText2;
	private String editableAttrFromLine;
	private String editableAttrFir;
	
	private HashMap<String, Control> attrControlMap = new HashMap<String,Control>();
	private HashMap<String, Button[] > attrButtonMap = new HashMap<String,Button[]>();
	private HashMap<Control,Control[]> controlEnablerMap = new HashMap<Control,Control[]>();		
	
	protected SigmetAttrDlg(Shell parShell) throws VizException {
		super(parShell);		
	}
	
	public static SigmetAttrDlg getInstance( Shell parShell){		
		if ( INSTANCE == null ){					
			try {
				INSTANCE = new SigmetAttrDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}			
		}		
		return INSTANCE;
	}

	private void resetText(String coorsLatLon, Text txtInfo){
		
		StringBuilder sb = new StringBuilder();
		String[] strings = coorsLatLon.split(SigmetInfo.LINE_SEPERATER);
    	for(int i=0; i<strings.length; i++){
    		if(i != 0 && (i%6 == 0)) sb.append("\n");
    		if((i==strings.length-1)
    		    &&("New".equals(strings[i])||"Old".equals(strings[i])||"VOR".equals(strings[i]))){    			
    		}else{
    			sb.append(strings[i]+"  ");
    		}
    	}
    	txtInfo.setText(sb.toString());
		
	}
	
	public void okPressed() {
		ArrayList<AbstractDrawableComponent> adcList = null;
		ArrayList<AbstractDrawableComponent> newList = new ArrayList<AbstractDrawableComponent>() ;

		//get the list of selected tracks
		if ( drawingLayer != null ) {
			adcList = (ArrayList<AbstractDrawableComponent>) drawingLayer.getAllSelected();
		}
		
		if ( adcList != null && !adcList.isEmpty() ){
			
			//loop through the list and update attributes
			for ( AbstractDrawableComponent adc : adcList){

				Sigmet el = (Sigmet)adc.getPrimaryDE();	

				if ( el != null ){
					// Create a copy of the currently selected element
					Sigmet newEl = (Sigmet)el.copy();
					
					// Update the new Element with these current attributes
					copyEditableAttrToSigmet(newEl);//20100115 newEl.update(this);
					
					// Change type and update From line
					newEl = convertType(newEl);
					
					newList.add(newEl);

				}
			}
			
			ArrayList<AbstractDrawableComponent> oldList = new ArrayList<AbstractDrawableComponent>(adcList);
			drawingLayer.replaceElements(oldList, newList);
		}
		
		// set the new elements as selected.
		drawingLayer.removeSelected();
		for ( AbstractDrawableComponent adc : newList ){
			drawingLayer.addSelected(adc);
		}
		
		if ( mapEditor != null ) {
			mapEditor.refresh();
		}
		
	}
	
	public void setMouseHandlerName(String mhName){
		this.mouseHandlerName = mhName;
	}

	/**
	 * Return color from the color picker of the dialog
	 */
	public Color[] getColors(){		
		  // IAttribute requires to return an array of colors
		  // Only the first color is used at this time.		
	      Color[] colors = new Color[2];          
          colors[0] =new java.awt.Color( cs.getColorValue().red,
				cs.getColorValue().green, cs.getColorValue().blue );          
          colors[1] = Color.green;

          return colors;  	
	}

	private void setColor( Color clr ){
		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
	}
	
	public String getLineType(){
		return this.lineType;
	}
	
	public void setLineType(String lType){
		setOrigLineType(getLineType());
		this.lineType = lType;
	}	
	
	public String getOrigLineType(){
		return this.origLineType;
	}
	
	public void setOrigLineType(String lType){
		this.origLineType = lType;
	}
	
	public String getSideOfLine(){
		return this.sideOfLine;
	}	
	
	public void setSideOfLine(String lineSideString){
		this.sideOfLine = lineSideString;
	}
	
	public double getWidth(){	
		return Double.parseDouble(this.widthStr);
	}
	
	public  String getWidthStr(){	
		return this.widthStr;
	}
		
	public void setWidthStr(String widthString){
		this.widthStr = widthString;
	}
	
	private double stringToDouble(String s){
		double dValue = 10.00; //default;
		try{ 
			dValue = Double.parseDouble(s);
			dValue = (dValue < 00.00 || dValue > 5000.00) ? 10.00 : dValue;
		}catch(NumberFormatException e){
			
		}finally{
			return dValue;
		}
	}

	public void setAttrForDlg( IAttribute attr ){		
		Color clr = attr.getColors()[0];
		if ( clr != null ) this.setColor(clr);
	
/*		float lw =  attr.getLineWidth();
		if ( lw > 0 ) this.setLineWidth(lw);

*/		
	}	
	
    public int getSmoothFactor() {
    	return 0; 
    }
    
    public String getLinePattern() {
    	return "Solid Line"; 
    }
    
    public FillPattern getFillPattern() {
    	return FillPattern.FILL_PATTERN_0; 
    }
    
    public boolean isClosed() {
    	return false; 
    }
    
    public Boolean isFilled() {
    	return false; 
    }
 	
    public float getLineWidth() {
    	return (float)1.0; 
    }
    
    public double getSizeScale() {
    	return 2; 
    }
    
    //public com.raytheon.viz.ui.glmap.GLMapEditor getGLMapEditor(){    	return mapEditor;    }
    
    @Override
    public void createButtonsForButtonBar(Composite parent) { // from Dialog

    	if("Pgen Select".equals(mouseHandlerName) || withExpandedArea ){
    		createButton(parent, 20091020,"Save",false);//id creation_date
    		createButton(parent, 20091021,"Apply",false);
    		
    		this.getButton(20091020).setEnabled(true);
    		this.getButton(20091020).addListener(SWT.Selection, new Listener(){
    			public void handleEvent(Event e){
    				//20091123 Dave requires to apply with save
    				Sigmet sig = (Sigmet) SigmetAttrDlg.this.getSigmet();    				
    				okPressed();//copyEditableAttrToSigmet(sig);
    				
    				SigmetAttrDlgSaveMsgDlg md = null;    				
    				try{ 
    					md = new SigmetAttrDlgSaveMsgDlg( getShell() );
    				}catch(Exception ee){
    					System.out.println(ee.getMessage());
    				}
    				if(md != null) 
    					md.open();
    			}
    		});
    		
    		this.getButton(20091021).setEnabled(true);
    		this.getButton(20091021).addListener(SWT.Selection, new Listener(){
    			public void handleEvent(Event e){
    				//SigmetAttrDlg was set a DrawableElement in PgenSelectingTool's handleMouseDown( ) @line362
    				Sigmet sig = (Sigmet) SigmetAttrDlg.this.getSigmet();    				
    				okPressed();//copyEditableAttrToSigmet(sig);
    				
    			}
    		});
    		
    	}else{
    		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,	true);
    	}
		createButton(parent, IDialogConstants.CANCEL_ID,IDialogConstants.CANCEL_LABEL, false);
		setMouseHandlerName(null);//reset ???
		withExpandedArea = false;
		
		
		
	}
    
    @Override
	public void enableButtons(){ // from AttrDlg
		
		this.getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
  		//this.getButton(IDialogConstants.OK_ID).setEnabled(true);

  		
	}

    
    private Control createDetailsArea(Composite parent) {
    	Composite top5 = (Composite) super.createDialogArea(parent);
GridData gdText = new GridData(); 	gdText.widthHint=66;    	
 	   GridLayout mainLayout5 = new GridLayout(8,false);
	        mainLayout5.marginHeight = 3;
	        mainLayout5.marginWidth = 3;
	        top5.setLayout(mainLayout5);
	        
	        Group top_3 = new Group(top5,SWT.LEFT);
	        top_3.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
	        top_3.setLayout(new GridLayout(8,false));
	        		        
	        final Button btnNewUpdate = new Button(top_3,SWT.RADIO);	
	        btnNewUpdate.setText("New/Update");
	        btnNewUpdate.setSelection(true);	this.setEditableAttrStatus("0");
	        btnNewUpdate.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,2,1));
	        btnNewUpdate.addListener(SWT.Selection, new Listener(){
        		public void handleEvent(Event e){
        			SigmetAttrDlg.this.setEditableAttrStatus("0");//0=new
        		}
        	});

	        
	        final Button btnAmend = new Button(top_3,SWT.RADIO);		
	        btnAmend.setText("Amend");
	        btnAmend.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,2,1));
	        btnAmend.addListener(SWT.Selection, new Listener(){
        		public void handleEvent(Event e){
        			SigmetAttrDlg.this.setEditableAttrStatus("1");//1=amend
        		}
        	});
	        
	        final Button btnCancel = new Button(top_3, SWT.RADIO);	
	        btnCancel.setText("Cancel");
	        btnCancel.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,4,1));	     
	        btnCancel.addListener(SWT.Selection, new Listener(){
        		public void handleEvent(Event e){
        			SigmetAttrDlg.this.setEditableAttrStatus("2");//1=amend
        		}
        	});
	        attrButtonMap.put("editableAttrStatus", new Button[]{btnNewUpdate, btnAmend, btnCancel});
	        
	        Label lblValidFrom = new Label(top_3,SWT.LEFT);
	        lblValidFrom.setText("Valid from:");
	        
	        final Text txtValidFrom = new Text(top_3, SWT.LEFT | SWT.BORDER);
	        attrControlMap.put("editableAttrStartTime", txtValidFrom);     
	        String startTime = editableAttrStartTime==null||this.editableAttrStartTime.equals("") 
	        					? this.getTimeStringPlusHourInHMS(0)
	        					: this.editableAttrStartTime;
	        txtValidFrom.setText( startTime);//getTimeStringPlusHourInHMS(0) );
	        setEditableAttrStartTime(txtValidFrom.getText());
	        //((Sigmet)this.getSigmet()).setEditableAttrStartTime(txtValidFrom.getText());
	        
	        txtValidFrom.addListener (SWT.Verify, new Listener () {
	    		public void handleEvent (Event e) {	
	    			e.doit = validateHMSInput(e,txtValidFrom);	   			
	    			if ( !  e.doit ) return;
	    			SigmetAttrDlg.this.setEditableAttrStartTime(txtValidFrom.getText());
	    		}
	    	});
	        txtValidFrom.addFocusListener(new org.eclipse.swt.events.FocusListener(){
	        	public void focusGained(org.eclipse.swt.events.FocusEvent e){     	}
	        	
	        	public void focusLost(org.eclipse.swt.events.FocusEvent e){
	        		String timeString = txtValidFrom.getText();
	        		if( timeString == null 
	        				|| timeString.length() != 6
	        				|| ! validateTimeStringInHMS(timeString)){
	        			txtValidFrom.setText(getTimeStringPlusHourInHMS(0));
	        		}
	        		SigmetAttrDlg.this.setEditableAttrStartTime(txtValidFrom.getText());
	        	}
	        });
	        txtValidFrom.addKeyListener(new org.eclipse.swt.events.KeyListener(){
	        	public void keyPressed(org.eclipse.swt.events.KeyEvent e){
	        		if(e.keyCode == SWT.KEYPAD_CR || e.keyCode == SWT.CR){
	        			String timeString = txtValidFrom.getText();
		        		if( timeString == null 
		        				|| timeString.length() != 6
		        				|| ! validateTimeStringInHMS(timeString)){
		        			txtValidFrom.setText(getTimeStringPlusHourInHMS(0));
		        		}
	        		}
	        		SigmetAttrDlg.this.setEditableAttrStartTime(txtValidFrom.getText());	        		
	        	}
	        	public void keyReleased(org.eclipse.swt.events.KeyEvent e){   	}
	        });
	        
	        Label lblTo = new Label(top_3,SWT.LEFT);
	        lblTo.setText("To:");
	        
	        final Text txtTo = new Text(top_3, SWT.LEFT | SWT.BORDER);
	        attrControlMap.put("editableAttrEndTime", txtTo);	
	        String endTime = editableAttrEndTime==null||editableAttrEndTime.equals("")
	        					? this.getTimeStringPlusHourInHMS(4)
	        					: this.editableAttrEndTime;
	        txtTo.setText( endTime);//getTimeStringPlusHourInHMS(4) );
	        setEditableAttrEndTime(txtTo.getText());
	        //((Sigmet)this.getSigmet()).setEditableAttrEndTime(txtTo.getText());
	        
	        txtTo.addListener (SWT.Verify, new Listener () {
	    		public void handleEvent (Event e) {	
	    			e.doit = validateHMSInput(e,txtTo);	   			
	    			if ( !  e.doit ) return;
	    			SigmetAttrDlg.this.setEditableAttrEndTime(txtTo.getText());
	    		}
	    	});	        
	        txtTo.addFocusListener(new org.eclipse.swt.events.FocusListener(){
	        	public void focusGained(org.eclipse.swt.events.FocusEvent e){     	}
	        	
	        	public void focusLost(org.eclipse.swt.events.FocusEvent e){
	        		String timeString = txtValidFrom.getText();
	        		if( timeString == null 
	        				|| timeString.length() != 6
	        				|| ! validateTimeStringInHMS(timeString)){
	        			txtValidFrom.setText(getTimeStringPlusHourInHMS(4));
	        		}
	        		SigmetAttrDlg.this.setEditableAttrEndTime(txtTo.getText());
	        	}
	        });
	        txtTo.addKeyListener(new org.eclipse.swt.events.KeyListener(){
	        	public void keyPressed(org.eclipse.swt.events.KeyEvent e){
	        		if(e.keyCode == SWT.KEYPAD_CR || e.keyCode == SWT.CR){
	        			String timeString = txtValidFrom.getText();
		        		if( timeString == null 
		        				|| timeString.length() != 6
		        				|| ! validateTimeStringInHMS(timeString)){
		        			txtValidFrom.setText(getTimeStringPlusHourInHMS(4));
		        		}
	        		}
	        		SigmetAttrDlg.this.setEditableAttrEndTime(txtTo.getText());	        		
	        	}
	        	public void keyReleased(org.eclipse.swt.events.KeyEvent e){   	}
	        });
	        
	        Label lblStartPlus = new Label(top_3,SWT.LEFT);
	        lblStartPlus.setText("Start plus:");
	        final Button btnStartPlus4hrs = new Button(top_3,SWT.PUSH);
	        btnStartPlus4hrs.setText("4hrs");
	        btnStartPlus4hrs.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		if(txtValidFrom.getText().length() != 6) return;	    	        
	    	        txtTo.setText( convertTimeStringPlusHourInHMS(txtValidFrom.getText(),4,true) );
	        	}
	        });
	        
	        final Button btnStartPlus6hrs = new Button(top_3,SWT.PUSH);
	        btnStartPlus6hrs.setText("6hrs");
	        btnStartPlus6hrs.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		if(txtValidFrom.getText().length() != 6) return;	    	       
	    	        txtTo.setText( convertTimeStringPlusHourInHMS(txtValidFrom.getText(),6,true) );
	        	}
	        });
	        
	        
	        Label lblDummy5_1 = new Label(top_3,SWT.LEFT);	        
	        
	        
	        Label lblPhenom = new Label(top_3,SWT.LEFT);
	        lblPhenom.setText("Phenom:");
	        
	        	        
	        final Combo comboPhenom = new Combo(top_3,SWT.LEFT | SWT.READ_ONLY);
	        attrControlMap.put("editableAttrPhenom", comboPhenom);
	        comboPhenom.setItems(/*SigmetInfo.PHEN_MAP.get*/getPhenomenons(SigmetInfo.getSigmetTypeString(pgenType)));
	        setControl(comboPhenom,"editableAttrPhenom");//comboPhenom.select(0);//phenomText);
	        comboPhenom.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,true,7,1));
	        //this.setEditableAttrPhenom(comboPhenom.getText());	        
	       
	        comboPhenom.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrPhenom(comboPhenom.getText());
	        		withExpandedArea = true;
	        		tropCycFlag = "TROPICAL_CYCLONE".equals(editableAttrPhenom);
	        		copyEditableAttrToSigmet((Sigmet)SigmetAttrDlg.this.getSigmet());
	        	}
	        });

//++++++++++++++++++++++++++++++++++++++++++
	        
	        if("TROPICAL_CYCLONE".equals(editableAttrPhenom) || "VOLCANIC_ASH".equals(editableAttrPhenom)){
	        	Group top_phenom = new Group(top5,SWT.LEFT);
	        	top_phenom.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
	        	top_phenom.setLayout(new GridLayout(8,false));
		        
		        Shell shell = getShell();
		        Label lblSEPhenomName = new Label(top_phenom,SWT.LEFT);
		        lblSEPhenomName.setText("Select / Enter\nPhenom Name: ");
		        //lblSEPhenomName.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,2,1));
		        
		        final Text  txtSEPhenomName = new Text(top_phenom,SWT.LEFT | SWT.BORDER);
		        attrControlMap.put("editableAttrPhenomName", txtSEPhenomName);
		        txtSEPhenomName.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,6,1));
		        txtSEPhenomName.addListener(SWT.Modify, new Listener(){
		        	public void handleEvent(Event e){
		        		SigmetAttrDlg.this.setEditableAttrPhenomName(txtSEPhenomName.getText());
		        	}
		        });
		        
		        final ToolBar tb = new ToolBar(top_phenom, SWT.HORIZONTAL);
		    	final ToolItem ti = new ToolItem(tb, SWT.DROP_DOWN);
		    	if("TROPICAL_CYCLONE".equals(editableAttrPhenom)) ti.setEnabled(false);
		    	
		    	final Menu mu = new Menu(shell, SWT.POP_UP);
		    	
		    	for(int i=0; i<VOL_NAME_BUCKET_ARRAY.length; i++){	
					if(i==0){// first option is entering name
						MenuItem mi1 = new MenuItem(mu, SWT.PUSH);
			    		mi1.setText(SigmetInfo.VOL_NAME_BUCKET_ARRAY[i]);
					}else{
			    		MenuItem mi1 = new MenuItem(mu, SWT.CASCADE);
			    		mi1.setText(SigmetInfo.VOL_NAME_BUCKET_ARRAY[i]);//"test1");
			    		Menu mi1Menu = new Menu(shell,SWT.DROP_DOWN);
			    		mi1.setMenu(mi1Menu);
			    	
					    List<String> list = SigmetInfo.VOLCANO_BUCKET_MAP.get(SigmetInfo.VOL_NAME_BUCKET_ARRAY[i]);	
					    int size = list.size();
					    for(int j=0; j<size; j++){
					    	final MenuItem mi1MenuMi1 = new MenuItem(mi1Menu,SWT.PUSH);
					    	mi1MenuMi1.setText(list.get(j));
					    	mi1MenuMi1.addListener(SWT.Selection, new Listener(){
					    		public void handleEvent(Event e){
					    			txtSEPhenomName.setText(mi1MenuMi1.getText());
					    		}
					    	});	
					    }
			    	}
		    	}
		    	
		    	ti.addListener(SWT.Selection, new Listener() {
		        	/* Main button clicked:  Pop up the menu showing all the symbols. */
		        	public void handleEvent(Event event)
		        	{
		        		Rectangle bounds = ti.getBounds();
		        		Point point = tb.toDisplay(bounds.x, bounds.y + bounds.height);
		        		mu.setLocation(point);
		        		mu.setVisible(true);
		        	}
		        });

		    	Label lblPheLat = new Label(top_phenom,SWT.LEFT);
		    	lblPheLat.setText("Phenom\nLat: ");
		    	final Text  txtPheLat = new Text(top_phenom,SWT.LEFT | SWT.BORDER);
		    	attrControlMap.put("editableAttrPhenomLat", txtPheLat);
		    	
		    	txtPheLat.addListener(SWT.Modify, new Listener(){
		    		public void handleEvent(Event e){
		    			String phenomLat = getPhenomLatLon(txtPheLat.getText().trim(), true);
		    			if(!"".equals(phenomLat))
		    				SigmetAttrDlg.this.setEditableAttrPhenomLat(phenomLat);
		    			else
		    				SigmetAttrDlg.this.setEditableAttrPhenomLat(null);
		    		}
		    	});		
		    	txtPheLat.addFocusListener(new FocusListener() {	
						@Override
						public void focusGained(FocusEvent e) {							
						}

						@Override
						public void focusLost(FocusEvent e) {
							if (SigmetAttrDlg.this.getEditableAttrPhenomLat() != null)
								txtPheLat.setText(SigmetAttrDlg.this.getEditableAttrPhenomLat());
							else
								txtPheLat.setText("???");
						}
		    	});	
		    	
		    	txtPheLat.setLayoutData(gdText);
		    	
		    	Label lblPheLon = new Label(top_phenom,SWT.LEFT);
		    	lblPheLon.setText("Phenom\nLon: ");
		    	final Text  txtPheLon = new Text(top_phenom,SWT.LEFT | SWT.BORDER);
		    	attrControlMap.put("editableAttrPhenomLon", txtPheLon);
		    	
		    	txtPheLon.addListener(SWT.Modify, new Listener(){
		    		public void handleEvent(Event e){
		    			String phenomLon = getPhenomLatLon(txtPheLon.getText().trim(), false);		    		
		    			if(!"".equals(phenomLon)) 
		    				SigmetAttrDlg.this.setEditableAttrPhenomLon(phenomLon);	
		    			else
		    				SigmetAttrDlg.this.setEditableAttrPhenomLon(null);	
		    		}
		    	});		    	
		    	txtPheLon.addFocusListener(new FocusListener() {	
					@Override
					public void focusGained(FocusEvent e) {						
					}

					@Override
					public void focusLost(FocusEvent e) {
						if (SigmetAttrDlg.this.getEditableAttrPhenomLon() != null)
							txtPheLon.setText(SigmetAttrDlg.this.getEditableAttrPhenomLon());
						else
							txtPheLon.setText("???");
					}
		    	});
		    			    	
		    	txtPheLon.setLayoutData(gdText);
		    	
		    	Label lblPressure = new Label(top_phenom,SWT.LEFT);
		    	lblPressure.setEnabled(tropCycFlag);
		    	lblPressure.setText("Pressure\nHPA: ");
		    	final Text  txtPressure = new Text(top_phenom,SWT.LEFT | SWT.BORDER); 
		    	txtPressure.setEnabled(tropCycFlag);
		    	
		    	txtPressure.addListener(SWT.Modify, new Listener(){ 
		        	public void handleEvent(Event e){
		        		if( validateNumInput(txtPressure.getText()) ) 
		        			SigmetAttrDlg.this.setEditableAttrPhenomPressure(txtPressure.getText());//else txtPressure.setText("");
		        	}
		        });
		    	txtPressure.setLayoutData(gdText);
		    	attrControlMap.put("editableAttrPhenomPressure", txtPressure);	
		    	
		    	Label lblMaxWinds = new Label(top_phenom,SWT.LEFT);
		    	lblMaxWinds.setEnabled(tropCycFlag);
		    	lblMaxWinds.setText("Max\nWinds: ");
		    	final Text  txtMaxWinds = new Text(top_phenom,SWT.LEFT | SWT.BORDER);
		    	txtMaxWinds.setEnabled(tropCycFlag);
		    	this.setEditableAttrPhenomMaxWind(txtMaxWinds.getText());
		    	
		    	txtMaxWinds.addListener(SWT.Modify, new Listener(){ 
		        	public void handleEvent(Event e){
		        		if(validateNumInput(txtMaxWinds.getText() ) )
		        			SigmetAttrDlg.this.setEditableAttrPhenomMaxWind(txtMaxWinds.getText());//else txtMaxWinds.setText("");
		        	}
		        });
		    	txtMaxWinds.setLayoutData(gdText);
		    	attrControlMap.put("editableAttrPhenomMaxWind", txtMaxWinds );		    	
		        
	        }
//++++++++++++++++++++++++++++++++++++++++++	        
	        
	        //------------------------ Phenom Attributes
	        
	        final Group top_4 = new Group(top5,SWT.LEFT);
	        top_4.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
	        top_4.setLayout(new GridLayout(8,false));	        
	        top_4.setText("".equals(editableAttrPhenom) ? comboPhenom.getText():editableAttrPhenom+" Attributes: ");
	        
	        comboPhenom.addListener(SWT.Selection, new Listener(){
				public void handleEvent(Event event){
					editableAttrPhenom = comboPhenom.getText().trim();
					comboPhenomCalled = true;
					top_4.setText(editableAttrPhenom+" Attributes: ");
					copyEditableAttrToSigmet((Sigmet)SigmetAttrDlg.this.getSigmet());
					//if("TROPICAL_CYCLONE".equals(editableAttrPhenom))
					showDetailsArea();
					
				}
			});
	        
	        Label lblMovement = new Label(top_4,SWT.LEFT);
	        lblMovement.setText("Movement: ");
	        
	        final Button btnSTNRY = new Button(top_4,SWT.RADIO);
	        btnSTNRY.setText("STNRY");
	        btnSTNRY.setSelection(true);	
	        this.setEditableAttrMovement("STNRY");
	        
	        btnSTNRY.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrMovement("STNRY");
	        	}
	        });
	        
	        final Button btnMVG = new Button(top_4,SWT.RADIO);
	        btnMVG.setText("MVG      ");
	        btnMVG.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,2,1));
	        
	        btnMVG.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrMovement("MVG");
	        	}
	        });
	        attrButtonMap.put("editableAttrMovement", new Button[]{btnSTNRY,btnMVG});
	        
	        Label lblSpeed = new Label(top_4, SWT.LEFT);
	        lblSpeed.setText("Speed: ");
	        final Combo comboSpeed = new Combo(top_4,SWT.READ_ONLY);
	        attrControlMap.put("editableAttrPhenomSpeed", comboSpeed );
	        comboSpeed.setItems(SigmetInfo.SPEED_ARRAY);
	        comboSpeed.select(0);
	        this.setEditableAttrPhenomSpeed(comboSpeed.getText());
	        
	        comboSpeed.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){	        		
	        		SigmetAttrDlg.this.setEditableAttrPhenomSpeed(comboSpeed.getText());
	        	}
	        });
	        
	        Label lblDirection = new Label(top_4,SWT.LEFT);
	        lblDirection.setText("Direction toward:");
	        final Combo comboDirection = new Combo(top_4,SWT.READ_ONLY);
	        attrControlMap.put("editableAttrPhenomDirection", comboDirection);
	        comboDirection.setItems(SigmetInfo.DIRECT_ARRAY);
	        comboDirection.select(0);
	        this.setEditableAttrPhenomDirection(comboDirection.getText());
	        
	        comboDirection.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){	        		
	        		SigmetAttrDlg.this.setEditableAttrPhenomDirection(comboDirection.getText());
	        	}
	        });
	        
	        Label lblTrend = new Label(top_4,SWT.LEFT);
	        lblTrend.setText("Trend: ");
	        final Combo comboTrend = new Combo(top_4, SWT.READ_ONLY);
	        attrControlMap.put("editableAttrTrend", comboTrend);
	        comboTrend.setItems(SigmetInfo.TREND_ARRAY);
	        comboTrend.select(0);	this.setEditableAttrTrend(comboTrend.getText());
	        comboTrend.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,false,false,7,1));
	        
	        comboTrend.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrTrend(comboTrend.getText());
	        	}
	        });
	        
//++++++++++++++++++++++++++++++++++++++++++
	        
	        if("TROPICAL_CYCLONE".equals(editableAttrPhenom)){
	        	Group top_secPhenom = new Group(top5,SWT.LEFT);
	        	top_secPhenom.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
	        	top_secPhenom.setLayout(new GridLayout(8,false));
		        
	        	Label lblSecPhenom = new Label(top_secPhenom,SWT.LEFT);
	        	lblSecPhenom.setText("Second Phenom: ");
	        	final Combo comboSecPhenom = new Combo(top_secPhenom,SWT.READ_ONLY);
	        	attrControlMap.put("editableAttrPhenom2", comboSecPhenom);
	        	comboSecPhenom.setItems(SigmetInfo.PHEN_MAP.get(SigmetInfo.getSigmetTypeString(pgenType)));	        	
	        	setControl(comboSecPhenom,"editableAttrPhenom2");
	        	comboSecPhenom.addListener(SWT.Selection, new Listener(){
	        		public void handleEvent(Event e){
	        			SigmetAttrDlg.this.setEditableAttrPhenom2(comboSecPhenom.getText());
	        		}
	        	});
	        }
//++++++++++++++++++++++++++++++++++++++++++	        
	        
	        //------------------------------ Level Info:
	        
	        Group top_5 = new Group(top5,SWT.LEFT);
	        top_5.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
	        top_5.setLayout(new GridLayout(8,false));
	        
	        Label lblLevelInfo = new Label(top_5, SWT.LEFT);
	        lblLevelInfo.setText("Level Info: ");
	        
	        final Combo comboLevel = new Combo(top_5,SWT.READ_ONLY);
	        attrControlMap.put("editableAttrLevel", comboLevel);
	        comboLevel.setItems(new String[]{"-none-","FCST","TOPS"});
	        //comboLevel.select(0);
	        setControl(comboLevel,"editableAttrLevel");
	        comboLevel.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrLevel(comboLevel.getText());
	        	}
	        });
	        
	        final Combo comboLevelInfo1 = new Combo(top_5,SWT.READ_ONLY);
	        attrControlMap.put("editableAttrLevelInfo1", comboLevelInfo1);
	        comboLevelInfo1.setItems(new String[]{"TO","ABV","BLW","BTN"});
	        //comboLevelInfo_1.select(0);
	        setControl(comboLevelInfo1,"editableAttrLevelInfo1");
	        comboLevelInfo1.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrLevelInfo1(comboLevelInfo1.getText());
	        	}
	        });
	        
	        final Text txtLevelInfo1 = new Text(top_5,SWT.SINGLE | SWT.BORDER);
	        attrControlMap.put("editableAttrLevelText1",txtLevelInfo1 );
	        txtLevelInfo1.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,2,1));
	        txtLevelInfo1.addListener (SWT.Verify, new Listener () {
	    		public void handleEvent (Event e) {
	    			e.doit = validateNumInput(e);	   			
	    	    		if ( !  e.doit ) return;
	    		}
	    	});
	        
	        GridData gdText1 = new GridData(); 	
	        gdText1.widthHint=66; 
	        gdText1.grabExcessHorizontalSpace = true; 
	        
	        txtLevelInfo1.setLayoutData(gdText1);	        
	        txtLevelInfo1.addListener(SWT.Modify, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrLevelText1(txtLevelInfo1.getText());
	        	}
	        });
	        
	        final Combo comboLevelInfo2 = new Combo(top_5,SWT.READ_ONLY);
	        attrControlMap.put("editableAttrLevelInfo2", comboLevelInfo2);
	        comboLevelInfo2.setItems(new String[]{"-none-","AND"});
	        //comboLevelInfo_2.select(0);
	        setControl(comboLevelInfo2,"editableAttrLevelInfo2");
	        
	        comboLevelInfo2.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrLevelInfo2(comboLevelInfo2.getText());
	        	}
	        });
	        
	        final Text txtLevelInfo2 = new Text(top_5,SWT.SINGLE | SWT.BORDER);
	        attrControlMap.put("editableAttrLevelText2", txtLevelInfo2);
	        txtLevelInfo2.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,2,1));
	        
	        txtLevelInfo2.addListener (SWT.Verify, new Listener () {
	    		public void handleEvent (Event e) {
	    			e.doit = validateNumInput(e);	   			
	    	    		if ( !  e.doit ) return;
	    		}
	    	});
	        txtLevelInfo2.setLayoutData(gdText1);	        
	        
	        txtLevelInfo2.addListener(SWT.Modify, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrLevelText2(txtLevelInfo2.getText());
	        	}
	        });
	        
	        //------------------------------- Remarks
	        
	        Group top_6 = new Group(top5,SWT.LEFT);
	        top_6.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
	        top_6.setLayout(new GridLayout(8,false));
	        
	        /*Label lblRemarks = new Label(top_6,SWT.LEFT);
	        lblRemarks.setText("Remarks:   ");
	        final Combo comboRemarks = new Combo(top_6,SWT.READ_ONLY);
	        attrControlMap.put("editableAttrRemarks", comboRemarks);
	        comboRemarks.setItems(SigmetInfo.REM_ARRAY);
	        setControl(comboRemarks,"editableAttrRemarks");
	        comboRemarks.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,7,1));
	        
	        comboRemarks.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrRemarks(comboRemarks.getText());
	        	}
	        });*/
	        
	        Label lblFreeText = new Label(top_6,SWT.LEFT);
	        lblFreeText.setText("Free Text:   ");
	        lblFreeText.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));
	        final Text txtFreeText = new Text(top_6,SWT.MULTI | SWT.BORDER);
	        attrControlMap.put("editableAttrFreeText", txtFreeText);
	        GridData gData = new GridData(SWT.FILL,SWT.CENTER,true,true,7,1);
	        gData.heightHint = 48;
	        txtFreeText.setLayoutData(gData);  
	        txtFreeText.addListener(SWT.Modify, new Listener(){
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setEditableAttrFreeText(txtFreeText.getText());
	        	}
	        });
	        
	        if( ! "TROPICAL_CYCLONE".equals(editableAttrPhenom) && ! "VOLCANIC_ASH".equals(editableAttrPhenom)){
	        	lblFreeText.setEnabled(false);
	        	txtFreeText.setEnabled(false);
	        }
        
	        //------------------------------- buttons
	        
	        Label lblDummy = new Label(top5,SWT.CENTER);           
            lblDummy.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,4,1));  
                       
            createButtonsForButtonBar(top5);     
            
            if( comboPhenomCalled == true){ 
            	withExpandedArea = true; 
            	comboPhenomCalled = false;
            }           
            
            Label lblDummy1 = new Label(top5,SWT.CENTER);              
            lblDummy1.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,1,1));
            
            if(copiedToSigmet == true) { 
            	init(); 
            	copiedToSigmet = false;
            }    
		
            return top5;

    }
    
    protected final void showDetailsArea() {
    	withExpandedArea = true;// for save, apply buttons
    	
    	Point oldWindowSize = getShell().getSize(), newWindowSize = cachedWindowSize;
		if(detailsArea == null){
			detailsArea = createDetailsArea( (Composite) getContents());
			
		}else {		
			detailsArea.dispose();
			detailsArea = createDetailsArea( (Composite) getContents());
			//detailsArea = null;		
		}
		
		Point oldSize = getContents().getSize();
		Point newSize = getContents().computeSize(SWT.DEFAULT, SWT.DEFAULT);
		
		if(newWindowSize == null) newWindowSize = new Point(oldWindowSize.x, oldWindowSize.y+(newSize.y - oldSize.y));
		
		Point windowLoc = getShell().getLocation();
		Rectangle  screenArea = getContents().getDisplay().getClientArea();
		
		if( newWindowSize.y > screenArea.height - (windowLoc.y - screenArea.y) )
			newWindowSize.y = screenArea.height - (windowLoc.y - screenArea.y);
		
		getShell().setSize(newWindowSize); //getShell().pack();
		((Composite)getContents()).layout(true,true);//.layout();
		//getShell().pack();
		//detailsArea = null;
		//withExpandedArea = false;// moved to the btnEdit listener
		
    }
    
    private String getVOR(Coordinate[] coors){
    	return SnapUtil.getVORText(coors,"-",this.lineType, 6,false);
    }
    
//-----------------------------------------------------------------------------------------------------    
	@Override
	public Control createDialogArea(Composite parent) {
		
	        this.top = (Composite) super.createDialogArea(parent);

	        GridLayout mainLayout = new GridLayout(8, false);
	        mainLayout.marginHeight = 3;
	        mainLayout.marginWidth = 3;
	        top.setLayout(mainLayout);	       
	        
	        this.getShell().setText("International SIGMET Edit");	        
	        
	        final Button btnArea = new Button(top,SWT.RADIO);	        
	        btnArea.setSelection(true); //default 	  
	        btnArea.setText("Area");
	        
	        
	        final Button btnLine = new Button(top,SWT.RADIO);btnLine.setText("Line");
	        
	        final Combo comboLine = new Combo(top,SWT.READ_ONLY);  	  
	        attrControlMap.put("lineType", comboLine);  //sideOfLine   	        
	        comboLine.setItems(LINE_SIDES);attrControlMap.put("sideOfLine", comboLine);
	        comboLine.select(0);//default: ESOL
	        comboLine.setEnabled(false);
	        
	        final Button btnIsolated = new Button(top,SWT.RADIO);btnIsolated.setText("Isolated  ");
	       	        
	        Label lblText = new Label(top,SWT.LEFT);			
	        lblText.setText("Width: ");
	        final Text txtWidth = new Text(top, SWT.SINGLE | SWT.BORDER);	
	        attrControlMap.put("widthStr", txtWidth);
	        txtWidth.setText("10.00");
	        txtWidth.setEnabled(false);
	        attrButtonMap.put("lineType", new Button[]{btnArea,btnLine,btnIsolated});	        
	        
	        btnArea.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		comboLine.setEnabled(false);
	        		txtWidth.setEnabled(false);
	        		
	        		SigmetAttrDlg.this.setLineType(AREA);
	        	}
	        });
	        
	        btnLine.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		comboLine.setEnabled(true);
	        		txtWidth.setEnabled(true);
	        		
	        		SigmetAttrDlg.this.setLineType(LINE + SigmetInfo.LINE_SEPERATER + SigmetAttrDlg.this.getSideOfLine());
	        	}
	        });
	        
	        btnIsolated.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		comboLine.setEnabled(false);
	        		txtWidth.setEnabled(true);
	        		
	        		SigmetAttrDlg.this.setLineType(ISOLATED);
	        	}
	        });	  
	        
	        comboLine.addListener(SWT.Selection,new Listener(){ // Line:::ESOL, Line:::SOF,... etc
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setSideOfLine(comboLine.getText());
	        		SigmetAttrDlg.this.setLineType(LINE + SigmetInfo.LINE_SEPERATER + SigmetAttrDlg.this.getSideOfLine());//::: seperater
	        	}
	        });
	        
	        txtWidth.addListener(SWT.Modify, new Listener(){ // or ModifyListener
	        	public void handleEvent(Event e){
	        		SigmetAttrDlg.this.setWidthStr(txtWidth.getText());
	        	}
	        });
	        
	        Label colorLbl = new Label(top, SWT.LEFT);
	        colorLbl.setText("Color:");
	        
            cs = new ColorButtonSelector( top );
	        Color clr = Color.cyan;	cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue() ));//0,255,0));  
	        
	        this.setLineType(AREA);//set and reset
	        this.setSideOfLine(comboLine.getText());
	        this.setWidthStr(txtWidth.getText());
	        
	        if( ! "INTL_SIGMET".equals(pgenType) && ! "CONV_SIGMET".equals(pgenType) ){// ONLY the two with all
	        	btnLine.setEnabled(false);		
	        	btnIsolated.setEnabled(false);
	        	comboLine.setEnabled(false);
	        	txtWidth.setEnabled(false);
	        }
	        
//++++++++++++++++++++++++++++++++++++++++++++++++++++ selected
	        
		       if("Pgen Select".equals(mouseHandlerName) || withExpandedArea ){		    	   
		    	   
		    	   String[] MWO_ITEMS = SigmetInfo.AREA_MAP.get(SigmetInfo.getSigmetTypeString(pgenType));
		    	   String[] ID_ITEMS  = SigmetInfo.ID_MAP.get(SigmetInfo.getSigmetTypeString(pgenType));
		    	   	    	   
		    	   Composite topSelect = (Composite) super.createDialogArea(parent);
		    	   
		    	   GridLayout mainLayout2 = new GridLayout(8,false);//8, false);
			        mainLayout2.marginHeight = 3;
			        mainLayout2.marginWidth = 4;   //qu
			        topSelect.setLayout(mainLayout2);
			        
			        Group top2 = new Group(topSelect,SWT.LEFT);
			        top2.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
			        top2.setLayout(new GridLayout(8,false)); 

		        	Label lblISU = new Label(top2, SWT.LEFT);
		        	lblISU.setText("ISSUE: ");
		        	final Combo comboISU = new Combo(top2,SWT.READ_ONLY);
		        	attrControlMap.put("editableAttrIssueOffice", comboISU);
		        	comboISU.setItems(MWO_ITEMS);
		        	comboISU.select(0);	this.setEditableAttrIssueOffice(comboISU.getText());		
		        	comboISU.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,true,1,1));
		        	
		        	comboISU.addListener(SWT.Selection, new Listener(){
		        		public void handleEvent(Event e){
		        			SigmetAttrDlg.this.setEditableAttrIssueOffice(comboISU.getText());
		        		}
		        	});
		        	
		        	Label lblMWO = new Label(top2, SWT.LEFT);
		        	lblMWO.setText(" MWO: ");
		        	final Combo comboMWO = new Combo(top2,SWT.READ_ONLY);
		        	attrControlMap.put("editableAttrArea", comboMWO);
		        	comboMWO.setItems(MWO_ITEMS);
		        	comboMWO.select(0);	this.setEditableAttrArea(comboMWO.getText());		
		        	comboMWO.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,true,1,1));
		        	
		        	comboMWO.addListener(SWT.Selection, new Listener(){
		        		public void handleEvent(Event e){
		        			SigmetAttrDlg.this.setEditableAttrArea(comboMWO.getText());
		        		}
		        	});
		        	
		        	Label lblID = new Label(top2, SWT.LEFT);
		        	lblID.setText("ID: ");
		        	final Combo comboID = new Combo(top2,SWT.READ_ONLY);
		        	attrControlMap.put("editableAttrId", comboID);
		        	comboID.setItems(ID_ITEMS);
		        	comboID.select(0);	this.setEditableAttrId(comboID.getText());		
		        	comboID.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,true,1,1));
		        	
		        	comboID.addListener(SWT.Selection, new Listener(){
		        		public void handleEvent(Event e){
		        			SigmetAttrDlg.this.setEditableAttrId(comboID.getText());
		        		}
		        	});
		        	
		        	Label lblSequence = new Label(top2, SWT.LEFT);
		        	lblSequence.setText("Sequence: ");
		        	final Spinner spiSeq = new Spinner(top2,SWT.BORDER);
		        	attrControlMap.put("editableAttrSeqNum", spiSeq);
		        	spiSeq.setMinimum(1);
		        	spiSeq.setMaximum(300);	this.setEditableAttrSeqNum(""+spiSeq.getSelection());
		        	spiSeq.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,1,1));
		        	
		        	spiSeq.addListener(SWT.Selection, new Listener(){
		        		public void handleEvent(Event e){
		        			SigmetAttrDlg.this.setEditableAttrSeqNum(""+spiSeq.getSelection());
		        		}
		        	});
		        	
			        final Button btnNew = new Button(top2,SWT.RADIO);
			        btnNew.setSelection(true);	
			        btnNew.setText("LATLON");//New(Prepend dir)");
			        btnNew.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,2,1));
			        
//			        final Button btnOld = new Button(top2,SWT.RADIO);
//			        btnOld.setText("Old(Postpend dir)");
//			        btnOld.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,2,1));
			        			        
			        final Button btnVor = new Button(top2,SWT.RADIO);			        
			        btnVor.setText("VOR");
			        btnVor.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,4,1));
			        
				        int style = SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL | SWT.READ_ONLY;
				       txtInfo = new Text( top2, style );
				       attrControlMap.put("editableAttrFromLine", txtInfo);
				       GridData gData = new GridData(600,48);//618, 48 );
				       gData.horizontalSpan = 8;
				       txtInfo.setLayoutData( gData );
				      
				       // if(editableAttrFromLine==null || editableAttrFromLine.equals(""))  this.setEditableAttrFromLine(txtInfo.getText());
				       // txtInfo.addListener(SWT.Modify, new Listener(){  public void handleEvent(Event e){  SigmetAttrDlg.this.setEditableAttrFromLine(txtInfo.getText());  } });
				      
				       attrButtonMap.put("editableAttrFromLine", new Button[]{btnNew,btnVor});//btnOld,btnVor });
		        	
				       final StringBuilder coorsLatLon = new StringBuilder(); 	        	
				       final AbstractDrawableComponent elSelected = PgenSession.getInstance().getPgenResource().getSelectedComp();//drawingLayer.getSelectedComp();
		        	   final Coordinate[] coors = (elSelected == null) ? null : elSelected.getPoints().toArray(new Coordinate[]{});
		        	
				       if( coors != null ) {	
				    	   if(editableAttrFromLine==null || editableAttrFromLine.equals("")){
				    		   coorsLatLon.append(getLatLonStringPrepend2(coors, AREA.equals( ((Sigmet)elSelected).getType() ) ));	        	
				    		   resetText(coorsLatLon.toString(), txtInfo);					        	
				    		   coorsLatLon.append(SigmetInfo.LINE_SEPERATER);// for Sigment element use later
				    		   String latLonFmtText = coorsLatLon.append("New").toString();
				    		   SigmetAttrDlg.this.setLatLonFormatFlagAndText(latLonFmtText);
				    		   SigmetAttrDlg.this.setEditableAttrFromLine(latLonFmtText);
				    	   }
			        	
		        	}		        	
		        	
		        	btnNew.addListener(SWT.Selection, new Listener(){
			        	public void handleEvent(Event e){
			        		StringBuilder sb = new StringBuilder();
				        	sb.append(getLatLonStringPrepend2(coors, AREA.equals( ((Sigmet)elSelected).getType() ) ));
				        	resetText(sb.toString(), txtInfo);
				        	
				        	sb.append(SigmetInfo.LINE_SEPERATER);// for Sigment element use later
				        	String latLonFmtText = sb.append("New").toString();
				        	SigmetAttrDlg.this.setLatLonFormatFlagAndText(latLonFmtText);
				        	SigmetAttrDlg.this.setEditableAttrFromLine(latLonFmtText);//txtInfo.getText());
			        	}
		        	});
		        	
//		        	btnOld.addListener(SWT.Selection, new Listener(){
//			        	public void handleEvent(Event e){
//			        		StringBuilder sb = new StringBuilder();
//				        	sb.append(PgenUtil.getLatLonStringPostpend(coors, AREA.equals( ((Sigmet)elSelected).getType() ) ));
//				        	resetText(sb.toString(), txtInfo);
//				        	
//				        	sb.append(SigmetAttrDlg.LINE_SEPERATER);// for Sigment element use later
//				        	String latLonFmtText = sb.append("Old").toString();
//				        	SigmetAttrDlg.this.setLatLonFormatFlagAndText(latLonFmtText);
//				        	SigmetAttrDlg.this.setEditableAttrFromLine(latLonFmtText);//txtInfo.getText());
//			        	}
//		        	});
		        	
		        	btnVor.addListener(SWT.Selection, new Listener(){
			        	public void handleEvent(Event e){
			        		StringBuilder sb = new StringBuilder();
				        	sb.append(getVOR(coors ));
				        	resetText(sb.toString(), txtInfo);
				        	
				        	sb.append(SigmetInfo.LINE_SEPERATER);// for Sigment element use later
				        	String latLonFmtText = sb.append("VOR").toString();
				        	SigmetAttrDlg.this.setLatLonFormatFlagAndText(latLonFmtText);
				        	SigmetAttrDlg.this.setEditableAttrFromLine(latLonFmtText);//txtInfo.getText());
			        	}
		        	});    
		        	
		        	if( ! withExpandedArea ){
		        				        		
			        	final Button btnEdit = new Button(top2,SWT.PUSH);			        	
			        	btnEdit.setText("Edit Attributes");
			        	btnEdit.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,false,false,8,1));
			        	
			        	btnEdit.addListener(SWT.Selection, new Listener(){
			        		public void handleEvent(Event e){
			        			withExpandedArea = true;
			        			btnEdit.dispose();
			        			SigmetAttrDlg.this.getButton(20091020).dispose();
			        			SigmetAttrDlg.this.getButton(20091021).dispose();
			        			SigmetAttrDlg.this.getButton(IDialogConstants.CANCEL_ID).dispose();
			        			copyEditableAttrToSigmet((Sigmet) SigmetAttrDlg.this.getSigmet());			        			
			        			showDetailsArea();	
			        			withExpandedArea = true;
			        			init();		//extra since createDetailArea() inside calls init()
			        			withExpandedArea = false;        			
			        		}
			        	});
		        	}
		        	
		       }
		    init();
	        return top;
	}

	private String getTimeStringPlusHourInHMS(int plusHour){
        Calendar c = Calendar.getInstance();      
        c.set(Calendar.HOUR_OF_DAY, c.get(Calendar.HOUR_OF_DAY)+plusHour); 
        
        // round to 5 min 
        c.set(Calendar.MINUTE, ((int) (c.get(Calendar.MINUTE)/5)) * 5); 

        DateFormat dateFormat = new SimpleDateFormat("ddHHmm");	    	      	      
	    dateFormat.setTimeZone(TimeZone.getTimeZone("GMT")); 
	    return dateFormat.format(c.getTime());
        /*
        String day = c.get(Calendar.DAY_OF_MONTH) > 9 ? ""+c.get(Calendar.DAY_OF_MONTH) : "0"+c.get(Calendar.DAY_OF_MONTH);
        String hour = c.get(Calendar.HOUR_OF_DAY) > 9 ? ""+c.get(Calendar.HOUR_OF_DAY) : "0"+c.get(Calendar.HOUR_OF_DAY);
        String minute = c.get(Calendar.MINUTE) > 9 ? ""+c.get(Calendar.MINUTE) : "0"+c.get(Calendar.MINUTE);

		return day+hour+minute;
		*/
	}
	
	private String convertTimeStringPlusHourInHMS(String timeString, int plusHour, boolean dayNeeded){
        Calendar c = Calendar.getInstance();	System.out.println("____________timeString: "+timeString);
        c.set(Calendar.DAY_OF_MONTH, Integer.parseInt(timeString.substring(0,2)));
        c.set(Calendar.HOUR_OF_DAY,  Integer.parseInt(timeString.substring(2,4)) + plusHour );
        c.set(Calendar.MINUTE, Integer.parseInt(timeString.substring(4,6)));
        String day = c.get(Calendar.DAY_OF_MONTH) > 9 ? ""+c.get(Calendar.DAY_OF_MONTH) : "0"+c.get(Calendar.DAY_OF_MONTH);
        String hour = c.get(Calendar.HOUR_OF_DAY) > 9 ? ""+c.get(Calendar.HOUR_OF_DAY) : "0"+c.get(Calendar.HOUR_OF_DAY);
        String minute = c.get(Calendar.MINUTE) > 9 ? ""+c.get(Calendar.MINUTE) : "0"+c.get(Calendar.MINUTE);
        
        return	dayNeeded 	? new StringBuilder().append(day).append(hour).append(minute).toString()
        					: new StringBuilder().append(hour).append(minute).toString();
	}
	
	private boolean validateTimeStringInHMS(String time){
		if(time == null || time.trim().length() != 6 ) return false;
		String txt = time.trim();
		char [] chars = txt.toCharArray();
		boolean result = true;
		if(  chars[0] < '0' || chars[0] > '3' ) {
            result = false;
        }
        if( chars[1] < '0'     
        		|| chars[1] > '9'
                || (chars[0]=='3'&&chars[1]>'1')
                || (chars[0]=='0'&&chars[1]<'1'))  {
            result = false;
        }       
        if(  chars[2] < '0' || chars[2] > '2')  {
            result = false;
        }
        if( chars[3] < '0' || chars[3] > '9' || (chars[2]=='2'&&chars[3]>'3'))  {
            result = false;
        }       
        if( chars[4] < '0' || chars[4] > '5')  {
            result = false;
        }       
        if( chars[5] < '0' || chars[5] > '9')  {
            result = false;
        }
		
		return result;		
	}
	
	private boolean validateHMSInput(Event e, Text txt){
		
		boolean result = true;
		
		String string = e.text.trim();//e.text;
		char [] chars = new char [string.length ()];
		string.getChars (0, chars.length, chars, 0);
		
		int i=e.start;
		
		if(i > 5) { 
			result = false;			
		}   			
				
		if ( chars.length>0 ) {			
			
			if( i==0 &&  ( chars[0] < '0' || chars[0] > '3') ) {
                result = false;
            }

            if( i==1 && (chars[0] < '0'     || chars[0] > '9'
                          || (txt.getText().charAt(0)=='3'&&chars[0]>'1')
                          || (txt.getText().charAt(0)=='0'&&chars[0]<'1')) ) {
                result = false;
            }
           
            if( i==2 &&  ( chars[0] < '0' || chars[0] > '2') ) {
                result = false;
            }

            if( i==3 && (chars[0] < '0' || chars[0] > '9' || (txt.getText().charAt(2)=='2'&&chars[0]>'3')) ) {
                result = false;
            }
           
            if( i==4 && (chars[0] < '0' || chars[0] > '5') ) {
                result = false;
            }
           
            if( i==5 && (chars[0] < '0' || chars[0] > '9') ) {
                result = false;
            }
		}
		return result;
	}
	
	private boolean validateNumInput(Event e){
		
		boolean result = true;		
		String string = e.text;
		char [] chars = new char [string.length ()];
		string.getChars (0, chars.length, chars, 0);
		for (int i=0; i<chars.length; i++) {
			if ( ! ('0' <= chars [i] && chars [i] <= '9')) {
				result = false;				
			}
		}
		return result;
	}	
	
    public void copyEditableAttrToSigmet(Sigmet ba){
    	Field[] ff = this.getClass().getDeclaredFields();
        for(Field f : ff){
            try{               
                if(f.getName().contains("editableAttr"))                   
                    BeanUtils.copyProperty(ba, f.getName(), f.get(this));               
            }catch(Exception e){    System.out.println( e.getMessage() );    }
        }
        ba.setType(this.getLineType()); 
        ba.setWidth(this.getWidth());
        ba.setEditableAttrFromLine(this.latLonFormatFlagAndText);//20091203
        
        ba.setColors(this.getColors());//20100115
        //this.okPressed();//20100115
        
        copiedToSigmet = true;
    }

	public String getEditableAttrIssueOffice() {
		return (editableAttrIssueOffice==null||editableAttrIssueOffice.length()==0) ? AREA_MAP.get(SIGMET_TYPES[0])[0] : editableAttrIssueOffice;
	}

	public void setEditableAttrIssueOffice(String editableAttrIssueOffice) {
		this.editableAttrIssueOffice = editableAttrIssueOffice;
	}

	public String getEditableAttrArea() {
		return (editableAttrArea==null||editableAttrArea.length()==0) ? AREA_MAP.get(SIGMET_TYPES[0])[0] : editableAttrArea;
	}

	public void setEditableAttrArea(String editableAttrArea) {
		this.editableAttrArea = editableAttrArea;
	}

	public String getEditableAttrStatus() {
		return editableAttrStatus;
	}

	public void setEditableAttrStatus(String editableAttrStatus) {
		this.editableAttrStatus = editableAttrStatus;
	}

	public String getEditableAttrId() {
		return (editableAttrId==null||editableAttrId.length()==0) ? ID_MAP.get(SIGMET_TYPES[0])[0] : editableAttrId;
	}

	public void setEditableAttrId(String editableAttrId) {
		this.editableAttrId = editableAttrId;
	}

	public String getEditableAttrSeqNum() {
		return (editableAttrSeqNum==null||editableAttrSeqNum.length()==0) ? "1" : editableAttrSeqNum;
	}

	public void setEditableAttrSeqNum(String editableAttrSeqNum) {
		this.editableAttrSeqNum = editableAttrSeqNum;
	}

	public String getEditableAttrStartTime() {
		return editableAttrStartTime;
	}

	public void setEditableAttrStartTime(String editableAttrStartTime) {
		this.editableAttrStartTime = editableAttrStartTime;
		((Sigmet)this.getSigmet()).setEditableAttrStartTime(editableAttrStartTime);
	}

	public String getEditableAttrEndTime() {
		return editableAttrEndTime;
	}

	public void setEditableAttrEndTime(String editableAttrEndTime) {
		this.editableAttrEndTime = editableAttrEndTime;
		((Sigmet)this.getSigmet()).setEditableAttrEndTime(editableAttrEndTime);
	}

	public String getEditableAttrRemarks() {
		return editableAttrRemarks;
	}

	public void setEditableAttrRemarks(String editableAttrRemarks) {
		this.editableAttrRemarks = editableAttrRemarks;
	}

	public String getEditableAttrPhenom() {
		return editableAttrPhenom;
	}

	public void setEditableAttrPhenom(String editableAttrPhenom) {
		this.editableAttrPhenom = editableAttrPhenom;
	}

	public String getEditableAttrPhenom2() {
		return editableAttrPhenom2;
	}

	public void setEditableAttrPhenom2(String editableAttrPhenom2) {
		this.editableAttrPhenom2 = editableAttrPhenom2;
	}

	public String getEditableAttrPhenomName() {
		return editableAttrPhenomName;
	}

	public void setEditableAttrPhenomName(String editableAttrPhenomName) {
		this.editableAttrPhenomName = editableAttrPhenomName;
	}

	public String getEditableAttrPhenomLat() {
		return editableAttrPhenomLat;
	}

	public void setEditableAttrPhenomLat(String editableAttrPhenomLat) {
		this.editableAttrPhenomLat = editableAttrPhenomLat;
	}

	public String getEditableAttrPhenomLon() {
		return editableAttrPhenomLon;
	}

	public void setEditableAttrPhenomLon(String editableAttrPhenomLon) {
		this.editableAttrPhenomLon = editableAttrPhenomLon;
	}

	public String getEditableAttrPhenomPressure() {
		return editableAttrPhenomPressure;
	}

	public void setEditableAttrPhenomPressure(String editableAttrPhenomPressure) {
		this.editableAttrPhenomPressure = editableAttrPhenomPressure;
	}

	public String getEditableAttrPhenomMaxWind() {
		return editableAttrPhenomMaxWind;
	}

	public void setEditableAttrPhenomMaxWind(String editableAttrPhenomMaxWind) {
		this.editableAttrPhenomMaxWind = editableAttrPhenomMaxWind;
	}

	public String getEditableAttrFreeText() {
		return editableAttrFreeText;
	}

	public void setEditableAttrFreeText(String editableAttrFreeText) {
		this.editableAttrFreeText = editableAttrFreeText;
	}

	public String getEditableAttrTrend() {
		return editableAttrTrend;
	}

	public void setEditableAttrTrend(String editableAttrTrend) {
		this.editableAttrTrend = editableAttrTrend;
	}

	public String getEditableAttrMovement() {
		return editableAttrMovement;
	}

	public void setEditableAttrMovement(String editableAttrMovement) {
		this.editableAttrMovement = editableAttrMovement;
	}

	public String getEditableAttrPhenomSpeed() {
		return editableAttrPhenomSpeed;
	}

	public void setEditableAttrPhenomSpeed(String editableAttrPhenomSpeed) {
		this.editableAttrPhenomSpeed = editableAttrPhenomSpeed;
	}

	public String getEditableAttrPhenomDirection() {
		return editableAttrPhenomDirection;
	}

	public void setEditableAttrPhenomDirection(String editableAttrPhenomDirection) {
		this.editableAttrPhenomDirection = editableAttrPhenomDirection;
	}

	public String getEditableAttrLevel() {
		return editableAttrLevel;
	}

	public void setEditableAttrLevel(String editableAttrLevel) {
		this.editableAttrLevel = editableAttrLevel;
	}
	
	public String getEditableAttrLevelInfo1() {
		return editableAttrLevelInfo1;
	}

	public void setEditableAttrLevelInfo1(String editableAttrLevelInfo1) {
		this.editableAttrLevelInfo1 = editableAttrLevelInfo1;
	}

	public String getEditableAttrLevelInfo2() {
		return editableAttrLevelInfo2;
	}

	public void setEditableAttrLevelInfo2(String editableAttrLevelInfo2) {
		this.editableAttrLevelInfo2 = editableAttrLevelInfo2;
	}

	public String getEditableAttrLevelText1() {
		return editableAttrLevelText1;
	}

	public void setEditableAttrLevelText1(String editableAttrLevelText1) {
		this.editableAttrLevelText1 = editableAttrLevelText1;
	}

	public String getEditableAttrLevelText2() {
		return editableAttrLevelText2;
	}

	public void setEditableAttrLevelText2(String editableAttrLevelText2) {
		this.editableAttrLevelText2 = editableAttrLevelText2;
	}
	
	public String getEditableAttrFromLine() {
		return editableAttrFromLine;
	}

	public void setEditableAttrFromLine(String editableAttrFromLine) {
		this.editableAttrFromLine = editableAttrFromLine;
		((Sigmet)this.getSigmet()).setEditableAttrFromLine(editableAttrFromLine);
	}
	
	public String getLatLonFormatFlagAndText() {
		return latLonFormatFlagAndText;
	}

	public void setLatLonFormatFlagAndText(String latLonFormatFlagAndText) {
		this.latLonFormatFlagAndText = latLonFormatFlagAndText;
	}
	
	public String getEditableAttrFir() {
		return editableAttrFir;
	}

	public void setEditableAttrFir(String editableAttrFir) {
		this.editableAttrFir = editableAttrFir;
	}
	
	private void setControl(Control cont, String prop){
		PropertyDescriptor pd;
		Method pdReadMethod, pdWriteMethod;	
		String propValue;
		try{ 
			pd = new PropertyDescriptor(prop, this.getClass()); 
			if( pd != null){ 
				pdReadMethod = pd.getReadMethod(); 
				pdWriteMethod = pd.getWriteMethod(); 
				
				if( pdReadMethod != null){
					propValue = (String) pdReadMethod.invoke(this, null);
					
					if(propValue == null){// Text Controls NO needs ???
						if(cont instanceof Combo){ 
							Combo contCombo =  (Combo)cont;
							contCombo.select(0);
							pdWriteMethod.invoke(this, contCombo.getText() );
						}
						
						if(cont instanceof Spinner){
							Spinner contSpinner = (Spinner)cont;
							contSpinner.setSelection(0);
							pdWriteMethod.invoke(this, contSpinner.getText());
						}
					}else{													
						if(cont instanceof Combo){ 
							Combo c=(Combo)cont;
							c.setText(propValue); 
							
							if(c.getText().contains("CYCLONE"))
								tropCycFlag=true;
						}						
						if(cont instanceof Text){
							((Text)cont).setText(propValue); 
							//Text t = (Text)cont;if(prop.equals("editableAttrFromLine"))resetText(propValue,t); 	
						}
						if(cont instanceof Spinner)((Spinner)cont).setSelection(Integer.parseInt(propValue));
					}
				}				
			}
		}catch(Exception e){
			System.out.println("--- inside setControl(): "+e.getMessage());			
		}		
		
	}
	
	private String getLineTypeForSOL(String typeString){
		String[] lineTypes = new String[]{
				"EITHER SIDE OF","NORTH OF"
				,"SOUTH OF","EAST OF","WEST OF"};
		
		String type = typeString.split(SigmetInfo.LINE_SEPERATER)[1];
		int index = 0;
		for(int i=0; i<LINE_SIDES.length; i++){
			if(this.LINE_SIDES[i].equals(type))
				index = i;
		}
		return lineTypes[index];
	}
	
	private class SigmetAttrDlgSaveMsgDlg extends AttrDlg{
		String startTime="", endTime="";
		Text txtInfo;
		Text txtSave;
		
		boolean firCalledForSecondLine = false;
		
		SigmetAttrDlgSaveMsgDlg(Shell parShell) throws VizException {
			super(parShell);		
		}
		
		@Override
		public Control createDialogArea(Composite parent) {
			Composite top = (Composite) super.createDialogArea(parent);
			
			GridLayout mainLayout = new GridLayout(3, false);
	        mainLayout.marginHeight = 3;
	        mainLayout.marginWidth = 3;
	        top.setLayout(mainLayout);
	        
	        this.getShell().setText("SIGMET Save");	        
	        
		    txtInfo = new Text( top, SWT.MULTI | SWT.BORDER | SWT.READ_ONLY | SWT.WRAP);
		    GridData gData = new GridData(512,300);//618, 48 );
		    gData.horizontalSpan = 3;
		    txtInfo.setLayoutData( gData );	    
	        txtInfo.setText(getFileContent());// formatted string from the dialog fields
	        
	        txtSave = new Text(top, SWT.BORDER | SWT.READ_ONLY);
	        txtSave.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,3,1));
	        txtSave.setText(getFileName());
			
			return top;			
		}
		
		@Override
		public void createButtonsForButtonBar(Composite parent){
			createButton(parent, IDialogConstants.OK_ID, "Save",	true);  	
			createButton(parent, IDialogConstants.CANCEL_ID,IDialogConstants.CANCEL_LABEL, false);
		}
		
		@Override
		public void enableButtons(){ 			
			this.getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
			this.getButton(IDialogConstants.OK_ID).setEnabled(true);	  		
		}
		
		@Override
		public void cancelPressed(){			
			setReturnCode(CANCEL);
			close();		
		}
		
		@Override
		public void okPressed() {// listener function of Save Button
			FileTools.writeFile(PgenUtil.getPgenActivityTextProdPath()+File.separator+txtSave.getText(), 
					txtInfo.getText());

				setReturnCode(OK);
				close();
				SigmetAttrDlg.this.drawingLayer.removeSelected();
		    	SigmetAttrDlg.this.close();
				PgenUtil.setSelectingMode();
			
		}
		
		private String getFileContent(){
			StringBuilder sb = new StringBuilder();			
			
			if ( SigmetInfo.getAFOSflg() )  {	//C code: @4146, @5377					
				sb.append("ZCZC ");
				sb.append(getIdnode());
				sb.append(getAfospil());
				sb.append("\n").append(getWmo());
				sb.append("\n").append(getFirstLine());
				sb.append("\n").append(getSecondLine());
				sb.append("\n").append("NNNN");
				sb.append("\n");					
			}else{				
				sb.append(getWmo());
				sb.append("\n").append(getAfospil());
				sb.append("\n").append(getFirstLine());
				sb.append("\n").append(getSecondLine());
			}		
			
			return sb.toString();
		}
		
		private String getFileName(){
			StringBuilder sb = new StringBuilder();
			sb.append( SigmetAttrDlg.this.getEditableAttrArea() );
			sb.append("_").append(SigmetAttrDlg.this.getEditableAttrId());
			sb.append("_").append(SigmetAttrDlg.this.getEditableAttrSeqNum());
			sb.append(".sigintl");			
			return sb.toString();
		}
		
		private String getWmo(){
			int HEADER_WMO = 1;
			StringBuilder sb = new StringBuilder();
			sb.append("W");
			sb.append(getWmoPhen());
			sb.append(getOcnWmoAwpHeaders()[HEADER_WMO]);
			sb.append(getInum());
			sb.append(" ").append(SigmetAttrDlg.this.getEditableAttrArea());
			sb.append(" ").append(getTimeStringPlusHourInHMS(0));
			
			return sb.toString();
		}
		
		private String getAfospil(){
			int HEADER_AFOSPIL = 2;
			StringBuilder sb = new StringBuilder();
			
			if("PAWU".equals(SigmetAttrDlg.this.getEditableAttrArea())){
				sb.append(getAwpPhen());
				sb.append(getOcnWmoAwpHeaders()[HEADER_AFOSPIL]);
				sb.append(getInum());
				sb.append("\n").append(getIdnode());
				sb.append(SigmetAttrDlg.this.getEditableAttrId().charAt(0));
				sb.append(" WS ").append(getTimeStringPlusHourInHMS(0));				
			}else{
				sb.append(getAwpPhen());
				sb.append(getOcnWmoAwpHeaders()[HEADER_AFOSPIL]);
				sb.append(SigmetAttrDlg.this.getEditableAttrId().substring(0,1));
			}			
			
			return sb.toString();
		}
		
		private String getFirstLine(){
			StringBuilder sb = new StringBuilder();
			startTime=getTimeStringPlusHourInHMS(0); 
			endTime=getTimeStringPlusHourInHMS(4);
			
			sb.append(getFirs());
			sb.append(" ").append("SIGMET");
			sb.append(" ").append(SigmetAttrDlg.this.getEditableAttrId());
			sb.append(" ").append( SigmetAttrDlg.this.getEditableAttrSeqNum() );
			sb.append(" ").append("VALID");
			sb.append(" ").append(SigmetAttrDlg.this.getEditableAttrStartTime()==null?startTime:SigmetAttrDlg.this.getEditableAttrStartTime());
			sb.append("/").append(SigmetAttrDlg.this.getEditableAttrEndTime()==null?endTime:SigmetAttrDlg.this.getEditableAttrEndTime());// should be from the widget
			sb.append(" ").append(SigmetAttrDlg.this.getEditableAttrIssueOffice()).append("-");			
			
			return sb.toString();
		}
		
		private String getSecondLine(){
			StringBuilder sb = new StringBuilder();
			boolean isPhenNameEntered = false, isTropCyc = false;//in C code: entvname,tc 
			
			String phen = SigmetAttrDlg.this.getEditableAttrPhenom();	
			String phenName = SigmetAttrDlg.this.getEditableAttrPhenomName();
			phenName = phenName == null ? phenName : phenName.toUpperCase();
			if( "VOLCANIC_ASH".equals(phen)){				
				isPhenNameEntered = SigmetInfo.isVolcanoNameEntered(phenName);
			}
			
			//---------------------FIR
			firCalledForSecondLine = true;
			sb.append(getFirs());
			firCalledForSecondLine = false;
			//---------------------phenomnon
			
			if( ! ("TROPICAL_CYCLONE".equals(phen))){
				String pString = phen==null ? SigmetInfo.PHEN_MAP.get(SigmetInfo.SIGMET_TYPES[0])[0] : phen;				
				sb.append(pString.replace('_', ' ')).append(" ");				
			}else{
				isTropCyc = true; 
				isPhenNameEntered = true;
				
				sb.append("TC");				
				
				if(phenName != null) sb.append(" ").append(phenName.trim()).append(" ");
				
				String presHPA = SigmetAttrDlg.this.getEditableAttrPhenomPressure();
				if(presHPA != null)	 sb.append(" ").append(presHPA.trim()).append("HPA ");
				
				
				
				Sigmet sig =( (Sigmet) SigmetAttrDlg.this.drawingLayer.getSelectedDE());
				if(SigmetAttrDlg.this.ISOLATED.equals(sig.getType())){
					sb.append(" ").append("NEAR");
					//sb.append(" ").append(sig.getLinePoints()[0].y); sb.append(" ").append(sig.getLinePoints()[0].x);
				}else{
					//TODO: legacy code phenlat/phenlon source ???
				}
				
				sb.append(" ").append("AT ");
				sb.append(getTimeStringPlusHourInHMS(0).substring(0,4));//C code: loctim/local time
				sb.append("Z.");
				
				//--------------- movement
				
				String movement = SigmetAttrDlg.this.getEditableAttrMovement();
				if("STNRY".equals(movement)){
					sb.append(" ").append("STNR. ");
				}else if( "MVG".equals(movement)){
					sb.append(" ").append("MOV");
					sb.append(" ").append(SigmetAttrDlg.this.getEditableAttrPhenomDirection());
					sb.append(" ").append(SigmetAttrDlg.this.getEditableAttrPhenomSpeed());
					sb.append("KT.  ");
				}
				
				//--------------- max winds
				
				String maxWinds = SigmetAttrDlg.this.getEditableAttrPhenomMaxWind();
				if(maxWinds != null && ! "".equals(maxWinds.trim())){
					sb.append(" ").append("MAX WINDS ");
					sb.append(maxWinds).append("KT.  ");
				}
				
				//---------------- trend
				
				String trend = SigmetAttrDlg.this.getEditableAttrTrend();
				if( ! "-none-".equals(trend)){
					sb.append(trend).append(".");
				}
				
				//---------------- second phenom
				
				String phen2 = SigmetAttrDlg.this.getEditableAttrPhenom2();
				if( phen2 != null && ! "".equals( phen2.trim()) ){
					sb.append(" ").append(phen2.replace('_', ' ')).append(" ");
				}				
			}
			
			//------------ VOLCANIC_ASH
			
			if("VOLCANIC_ASH".equals(phen) ){
				sb.append(" ").append("FROM ");
				sb.append(phenName==null ? "":phenName).append(".");//phenName in C code: volcn
				
				String phenLat = SigmetAttrDlg.this.getEditableAttrPhenomLat();//phenlat,phenlon in C code
				String phenLon = SigmetAttrDlg.this.getEditableAttrPhenomLon();
				
				if( isPhenNameEntered 
					&& phenLat != null 
					&& ! "".equals(phenLat.trim()) 
					&& phenLon != null
					&& ! "".equals(phenLon.trim()) ){
					
					sb.append(" ").append("LOC ");
					sb.append(phenLat);
					sb.append(phenLon);
				}				
			}
			
			//----------------tops
			
			String tops = SigmetAttrDlg.this.getEditableAttrLevel();
			
			if( "FCST".equals(tops) || isTropCyc){
				sb.append(tops.equals("-none-")?"":tops).append(" ");
				sb.append(SigmetAttrDlg.this.getEditableAttrLevelInfo1()).append(" ");
				sb.append("FL");
				String text1 = SigmetAttrDlg.this.getEditableAttrLevelText1();
				sb.append(text1 == null ? "" : text1).append(" ");
				
				String levelInfo2 = SigmetAttrDlg.this.getEditableAttrLevelInfo2();
				if( ! "-none-".equals(levelInfo2)){
					sb.append(levelInfo2).append(" ");
					sb.append("FL");					
				}
				String text2 = SigmetAttrDlg.this.getEditableAttrLevelText2();
				sb.append(text2 == null ? "" : text2).append(" ");
			}
			
			//---------------- FCST level info nmap_pgsigw.c@3989
			
			if(tops != null && tops.contains("FCST")){
				if("VOLCANIC_ASH".equals(phen))
					sb.append(" ");
				//sb.append(tops).append(".");		an extra FCST ???		
			}
			
			if(isTropCyc){
				//----------- TOPS level info
				//if(tops.contains("TOPS")) sb.append(" ").append(tops);				
			}
		
			String lineType = ((Sigmet)SigmetAttrDlg.this.drawingLayer.getSelectedDE()).getType();
			String fromLineWithFormat =  SigmetAttrDlg.this.getLatLonFormatFlagAndText();//from_line without format in C
			String[] lineArray = fromLineWithFormat.split(SigmetInfo.LINE_SEPERATER);
			
			if(SigmetAttrDlg.ISOLATED.equals(lineType)){//in C: switch( _subType)nmap_pgsigw.c@4008
				if(isTropCyc){
					sb.append(" ").append("WITHIN ");
					sb.append(SigmetAttrDlg.this.getWidth());
					sb.append(" ").append("NM CENTER.");
				}else{
					sb.append(" ").append("WI ");
					sb.append(SigmetAttrDlg.this.getWidth());
					sb.append(" ").append("NM OF ");
					for(int i=0; i<lineArray.length-1;i++)
						sb.append(" ").append(lineArray[i]);
					sb.append(".");
					//sb.append(SigmetAttrDlg.this.getEditableAttrFromLine()).append(".");
				}				
			}else if(SigmetAttrDlg.AREA.equals(lineType)){
				if( ! fromLineWithFormat.contains("VOR")){
					if(sb.toString().contains("VOLCANIC")){
						sb.append(" ").append("VA CLD WI AREA BOUNDED BY ");						
					}else{
						sb.append(" ").append("WI AREA BOUNDED BY");						
					}					
				}else{					
					sb.append(" ").append("WI AREA BOUNDED BY LINE FM ");					
				}
				
				for(int i=0; i<lineArray.length-1;i++)
					sb.append(" ").append(lineArray[i]);
				sb.append(".");
				
			}else{//line with LINE_SEPERATER
				sb.append(" ").append("WI ");
				sb.append(SigmetAttrDlg.this.getWidth());
				sb.append(" ").append("NM ");
				sb.append( getLineTypeForSOL(lineType) );//[lineArray.length-2]);//watch out for index 
				
				if( ! fromLineWithFormat.contains("VOR")){					
					sb.append(" ").append("LINE");
				}else{
					sb.append(" ").append("LINE FM");
				}
				
				for(int i=0; i<lineArray.length-1;i++)
					sb.append(" ").append(lineArray[i]);
				sb.append(".");				
			}
			
			if( ! isTropCyc ){//in C: if( ! tc )nmap_pgsigw.c@4062
				
				//------ TOPS
				if("TOPS".equals(tops)){
					sb.append(" ").append(tops).append(" "); 
					sb.append(SigmetAttrDlg.this.getEditableAttrLevelInfo1()).append(" ");
					sb.append("FL");
					String text1 = SigmetAttrDlg.this.getEditableAttrLevelText1();
					sb.append(text1 == null ? "" : text1).append(" ");
					
					String levelInfo2 = SigmetAttrDlg.this.getEditableAttrLevelInfo2();
					if( ! "-none-".equals(levelInfo2)){
						sb.append(levelInfo2).append(" ");
						sb.append("FL");					
					}
					String text2 = SigmetAttrDlg.this.getEditableAttrLevelText2();
					sb.append(text2 == null ? "" : text2).append(" . ");
				}
				
				//------ movement
				String movement = SigmetAttrDlg.this.getEditableAttrMovement();
				if("STNRY".equals(movement) || movement == null){
					sb.append(" ").append("STNR.");
				}else if( "MVG".equals(movement)){
					sb.append(" ").append("MOV");
					sb.append(" ").append(SigmetAttrDlg.this.getEditableAttrPhenomDirection());
					sb.append(" ").append(SigmetAttrDlg.this.getEditableAttrPhenomSpeed());
					sb.append("KT. ");
				}
				
				//------ trend
				String trend = SigmetAttrDlg.this.getEditableAttrTrend();
				if( ! "-none-".equals(trend) && trend != null){
					sb.append(" ").append(trend).append(".");
				}				
			}
			
			//------ remarks
			String remarks = SigmetAttrDlg.this.getEditableAttrRemarks();
			if( ! "-none-".equals(remarks) && remarks != null){
				sb.append(" ").append(remarks).append(".");
			}
			
			//------ outlook if volcano ash
			String startTime = getEditableAttrStartTime();
			
			if("VOLCANIC_ASH".equals(phen)){
				sb.append(" ").append("FORECAST ");				
				sb.append( convertTimeStringPlusHourInHMS(startTime,6,false) ).append("Z");
				sb.append(" ").append("VA CLD APRX ");
				
				String freeText = SigmetAttrDlg.this.getEditableAttrFreeText();
				if(freeText != null && freeText.length() > 0) sb.append(freeText.toUpperCase());
			}
			
			//------ outlook if tropical cyclone
			if("TROPICAL_CYCLONE".equals(phen)){
				sb.append(" ").append("FORECAST ");				
				sb.append( convertTimeStringPlusHourInHMS(startTime,6,false) ).append("Z");
				sb.append(" ").append("TC CENTER ");
				
				String freeText = SigmetAttrDlg.this.getEditableAttrFreeText();
				if(freeText != null && freeText.length() > 0) sb.append(freeText.toUpperCase());
				
			}		
						
			return sb.toString();
		}
		
		private String getWmoPhen(){
			String phen = SigmetAttrDlg.this.getEditableAttrPhenom();
			if(phen != null) phen = phen.trim();
			if("VOLCANIC_ASH".equals(phen)) 	return "V";
			if("TROPICAL_CYCLONE".equals(phen))	return "C";
			return "S";
		}
		
		private String getAwpPhen(){
			String wmophen = getWmoPhen();
			if("S".equals(wmophen))	return "SIG";
			if("V".equals(wmophen))	return "WSV";			
			return "WST";
		}
		
		private String getInum(){
			char firstIdChar = SigmetAttrDlg.this.getEditableAttrId().charAt(0);			
			
			if("PHFO".equals(SigmetAttrDlg.this.getEditableAttrArea()) ){
				int inum = firstIdChar - 77;
				return inum < 0 || inum >9 ? "" + inum : "0" + inum;
			}
			else if("PAWU".equals(SigmetAttrDlg.this.getEditableAttrArea()) ){
				int inum = firstIdChar - 72;
				return inum < 0 || inum >9 ? "" + inum : "0" + inum;				
			}
			int inum = firstIdChar - 64;
			return inum < 0 || inum >9 ? "" + inum : "0" + inum;								
		}
		
		private String getIdnode(){
			String area = SigmetAttrDlg.this.getEditableAttrArea();
			
			if("KKCI".equals(area)) return "MKC";
			if("KNHC".equals(area)) return "NHC";
			if("PHFO".equals(area)) return "HFO";
			return "ANC";//PAWU
		}
		
		private String[] getOcnWmoAwpHeaders(){
			String area = SigmetAttrDlg.this.getEditableAttrArea();
			String[] headers = new String[3];//0:hdrocn, 1:hdrwmo, 2:hdrawp
			
			headers[0] = headers[1] = headers[2] ="";
			
			if("PAWU".equals(area)){
				headers[1] = "AK";
				headers[2] = "AK";
			}else if("PHFO".equals(area)){
				headers[1] = "PA";
				headers[2] = "PA";
			}else{// area is KKCI or KNHC
				String fir = getFirs();			
				if(fir == null || fir.length() == 0){
					
				}else if(fir.contains("KZHU")||fir.contains("KZMA")||fir.contains("KZNY")||fir.contains("TJZS")){
					headers[0] = "NT";
					headers[1] = "NT";
					headers[2] = "A0";
				}else if(fir.contains("KZAK") || fir.contains("PAZA")){
					headers[0] = "PN";
					headers[1] = "PN";
					headers[2] = "P0";
				}
			}
			
			return headers;			
		}
		
		private String getFirs(){		
			StringBuilder fir = new StringBuilder();			
			//System.out.println("---Test FIR_POLYGON_MAP: size: "+FIR_POLYGON_MAP.size());			
			
			AbstractDrawableComponent elSelected = SigmetAttrDlg.this.drawingLayer.getSelectedComp();
        	Coordinate[] coors = (elSelected == null) ? null : elSelected.getPoints().toArray(new Coordinate[]{});
        	
        	
        	String lineType = ((Sigmet)SigmetAttrDlg.this.drawingLayer.getSelectedDE()).getType();
        	
        	if( coors != null ) {
        		IMapDescriptor mapDescriptor = SigmetAttrDlg.this.drawingLayer.getDescriptor();
        		double width = Double.parseDouble(SigmetAttrDlg.this.widthStr);
        		
        		if( SigmetAttrDlg.this.AREA.equals(lineType) ){
        			
        			Coordinate[] coorsP = new Coordinate[coors.length+1];
                	coorsP = (Coordinate[]) Arrays.copyOf(coors, coorsP.length);
                	coorsP[coorsP.length-1] = coors[0];
        				
        			Polygon areaP = SigmetInfo.getPolygon(coorsP, mapDescriptor);
        			fir.append( getFirString(areaP) );   	
        			        			
        		}else if( SigmetAttrDlg.this.ISOLATED.equals(lineType) ){
        			
        			Polygon areaP = SigmetInfo.getIsolatedPolygon(coors[0], width, mapDescriptor);
        			fir.append( getFirString(areaP) );     			
        			
        		}else{//Lines
        			String subLineType = lineType.split(SigmetInfo.LINE_SEPERATER)[1];
        			Polygon areaP = SigmetInfo.getSOLPolygon(coors, subLineType, width, mapDescriptor);
        			fir.append( getFirString(areaP) );    			      			
        		}
        	}
			return fir.toString();
		}
		
		private String getFirString(Polygon areaP){			
			StringBuilder fir = new StringBuilder();
			Map<String, Polygon>  FIR_POLYGON_MAP = SigmetInfo.initFirPolygonMapFromShapfile();
			
			for(String aFir : FIR_POLYGON_MAP.keySet()){
				Polygon firP = FIR_POLYGON_MAP.get(aFir);
				if(firP.covers(areaP) || firP.intersects(areaP)){ 
					if( ! fir.toString().contains(aFir.substring(0,4)) )
						fir.append(aFir.substring(0,4)).append(" ");
				}
			}			
			String firId = fir.toString();
			
			String[] firIdArray = firId.split(" ");
			StringBuilder firNameBuilder = new StringBuilder();	
			for(String id : firIdArray){
				String firName = "";
				for(String s : SigmetInfo.FIR_ARRAY){
					if(id.equals(s.substring(0, 4)) )
						firName = s.substring(5, s.length() );
				}				
					
				String[] ss = firName.split("_");
				for(int i=0; i<ss.length; i++){
					firNameBuilder.append(ss[i]).append(" ");
				}	
				if( ! firId.toString().trim().equals(""))firNameBuilder.append(" FIR ");
			}
			
			return firCalledForSecondLine ? firNameBuilder.toString() : firId;
		}
		
		public HashMap<String, Object> getAttrFromDlg(){ return new HashMap<String,Object>();}
		public void setAttrForDlg(IAttribute ia){}
	}

	public gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement getSigmet() {
		return sigmet;
	}

	public void setSigmet(gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement sigmet) {		
		this.sigmet = (Sigmet)sigmet;		
		Button[] buttons = (Button[]) attrButtonMap.get("editableAttrFromLine");
		Coordinate[] coors = ((Sigmet)sigmet).getLinePoints();
		String s = "";
		
		for(int i=0; buttons!=null && i<buttons.length; i++){
			Button btn = buttons[i];
			if(btn!=null&&(!btn.isDisposed())&&btn.getSelection()&&btn.getText()!=null&&btn.getText().length()>0){
				if(btn.getText().contains("VOR")) 
					s = this.getVOR(coors); 
				else //if(btn.getText().contains("New")) 
					s = getLatLonStringPrepend2(coors, AREA.equals( ((Sigmet)sigmet).getType() ) );
				//else 	
					//s = PgenUtil.getLatLonStringPostpend(coors, AREA.equals( ((Sigmet)sigmet).getType() ) );
			}
		}
		if(txtInfo != null && ! txtInfo.isDisposed() && s != null )			 
			this.resetText(s, txtInfo);
		
	}

	private void init(){  
		
		Sigmet sigmet = (Sigmet)this.getSigmet();
		if(sigmet != null) copyEditableAttrToSigmetAttrDlg(sigmet);
			
		Field[] fields = this.getClass().getDeclaredFields();
		for(Field f : fields){
			String attr = f.getName();
			String typeValue = "";
			try{	typeValue = (String) f.get(this);	}catch(Exception e){	}			
			Control cont = attrControlMap.get(attr);
						
			if(cont instanceof Combo || cont instanceof Spinner || cont instanceof Text){				
				if( ! cont.isDisposed() ) {
					if(attr.equals("editableAttrFromLine")){
						this.resetText(typeValue, (Text)cont);
					}else{
						setControl(cont, attr);	
					}
				}
				
				if("editableAttrFromLine".equals(attr)&&( ! cont.isDisposed() )&& typeValue != null){//New, Old, VOR Buttons 
					Button[] butts = attrButtonMap.get(attr);
					//String firstWord = typeValue.split(" ")[0];
					//char firstChar = firstWord.charAt(0), lastChar = firstWord.charAt(firstWord.length()-1);
					
					String[] words = typeValue.split(SigmetInfo.LINE_SEPERATER);
					String lastPart = words[words.length-1];
					
					if(butts != null){
						if("New".equals(lastPart)){//firstChar == 'N' || firstChar == 'S'){//New
							butts[0].setSelection(true);
							butts[1].setSelection(false);
							//butts[2].setSelection(false);							
						}else if("Old".equals(lastPart)){//lastChar == 'W' || lastChar == 'E') {//Old
							butts[0].setSelection(false);
							butts[1].setSelection(true);
							//butts[2].setSelection(false);
						}else{//VOR
							butts[0].setSelection(false);
							butts[1].setSelection(true);//false);
							//butts[2].setSelection(true);
						}
					}
				}
								
				if("lineType".equals(attr)  ){//Area, Line, Isolated
					Button[] butts = attrButtonMap.get(attr);					
					if(butts != null){
						for(Button butt : butts){
							if(butt != null && typeValue.contains(butt.getText().trim())) {
								butt.setSelection(true);
								butt.notifyListeners(SWT.Selection, new Event());
								for(Button butt2 : butts) if(butt2 != butt) butt2.setSelection(false);								
								break;
							}
						}
					}					
				}				
			}
				///*
			if( "editableAttrMovement".equals(attr)	|| "editableAttrStatus".equals(attr) ) {//Buttons				

				Button[] butts = null;
			
				if( ("editableAttrMovement".equals(attr)&&this.withExpandedArea) ){// STNRY,MVG
					butts = attrButtonMap.get(attr);
					if(butts != null){
						for(Button butt : butts){
							if(butt != null && ! butt.isDisposed() && typeValue != null && typeValue.contains(butt.getText().trim())) {
								butt.setSelection(true);
								for(Button butt2 : butts) if(butt2 != butt) butt2.setSelection(false);
								break;
							}
						}
					}
					continue;
				}
				
				if("editableAttrStatus".equals(attr)&&this.withExpandedArea){//New/Update,Amend,Cancel
					butts = attrButtonMap.get(attr);
					if(butts != null && typeValue != null){
						char status = typeValue.charAt(0);
						switch(status) {
						case '0': butts[0].setSelection(true); butts[1].setSelection(false);butts[2].setSelection(false);break;
						case '1': butts[1].setSelection(true); butts[0].setSelection(false);butts[2].setSelection(false);break;
						case '2': butts[2].setSelection(true); butts[1].setSelection(false);butts[0].setSelection(false);break;
						default : butts[0].setSelection(true); }
					}
					continue;
				}			
							
			}//*/			
			
		}
		tropCycFlag=false;//copiedToSigmet = false;//reset
		withExpandedArea = false;// ???
	}
	
	private boolean validateLatLon(String coor, boolean isLat){
        String regexLat = "(-?[0-8]?[0-9](\\.\\d*)?)|-?90(\\.[0]*)?";
        String regexLon = "(-?([1]?[0-7][1-9]|[1-9]?[0-9])?(\\.\\d*)?)|-?180(\\.[0]*)?";

        java.util.regex.Matcher m;
        if( isLat) { m = java.util.regex.Pattern.compile(regexLat).matcher(coor);      }
        else {        m = java.util.regex.Pattern.compile(regexLon).matcher(coor);     }

        //System.out.println(m.matches());
        return m.matches();
    }
	
	private boolean validateNumInput(String num){
		try{	Double.parseDouble(num); }catch(Exception e){	return false;	}
		return true;
	}
	
	public static String latlonWithSpace(String s){
		if(s == null || s.length() < 1)
			return "";
		
		String rr = "";
		if( s.length() > 0){
			char r = s.charAt(0);
			
			if(r=='N' || r=='S'){
				rr=latlonAddSpace(s, true);
			}else{
				char e = s.charAt(s.length()-1);
				if(e=='W' || e=='E'){
					rr = latlonAddSpace(s, false);
				}
			}
		}
		
		return rr;
	}
	
	public static String latlonAddSpace(String s, boolean isHeadNS){
		if( isHeadNS ){
			if(s.contains("W"))
				return s.replace("W", " W");
			else if(s.contains("E"))
				return s.replace("E", " E");
		}else{
			if(s.contains("N"))
				return s.replace("N", "N ");
			else if(s.contains("S"))
				return s.replace("S", "S ");			
		}
		
		return "";		
	}
	
	public static final String getLatLonStringPrepend2(Coordinate[] coors, boolean isLineTypeArea){
		
		String paddedDash = " - ";
		
		String FOUR_ZERO = "0000", FIVE_ZERO = "00000";
		
    	StringBuilder result = new StringBuilder();
    	for(int i=0; i<coors.length; i++){//Coordinate coor : coors){
    		Coordinate coor = coors[i];
    		    		
    		result.append(coor.y>=0 ? "N":"S");      		
    		int y = (int)Math.abs(coor.y*100);  
    		result.append( new DecimalFormat(FOUR_ZERO).format(y) );
    		
    		result.append(coor.x>=0 ? " E":" W");    		
    		int x = (int)Math.abs(coor.x*100);
    		result.append( new DecimalFormat(FIVE_ZERO).format(x) );     		
    		
    		if( i < (coors.length-1) ) result.append(paddedDash);
    	}    
    	
    	if( isLineTypeArea )	
    		result.append(paddedDash).append(result.toString().split(paddedDash)[0]);    	    		
    	
    	return result.toString();
	}
	
	
	public static String[] getPhenomenons(String type){
		
	       java.util.Set<String> set = new java.util.LinkedHashSet<String>();
		  	             
		   if(type==null || type.isEmpty())	
			   return set.toArray(new String[]{});
	       
		   java.util.logging.Logger log = java.util.logging.Logger.getAnonymousLogger();
		   log.setLevel(java.util.logging.Level.SEVERE);  
		   
		   org.w3c.dom.NodeList nlist = null;      
		   
		   File file = PgenStaticDataProvider.getProvider().getStaticFile( 
				   PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "phenomenons.xml" );
	             
		   try {            	 
			   org.w3c.dom.Document doc  = 
				   javax.xml.parsers.DocumentBuilderFactory.newInstance().
				            newDocumentBuilder().parse(file.getAbsoluteFile());	           
			   nlist = doc.getElementsByTagName(type);
	           
			   if(nlist!=null && nlist.getLength()>0){ 
	        	   nlist = nlist.item(0).getChildNodes();
			   
				   for(int i=0;  nlist!=null && i<nlist.getLength(); i++){	
					   String phenom = nlist.item(i).getTextContent();

					    
					   
					   if( phenom !=null && (phenom.trim().length() > 0)){
						   set.add(phenom.trim()); 
						   //log.info("_____ set size: "+set.size()+" phenom length: "+phenom.length());
					   }
		           }  
			   }
		   } catch (Exception e) { log.log(java.util.logging.Level.SEVERE, "___SigmetAttrDlg: getPhenomenons(): "+e.getMessage()); }	   
	       
		   if(set.size() == 0)
			   set.add("--");
		   
		   return set.toArray(new String[]{});
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
	public Boolean isClosedLine() {
		// TODO Auto-generated method stub
		return null;
	}
	
	private String getPhenomLatLon(String input, boolean isLat) {		
		
		if (input.startsWith("S") || input.startsWith("s") || input.startsWith("W") || input.startsWith("w")
				|| input.endsWith("S") || input.endsWith("s") || input.endsWith("W") || input.endsWith("w"))
			input = "-" + input;
    	input = input.replaceAll("[^-0-9.]","");
    	
    	StringBuilder result = new StringBuilder();
		if ( !"".equals(input) && !"-".equals(input) && validateLatLon(input, isLat) ) {
			Double value = Double.parseDouble(input);
			
	    	if (isLat) {
	    		result.append(value>=0.0 ? "N":"S");   
	    		double y = (double)Math.abs(value);
	        	result.append(y);      
	    	}    		
	    	else {
	    		result.append(value>=0.0 ? " E":" W");     		
	        	double x = (double)Math.abs(value);
	        	result.append(x);
	    	}
	    
	    	return result.toString().trim();
		} 
		else 
			return "";
	}
	
	public Sigmet convertType(Sigmet newEl) {
		 
		String origLineType = this.getOrigLineType(); 
		String newLineType = this.getLineType();
				
		if (!newLineType.equals(origLineType)) {
			
			float p45 = 45.0F, p135 = 135.0F, p225 = 225.0F, p315 = 315.0F;
			
			ArrayList<Coordinate> ptsCopy = newEl.getPoints();
			ArrayList<Coordinate> newPtsCopy = new ArrayList<Coordinate>();
					
			if (ISOLATED.equals(origLineType)) {    /* converting from a point (Isolated) */
				
				Coordinate centerCoor = ptsCopy.get(0);
			
				if (newLineType.startsWith(LINE)) { /* to a Line */	
					 /*
				     * 3 point diagonal with original point as middle point
				     */
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(widthStr), p315));
					newPtsCopy.add(centerCoor);
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(widthStr), p135));					
				}
				else {                              /* to an Area */	
					/*
				     * square centered around original point
				     */
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(widthStr), p45));
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(widthStr), p135));		
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(widthStr), p225));
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(widthStr), p315));		
				}			

				newEl.setPoints(newPtsCopy);
			}
			else if (ISOLATED.equals(newLineType)) { /* converting to a point (Isolated) */
				newPtsCopy.add(ptsCopy.get(0));	
				newEl.setPoints(newPtsCopy);
			}
		}
		
		setSigmetFromLine(newEl);
		return newEl;
	}
	
	public void setSigmetFromLine(gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement sigmet) {		
		this.sigmet = (Sigmet)sigmet;		
		Button[] buttons = (Button[]) attrButtonMap.get("editableAttrFromLine");
		Coordinate[] coors = ((Sigmet)sigmet).getLinePoints();
		StringBuilder s = new StringBuilder(); 
		 
		for(int i=0; buttons!=null && i<buttons.length; i++){
			Button btn = buttons[i];
			if(btn!=null&&(!btn.isDisposed())&&btn.getSelection()&&btn.getText()!=null&&btn.getText().length()>0){
				if(btn.getText().contains("VOR")) {
					s.append(this.getVOR(coors)); 
					s.append(SigmetInfo.LINE_SEPERATER);
		        	String latLonFmtText = s.append("VOR").toString();
		        	SigmetAttrDlg.this.setLatLonFormatFlagAndText(latLonFmtText);
		        	SigmetAttrDlg.this.setEditableAttrFromLine(latLonFmtText);
				}
				else {
				   s.append(getLatLonStringPrepend2(coors, AREA.equals( ((Sigmet)sigmet).getType() ) ));
				   s.append(SigmetInfo.LINE_SEPERATER);
	    		   String latLonFmtText = s.append("New").toString();
	    		   SigmetAttrDlg.this.setLatLonFormatFlagAndText(latLonFmtText);
	    		   SigmetAttrDlg.this.setEditableAttrFromLine(latLonFmtText);
				}
			}
		}
		if(txtInfo != null && ! txtInfo.isDisposed() && s != null )			 
			this.resetText(s.toString(), txtInfo);
		
	}

	public void copyEditableAttrToSigmetAttrDlg(Sigmet sig){
    	this.setLineType(sig.getType());
    	this.setWidthStr(""+sig.getWidth());//legacy properties
        
		this.setEditableAttrFreeText(sig.getEditableAttrFreeText());
		this.setEditableAttrStatus(sig.getEditableAttrStatus());
		this.setEditableAttrStartTime(sig.getEditableAttrStartTime());
		this.setEditableAttrEndTime(sig.getEditableAttrEndTime());
		this.setEditableAttrPhenom(sig.getEditableAttrPhenom());
		this.setEditableAttrPhenom2(sig.getEditableAttrPhenom2());
		this.setEditableAttrPhenomLat(sig.getEditableAttrPhenomLat());
		this.setEditableAttrPhenomLon(sig.getEditableAttrPhenomLon());
		this.setEditableAttrPhenomSpeed(sig.getEditableAttrPhenomSpeed());
		this.setEditableAttrPhenomDirection(sig.getEditableAttrPhenomDirection());
		
		this.setEditableAttrRemarks(sig.getEditableAttrRemarks());
		this.setEditableAttrPhenomName(sig.getEditableAttrPhenomName());
		this.setEditableAttrPhenomPressure(sig.getEditableAttrPhenomPressure());
		this.setEditableAttrPhenomMaxWind(sig.getEditableAttrPhenomMaxWind());
		this.setEditableAttrTrend(sig.getEditableAttrTrend());
		this.setEditableAttrMovement(sig.getEditableAttrMovement());
		this.setEditableAttrLevel(sig.getEditableAttrLevel());
		this.setEditableAttrLevelInfo1(sig.getEditableAttrLevelInfo1());
		this.setEditableAttrLevelInfo2(sig.getEditableAttrLevelInfo2());
		this.setEditableAttrLevelText1(sig.getEditableAttrLevelText1());
		this.setEditableAttrLevelText2(sig.getEditableAttrLevelText2());
		this.setEditableAttrFir(sig.getEditableAttrFir());
        
        String lineType = this.getType();
        if(lineType != null && lineType.contains(SigmetInfo.LINE_SEPERATER)){
        	this.setSideOfLine(lineType.split(SigmetInfo.LINE_SEPERATER)[1]);
        }
       
        this.setWidthStr(""+(sig.getWidth()));//NM
        this.setLatLonFormatFlagAndText(sig.getEditableAttrFromLine());
        
        //from AbstractSigmet: Class.getDeclaredFields() excludes inherited fields.
        this.setEditableAttrArea(sig.getEditableAttrArea());
        this.setEditableAttrIssueOffice(sig.getEditableAttrIssueOffice());
        this.setEditableAttrFromLine(sig.getEditableAttrFromLine());
        this.setEditableAttrId(sig.getEditableAttrId());
        this.setEditableAttrSeqNum(sig.getEditableAttrSeqNum());
        
    }
}