/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.SigmetCommAttrDlg
 * 
 * December 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IMultiPoint;
import gov.noaa.nws.ncep.ui.pgen.sigmet.AbstractSigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.ISigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo;
import gov.noaa.nws.ncep.viz.common.SnapUtil;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;

import java.awt.Color;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.io.WKBReader;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

/**
 * Singleton attribute dialog for ConvSigmet, NonConvSigmet,Airmet, and Outlook.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ---------	--------	----------	--------------------------
 * 12/09		182			G. Zhang 	Initial Creation. 
 * 03/10        231         Archana     Altered the common dialog for  
 *                                      ConvSigmet, NonConvSigmet,Airmet and Outlook
 *                                      to display only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix.
 * 03/10		#223		M.Laryukhin	Refactored getVOR method to be used with gfa too.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 12/11		#526		B. Yin		Close dialog after saving text.
 * 01/12		#597		S. Gurung	Removed Snapping for ConvSigmet
 * 02/12        #597        S. Gurung   Removed snapping for NonConvSigmet. Moved snap functionalities to SnapUtil from SigmetInfo.
 * 03/12        #611        S. Gurung   Fixed ability to change SIGMET type (from Area to Line/Isolated and back and forth)
 * 11/12		#873		B. Yin		Pass sigmet type "CONV_SIGMET" for snapping.
 * </pre>
 * 
 * @author	gzhang
 */

public class SigmetCommAttrDlg extends AttrDlg implements ISigmet{	
	
	private static SigmetCommAttrDlg INSTANCE = null;
	private static String mouseHandlerName = null;
	
	private AbstractSigmet asig = null;

	private HashMap<String, Object> attr = new HashMap<String, Object>( );
	
	public static final String AREA = "Area", LINE = "Line", ISOLATED = "Isolated";
	public static final String LINE_SEPERATER = ":::";
	
	private String lineType = AREA;
	private String origLineType = lineType; 
	private static final String WIDTH = "10.00";
	private String width = WIDTH;
	
	private static final String[] LINE_SIDES = new String[]{"ESOL"};	
	private String sideOfLine = LINE_SIDES[0]; 
		
	protected Composite top = null;
    protected ColorButtonSelector cs = null;
    
    private boolean withExpandedArea = false, copiedToSigmet = false;
    
    private HashMap<String, Control> attrControlMap = new HashMap<String,Control>();
	private HashMap<String, Button[] > attrButtonMap = new HashMap<String,Button[]>();
	private HashMap<Control,Control[]> controlEnablerMap = new HashMap<Control,Control[]>();
	
	private String editableAttrArea = "";
	private String editableAttrFromLine = "";
	private String editableAttrId = "EAST";
	private String editableAttrSequence = "";
	
	private Combo comboMWO = null;	
	private Combo comboID = null;
	private Spinner spiSeq = null;
	private Text txtInfo = null;// for VOR-text and element-selected synchronization
	private String relatedState = "";
	private static Font txtfont; 
		
	protected SigmetCommAttrDlg(Shell parShell) throws VizException {
		super(parShell);		
		if ( txtfont == null ) {
			txtfont = new Font(parShell.getDisplay(),"Courier",12, SWT.NORMAL);
		}
	}
	
	public static SigmetCommAttrDlg getInstance( Shell parShell){		
		if ( INSTANCE == null ){					
			try {
				INSTANCE = new SigmetCommAttrDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}			
		}		
		return INSTANCE;
	}
	
	@Override
    public void createButtonsForButtonBar(Composite parent) { 

    	if("Pgen Select".equals(mouseHandlerName) || withExpandedArea ){
    		createButton(parent, 20091229,"Save",false);//replace OK_ID which causes old sig NOT found error
    		createButton(parent, 20091021,"Apply",false);
    		
    		this.getButton(20091229).setEnabled(true);
    		this.getButton(20091229).addListener(SWT.Selection, new Listener(){
    			public void handleEvent(Event e){				
    				
    				saveApplyPressed();
    								
    				SigmetCommAttrDlgSaveMsgDlg md = null;    				
    				
    				try{ 
    					md = new SigmetCommAttrDlgSaveMsgDlg( getShell() ); 
    				}catch(Exception ee){ System.out.println(ee.getMessage()); }
    				
    				if(md != null){    					
    					md.open();
    					
    				}
    				
    			}
    		});
    		
    		this.getButton(20091021).setEnabled(true);
    		this.getButton(20091021).addListener(SWT.Selection, new Listener(){
    			public void handleEvent(Event e){  			
    				
    				saveApplyPressed(); 				
    			}
    		});
    		
    	}else{
    		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,	true);
    	}
		createButton(parent, IDialogConstants.CANCEL_ID,IDialogConstants.CANCEL_LABEL, false);
		setMouseHandlerName(null);
		withExpandedArea = false;		
	}
	
	@Override
	public void enableButtons(){ 		
		
		this.getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
  		  		
	}
	
	public AbstractSigmet getAbstractSigmet(){
		
		return this.asig;
	}
	
	public void setAbstractSigmet(gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement de){
		this.asig = (AbstractSigmet)de;		
		this.setLineType(this.asig.getType());//when opening from saved files the first time.
		Coordinate[] coors = asig.getLinePoints();	
		String s = this.getVOR(coors); 
		this.setEditableAttrFromLine(s);	
		this.relatedState = this.getRelatedStates(coors, de);
		((AbstractSigmet)de).setEditableAttrFromLine(s);
		if(txtInfo != null && ! txtInfo.isDisposed() && s != null )	
			txtInfo.setText(s); 
	}
	
	private void copyEditableAttrToAbstractSigmet(AbstractSigmet ba){
		
		ba.setColors(SigmetCommAttrDlg.this.getColors());
        ba.setEditableAttrArea(this.getEditableAttrArea());
        ba.setEditableAttrFromLine(this.getEditableAttrFromLine());
        ba.setEditableAttrId(this.getEditableAttrId());
        if(this.getEditableAttrSequence()==null||this.getEditableAttrSequence().length()==0)	
        	this.setEditableAttrSequence("1");
        ba.setEditableAttrSeqNum(this.getEditableAttrSequence());
        ba.setType(this.getLineType()); 
        ba.setWidth(this.getWidth());
        
        copiedToSigmet = true;
		
	}
	
	public Color[] getColors(){		
		  		
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
		return Double.parseDouble(this.width);
	}
	
	public void setWidth(String widthString){
		this.width = widthString;
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

	@Override
	public void setAttrForDlg(IAttribute attr) {
		Color clr = attr.getColors()[0];
		if ( clr != null ) this.setColor(clr);
		
	}	
   
	@Override
	public Control createDialogArea(Composite parent) {
		
	        this.top = (Composite) super.createDialogArea(parent);

	        GridLayout mainLayout = new GridLayout(8, false);
	        mainLayout.marginHeight = 3;
	        mainLayout.marginWidth = 3;
	        top.setLayout(mainLayout);	       
	        
	        if("CONV_SIGMET".equals(this.pgenType) ) this.getShell().setText("Convective SIGMET Edit");
	        else if("NCON_SIGMET".equals(this.pgenType)) this.getShell().setText("Non-convective SIGMET Edit");
	        else if("AIRM_SIGMET".equals(this.pgenType)) this.getShell().setText("AIRMET Edit");
	        else if("OUTL_SIGMET".equals(this.pgenType)) this.getShell().setText("Convective Outlook Edit");
	        
	        if("NCON_SIGMET".equals(this.pgenType) || "AIRM_SIGMET".equals(this.pgenType) || "OUTL_SIGMET".equals(this.pgenType)) {
	        	SigmetCommAttrDlg.this.setLineType(AREA);
	        }
	        
	        final Button btnArea = new Button(top,SWT.RADIO);	        
	        btnArea.setSelection(true); //default 	  
	        btnArea.setText("Area");
	        if(mouseHandlerName == null) setLineType(AREA);//when NOT selecting element	        
	        
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
	        attrControlMap.put("width", txtWidth);
	        txtWidth.setText("10.00");
	        txtWidth.setEnabled(false);
	        attrButtonMap.put("lineType", new Button[]{btnArea,btnLine,btnIsolated});	        
	        
	        btnArea.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		comboLine.setEnabled(false);
	        		txtWidth.setEnabled(false);
	        		
	        		SigmetCommAttrDlg.this.setLineType(AREA);
	        	}
	        });
	        
	        btnLine.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		comboLine.setEnabled(true);
	        		txtWidth.setEnabled(true);
	        		
	        		SigmetCommAttrDlg.this.setLineType(LINE + LINE_SEPERATER + SigmetCommAttrDlg.this.getSideOfLine());
	        	}
	        });
	        
	        btnIsolated.addListener(SWT.Selection, new Listener(){
	        	public void handleEvent(Event e){
	        		comboLine.setEnabled(false);
	        		txtWidth.setEnabled(true);
	        		
	        		SigmetCommAttrDlg.this.setLineType(ISOLATED);
	        	}
	        });	  
	        
	        comboLine.addListener(SWT.Selection,new Listener(){ 
	        	public void handleEvent(Event e){
	        		SigmetCommAttrDlg.this.setSideOfLine(comboLine.getText());
	        		SigmetCommAttrDlg.this.setLineType(LINE + LINE_SEPERATER + SigmetCommAttrDlg.this.getSideOfLine());
	        	}
	        });
	        
	        txtWidth.addListener(SWT.Modify, new Listener(){ 
	        	public void handleEvent(Event e){
	        		SigmetCommAttrDlg.this.setWidth(txtWidth.getText());
	        	}
	        });
	        
	        Label colorLbl = new Label(top, SWT.LEFT);
	        colorLbl.setText("Color:");
	        
            cs = new ColorButtonSelector( top );
	        Color clr = getDefaultColor(this.pgenType); cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()) );//new RGB(0,255,0));  	        
	        
	        if( ! "INTL_SIGMET".equals(pgenType) && ! "CONV_SIGMET".equals(pgenType) ){
	        	btnLine.setEnabled(false);		
	        	btnIsolated.setEnabled(false);
	        	comboLine.setEnabled(false);
	        	txtWidth.setEnabled(false);
	        }
	        
	        
		       if("Pgen Select".equals(mouseHandlerName) || withExpandedArea ){		    	   
		    	   
		    	   String[] MWO_ITEMS = SigmetInfo.AREA_MAP.get(SigmetInfo.getSigmetTypeString(pgenType));
		    	   String[] ID_ITEMS  = SigmetInfo.ID_MAP.get(SigmetInfo.getSigmetTypeString(pgenType));
		    	   	 
		    	   Composite topSelect = (Composite) super.createDialogArea(parent);
		    	   
		    	   GridLayout mainLayout2 = new GridLayout(8,false);
			        mainLayout2.marginHeight = 3;
			        mainLayout2.marginWidth = 3;
			        topSelect.setLayout(mainLayout2);
			        
			        Group top2 = new Group(topSelect,SWT.LEFT);
			        top2.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
			        top2.setLayout(new GridLayout(8,false)); 
		        	
		        	Label lblMWO = new Label(top2, SWT.LEFT);
		        	lblMWO.setText(" MWO: ");
		        	comboMWO = new Combo(top2,SWT.READ_ONLY);
		        	attrControlMap.put("editableAttrArea", comboMWO);
		        	comboMWO.setItems(MWO_ITEMS);  
		        	comboMWO.select(0);	
		        	//this.setEditableAttrArea(comboMWO.getText());		
		        	comboMWO.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,true,1,1));
		        	
		        	comboMWO.addListener(SWT.Selection, new Listener(){
		        		public void handleEvent(Event e){
		        			SigmetCommAttrDlg.this.setEditableAttrArea(comboMWO.getText());
		        		}
		        	});
		        	
		        	Label lblID = new Label(top2, SWT.LEFT);
		        	lblID.setText(" ID: ");
		        	comboID = new Combo(top2,SWT.READ_ONLY);
		        	attrControlMap.put("editableAttrId", comboID);
		        	comboID.setItems(ID_ITEMS);
		        	comboID.select(0);	
		        			
		        	comboID.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,true,1,1));
		        	
		        	comboID.addListener(SWT.Selection, new Listener(){
		        		public void handleEvent(Event e){
		        			SigmetCommAttrDlg.this.setEditableAttrId(comboID.getText());
		        		}
		        	});
		        	
		        	Label lblSequence = new Label(top2, SWT.LEFT);
		        	lblSequence.setText(" Sequence: ");
		        	spiSeq = new Spinner(top2,SWT.BORDER);
		        	attrControlMap.put("editableAttrSeqNum", spiSeq);
		        	spiSeq.setMinimum(1);
		        	spiSeq.setMaximum(300);	
		        	
		        	spiSeq.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,3,1));
		        	
		        	spiSeq.addListener(SWT.Selection, new Listener(){
		        		public void handleEvent(Event e){
		        			SigmetCommAttrDlg.this.setEditableAttrSequence(""+spiSeq.getSelection());
		        		}
		        	});
		        	
			        final Button btnNew = new Button(top2,SWT.RADIO);
			        //btnNew.setSelection(true);	
			        btnNew.setText("New(Prepend dir)");
			        btnNew.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,2,1));
			        btnNew.setEnabled(false);	
			        
			        final Button btnOld = new Button(top2,SWT.RADIO);
			        btnOld.setText("Old(Postpend dir)");
			        btnOld.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,2,1));
			        btnOld.setEnabled(false);
			        
			        final Button btnVor = new Button(top2,SWT.RADIO);
			        btnVor.setText("VOR");
			        btnVor.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER,true,false,4,1));
			        btnVor.setEnabled(true);
			        btnVor.setSelection(true);	
			        
				        int style = SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.READ_ONLY;
				       txtInfo = new Text( top2, style );
				       txtInfo.setFont( txtfont);
				       attrControlMap.put("editableAttrFromLine", txtInfo);
				       GridData gData = new GridData(600,48);
				       gData.horizontalSpan = 8;
				       txtInfo.setLayoutData( gData );
				       txtInfo.setText(this.getEditableAttrFromLine());				      
				       if(editableAttrFromLine==null || editableAttrFromLine.equals(""))  
				    	   this.setEditableAttrFromLine(txtInfo.getText());
				       				      
				       attrButtonMap.put("editableAttrFromLine", new Button[]{btnNew,btnOld,btnVor });
		        	
				       final StringBuilder coorsLatLon = new StringBuilder(); 	        	
				       final AbstractDrawableComponent elSelected = PgenSession.getInstance().getPgenResource().getSelectedComp();
		        	   final Coordinate[] coors = (elSelected == null) ? null : elSelected.getPoints().toArray(new Coordinate[]{});
		        	
				       if( coors != null ) {	
				    	   if(editableAttrFromLine==null || editableAttrFromLine.equals("")){
				    		   	        	
				    		   StringBuilder sb = new StringBuilder();
					           sb.append(getVOR(coors ));
				    		   txtInfo.setText(sb.toString());					        	
				    		   
				    		   coorsLatLon.append(SigmetCommAttrDlg.LINE_SEPERATER);
				    		   String latLonFmtText = coorsLatLon.append("VOR").toString();
				    		   
				    	   }
			        	
				       }
				       if(this.asig == null) {		
				    	    this.setEditableAttrArea(comboMWO.getText());
					        this.setEditableAttrId(comboID.getText());
					        this.setEditableAttrSequence(""+spiSeq.getSelection());
					   }    
				       
		     
		       }
		       
		    if(this.asig == null) {
		    	this.setLineType(AREA);	        
			    this.setSideOfLine(comboLine.getText());
			    this.setWidth(txtWidth.getText());			    
			}   
		    init();
	        return top;
	}
	
    private String getVOR(Coordinate[] coors){
    	boolean isSnapped = false;//"NCON_SIGMET".equals(pgenType)||"OUTL_SIGMET".equals(pgenType)||"CONV_SIGMET".equals(pgenType);
		String vorConnector = ("NCON_SIGMET".equals(pgenType)||"AIRM_SIGMET".equals(pgenType))? " TO " : "-";

		/*if("OUTL_SIGMET".equals(pgenType)) {//20100824 outlook GUI NOT snapped but text is with 16 pts
			ArrayList<Coordinate> p = SigmetInfo.getSnapWithStation(Arrays.asList(coors), SigmetInfo.VOR_STATION_LIST, 10, 16); 
			return SigmetInfo.getVORText(p.toArray(new Coordinate[]{}), vorConnector, lineType, 6, true);
		}  */  	
		if("OUTL_SIGMET".equals(pgenType) || "CONV_SIGMET".equals(pgenType) ||"NCON_SIGMET".equals(pgenType) ) {
			return SnapUtil.getVORText(coors, vorConnector, lineType, 6, isSnapped, true, false, pgenType);
		} 
		return SnapUtil.getVORText(coors, vorConnector, lineType, 6, isSnapped);
    }
    
	private void init(){
		if(this.asig == null) return;
		else{
			Button[] btns = attrButtonMap.get("lineType");
			if(btns != null){
				if(lineType.equals(AREA)|| "NCON_SIGMET".equals(pgenType) || "AIRM_SIGMET".equals(pgenType) || "OUTL_SIGMET".equals(pgenType)){ 
					btns[0].setSelection(true); 
					btns[1].setSelection(false); 
					btns[2].setSelection(false); 
				}else if(lineType.contains(LINE)){ 
					btns[0].setSelection(false); 
					btns[1].setSelection(true);
					btns[2].setSelection(false); 
					
					attrControlMap.get("lineType").setEnabled(true);
					attrControlMap.get("width").setEnabled(true);
				}else if(lineType.equals(ISOLATED)){ 
					btns[0].setSelection(false); 
					btns[1].setSelection(false); 
					btns[2].setSelection(true); 
					
					attrControlMap.get("width").setEnabled(true);
				}
			}
			
			Combo comboLine = (Combo) attrControlMap.get("lineType"); 
			String lt = this.getLineType();
			if(comboLine != null && ! comboLine.isDisposed() && lt != null && lt.contains(this.LINE_SEPERATER)) {
				if(lt.length() > 7) 
					comboLine.setText(lt.substring(7));
				else
					comboLine.select(0);
			}
			
			Text txtWidth = (Text) attrControlMap.get("width");
			String width = this.width == null ? "10.00" : this.width;
			if(txtWidth != null && ! txtWidth.isDisposed()) txtWidth.setText(width);
			
			Combo comboMWO = (Combo) attrControlMap.get("editableAttrArea");
			if(comboMWO !=null && ! comboMWO.isDisposed() && this.getEditableAttrArea() != null){
				String area = this.getEditableAttrArea();
				if(area != null && area.length() > 0)
					comboMWO.setText(area);
				else
					comboMWO.select(0);
			}
		
			Combo comboId = (Combo) attrControlMap.get("editableAttrId");
			if(comboId != null && ! comboId.isDisposed() && this.getEditableAttrId() != null) {
				String id = this.getEditableAttrId();
				if( id != null && id.length() > 0)
					comboId.setText(id);
				else 
					comboId.select(0);
			}
			
			Spinner seq = (Spinner) attrControlMap.get("editableAttrSeqNum");
			if( seq != null && ! seq.isDisposed() && this.getEditableAttrSequence() != null){
				String seqAttr = this.getEditableAttrSequence();
				int i = 0;
				try{ i = Integer.parseInt(seqAttr);}catch(Exception e){ i=0;}
				seq.setSelection(i);
			}
			
		}
	}
	
	public String getEditableAttrArea() {
		return editableAttrArea;
	}

	public void setEditableAttrArea(String editableAttrArea) {
		this.editableAttrArea = editableAttrArea;
	}

	public String getEditableAttrFromLine() {
		return editableAttrFromLine;
	}

	public void setEditableAttrFromLine(String editableAttrFromLine) {
		this.editableAttrFromLine = editableAttrFromLine;
	}

	public String getEditableAttrId() {
		return editableAttrId;
	}

	public void setEditableAttrId(String editableAttrId) {	
		this.editableAttrId = editableAttrId;	
	}

	public String getEditableAttrSequence() {
		return editableAttrSequence;
	}

	public void setEditableAttrSequence(String editableAttrSequence) {
		this.editableAttrSequence = editableAttrSequence;
	}
	
	private class SigmetCommAttrDlgSaveMsgDlg extends AttrDlg{
			
			Text txtInfo;
			Text txtSave;
			
			String dirLocal = ".";
			
			boolean firCalledForSecondLine = false;
			
			SigmetCommAttrDlgSaveMsgDlg(Shell parShell) throws VizException {
				super(parShell);		
			}
			
			public HashMap<String, Object> getAttrFromDlg(){
				HashMap<String,Object> attr = null;
		    	return attr;
			}
			
			@Override
		    public void createButtonsForButtonBar(Composite parent) { 			
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
			public void okPressed() {
				      
				FileTools.writeFile(PgenUtil.getPgenActivityTextProdPath()+File.separator+txtSave.getText(), 
						txtInfo.getText());

					setReturnCode(OK);
					close();
					SigmetCommAttrDlg.this.drawingLayer.removeSelected();
			    	SigmetCommAttrDlg.this.close();
					PgenUtil.setSelectingMode();

			}
	
			@Override
			public void setAttrForDlg(IAttribute ia) {
				
				
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
			    txtInfo.setFont(txtfont);
			    GridData gData = new GridData(512,300);
			    gData.horizontalSpan = 3;
			    txtInfo.setLayoutData( gData );	    
		        txtInfo.setText(getFileContent());
		        
		        txtSave = new Text(top, SWT.BORDER | SWT.READ_ONLY);
		        txtSave.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,3,1));
		        txtSave.setText(getFileName());	
				
				return top;
			}
			
			private String getFileContent(){
				String s = "From "+ SigmetCommAttrDlg.this.getEditableAttrFromLine();
				if(s == null || s.contains("null"))
					s = "";
				
				if("NCON_SIGMET".equalsIgnoreCase(SigmetCommAttrDlg.this.pgenType)
						|| "AIRM_SIGMET".equalsIgnoreCase(SigmetCommAttrDlg.this.pgenType) 
						|| "OUTL_SIGMET".equalsIgnoreCase(SigmetCommAttrDlg.this.pgenType) )
					return s.toUpperCase();
				
				return SigmetCommAttrDlg.this.relatedState + "\n" + s.toUpperCase();
			}
			
			private String getFileName(){
				String s = "";
				if("CONV_SIGMET".equalsIgnoreCase(SigmetCommAttrDlg.this.pgenType))
					s = SigmetCommAttrDlg.this.getAbstractSigmet().getTopText();
				else if("OUTL_SIGMET".equalsIgnoreCase(SigmetCommAttrDlg.this.pgenType))
					s = SigmetCommAttrDlg.this.getAbstractSigmet().getTopText() + "O";//TTR#111 upper case letter o, NOT number zero
				else
					s = SigmetCommAttrDlg.this.editableAttrId+"_"+SigmetCommAttrDlg.this.editableAttrSequence;
				
				if(s == null || s.contains("null"))
					s = "";
				else
					s=s+".from";
				return s;
			}
	}	
	
	private String getRelatedStates(Coordinate[] c1, DrawableElement de){
		
		StringBuilder sb = new StringBuilder();		
		
		String lineType = ((Sigmet)de).getType();//20100810 PgenSession.getInstance().getPgenResource().getSelectedDE()).getType();
		Polygon cSigPoly = null;
		com.raytheon.uf.viz.core.map.IMapDescriptor mapDescriptor = PgenSession.getInstance().getPgenResource().getDescriptor();		
    	if( c1 != null ) {
    		
    		double width = Double.parseDouble(SigmetCommAttrDlg.this.width);
    		
    		if( SigmetCommAttrDlg.this.AREA.equals(lineType) ){
    			
    			Coordinate[] coorsP = new Coordinate[c1.length+1];
            	coorsP = (Coordinate[]) Arrays.copyOf(c1, coorsP.length);
            	coorsP[coorsP.length-1] = c1[0];
    				
    			cSigPoly = SigmetInfo.getPolygon(coorsP, mapDescriptor);
   			   	
    			        			
    		}else if( SigmetCommAttrDlg.this.ISOLATED.equals(lineType) ){
    			
    			cSigPoly = SigmetInfo.getIsolatedPolygon(c1[0], width, mapDescriptor);
    			    			
    			
    		}else{//Lines
    			String subLineType = lineType.split(SigmetCommAttrDlg.this.LINE_SEPERATER)[1];
    			cSigPoly = SigmetInfo.getSOLPolygon(c1, subLineType, width, mapDescriptor);    			   			      			
    		}
    	}
    	
    	List<String> s = getAreaStringList(cSigPoly, mapDescriptor, "state", "bounds.statebnds");
    	List<String> l = getAreaStringList(cSigPoly, mapDescriptor, "id", "bounds.greatlakesbnds");
    	List<String> c = getAreaStringList(cSigPoly, mapDescriptor, "id", "bounds.adjcstlbnds");
    	
    	return getStates(s,l,c);		
/*		
    	WKBReader wkbReader = new WKBReader();
		
		for (Object[] state : SigmetInfo.ALL_STATES) {

			byte[] wkb = (byte[]) state[0];			
			
			MultiPolygon stateGeo = null;
			
			try {
				stateGeo = (MultiPolygon) wkbReader.read(wkb);
			} catch (Exception e) {				
				System.out.println("++++++++++++ read geometry exception: "+e.getMessage());
			}

			if (  stateGeo != null ){
				for(int i=0; i<stateGeo.getNumGeometries(); i++){
					Geometry g =  stateGeo.getGeometryN(i);					
					if(g != null){
						Coordinate[] cc = g.getCoordinates();
						Coordinate[] ccc = new Coordinate[cc.length+1];
						ccc = (Coordinate[]) Arrays.copyOf(cc, cc.length);
						ccc[ccc.length-1] = cc[0];
						
						Polygon sp = SigmetInfo.getPolygon(ccc, mapDescriptor);						
						if (cSigPoly.intersects(sp)||cSigPoly.covers(sp)||cSigPoly.within(sp) ){
							String s = (String) state[1];
							if( s != null && (! sb.toString().contains(s)) ) sb.append(s).append(" ");
						}
					}
					
				}
			}
		}
		//sb.append("\n").append("From ");
		return sb.toString();
*/	}	
	
	public void saveApplyPressed() {		
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
			
					attrUpdate();
					
					copyEditableAttrToAbstractSigmet(newEl);
					
					// Change type and update From line
					newEl = convertType(newEl);
										
					this.setAbstractSigmet(newEl);					
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
	
	private void attrUpdate(){
		this.setEditableAttrArea(this.comboMWO.getText());
		this.setEditableAttrId(this.comboID.getText());
		this.setEditableAttrSequence(this.spiSeq.getText());
	}
	
	public void setMouseHandlerName(String mhName){
		mouseHandlerName = mhName;
	}
	
	static Color getDefaultColor(String pType){
		if("INTL_SIGMET".equalsIgnoreCase(pType)) 		return Color.cyan;
		else if("CONV_SIGMET".equalsIgnoreCase(pType)) 	return Color.yellow;
		else if("AIRM_SIGMET".equalsIgnoreCase(pType)) 	return Color.green;
		else if("NCON_SIGMET".equalsIgnoreCase(pType))	return Color.magenta;
		else if("OUTL_SIGMET".equalsIgnoreCase(pType))	return Color.orange;
		
		return Color.green;
	}
	
	//"id","bounds.greatlakesbnds"
	//"id","bounds.adjcstlbnds"
	public static List<Object[]> initAllStates(String field, String table){
		return PgenStaticDataProvider.getProvider().queryNcepDB(field, table);
	}
	
	public static List<String> getAreaStringList(Polygon cSigPoly, IMapDescriptor mapDescriptor, String field, String table){
		List<String> list = new ArrayList<String>();
		
		WKBReader wkbReader = new WKBReader();
		
		for (Object[] state : initAllStates(field,table)){//SigmetInfo.ALL_STATES) {//

			byte[] wkb = (byte[]) state[0];			
			
			MultiPolygon stateGeo = null;
			
			try {
				stateGeo = (MultiPolygon) wkbReader.read(wkb);
			} catch (Exception e) {				
				System.out.println("___ Error: SigmetCommAttrDlg: getAreaString(): "+e.getMessage());
			}

			if (  stateGeo != null ){
				for(int i=0; i<stateGeo.getNumGeometries(); i++){
					Geometry g =  stateGeo.getGeometryN(i);					
					if(g != null){
						Coordinate[] cc = g.getCoordinates();
						Coordinate[] ccc = new Coordinate[cc.length+1];
						ccc = (Coordinate[]) Arrays.copyOf(cc, cc.length);
						ccc[ccc.length-1] = cc[0];
						
						Polygon sp = SigmetInfo.getPolygon(ccc, mapDescriptor);						
						if (cSigPoly.intersects(sp)||cSigPoly.covers(sp)||cSigPoly.within(sp) ){
							String s = (String) state[1];
							if( s != null && ( ! list.contains(s)) ) 
								list.add(s.toUpperCase());
						}
					}
					
				}
			}
		}
		
		return list;
	}
	
	public static String getStates(List<String> states, List<String> lakes, List<String> coastal){		
		
		//NO for all
		if( (states==null || states.size()==0) 
				&& (lakes==null || lakes.size()==0) 
				&& (coastal==null || coastal.size()==0)){
			
			return "";
		}
				
		// NO lakes & coastal together without states involved
		if(states==null || states.size()==0){
			if(lakes==null || lakes.size()==0)
				return list2String(coastal)+" CSTL WTRS";
			
			return list2String(lakes);
		}		
		
		//NO coastal
		if(coastal==null || coastal.size()==0){
			if(states != null){
				states.addAll(lakes);
				return list2String(states);
			}
			
			return list2String(lakes);
		}
		
		//states and coastal all exist
		if(states.size() != coastal.size()){
			
			states.addAll(lakes);
			
			return 	list2String(states)
					+ " AND "
					+ list2String(coastal)
					+ " CSTL WTRS";
		}else{// need to see if states and coastal are the same
			
			if(is2ListSame(states, coastal))
				return list2String(states)+ " AND CSLT WTRS";
			
			states.addAll(lakes);
			return list2String(states)+" AND "+list2String(coastal)+" CSTL WTRS";
		}
		
	}
	
	public static boolean is2ListSame(List<String> s, List<String> c){
		
		for(String ss : s){
			if( ! c.contains(ss))
				return false;
		}
		
		return true;
	}
	
	
	public static String list2String(List<String> s){
		
		if(s == null || s.size() == 0){
			return "";
		}
		
		StringBuilder sb = new StringBuilder();
		
		for(String ss : s){
			sb.append(ss).append(" ");
		}
		
		return sb.toString().trim();
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
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(width), p315));
					newPtsCopy.add(centerCoor);
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(width), p135));					
				}
				else {                              /* to an Area */	
					/*
				     * square centered around original point
				     */
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(width), p45));
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(width), p135));		
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(width), p225));
					newPtsCopy.add(PgenUtil.computePoint(centerCoor, Float.parseFloat(width), p315));		
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
		Coordinate[] coors = ((Sigmet)sigmet).getLinePoints();
		
		StringBuilder s = new StringBuilder(); 
		s.append(this.getVOR(coors)); 
		s.append(SigmetInfo.LINE_SEPERATER);    	
		String fromLineText = s.append("VOR").toString();
    	SigmetCommAttrDlg.this.setEditableAttrFromLine(fromLineText);
    	
		if(txtInfo != null && ! txtInfo.isDisposed() && s != null )			 
			this.txtInfo.setText(s.toString());		
	}

	public void copyEditableAttrToSigmetAttrDlg(AbstractSigmet sig){
		this.setEditableAttrArea(sig.getEditableAttrArea());
		//sigDlg.setEditableAttrIssueOffice(this.getEditableAttrIssueOffice()); only intlSig need this
		this.setEditableAttrFromLine(sig.getEditableAttrFromLine());
		this.setEditableAttrId(sig.getEditableAttrId());
		this.setEditableAttrSequence(sig.getEditableAttrSeqNum());
		this.setLineType(sig.getType());
		this.setWidth(""+(sig.getWidth()));
	}	
	
}
