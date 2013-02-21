/*
 * gov.noaa.nws.ncep.ui.pgen.pgen.attrDialog.vaaDialog
 * 
 * Janurary 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;

import java.awt.Color;
import java.util.*;

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
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;

import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.*;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ISinglePoint;
import gov.noaa.nws.ncep.ui.pgen.elements.*;
import gov.noaa.nws.ncep.ui.pgen.sigmet.*;
import gov.noaa.nws.ncep.ui.pgen.*;
import gov.noaa.nws.ncep.ui.pgen.tools.*;

import com.raytheon.uf.viz.core.exception.VizException;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * The class for Volcano create dialog 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#165		G. Zhang   	Initial Creation.
 *
 * 04/11		#?			B. Yin			Re-factor IAttribute
 * 11/12		#884		B. Yin 		Set selecting mode after creation
 * </pre> 
 * @author	G. Zhang
 */

public class VolcanoCreateDlg extends AttrDlg implements ISinglePoint{
	
	/**
	 * singleton instance of this class
	 */
	private static VolcanoCreateDlg INSTANCE;
	
	/**
	 * top Composite of this dialog
	 */
	protected Composite top = null;
	
	/**
	 * Text fields for volcano name.
	 */
	Text txtVol=null;
	
	/**
	 * Text fields for volcano number.
	 */
	Text txtNum=null;
	
	/**
	 * Text fields for volcano location.
	 */
	Text txtLoc=null;
	
	/**
	 * Text fields for volcano elevation.
	 */
	Text txtEle=null;
	
	/**
	 * Text fields for volcano area.
	 */
	Text txtArea=null;	
	
	/**
	 * Volcano creating button
	 */
	Button btnVol=null;
	
	/**
	 * Volcano creating with Layers button
	 * NOT used but keeping here for reference
	 */
	Button btnLay=null;
	
	/**
	 * each Volcano is treated as a Station when loading from xml file
	 */
	private Station stn = null;
	
	/**
	 * Volcano creating class that calls this dialog 
	 */
	private PgenVolcanoCreateTool tool = null;
	
	/**
	 * the Volcano to be created
	 */
	private Volcano elem = null;
	
	/**
	 * flag for setting Station	
	 */
	private boolean setStnFlag = false;
	
	/**
	 * flag for the Volcano info sources 
	 * from file or from user manual input
	 */	
	private boolean isManualInput = false;
	
	
	/**
	 * constructor of this class.
	 * @param Shell: the parent Shell of this dialog
	 * @throws VizException
	 */
	public VolcanoCreateDlg(Shell parShell) throws VizException {
		
        super(parShell);        
                
	}
	
	/**
	 * singleton creation method for this class.
	 * @param Shell: parent Shell for this dialog.
	 * @return: the singleton instance of this class.
	 */
	public static VolcanoCreateDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				INSTANCE = new VolcanoCreateDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}
			
		}
		
		return INSTANCE;		
	}
	
	public HashMap<String, Object> getAttrFromDlg(){ return new HashMap<String,Object>(); }
	public void setAttrForDlg(IAttribute ia){ }
	
	/**
	 * method overridden of super class to
	 * create dialog area for this class
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		this.top = (Composite) super.createDialogArea(parent);
		
		this.getShell().setText("VAA Volcano Create");
		
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
		
		Group top5 = new Group(top,SWT.LEFT);
        top5.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,true,8,1));
        top5.setLayout(new GridLayout(8,false));			
		createArea5(top5);
		
		/*
		 * handling the listeners and validations
		 */
		new TextFieldListener(
			new Text[]{txtArea,txtEle,txtLoc,txtNum,txtVol},
			new Control[]{btnVol}//2010-03-17: taking off btnLay using Product Center
		);
		
		return top;
	}
	
	/**
	 * create part of the dialog area.
	 * @param top2
	 */
	private void createArea1(Group top2){
		
		Label lblVol = new Label(top2, SWT.LEFT);
		lblVol.setText("Volcano: ");
		
		txtVol = new Text(top2,SWT.READ_ONLY | SWT.LEFT | SWT.BORDER);
		txtVol.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,3,1));
		
		Shell shell = this.getShell();
		
        final ToolBar tb = new ToolBar(top2, SWT.HORIZONTAL);
    	final ToolItem ti = new ToolItem(tb, SWT.DROP_DOWN);
    	    	
    	final Menu mu = new Menu(shell, SWT.POP_UP);
    	
    	for(int i=0; i<SigmetInfo.VOL_NAME_BUCKET_ARRAY.length; i++){	
			if(i==0){// first option is entering name
				MenuItem mi1 = new MenuItem(mu, SWT.PUSH);
	    		mi1.setText(SigmetInfo.VOL_NAME_BUCKET_ARRAY[i]);
	    		
	    		mi1.addListener(SWT.Selection, new Listener() {
	    			public void handleEvent(Event e){	    				
	    				resetTxts();
	    				setTxtsEditable(true);	    				
	    			}
	    		});
			}else{
	    		MenuItem mi1 = new MenuItem(mu, SWT.CASCADE);
	    		mi1.setText(SigmetInfo.VOL_NAME_BUCKET_ARRAY[i]);//"test1");
	    		Menu mi1Menu = new Menu(shell,SWT.DROP_DOWN);
	    		mi1.setMenu(mi1Menu);
	    	
			    java.util.List<String> list = SigmetInfo.VOLCANO_BUCKET_MAP.get(SigmetInfo.VOL_NAME_BUCKET_ARRAY[i]);	
			    int size = list.size();
			    for(int j=0; j<size; j++){
			    	final MenuItem mi1MenuMi1 = new MenuItem(mi1Menu,SWT.PUSH);
			    	mi1MenuMi1.setText(list.get(j));
			    	mi1MenuMi1.addListener(SWT.Selection, new Listener(){
			    		public void handleEvent(Event e){			    			
			    			resetTxts();
			    			setTxtsEditable(false);
			    			//txtVol.setText(mi1MenuMi1.getText());
			    			setTextToTxts(mi1MenuMi1.getText());
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
		
		
		
		Label lblNum = new Label(top2, SWT.LEFT);		
		lblNum.setText("   Number: ");		

		txtNum = new Text(top2, SWT.READ_ONLY|SWT.BORDER);
		
	}
	
	/**
	 * create part of the dialog area.
	 * @param top
	 */
	private void createArea2(Group top){
		
		Label lblLoc = new Label(top, SWT.LEFT);		
		lblLoc.setText("Location ( e.g. N3900W07700 ):  ");		

		txtLoc = new Text(top, SWT.READ_ONLY|SWT.BORDER);
		txtLoc.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,3,1));
		
	}
	
	/**
	 * create part of the dialog area.
	 * @param top
	 */
	private void createArea3(Group top){
		
		Label lblArea = new Label(top, SWT.LEFT);		
		lblArea.setText("Area: ");		

		txtArea = new Text(top, SWT.READ_ONLY|SWT.BORDER);
		txtArea.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,2,1));
		
		Label lblEle = new Label(top, SWT.LEFT);		
		lblEle.setText("     Elevation: ");		

		txtEle = new Text(top, SWT.READ_ONLY|SWT.BORDER);
		
		Label lblFt = new Label(top, SWT.LEFT);
		lblFt.setText(" ft");
		
	}
	
	/**
	 * create part of the dialog area.
	 * @param top
	 */
	private void createArea4(Group top){
		Label lblDummy = new Label(top,SWT.LEFT);
		Label lblDummy2 = new Label(top,SWT.LEFT);
		Label lblDummy3 = new Label(top,SWT.LEFT);
		
		btnVol = new Button(top, SWT.PUSH);
		btnVol.setText("Create VAA Volcano");
		
		btnVol.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				if(txtVol != null && txtVol.getText() != null && txtVol.getText().length() > 0){
					createVol();
					cancelPressed();
				}				
			}
		});
		
		btnLay = new Button(top, SWT.PUSH);
		btnLay.setText("Create Volcano in Layers");
		
		btnLay.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
			/*	createVol();
				VolcanoLayerDlg vlDlg = VolcanoLayerDlg.getInstance(getParentShell());
				vlDlg.setVolcano(elem);
				vlDlg.open();
				cancelPressed();	*/			
			}
		});
		
		Button btnCan = new Button(top, SWT.PUSH);
		btnCan.setText("Cancel");
		btnCan.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				cancelPressed();
			}
		});
		/*
		boolean flag = VaaInfo.isValidElev(txtEle.getText().trim())
						&& VaaInfo.isValidLatLon(txtLoc.getText().trim());*/
		btnVol.setEnabled(false);
		btnLay.setEnabled(false);
		
	}
	
	/**
	 * create part of the dialog area.
	 * @param top
	 */
	private void createArea5(Group top){
		Label lblDummy = new Label(top,SWT.LEFT);
		Label lblDummy2 = new Label(top,SWT.LEFT);
		Label lblDummy3 = new Label(top,SWT.LEFT);
		
		Label lblPro = new Label(top, SWT.LEFT);
		lblPro.setText(" Special Products:  ");
		
		final Combo comboType = new Combo(top,SWT.READ_ONLY);
		comboType.setItems(VaaInfo.ProductInfo.getProduct(VaaInfo.LOCS[1]));
		comboType.select(0);
		
		Button btnGo = new Button(top, SWT.PUSH);
		btnGo.setText(" Go....... ");
		btnGo.addListener(SWT.Selection, new Listener(){
			public void handleEvent(Event e){
				VolcanoVaaAttrDlg vaDlg = VolcanoVaaAttrDlg.getInstance(VolcanoCreateDlg.this.getParentShell());
				
				vaDlg.setFromSelection(false);				
				Volcano vol = new Volcano();		
				vol.setPgenCategory(VaaInfo.PGEN_CATEGORY);
				vol.setPgenType(VaaInfo.PGEN_TYPE_VOLCANO);
				vol.setProduct(comboType.getText().trim());
				VaaInfo.setVolcanoFields(vol, comboType.getText().trim(), false);
				//VaaInfo.addVolcano2Product(vol);
				drawingLayer.addElement(vol);//2010-03-17//VaaInfo.setVolcano(vol);
				VaaInfo.VOL_PROD_MAP.put(vol, drawingLayer.getActiveProduct());
				vaDlg.setVolcano(vol);
				cancelPressed();/*2010-04-14*/	
				vaDlg.open();
			}
		});
		
	}	
	
	@Override
	public void createButtonsForButtonBar(Composite parent){
		
	}
	
	/**
	 * enable or disable a Volcano info fields
	 * @param boolean: flag indicating enable/disable
	 */
	private void setTxtsEditable(boolean flag){
		txtVol.setEditable(flag);
		txtNum.setEditable(flag);
		txtLoc.setEditable(flag);
		txtArea.setEditable(flag);
		txtEle.setEditable(flag);	
		isManualInput = flag;
		setStnFlag = flag;
	}
	
	/**
	 * reset the Volcanno info fields
	 */
	private void resetTxts(){
		txtVol.setText("");
		txtNum.setText("");
		txtLoc.setText("");
		txtArea.setText("");
		txtEle.setText("");
	}
	
	/**
	 * set the Volcano info fields from file data
	 * @param String: volName: Volcano name
	 */
	private void setTextToTxts(String volName){
		txtVol.setText(volName);
		
		java.util.List<Station> list = SigmetInfo.VOLCANO_STATION_LIST;
		
		//Station stn = null;
		
		for(Station station : list){
			if( volName != null && volName.equals(station.getStnname()) ) 
					stn = station;
		}
		
		txtNum.setText(stn.getStid());
		txtLoc.setText(getLocText(stn));
		txtArea.setText(stn.getLocation());
		//String ele = ""+stn.getElevation()*3.281;
		txtEle.setText(VaaInfo.getFootTxtFromMeter(stn.getElevation(), 1));//ele.substring(0, ele.indexOf(".")+2));
		
	}	
	
	/**
	 * obtain Volcano location data 
	 * @param stn
	 * @return
	 */
	private String getLocText(Station stn){
		String s = PgenUtil.getLatLonStringPrepend(new Coordinate[]{new Coordinate(stn.getLongitude(),stn.getLatitude())}, false);		
		return s.replaceAll(VaaInfo.SEPERATER, "");
	}
	
	/**
	 *  create a drawable Volcano then set its fields from the Station 	
	 */
	public void createVol(){    
		
		setStation(setStnFlag);   	 
		
    	DrawableElementFactory def = new DrawableElementFactory();	    	
    	
    	elem = (Volcano)def.create(DrawableType.VAA, (IAttribute)this,
		        		pgenCategory, pgenType, new Coordinate(stn.getLongitude(), 
		        		stn.getLatitude()), drawingLayer.getActiveLayer() );
    	
    	if ( elem != null ) {  

    		elem.setProduct(VaaInfo.DEFAULT_PRODUCT);//TODO: 2010-03-09
    		
    		elem.setArea(stn.getLocation());
    		elem.setElev( getElevText(stn) );//convert 100 m to 333 ft (100 m)
    		elem.setTxtLoc(getLocText(stn));
    		elem.setNumber(stn.getStid());
    		elem.setName(stn.getStnname());
    		//VaaInfo.setVolcano(elem);//---2010-03-17 
    		/*	2010-03-12. 
    		 *
    		 *---TODO: put the statements into a method? 
    		 *		   check VOL_PROD_MAP first before creating Product?
    		 
    		VaaInfo.setVolcano(elem);
    		Product prod = VaaInfo.getVaaProduct(elem);
    		prod.getLayer(VaaInfo.LAYERS[0]).addElement(elem);
    		VaaInfo.VOL_PROD_MAP.put(elem, prod);
    		drawingLayer.addProduct(prod);   
    		drawingLayer.setActiveProduct(prod);
    		drawingLayer.setActiveLayer(prod.getLayer(VaaInfo.LAYERS[0]));
    		*/
    		//VaaInfo.removeNonDrawableVol();//20100324 NOT used any more; see Volcano.java's getPoints()
    		drawingLayer.addElement( elem );    
    		VaaInfo.VOL_PROD_MAP.put(elem, drawingLayer.getActiveProduct());
            mapEditor.refresh();            
	    }
	}
	
	
	public void setCreateTool(PgenVolcanoCreateTool tool){
		this.tool = tool;
	}
	
	@Override
	public void cancelPressed() {
		setReturnCode(CANCEL);
		close();
		PgenUtil.setSelectingMode();
	}
	
	/**
	 * set the station from the values of text fields
	 */
	
	private void setStation(boolean flag){	
		if(flag){
			stn = new Station();
			
			stn.setStnname(txtVol.getText().trim());
			stn.setStid(txtNum.getText().trim());
			
			String latlon = txtLoc.getText().trim();
			Float lat = VaaInfo.getLatLonFromTxt(latlon, true);
			stn.setLatitude( lat );
			Float lon = VaaInfo.getLatLonFromTxt(latlon, false);			
			stn.setLongitude( lon );
						
			stn.setElevation(VaaInfo.getMeterIntFromFoot(txtEle.getText().trim()));
			
			stn.setLocation(this.txtArea.getText().trim());			
		}
	}
	
	/**
	 * class with Text fields listeners for checking
	 * the validity of input values	 
	 *
	 */	
	class TextFieldListener extends KeyAdapter 
		implements ModifyListener, VerifyListener {

		/*
		* Text fields need to add the listener
		*/
		private Text[] texts;
		
		/*
		* Controls need to be enabled/disenabled
		*/
		private Control[] controls;		
		
		public TextFieldListener(Text[] txts, Control[] ctrls){		
			texts = txts;
			controls = ctrls;	
			
			for(Text text : txts){
				text.addModifyListener(this);
				text.addVerifyListener(this);
				text.addKeyListener(this);
			}
		}
		
		@Override
		public void keyPressed(KeyEvent e) {
		
		}
		
		/**
		 * checks the Text fields
		 * 1. null or empty
		 * 2. invalid inputs of Lat/Lon
		 */
		
		@Override
		public void modifyText(ModifyEvent e) {		
			
			for(Text txt : texts){
				
				// if one text if empty then cannot create a volcano
				if( txt.getText()==null || txt.getText().length()==0){					
					for(Control cont : controls) cont.setEnabled(false);					
					return;
				}				
			}
			
			
			 
			// only when manual input with correct Lat/Lon and Elevation			 
			
			if(  isManualInput ){
				
				if( VaaInfo.isValidLatLon(txtLoc.getText().trim()) 
						&& VaaInfo.isValidElev(txtEle.getText().trim() )){				
					for(Control cont : controls) cont.setEnabled(true);
				}else{
					for(Control cont : controls) cont.setEnabled(false);
				}	
				
			}else{
				
				/* some Lat/Lon from file are NOT standard:
				 * Example: Volcano Salak in java: S671E10673
				 * No check performed on volcanos from the file	
				 */
				for(Control cont : controls) cont.setEnabled(true);
			}
			
		}
		
		@Override
		public void verifyText(VerifyEvent e){
			
		}	

	}
	
	/**
	 * get the elevation data of the Volcano
	 * @param Station:	stn representing the Volcano
	 * @return String:  the elevation in string 
	 */
	private String getElevText(Station stn){		
				
		StringBuilder sb = new StringBuilder();
		
		double dMeter = 0;
		String sMeter = ""+stn.getElevation();
		try{
			dMeter = Double.parseDouble(sMeter);
		}catch(Exception e){ }
		
		String feet = VaaInfo.getFootTxtFromMeter(dMeter,0);
		sb.append(feet).append("  ft (");
		sb.append(sMeter).append("  m)");		
		
		return sb.toString();
	}	

	@Override
	public Color[] getColors(){
		return null;
	}
	
	@Override
    public Boolean isClear(){
    	return false;
    }
    
	@Override
    public Coordinate getLocation(){
    	return null;
    }
   
}