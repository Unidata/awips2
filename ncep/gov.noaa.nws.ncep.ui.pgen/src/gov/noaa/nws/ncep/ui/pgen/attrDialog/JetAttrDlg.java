/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.JetAttrDlg
 * 
 * 07 July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrDialog;

import java.awt.Color;
import java.util.Iterator;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.ui.pgen.tools.IJetBarb;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenJetDrawingTool;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenSelectingTool;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Singleton attribute dialog for Jet.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/09		#135		B. Yin   	Initial Creation.
 * 03/10        #231        Archana     Altered the Jet attribute dialog
 *                                      to display only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix .
 * 12/10		#366		B. Yin		Added "AddHash" and "DelHash" buttons 
 *
 *</pre>
 * 
 * @author	B. Yin
 */

public class JetAttrDlg extends LineAttrDlg {
	
	/**
	 * single instance
	 */
	static private JetAttrDlg INSTANCE = null;
	
	/**
	 * Pgen drawing tool, can be PgenSelecting or PgenJetDrawing
	 */
	private IJetBarb jetTool;

	/**
	 * Barb attribute dialog
	 */
	private BarbAttrDlg barbAttrDlg;
	
	/**
	 * Hash attribute dialog
	 */
	private HashAttrDlg hashAttrDlg;

	/**
	 * Flight level text attribute dialog
	 */
	private FLAttrDlg flAttrDlg;
	
	/**
	 * Barb and flight level info input dialog
	 */
	private BarbDlg barbDlg;
	
	/**
	 * 'Add Barb' button
	 */
	private Button addBarbBtn;
	
	/**
	 * 'Delete Barb' button
	 */
	private Button delBarbBtn;

	/**
	 * 'Add Hash' button
	 */
	private Button addHashBtn;
	
	/**
	 * 'Delete Hash' button
	 */
	private Button delHashBtn;
	
	private Composite segPane;
	
	/**
	 * 'Barb Attr' button
	 */
	private Button barbAttrBtn;
	
	/**
	 * 'Hash Attr' button
	 */
	private Button hashAttrBtn;
	
	/**
	 * 'Fl Attr' button
	 */
	private Button flAttrBtn;

	/**
	 * stored barb attribute
	 */
	private Vector barbTemplate;
	
	/**
	 * stored hash attribute
	 */
	private Vector hashTemplate;
	/**
	 * stored flight level text attribute
	 */
	private gov.noaa.nws.ncep.ui.pgen.elements.Text flTemplate;
	
	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private JetAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);
        barbDlg = new BarbDlg(parShell);

    }
	
	/**
	 * Creates a volcano attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static JetAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new JetAttrDlg( parShell );
				
			} catch (VizException e) {
				
				e.printStackTrace();
				
			}	
		}
		
		return INSTANCE;
		
	} 	
	
	@Override
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 */
	protected void initializeComponents() {
		
        this.getShell().setText("Jet Attributes");
        
        // set main layout
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);
        
        // create pane1 which contains jet line attributes
        Composite pane1 = new Composite(top,SWT.None);
        GridLayout layout1 = new GridLayout(2, false);
	    layout1.marginHeight = 3;
	    layout1.marginWidth = 3;
	    pane1.setLayout(layout1);
        
        Label colorLbl = new Label(pane1, SWT.LEFT);
        colorLbl.setText("Color:");
        
//        Composite colorGroup = new Composite(pane1, SWT.NONE);
//        cs = new ColorMatrixSelector(colorGroup, true, true, 30, 30, 14, 16, 24, 24, 0, 2, 2);
		cs = new ColorButtonSelector( pane1 );
        cs.setColorValue(new RGB(0,255,0));
      
        Label widthLbl = new Label(pane1, SWT.LEFT);
        widthLbl.setText("Line Width:");
		
		GridLayout gl = new GridLayout( 2, false );

		Group widthGrp = new Group( pane1, SWT.NONE ) ;
	    widthGrp.setLayout( gl );	    

	    widthSpinnerSlider = 
	    	new gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.SpinnerSlider(widthGrp, SWT.HORIZONTAL,1);
	    widthSpinnerSlider.setLayoutData(new GridData(180,30));
	    widthSpinnerSlider.setMinimum(1);            
	    widthSpinnerSlider.setMaximum(20);
	    widthSpinnerSlider.setIncrement(1);
	    widthSpinnerSlider.setPageIncrement(3);        
	    widthSpinnerSlider.setDigits(0);

       // widthSlider = new Slider(pane1, SWT.HORIZONTAL);
       // widthSlider.setValues(2, 1, 20, 2, 1, 3);
		patternSizeLbl = new Label(pane1, SWT.LEFT);
		patternSizeLbl.setText("Pattern Size:");

		Group psGrp = new Group( pane1, SWT.NONE ) ;
	    psGrp.setLayout( gl );
	    
	    patternSizeSpinnerSlider = 
	    	new gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.SpinnerSlider(psGrp, SWT.HORIZONTAL,1);
	    patternSizeSpinnerSlider.setLayoutData(new GridData(180,30));
	    patternSizeSpinnerSlider.setMinimum(1);            
	    patternSizeSpinnerSlider.setMaximum(100);
	    patternSizeSpinnerSlider.setIncrement(1);
	    patternSizeSpinnerSlider.setPageIncrement(10);        
	    patternSizeSpinnerSlider.setDigits(1);  
		  
        Label smoothLbl = new Label(pane1, SWT.LEFT);
        smoothLbl.setText("Smooth Level:");

        smoothLvlCbo = new Combo( pane1, SWT.DROP_DOWN | SWT.READ_ONLY );

        smoothLvlCbo.add("0");
        smoothLvlCbo.add("1");
        smoothLvlCbo.add("2");
        smoothLvlCbo.select( 2 );
        
        addSeparator(top);
        
        // create pane2 which contains add/delete barb buttons
        Composite pane2 = new Composite(top,SWT.NONE);
        GridLayout layout2 = new GridLayout(2, false);
	    layout2.marginHeight = 3;
	    layout2.marginWidth = 3;
	    pane2.setLayout(layout2);
	    GridData gd2 = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        pane2.setLayoutData(gd2);
	    
		addBarbBtn = new Button(pane2, SWT.PUSH);
		addBarbBtn.setText("Add Barb");
		delBarbBtn = new Button(pane2, SWT.PUSH);
		delBarbBtn.setText("Delete Barb");
		
		//enableBarbBtns(false);
		
		addBarbBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {
				barbDlg.close();
				barbDlg.open();
				jetTool.setAddingBarbHandler();
				
				// highlight the jet line 
				if ( jetTool instanceof PgenJetDrawingTool){
					((PgenJetDrawingTool)jetTool).setSelected();
				}
			}
		 });
		
		delBarbBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {
				jetTool.setDeletingBarbHandler();
				closeBarbDlg();
			}
		});	
		
		addSeparator(top);
		
	     // create pane2 which contains add/delete barb buttons
        Composite paneHash = new Composite(top,SWT.NONE);
        GridLayout layoutHash = new GridLayout(2, false);
	    layoutHash.marginHeight = 3;
	    layoutHash.marginWidth = 3;
	    paneHash.setLayout(layoutHash);
	    GridData gdHash = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        paneHash.setLayoutData(gdHash);
	    
		addHashBtn = new Button(paneHash, SWT.PUSH);
		addHashBtn.setText("Add Hash");
		delHashBtn = new Button(paneHash, SWT.PUSH);
		delHashBtn.setText("Delete Hash");
		
		enableBarbBtns(false);
		
		addHashBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {
				barbDlg.close();
				jetTool.setAddingHashHandler();
				
				// highlight the jet line 
				if ( jetTool instanceof PgenJetDrawingTool){
					((PgenJetDrawingTool)jetTool).setSelected();
				}
			}
		 });
		
		delHashBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {
				jetTool.setDeletingHashHandler();
				closeBarbDlg();
			}
		});	
		
		segPane = new Composite(top,SWT.NONE);
		GridLayout segGl = new GridLayout(15, false);
		segPane.setLayout(segGl);
		segPane.pack();
				
		addSeparator(top);	
		// create pane3 which contains barb/hash/fl attr buttons
        Composite pane3 = new Composite(top,SWT.NONE);
        GridLayout layout3 = new GridLayout(3, false);
	    layout3.marginHeight = 3;
	    layout3.marginWidth = 3;
	    pane3.setLayout(layout3);
	    
	    GridData gd3 = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        pane3.setLayoutData(gd3);
	    
		barbAttrBtn = new Button(pane3, SWT.PUSH);
		barbAttrBtn.setText("Barb Attr");
		hashAttrBtn = new Button(pane3, SWT.PUSH);
		hashAttrBtn.setText("Hash Attr");
		flAttrBtn = new Button(pane3, SWT.PUSH);
		flAttrBtn.setText("FL Attr");
		
        addSeparator(top);
        
		barbAttrBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {

				try {
					if ( barbAttrDlg == null ){
						barbAttrDlg = new BarbAttrDlg(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
					}
					openAttrDlg(barbAttrDlg);
					barbAttrDlg.initDlg();
					barbAttrDlg.getShell().setText("Jet Barb Attributes");

					//get stored attributes
					if ( barbTemplate != null ){
						barbAttrDlg.setAttrForDlg((IAttribute)barbTemplate);
					}
					else {
						//set default from settings table
						IAttribute barbAttr = getBarbAttrFromSettings();
						if ( barbAttr != null )
						barbAttrDlg.setAttrForDlg(barbAttr);
					}
					// disable some un-used widgets
					barbAttrDlg.disableWidgets();

				}
				
				catch (VizException e) {
					e.printStackTrace();
				}

			}
		 });
		 
		 hashAttrBtn.addListener(SWT.MouseDown, new Listener(){

			 @Override
			 public void handleEvent(Event event) {
					try {
						if ( hashAttrDlg == null ){
							hashAttrDlg = new HashAttrDlg(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
						}
						openAttrDlg(hashAttrDlg);
						hashAttrDlg.initDlg();
						hashAttrDlg.getShell().setText("Jet Hash Attributes");

						//get stored attributes
						if ( hashTemplate != null ){
							hashAttrDlg.setAttrForDlg((IAttribute)hashTemplate);
						}
						else {
							//set default from settings table
							IAttribute hashAttr = getHashAttrFromSettings();
							if ( hashAttr != null )
							hashAttrDlg.setAttrForDlg(hashAttr);
						}
						
						// disable some un-used widgets
						hashAttrDlg.disableWidgets();

					}
					
					catch (VizException e) {
						e.printStackTrace();
					}		 
			 }
		 });	
		 
		 flAttrBtn.addListener(SWT.MouseDown, new Listener(){

			 @Override
			 public void handleEvent(Event event) {
				 try {
					 if ( flAttrDlg == null ){
						 flAttrDlg = new FLAttrDlg(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
					 }
					 openAttrDlg(flAttrDlg);
					 flAttrDlg.initDlg();
					 
					 //get stored attributes
					 if ( flTemplate != null ){
						 flAttrDlg.setAttrForDlg((IAttribute)flTemplate);
					 }
					else {
							//set default from settings table
							IAttribute flAttr = getFlAttrFromSettings();
							if ( flAttr != null )
							flAttrDlg.setAttrForDlg(flAttr);
					}
					 
					 // disable un-used attributes
					 flAttrDlg.disableWidgets();

				 }
				 catch (VizException e) {
					 e.printStackTrace();
				 }
			 }
		 });	 
		 
	}

	/**
	 * initialize and open the dialog
	 * @param dlg
	 */
	private void openAttrDlg(AttrDlg dlg){
		dlg.setBlockOnOpen( false );
		dlg.setDrawingLayer(drawingLayer);
		dlg.setMapEditor( mapEditor );  
		dlg.open();
		dlg.enableButtons();
	}
	
    /**
     * enable/disable add/delete barb buttons
     */
    public void enableBarbBtns(boolean flag){
    	addBarbBtn.setEnabled(flag);
    	delBarbBtn.setEnabled(flag);
       	addHashBtn.setEnabled(flag);
    	delHashBtn.setEnabled(flag);    	
    }
    
    /**
     * enable the ok/cancel and add/del barb buttons
     */
    @Override
	public void enableButtons(){
		super.enableButtons();
		enableBarbBtns(true);
	}
    
    /**
     * close all jet related dialogs.
     */
	@Override
	public boolean close(){
		
		if ( barbAttrDlg != null ) barbAttrDlg.close();
		if ( hashAttrDlg != null ) hashAttrDlg.close();
		if ( flAttrDlg != null ) flAttrDlg.close();
		jetTool = null;				//prevent reseting mouse handler
		barbDlg.close();
		return super.close();
		
	}
	
	/**
	 * close barb info dialg and all attribute dialogs
	 */
	public void closeBarbDlg(){
		
		if ( barbDlg.getShell() != null ){
			barbDlg.close();
			if ( barbAttrDlg != null ){
				barbAttrDlg.close();
			}
			if ( flAttrDlg != null ){
				flAttrDlg.close();
			}
			if ( hashAttrDlg != null ){
				hashAttrDlg.close();
			}
			if ( jetTool instanceof PgenJetDrawingTool ){
				((PgenJetDrawingTool)jetTool).deSelect();
			}
		}
	}
	
	
	/**
	 * set the drawing tool(can be PgenSeletingTool or PgenJetDrawingTool)
	 */
	public void setJetDrawingTool(IJetBarb tool ){
		jetTool = tool;
	}
	
	/**
	 * get barb speed
	 * @return
	 */
	public int getBarbSpeed(){
		
		int ret = 80;
		try {
		 ret = Integer.valueOf(barbDlg.speedTxt.getText().trim());
		}
		catch ( Exception e ) {
			setBarbSpeed( ret );
		}
		
		if ( ret < 80 ) {
			
			ret = 80;		
			setBarbSpeed( ret );

		}
		
		return ret;
	}
	
	public void  setBarbSpeed( int spd ){
		barbDlg.speedTxt.setText( String.valueOf(spd));
	}
	
	/**
	 * get the flight level
	 * @return
	 */
	public int getFlightLevel(){
		
		int ret = 300;
		try {
		 ret = Integer.valueOf(barbDlg.levelTxt.getText().trim());
		}
		catch ( Exception e ) {
			setFlightLevel( ret );
		}
		
		if ( ret < 100 ) {
			
			ret *= 10;		
			setFlightLevel( ret );

		}
		
		return ret;
	}
	
	public void setFlightLevel( int level ){
		barbDlg.levelTxt.setText( String.valueOf(level) );
	}
	
	/**
	 * get top/bottom of the flight level
	 * @return
	 */
	public String getFLDepth(){
		
		int top = 300;
		int bottom = 100;
		
		try {
			top = Integer.valueOf(barbDlg.topTxt.getText().trim());
		}
		catch ( Exception e ) {
			barbDlg.topTxt.setText( String.valueOf(top) );
		}
		
		try {
			bottom = Integer.valueOf(barbDlg.btmTxt.getText().trim());
		}
		catch ( Exception e ) {
			barbDlg.btmTxt.setText( String.valueOf(bottom) );
		}
		
		if ( top < 100 ) {
			top *= 10;
			barbDlg.topTxt.setText( String.valueOf(top) );
		}
		
		if ( bottom < 100 ){
			bottom *= 10;
			barbDlg.btmTxt.setText( String.valueOf(bottom) );
		}
		
		String flDepth = 
				barbDlg.topTxt.getText().trim() + "/" + 
				barbDlg.btmTxt.getText().trim();
		if (flDepth.length() == 1) flDepth = null;
		return flDepth;
	}
	
	/**
	 *  clears top and bottom fields.
	 */
	public void clearFLDepth(){
		barbDlg.topTxt.setText("");
		barbDlg.btmTxt.setText("");	
	}
	
	/**
	 * get barb attributes. If the dialog is open, get attributes from the dialog,
	 * otherwise, get them from the stored attributes.
	 * @return
	 */
	public IAttribute getBarbAttr(){
		if ( barbAttrDlg != null && barbAttrDlg.getShell() != null ) return barbAttrDlg;
		else if ( barbTemplate != null ) return barbTemplate;
		else return getBarbAttrFromSettings();
	}
	
	/**
	 * get hash attributes. If the dialog is open, get attributes from the dialog,
	 * otherwise, get them from the stored attributes.
	 * @return
	 */	
	public IAttribute getHashAttr(){
		if ( hashAttrDlg != null && hashAttrDlg.getShell() != null ) return hashAttrDlg;
		else if ( hashTemplate != null ) return hashTemplate;
		else return getHashAttrFromSettings();
	}
	
	/**
	 * get fl text attributes. If the dialog is open, get attributes from the dialog,
	 * otherwise, get them from the stored attributes.
	 * @return
	 */
	public IAttribute getFLAttr(){
		if ( flAttrDlg != null && flAttrDlg.getShell() != null ) return flAttrDlg;
		else if ( flTemplate != null ) return flTemplate;
		else return getFlAttrFromSettings();
	}
	
	/**
	 * Dialog class for barb info input
	 * @author bingfan
	 *
	 */
	private class BarbDlg  extends Window {
		
		private Text speedTxt;
		private Text levelTxt; 
		private Text topTxt;
		private Text btmTxt;
		
		private BarbDlg(Shell parentShell) {
			super(parentShell);
		}
	
		protected Control createContents(Composite parent){
			
			this.getShell().setText("Barb Info");
			
			// set layout
			GridLayout gl = new GridLayout(3, false);
		    gl.marginHeight = 3;
		    gl.marginWidth = 3;
		    parent.setLayout(gl);
		         
		    // speed
		    Label speedLbl = new Label(parent, SWT.CENTER);
		    speedLbl.setText("Speed");
		    speedTxt = new Text(parent, SWT.BORDER);
		    speedTxt.setText("  100");
	        speedTxt.addListener(SWT.Verify, new Listener(){
	        	@Override
	    		public void handleEvent(Event e) {
	    				e.doit = PgenUtil.validateNumberTextField( e );
	    		}
	        });

		    Label speedUnit = new Label(parent, SWT.CENTER);
		    speedUnit.setText("KTS");
		    
		    // flight level
		    Label levelLbl = new Label(parent, SWT.CENTER);
		    levelLbl.setText("Level");
		    levelTxt = new Text(parent, SWT.BORDER);
		    levelTxt.setText("  300");
		    levelTxt.addListener(SWT.Verify, new Listener(){
	        	@Override
	    		public void handleEvent(Event e) {
	    				e.doit = PgenUtil.validateNumberTextField( e );
	    		}
	        });
		    Label levelUnit = new Label(parent, SWT.CENTER);
		    levelUnit.setText("100 ft");

		    // top/bottom
		    Label topLbl = new Label(parent, SWT.CENTER);
		    topLbl.setText("Top/Bottom");
		    topTxt = new Text(parent, SWT.BORDER);
		    topTxt.addListener(SWT.Verify, new Listener(){
	        	@Override
	    		public void handleEvent(Event e) {
	    				e.doit = PgenUtil.validateNumberTextField( e );
	    		}
	        });
		    Composite btm = new Composite(parent,SWT.NONE);
	        btm.setLayout(new RowLayout(SWT.HORIZONTAL));

		    Label slashLbl = new Label(btm, SWT.None);
		    slashLbl.setText("/");
		    btmTxt = new Text(btm,SWT.BORDER);
		    btmTxt.addListener(SWT.Verify, new Listener(){
	        	@Override
	    		public void handleEvent(Event e) {
	    				e.doit = PgenUtil.validateNumberTextField( e );
	    		}
	        });
		    
		    return parent;
		}
		
		@Override
		public int open(){
			
			this.create();
			
	    	setBlockOnOpen( false );
	    	
	    	Point loc = INSTANCE.getShell().getLocation();
	    	getShell().setLocation(loc.x,loc.y+INSTANCE.getShell().getSize().y);
			
			return super.open();
			
		}
		
		@Override
		public boolean close() {
			
			if ( jetTool != null ) jetTool.resetMouseHandler();	//de-activate addBarbTool
			return super.close();
		}
	}

	/**
	 * Barb attribute dialog. 
	 * @author bingfan
	 *
	 */
	private class BarbAttrDlg extends VectorAttrDlg {
		
		private BarbAttrDlg(Shell parShell) throws VizException {
			
	        super(parShell);
			
	    }
		
		@Override
		public void okPressed(){
			barbTemplate = (Vector)new DrawableElementFactory().create(DrawableType.VECTOR, 
					this, "Vector", "Barb", (Coordinate)null, null);
			
			//apply to all barbs 
			if ( jetTool instanceof PgenSelectingTool ){
				((PgenSelectingTool)jetTool).applyBarbAttrOnJet(this, "Barb");
			}
			
			close();
		}
		
		/**
		 * disable un-used widgets
		 */
		protected void disableWidgets(){
			spdLbl.setEnabled( false );
			spdSlider.setEnabled( false );
			spdText.setEnabled( false );
			dirLbl.setEnabled( false );
			dirSlider.setEnabled( false );
			dirText.setEnabled( false );		
		}
		
		/**
		 * initialize dialog
		 */
		protected void initDlg(){      
			sizeText.setText("2");
			sizeSlider.setSelection(20);
			widthText.setText("2");
			widthSlider.setSelection(2);
			dirText.setText("270");
			dirSlider.setSelection(270);
		}

	}
	
	/**
	 * Hash attribute dialog. 
	 * @author bingfan
	 *
	 */
	private class HashAttrDlg extends BarbAttrDlg {
		
		private HashAttrDlg(Shell parShell) throws VizException {
			
	        super(parShell);
			
	    }
		
		@Override
		public void okPressed(){
			hashTemplate = (Vector)new DrawableElementFactory().create(DrawableType.VECTOR, 
					this, "Vector", "Hash", (Coordinate)null, null);
			
			//apply to all barbs 
			if ( jetTool instanceof PgenSelectingTool ){
				((PgenSelectingTool)jetTool).applyBarbAttrOnJet(this, "Hash");
			}
			
			close();
		}
	}
	
	/**
	 * Flght level text dialog class
	 * @author bingfan
	 *
	 */
	private class FLAttrDlg extends TextAttrDlg {
		
		private FLAttrDlg(Shell parShell) throws VizException {
			
	        super(parShell);
			
	    }
		
		@Override
		public void okPressed(){
			flTemplate = (gov.noaa.nws.ncep.ui.pgen.elements.Text)new DrawableElementFactory().create(DrawableType.TEXT, 
					this, "Text", "General Text", (Coordinate)null, null);
			
			// apply to all flight level texts
			if ( jetTool instanceof PgenSelectingTool ){
				((PgenSelectingTool)jetTool).applyFLAttrOnJet(this);
			}
			
			close();
		}
		
		/**
		 * disable un-used widgets
		 */
		private void disableWidgets(){
			text.setEnabled(false);
			textLabel.setEnabled(false);
			rotLbl.setEnabled(false);
			rotSlider.setEnabled(false);
			rotText.setEnabled(false);
			screenBtn.setEnabled(false);
			northBtn.setEnabled(false);
			
		}
		
		/**
		 * initialize dialog
		 */
		private void initDlg(){			
			this.getShell().setText("Jet Flight Level Attributes");
			sizeCombo.select(3);
		}

	}

	/**
	 * Return false because jet cannot be closed line.
	 */
	public Boolean isClosedLine(){
		return false;
	}

	/**
	 * Return false because jet cannot be filled.
	 */
	public Boolean isFilled(){
		return false;
	}

	/**
	 * Return color selection from the attribute dialog
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
	
	/**
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */
	@Override
	public void setColor( Color clr[] ){
		
		cs.setColorValue(new RGB(clr[0].getRed(), clr[0].getGreen(), clr[0].getBlue()));
		
	}
	
	/**
	 * Return line with from the attribute dialog
	 */
	public float getLineWidth(){
		return widthSpinnerSlider.getSelection();
	}

	/**
	 * Return smooth level from the attribute dialog 
	 */
	public int getSmoothFactor(){
		return smoothLvlCbo.getSelectionIndex();
	}
	
	/**
	 * Set dialog attributes using values of adc
	 */
    public void setAttr( AbstractDrawableComponent adc ){
    	if ( adc instanceof Jet ){
    		super.setAttrForDlg((IAttribute)((Jet)adc).getJetLine());
    	}
    }
    
    /**
     * Get default hash attributes from the settings table.
     * @return
     */
    private IAttribute getHashAttrFromSettings(){
    	
		AbstractDrawableComponent adc = AttrSettings.getInstance().getSettings().get( "JET" );
   	    if ( adc != null && adc instanceof Jet ) {
    	    Iterator<DrawableElement> it = ((Jet)adc).createDEIterator();
    	    while ( it.hasNext()){
    	    	DrawableElement de = it.next();
    	    	if ( de instanceof Vector && ((Vector)de).getVectorType() == VectorType.HASH_MARK ){
    	    		return (IAttribute)de;
    	    	}
    	    }
   	    }
   	    
   	    return null;
    }
    
    /**
     * Get default jet barb attributes from the settings table.
     * @return
     */   
    private IAttribute getBarbAttrFromSettings(){
    	
		AbstractDrawableComponent adc = AttrSettings.getInstance().getSettings().get( "JET" );
   	    if ( adc != null && adc instanceof Jet ) {
    	    Iterator<DrawableElement> it = ((Jet)adc).createDEIterator();
    	    while ( it.hasNext()){
    	    	DrawableElement de = it.next();
    	    	if ( de instanceof Vector && ((Vector)de).getVectorType() == VectorType.WIND_BARB ){
    	    		return (IAttribute)de;
    	    	}
    	    }
   	    }
   	    
   	    return null;
    }
    
    /**
     * Get default flight level text attributes from the settings table.
     * @return
     */
    private IAttribute getFlAttrFromSettings(){
    	
		AbstractDrawableComponent adc = AttrSettings.getInstance().getSettings().get( "JET" );
   	    if ( adc != null && adc instanceof Jet ) {
    	    Iterator<DrawableElement> it = ((Jet)adc).createDEIterator();
    	    while ( it.hasNext()){
    	    	DrawableElement de = it.next();
    	    	if ( de instanceof gov.noaa.nws.ncep.ui.pgen.elements.Text ){
    	    		return (IAttribute)de;
    	    	}
    	    }
   	    }
   	    
   	    return null;
    }
    
    /**
	 *	Get the size scale.
	 */
	public double getSizeScale(){
		return patternSizeSpinnerSlider.getSelection() / 10.0;
	} 
	
	/**
	 * Update the pane that indicates how many hashes are needed for each section of the jet.
	 * 
	 */
	public void updateSegmentPane(){
		
		clearSegPane();
		if ( jetTool != null && jetTool.getJet().getSnapTool() != null ) {

			java.util.Vector<Integer> seg = jetTool.getJet().getSnapTool().checkHashOnJet( jetTool.getJet() );

			for (int ii : seg){
				Label lbl = new Label( segPane, SWT.NONE);
				lbl.setText(String.valueOf(ii));
				if ( ii == 0 ) {
					lbl.setBackground(new org.eclipse.swt.graphics.Color(this.getShell().getDisplay(), new RGB(0,255,0)));
				}
				else lbl.setBackground(new org.eclipse.swt.graphics.Color(this.getShell().getDisplay(), new RGB(255,0,0)));
			}

			segPane.pack( true );
			segPane.layout();
			top.pack();
			top.layout();

			this.getShell().pack();
			this.getShell().layout();
		}
	}
	
	/**
	 * Clear the pane that indicates how many hashes are needed.
	 */
	private void clearSegPane(){
		
		for ( Control ctl : segPane.getChildren() ) {
			if ( ctl instanceof Label ){
				ctl.dispose();
			}
		}
		
		segPane.pack();
		segPane.layout();
	}
}
