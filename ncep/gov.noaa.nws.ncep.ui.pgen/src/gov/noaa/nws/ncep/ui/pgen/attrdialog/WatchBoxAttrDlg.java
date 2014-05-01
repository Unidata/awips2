/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.WatchBoxAttrDlg
 * 
 * 2 October 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.ArrayList;
import java.util.List;
import java.awt.Color;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.common.staticdata.SPCCounty;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IWatchBox;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox.WatchShape;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenWatchBoxDrawingTool;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenWatchBoxModifyTool;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

/**
 * Singleton attribute dialog for a watch box.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09		#159		B. Yin   	Initial Creation.
 * 12/09		#159		b. Yin		Implemented "fill" functions 
 * 03/10        #231        Archana     Altered the dialog for watch-box 
 *                                      to display only buttons showing the 
 *                                      selected colors instead of displaying 
 *                                      the complete color matrix.
 *                                      Altered the layout of the composite
 *                                      to align the color selection
 *                                      button correctly.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 05/12		TTR 526		B. Yin		Allow to change watch shape
 * 12/13		TTR 800		B. Yin		Added original county list
 * </pre>
 * 
 * @author	B. Yin
 */

public class WatchBoxAttrDlg extends AttrDlg implements IWatchBox{
	
	// instance of the watch box dialog
	static private WatchBoxAttrDlg INSTANCE = null;
	
	// instance of the watch box
	private WatchBox wb;

	// list of the symbols used for counties in the watch box
	private static final String[] SYMBOL_LIST = new String[] { "PLUS_SIGN","TRIANGLE" ,
			"UP_ARROW", "SMALL_X", "BOX", "OCTAGON" };

	private PgenWatchBoxModifyTool wbTool;
	
	// instance of the watch specifications/county list dialog
	private WatchInfoDlg infoDlg;
	
	// top level container that holds all widgets
	private Composite top;
	
	// color label and selector for the watch box 
	private Label colorLbl;
	private ColorButtonSelector cs;
	
	//check box for county fill
	private Button fillBtn;

	// color selector for county fill 
	private ColorButtonSelector symbolColor;

	// radio button for watch shapes
	private Button nsBtn;
	private Button ewBtn;
	private Button esolBtn;
	
	//list of markers used to draw counties 
	private SymbolCombo symbolCombo;
	
	//width slider/text for county marker 
	private Label widthLbl; 
	private Slider widthSlider;
    private Text widthText;
    
    //size slider/text for county marker
	private Label sizeLbl;
	private Slider sizeSlider;
    private Text sizeText;

    //Show/Hide button for the watch info dialog
	private Button dispBtn;
	
	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	protected WatchBoxAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);

    }
	
	/**
	 * Creates a symbol attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static WatchBoxAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new WatchBoxAttrDlg( parShell );
				
			} catch (VizException e) {
				
				e.printStackTrace();
				
			}	
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
	        top.setLayout(mainLayout);

	        // Initialize all of the menus, controls, and layouts
	        this.initializeComponents();

	        return top;
	        
	}
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 */
	protected void initializeComponents() {
		
        this.getShell().setText("Watch Attributes");
        
        createColorAttr();
        addSeparator(top);
        createShapeFillAttr();
        addSeparator(top);
        
        createTypeAttr();
        createSizeAttr();
        createWidthAttr();
       
        addSeparator(top);
        createDispBtn();
        
        addSeparator(top);
        
	}	
		
	@Override
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

	@Override
	/**
	 * Returns the width of watch box.
	 */
	public float getLineWidth(){

		return 1.5f;

	}

	/**
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */
	private void setColor( Color clr ){
		
		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
		
	}
	
	@Override
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute iattr){

		if ( iattr instanceof IWatchBox ){
			IWatchBox ia = (IWatchBox)iattr;
			Color clr = ia.getColors()[0];
			if ( clr != null ){
				this.setColor(clr);
			}

			Color fillClr =  ia.getColors()[1];
			if ( fillClr != null ){
				symbolColor.setColorValue(
						new RGB(fillClr.getRed(), fillClr.getGreen(), fillClr.getBlue()));
			}
			
			fillBtn.setSelection(ia.getFillFlag());
			
			setShapeBtn(ia.getWatchBoxShape());
		}
	}
	
	private void setShapeBtn(WatchShape ws ){
		if (ws == WatchShape.NS ){
			nsBtn.setSelection(true);
			ewBtn.setSelection(false);
			esolBtn.setSelection(false);
		}
		else if (ws == WatchShape.EW ){
			ewBtn.setSelection(true);
			esolBtn.setSelection(false);
			nsBtn.setSelection(false);
		}
		else if (ws == WatchShape.ESOL ){
			esolBtn.setSelection(true);
			nsBtn.setSelection(false);
			ewBtn.setSelection(false);
		}
	}
	
/*	@Override
    public void setAttr( AbstractDrawableComponent adc ){
    	if ( adc instanceof DECollection && adc.getName().equalsIgnoreCase("Watch") ){
    		AbstractDrawableComponent wb = adc.getPrimaryDE();
    		if ( wb instanceof WatchBox ){
    			setAttrForDlg( (IAttribute)wb);
    		}
    	}
    }
*/
	/**
	 * Create widgets for the Color attribute
	 */
	private void createColorAttr(){
		
		Composite colorComp = new Composite(top, SWT.NONE);
		GridLayout gl = new GridLayout(2, false);
		colorComp.setLayout(gl);
		
		colorLbl = new Label( colorComp, SWT.LEFT );
		colorLbl.setText("Color:");

		cs = new ColorButtonSelector( colorComp ) ;
		cs.setColorValue( new RGB( 255,0, 255 ) );
	}

	/**
	 * Create widgets for watch shape and fill attributes
	 */	
	private void createShapeFillAttr(){
		
		Composite shapeFillComp = new Composite(top, SWT.NONE);

		shapeFillComp.setLayout( new RowLayout());
		Group shapeGrp = new Group(shapeFillComp, SWT.NONE);
		shapeGrp.setText("Shape");
		
		GridLayout shapeGl = new GridLayout(1, false);

		shapeGrp.setLayout(shapeGl);
		
		nsBtn = new Button(shapeGrp, SWT.RADIO);
		nsBtn.setText("NS");
		ewBtn = new Button(shapeGrp, SWT.RADIO);
		ewBtn.setText("EW");
		esolBtn = new Button(shapeGrp, SWT.RADIO);
		esolBtn.setText("ESOL");

		//set default
		esolBtn.setSelection(true);
		
		Group fillGrp = new Group(shapeFillComp, SWT.NONE);
		fillGrp.setText("Fill");
		GridLayout fillGl = new GridLayout(1, false);
		fillGl.marginBottom = 13;
		fillGrp.setLayout(fillGl);
		
		fillBtn = new Button(fillGrp, SWT.CHECK);
		fillBtn.setText("Use Fill");
		
		Composite colorComp = new Composite(fillGrp,SWT.NONE);
		colorComp.setLayout(new GridLayout(2, false));
		
		Label colorLbl = new Label( colorComp, SWT.LEFT );
		colorLbl.setText("Color:");
		
		symbolColor = new ColorButtonSelector( colorComp ) ;
		symbolColor.setColorValue( new RGB( 0,255,0 ) );

	}
	
	/**
	 * Create marker type list attribute
	 */
	private void createTypeAttr(){
		
		Composite typeGrp = new Composite( top, SWT.NONE ) ;
		GridLayout gl = new GridLayout( 2, false );
		gl.marginHeight = 1;
		gl.verticalSpacing = 1;
		gl.marginBottom = 0;
		typeGrp.setLayout( gl );
		Label symbolLabel = new Label(typeGrp, SWT.LEFT);
		symbolLabel.setText("Type:");

		symbolCombo = new SymbolCombo( typeGrp );
		symbolCombo.setLayoutData(new GridData(10, 1));
		symbolCombo.setItems(SYMBOL_LIST);
		
	}

	/**
	 * Create widgets for the Width attribute
	 */
	private void createWidthAttr(){

		Composite lineWidthGrp = new Composite( top, SWT.NONE ) ;
		GridLayout gl = new GridLayout( 3, false );
		gl.marginHeight = 1;
		gl.verticalSpacing = 1;
		lineWidthGrp.setLayout( gl );

		widthLbl = new Label(lineWidthGrp, SWT.LEFT);
		widthLbl.setText("Width:");
		widthLbl.setLayoutData( new GridData( 50, 20 ) );

		widthSlider = new Slider( lineWidthGrp, SWT.HORIZONTAL);
		widthSlider.setValues( 15, 1, 101, 1, 1, 1 );
		widthSlider.setLayoutData( new GridData( 100, 25 ) );

		widthSlider.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent e ) {
				widthText.setText( new Float(widthSlider.getSelection()/10.).toString() );
			}
		});

		widthText = new Text(lineWidthGrp,  SWT.SINGLE | SWT.BORDER );                        
		widthText.setLayoutData( new GridData( 30, 10 ) );
		widthText.setEditable( true );   
		widthText.setText( "1.5" );
		widthText.addListener(SWT.Verify, new Listener() {

			@Override
			public void handleEvent(Event e) {

				e.doit = PgenUtil.validateNumberTextField( e );

				if( e.doit && e.keyCode != 0){


					StringBuffer str = new StringBuffer(widthText.getText());
					str.insert(e.start,e.text);

					float value = 0;
					try {
						value = Float.parseFloat(new String(str) );
						if ( value >= .1 && value <= 10 ) {
							e.doit = true;
						}
						else {
							e.doit = false;
						}

					} catch ( NumberFormatException e1 ) {
						e.doit = false;
					}
				}

			}
		});

		widthText.addKeyListener( new KeyAdapter() {
			public void keyReleased( KeyEvent e ) {
				float value = 0;
				try {
					value = Float.parseFloat( widthText.getText() );
					if ( value >= .1 && value <= 10 ) {
						widthSlider.setSelection( (int)(value*10) );
					}

				} catch ( NumberFormatException e1 ) {
					//	lineWidthText.setToolTipText( "Only integer values between 1 and 10 are accepted." );
				}
			}
		});

	}
	
	/**
	 * Create widgets for the Size attribute
	 */
	private void createSizeAttr(){

        Composite sizeGrp = new Composite( top, SWT.NONE ) ;
        GridLayout gl = new GridLayout( 3, false );
		gl.marginHeight = 1;
		gl.verticalSpacing = 1;
        sizeGrp.setLayout( gl );

        sizeLbl = new Label(sizeGrp, SWT.LEFT);
		sizeLbl.setText("Size:");
        sizeLbl.setLayoutData( new GridData( 50, 20 ) );

        sizeSlider = new Slider( sizeGrp, SWT.HORIZONTAL);
        sizeSlider.setValues( 7, 1, 101, 1, 1, 1 );
        sizeSlider.setLayoutData( new GridData( 100, 25 ) );

        sizeSlider.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent e ) {
            	sizeText.setText( new Float(sizeSlider.getSelection()/10.).toString() );
            }
        });
            
        sizeText = new Text(sizeGrp,  SWT.SINGLE | SWT.BORDER );                        
        sizeText.setLayoutData( new GridData( 30, 10 ) );
        sizeText.setEditable( true );   
        sizeText.setText( "0.7" );
        sizeText.addListener(SWT.Verify, new Listener() {
        	
    		@Override
    		public void handleEvent(Event e) {
    				
    			e.doit = PgenUtil.validateNumberTextField( e );
    			
    			if( e.doit && e.keyCode != 0){
    		
    				StringBuffer str = new StringBuffer(sizeText.getText());
    				str.insert(e.start,e.text);

    				float value = 0;
    				try {
    					value = Float.parseFloat(new String(str) );
    					if ( value >= 0.1 && value <= 10 ) {
    						e.doit = true;
    					}
    					else {
    						e.doit = false;
    					}

    				} catch ( NumberFormatException e1 ) {
    					e.doit = false;
    				}
    			}
    			
    		}
        });
    			
        sizeText.addKeyListener( new KeyAdapter() {
            public void keyReleased( KeyEvent e ) {
                float value = 0;
            	try {
                    value = Float.parseFloat( sizeText.getText() );
                	if ( value >= .1 && value <= 10 ) {
                		sizeSlider.setSelection( (int)(value*10) );
                	}
                	
                } catch ( NumberFormatException e1 ) {
                //	lineWidthText.setToolTipText( "Only integer values between 1 and 10 are accepted." );
                }
            }
        });
	
		
	}
	
	/**
	 * Create Show/Hide button for watch info dialog
	 */
	private void createDispBtn(){

		dispBtn = new Button(top, SWT.PUSH);
		dispBtn.setText("Show Display");
		GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
		dispBtn.setLayoutData(gd);
		
		dispBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {
				openSpecDlg( false );
			} 

		});
	}

	/**
	 * Open the watch specification dialog
	 */
	public void openSpecDlg(final boolean addCountyMode ){
		if (dispBtn.getText().equalsIgnoreCase("Show Display")){
			final Shell shell = this.getShell();
			//make sure the spec dialog is on top
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					if (!( shell == null || shell.isDisposed() )) { // make sure the dialog is not closed
						dispBtn.setText("Hide Display");
						if ( infoDlg == null ){
							infoDlg = WatchInfoDlg.getInstance(WatchBoxAttrDlg.this.getParentShell(), INSTANCE);
						}
						infoDlg.setBlockOnOpen(false);

						infoDlg.open();
						infoDlg.clearCwaPane();
						infoDlg.createCWAs(wb.getWFOs());
						infoDlg.setStatesWFOs();
						
						if ( addCountyMode ){
							infoDlg.setAddDelCountyMode();
						}
					}
				}
			});
		
		}
		else {
			dispBtn.setText("Show Display");
			if( infoDlg != null ){
				infoDlg.close();
			}
		}
	}
	
	/**
	 * Enable/disable watch shape radio buttons 
	 * @param flag
	 */
	public void enableShapeBtn(boolean flag){
		nsBtn.setEnabled(flag);
		ewBtn.setEnabled(flag);
		esolBtn.setEnabled(flag);
	}
	
	/**
	 * Enable/disable the Show/Hide button
	 * @param flag
	 */
	public void enableDspBtn( boolean flag){
		if ( !dispBtn.isDisposed() )
			dispBtn.setEnabled(flag);
	}
	
	@Override
	/**
	 * Close the watch attribute dialog and the watch info dialog 
	 */
	public boolean close(){
		this.drawingLayer.removeSelected();
		wbTool = null;
		if( infoDlg != null ){
			infoDlg.close();
		}
		return super.close();
	}
	
    @Override
    /**
     * Get the color to fill county maps
     */
    public Color getFillColor(){
    	if ( fillBtn.getSelection() ){
    		return new java.awt.Color( symbolColor.getColorValue().red,
					symbolColor.getColorValue().green, symbolColor.getColorValue().blue );
    		}
    	else {
    		return null;
    	}
    }
    
    @Override
    /**
     * Get watch box shape
     */
    public WatchShape getWatchBoxShape(){
    	
    	if ( nsBtn.getSelection()) return WatchShape.NS;
    	else if ( ewBtn.getSelection()) return WatchShape.EW;
    	else if ( esolBtn.getSelection()) return WatchShape.ESOL;
    	else return null;
    	
    }

	/**
	 * Set the watch box for the attribute dialog
	 * @param wb - the wb to set
	 */
	public void setWatchBox(WatchBox wb) {
		this.wb = wb;
	}

	/**
	 * Get the watch box of the attribute dialog
	 * @return - watch box
	 */
	public WatchBox getWatchBox() {
		return wb;
	}
	
	/**
	 * Get the instance of watch info dialog
	 * @return
	 */
	public WatchInfoDlg getWatchInfoDlg(){
		return infoDlg;
	}
	
	/**
	 * Get the symbol type of counties
	 */
	public String getWatchSymbolType(){
		return symbolCombo.getSelectedText();
	}
	
	/**
	 * Get symbol width
	 */
	public float getWatchSymbolWidth(){
		return widthSlider.getSelection()/10.f;
	}
	
	/**
	 * Get symbol Size
	 */
	public double getWatchSymbolSize(){
		return sizeSlider.getSelection()/10.;
	}
	
	/**
	 * Updates the selected watch box and redraws the PGEN layer.
	 */
	public void okPressed(){

		
			ArrayList<AbstractDrawableComponent> adcList = null;
			ArrayList<AbstractDrawableComponent> newList = new ArrayList<AbstractDrawableComponent>() ;

			// get the list of selected elements
			if ( drawingLayer != null ) {
				adcList = (ArrayList<AbstractDrawableComponent>) drawingLayer.getAllSelected();
			}

			if ( adcList != null && !adcList.isEmpty() ){

				//loop through the list and update attributes
				for ( AbstractDrawableComponent adc : adcList){

					WatchBox el = (WatchBox)adc.getPrimaryDE();

					if ( el != null ){

						// Create a copy of the currently selected element
						WatchBox newEl = (WatchBox)el.copy();
						newEl.setCountyList(el.getCountyList());
						
						// Update the new Element with these current attributes
						newEl.update(this);
						wb = (WatchBox)newEl;
						
						//re-calculate points of watch box if shape changes 
						boolean updateShape = false;
						if ( ((WatchBox)el).getWatchBoxShape() !=  this.getWatchBoxShape()){
							ArrayList<Station> anchorsInPoly =  PgenWatchBoxDrawingTool.getAnchorsInPoly(mapEditor, 
									WatchBox.generateWatchBoxPts(this.getWatchBoxShape(),
											wb.getHalfWidth(), wb.getPoints().get(0),  wb.getPoints().get(5)));
							if ( anchorsInPoly != null && !anchorsInPoly.isEmpty() ){
								DECollection dec = new DrawableElementFactory().createWatchBox("Met", "Watch", this.getWatchBoxShape(), 
										wb.getPoints().get(0),  wb.getPoints().get(5), anchorsInPoly, this);
								if ( dec != null ) {
									 wb.setLinePoints(((WatchBox)dec.getPrimaryDE()).getPoints());
									 wb.setAnchors(((WatchBox)dec.getPrimaryDE()).getAnchors()[0], ((WatchBox)dec.getPrimaryDE()).getAnchors()[1]);
									 updateShape = true;
								}
							}
						}
						
						//if for any reason, shape cannot be changed, set the shape bake to the original.
						if ( !updateShape ){
							wb.setWatchBoxShape(el.getWatchBoxShape());
							this.setShapeBtn(wb.getWatchBoxShape());
						}
						
						newList.add(newEl);
					}
				}

				ArrayList<AbstractDrawableComponent> oldList = new ArrayList<AbstractDrawableComponent>(adcList);
				drawingLayer.replaceElements(oldList, newList);
			}

			drawingLayer.removeSelected();

			//set new elements as selected
			for ( AbstractDrawableComponent adc : newList ){
				drawingLayer.addSelected(adc);
			}

		if ( mapEditor != null ) {
			mapEditor.refresh();
		}

	}
	
	@Override
	/**
	 * Get the fill flag.
	 */
	public boolean getFillFlag(){
		return fillBtn.getSelection();
	}

	/**
	 * Set the watch box modifying tool
	 * @param wbTool
	 */
	public void setWbTool(PgenWatchBoxModifyTool wbTool) {
		this.wbTool = wbTool;
	}

	/**
	 * Get the watch box modifying tool
	 * @return
	 */
	public PgenWatchBoxModifyTool getWbTool() {
		return wbTool;
	}

	@Override
	public Station[] getAnchors() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<SPCCounty> getCountyList() {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public List<SPCCounty> getOriginalCountyList() {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public int getWatchNumber() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getIssueFlag() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public Coordinate[] getLinePoints() {
		// TODO Auto-generated method stub
		return null;
	}
	

	@Override
	public void createButtonsForButtonBar(Composite parent){
		((GridLayout)parent.getLayout()).verticalSpacing = 0;
		((GridLayout)parent.getLayout()).marginHeight = 3;
		super.createButtonsForButtonBar(parent);
  		this.getButton(IDialogConstants.CANCEL_ID).setEnabled(false);
  		this.getButton(IDialogConstants.OK_ID).setEnabled(false);
  		
  		this.getButton(IDialogConstants.CANCEL_ID).setLayoutData( new GridData(90,30));
  		this.getButton(IDialogConstants.OK_ID).setLayoutData( new GridData(90,30));

	}

	@Override
	public Control createButtonBar(Composite parent){
		Control bar = super.createButtonBar(parent);
		GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
		gd.widthHint = 220;
		gd.heightHint = 35;
		
		bar.setLayoutData(gd);
		return bar;
		
	}
}

