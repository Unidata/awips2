/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.SymbolAttrDlg
 * 
 * 20 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.awt.Color;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ICombo;
import gov.noaa.nws.ncep.ui.pgen.display.ISymbol;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

/**
 * Singleton attribute dialog for symbols.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/09					B. Yin   	Initial Creation.
 * 04/09        90          B. Hebbard  Replace ColorSelector with ColorMatrixSelector.
 * 05/09		116			B. Yin		Enable lat/lon text fields
 * 08/09		149			B. Yin		Modified OkPressed to handle MultiSelect
 * 09/09		149			B. Yin		Added check boxes for multi-selection
 * 03/10        231         Archana     Altered the Symbol attribute dialog
 *                                      to display only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix . 
 * 11/10		?			B. Yin		Set the dialog title to the PgenCategory(symbol/combo/marker) 
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * </pre>
 * 
 * @author	B. Yin
 */

public class SymbolAttrDlg extends AttrDlg implements ISymbol{
	
	static private SymbolAttrDlg INSTANCE = null;
	
	protected static enum ChkBox { COLOR, CLEAR, WIDTH, SIZE, LAT, LON, LABEL };

	
	protected Composite top = null;
	
	protected Label colorLbl;
	private ColorButtonSelector cs = null;	
	
	protected Label clearLbl;
	protected Button clearBtn1 = null;
	protected Button clearBtn2 = null;
	
	protected Label widthLbl; 
	protected Slider widthSlider = null;
	protected Text widthText = null;
	protected gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider widthSpinnerSlider = null;	
	
	protected Label sizeLbl;
	protected Slider sizeSlider = null;
	protected Text sizeText = null;
	protected gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider sizeSpinnerSlider = null;
	
	protected Label latitudeLabel; 
	protected Text latitudeText = null;
	
	protected Label longitudeLabel; 
	protected Text longitudeText = null;
	
	protected Button placeBtn = null;
	private Button undoBtn = null;
	private boolean keyEvent = false;
	
	// location when 'Place Symbol' is pressed. (to undo)
	private Coordinate prevLoc;
	
    //Check boxes for multi-selection
    protected Button chkBox[];
    
	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	protected SymbolAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);

    }
	
	/**
	 * Creates a symbol attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static SymbolAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new SymbolAttrDlg( parShell );
				
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
	        GridLayout mainLayout = new GridLayout(3, false);
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
		
		String title = pgenCategory;
		if ( pgenCategory.equalsIgnoreCase("Combo")){
			title = pgenCategory +" Symbol";
		}
		
        this.getShell().setText(title + " Attributes");
        chkBox = new Button[7];
        
        createColorAttr();
        createClearAttr();
        createWidthAttr();
        createSizeAttr();
        createLatAttr();
        createLonAttr();
        
	}	
	
	/**
	 * Sets the text in the latitude field of the dialog
	 * @param lat
	 */
	public void setLatitude( double lat ){
		
		latitudeText.setText( new DecimalFormat("###.000").format( lat ));
	  		
	}
	
	/**
	 * Sets the text in the longitude field of the dialog
	 * @param lon
	 */	
	public void setLongitude( double lon ){
		
		longitudeText.setText(new DecimalFormat("####.000").format(lon));
		
	}
	
	/**
	 * Return color from the color picker of the dialog
	 */
	public Color[] getColors(){
		if ( chkBox[ChkBox.COLOR.ordinal()].getSelection() ){
		  // IAttribute requires to return an array of colors
		  // Only the first color is used at this time.
	      Color[] colors = new Color[2];
          
          colors[0] =new java.awt.Color( cs.getColorValue().red,
				cs.getColorValue().green, cs.getColorValue().blue );
          
          colors[1] = Color.green;

          return colors;
		}
          else {
  			return null;
  		}
	}
	
	/**
	 * Returns the line width from the dialog.
	 */
	public float getLineWidth(){
		if ( chkBox[ChkBox.WIDTH.ordinal()].getSelection() ){
			return widthSpinnerSlider.getSelection();
			//return widthSlider.getSelection();
		}
		else {
			return java.lang.Float.NaN;
		}
		
	}
	
	/**
	 * Returns the size scale from the dialog
	 */
	public double getSizeScale(){
		
		if ( chkBox[ChkBox.SIZE.ordinal()].getSelection() ){
			//double d = 1.0; try{d=Double.parseDouble(sizeText.getText());}catch(Exception e){d=(double)sizeSlider.getSelection();}
			return sizeSpinnerSlider.getSelection()/ Math.pow(10, sizeSpinnerSlider.getDigits());			//return (d==0.0 || d>10.0) ? 1.0 : d;//sizeSlider.getSelection();
		}
		else {
			return java.lang.Double.NaN;
		}
	}
	
	/**
	 * Returns the flag that indicates whether Clear is set or not.
	 */
	public Boolean isClear(){
		
		if ( chkBox[ChkBox.CLEAR.ordinal()].getSelection() ){
			return clearBtn1.getSelection();
		}
		else {
			return null;
		}
		
	}
	
	/**
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */
	public void setColor( Color clr ){
		
		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
		
	}
	
	/**
	 * Sets the line width value of the dialog.
	 * @param lw
	 */
	private void setLineWidth( float lw ){
		widthSpinnerSlider.setSelection((int)lw);		
		//widthSlider.setSelection( (int)(lw) );
		//widthText.setText(String.valueOf((int)lw));
	}
	
	/**
	 * Sets the size value of the dialog. 
	 * @param size
	 */
	private void setSize( double size ){
		sizeSpinnerSlider.setSelection( (int) (size * Math.pow(10, sizeSpinnerSlider.getDigits())) );		
		//size = ( size==0.0 ? 0.1 : size ); 
		//sizeSlider.setSelection( (int)(size*10) );				
		//sizeText.setText(String.format("%1$4.1f", size) );//String.valueOf((int)size));
	}
	
	/**
	 * Sets the Clear flag of the dialog.
	 * @param clr
	 */
	private void setClear( Boolean clr ){
		if ( clr ) {
			clearBtn1.setSelection(true);
			clearBtn2.setSelection(false);
		}
		else {
			clearBtn1.setSelection(false);
			clearBtn2.setSelection(true);
		}
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute iattr){

		if ( iattr instanceof ISymbol ){
			ISymbol ia = (ISymbol)iattr;
			Color clr = ia.getColors()[0];
			if ( clr != null ) this.setColor(clr);
			
			float lw =  ia.getLineWidth();
			if ( lw > 0 ) this.setLineWidth(lw);
			
			this.setClear( ia.isClear() );

			double size = ia.getSizeScale();
			if ( size >= 0 ) this.setSize(size);
		}
		else if ( iattr instanceof ICombo ){
			ICombo ia = (ICombo)iattr;
			Color clr = ia.getColors()[0];
			if ( clr != null ) this.setColor(clr);
			
			float lw =  ia.getLineWidth();
			if ( lw > 0 ) this.setLineWidth(lw);
			
			this.setClear( ia.isClear() );

			double size = ia.getSizeScale();
			if ( size >= 0 ) this.setSize(size);
		}
	}

	/**
	 * A key event listener for lat/lon text fields. 
	 * @author bingfan
	 *
	 */
	public class LatLonKeyListener extends KeyAdapter {
		
		@Override
		public void keyPressed(KeyEvent e) {

			keyEvent = true;
			drawingLayer.removeGhostLine();
			mapEditor.refresh();

		}

	}
	
	/**
	 * A Listener for lat/lon text fields to make sure input 
	 * can only be numbers, '-' or '.'
	 * @author bingfan
	 *
	 */
	public class LatLonVerifyListener implements Listener {
	
		@Override
		public void handleEvent(Event e) {
			
			if (keyEvent){
				
				e.doit = PgenUtil.validateNumberTextField( e );
				
			}
			
			if (!e.doit){
				
				keyEvent = false;
				
			}
		}
	}
	
	/**
	 * A Listener class that implements ModifyListener to validate 
	 * the latitude text field.
	 * @author bingfan
	 *
	 */	
	public class LatModifyListener implements ModifyListener {
    	@Override
    	public void modifyText(ModifyEvent e) {
    		
    		if ( keyEvent ){

    			//undoBtn.setEnabled(false);
    			Text txt = (Text)e.widget;

    			try{

    				if ( Double.valueOf( txt.getText()) > 90 ||
    						Double.valueOf( txt.getText()) < -90 ) {

    					placeBtn.setEnabled(false);

    				}
    				else {

    					placeBtn.setEnabled(true);

    				}
    			}
    			catch ( NullPointerException excp ){

    				placeBtn.setEnabled(false);

    			}
    			catch ( NumberFormatException excp){

    				placeBtn.setEnabled(false);

    			}

    			keyEvent = false;
    		}
    		else {

    			/*
    			 * If not key event, disable 'Place Symbol' button 
    			 * and 'Undo Symbol' button.
    			 */
    			placeBtn.setEnabled(false);
    			//undoBtn.setEnabled(false);

    		}
    	}
	}
	
	/**
	 * A Listener class that implements ModifyListener to validate 
	 * the longitude text field.
	 * @author bingfan
	 *
	 */
	public class LonModifyListener implements ModifyListener {
    	@Override
    	public void modifyText(ModifyEvent e) {
    		
    		if ( keyEvent ){

    			//undoBtn.setEnabled(false);
    			Text txt = (Text)e.widget;

    			try{

    				if ( Double.valueOf( txt.getText()) > 180 ||
    						Double.valueOf( txt.getText()) < -180 ) {

    					placeBtn.setEnabled(false);

    				}
    				else {
    					
    					placeBtn.setEnabled(true);

    				}
    			}
    			catch ( NullPointerException excp){
    				
    				placeBtn.setEnabled(false);

    			}
    			catch ( NumberFormatException excp){
    				
    				placeBtn.setEnabled(false);

    			}
    			
    			keyEvent = false;
    		}
    		else {

    			/*
    			 * If not key event, disable 'Place Symbol' button 
    			 * and 'Undo Symbol' button.
    			 */
    			placeBtn.setEnabled(false);
    			//undoBtn.setEnabled(false);
    			
    		}
    	}
	}
	
	/**
	 * Update the symbol.
	 * Disable 'Place Symbol' button and 'Undo Symbol' button.
	 * 
	 */
	@Override
	public void okPressed(){
		
		ArrayList<AbstractDrawableComponent> adcList = null;
		ArrayList<AbstractDrawableComponent> newList = new ArrayList<AbstractDrawableComponent>() ;

		//get the list of selected symbols
		if ( drawingLayer != null ) {
			adcList = (ArrayList<AbstractDrawableComponent>) drawingLayer.getAllSelected();
		}
		
		if ( adcList != null && !adcList.isEmpty() ){
			
			DrawableElement newEl = null;
			//loop through the list and update attributes
			for ( AbstractDrawableComponent adc : adcList){

				DrawableElement el = adc.getPrimaryDE();	

				if ( el != null ){
					// Create a copy of the currently selected element
					newEl = (DrawableElement)el.copy();
					// Update the new Element with these current attributes
					newEl.update(this);
					if ( latitudeText.isEnabled() && longitudeText.isEnabled()){
						ArrayList<Coordinate>loc = new ArrayList<Coordinate>();
						loc.add(new Coordinate(Double.valueOf(longitudeText.getText()),
								Double.valueOf(latitudeText.getText())));
						newEl.setPoints( loc );
					}
					
					newList.add(newEl);

				}
			}
			
			if ( newEl != null ){
				AttrSettings.getInstance().setSettings( newEl );
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
		
		placeBtn.setEnabled(false);
		undoBtn.setEnabled(false);
		
	}
	
	/**
	 * return the status of the label check box
	 * @return
	 */
	public boolean labelEnabled(){
		return false;
	}
	
	/**
	 * Enable/disable Lat/Lon text fields.
	 * @param flag
	 */
	public void enableLatLon(boolean flag){
		latitudeText.setEnabled(flag);
		longitudeText.setEnabled(flag);
	}

	/**
	 * Create widgets for the Color attribute
	 */
	private void createColorAttr(){
		chkBox[ChkBox.COLOR.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.COLOR.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.COLOR.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					colorLbl.setEnabled(true);
				}
				else {
					colorLbl.setEnabled(false);
				}
			}

		});  

		colorLbl = new Label( top, SWT.LEFT );
		colorLbl.setText("Color:");
		cs = new ColorButtonSelector( top );
		cs.setColorValue( new RGB( 0,255,0 ) );
	}

	/**
	 * Create widgets for the Clear attribute
	 */	
	private void createClearAttr(){
		chkBox[ChkBox.CLEAR.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.CLEAR.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.CLEAR.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					clearLbl.setEnabled(true);
					clearBtn1.setEnabled(true);
					clearBtn2.setEnabled(true);
				}
				else {
					clearLbl.setEnabled(false);
					clearBtn1.setEnabled(false);
					clearBtn2.setEnabled(false);
				}
			}

		}); 

		clearLbl = new Label(top, SWT.LEFT);
		clearLbl.setText("Clear:");

		Group clearGroup = new Group(top, SWT.NONE);
		GridLayout gl = new GridLayout(2, false);
		clearGroup.setLayout(gl);

		clearBtn1 = new Button(clearGroup, SWT.RADIO);
		clearBtn1.setText("On");
		clearBtn1.setSelection(true);

		clearBtn2 = new Button(clearGroup, SWT.RADIO);
		clearBtn2.setText("Off");   
	}

	/**
	 * Create widgets for the Width attribute
	 */
	private void createWidthAttr(){
		chkBox[ChkBox.WIDTH.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.WIDTH.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.WIDTH.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					widthLbl.setEnabled(true);
					widthSpinnerSlider.setEnabled(true);
				}
				else {
					widthLbl.setEnabled(false);
					widthSpinnerSlider.setEnabled(false);
				}
			}

		}); 

		widthLbl = new Label(top, SWT.LEFT);
		widthLbl.setText("Width:");

		GridLayout gl = new GridLayout( 3, false );	
		Group widthGrp = new Group( top, SWT.NONE ) ;	    
		widthGrp.setLayout( gl );
/*		
		widthSlider = new Slider(widthGrp, SWT.HORIZONTAL);
		widthSlider.setValues(2, 1, 20+2, 2, 1, 3);	
		
		widthText = new Text(widthGrp, SWT.SINGLE|SWT.BORDER);
		widthText.setLayoutData(new GridData(30,10));
		gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.SliderTxtKeyLtnVry listener = 
			new gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.SliderTxtKeyLtnVry(widthSlider, widthText,1,20);
		widthText.addKeyListener(listener );
		widthText.addVerifyListener(listener);
*/		
		
        widthSpinnerSlider = 
        	new gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider(widthGrp, SWT.HORIZONTAL,1);
        widthSpinnerSlider.setLayoutData(new GridData(180,30));
        widthSpinnerSlider.setMinimum(1);            
        widthSpinnerSlider.setMaximum(10);
        widthSpinnerSlider.setIncrement(1);
        widthSpinnerSlider.setPageIncrement(3);        
        widthSpinnerSlider.setDigits(0);
	}
	
	/**
	 * Create widgets for the Size attribute
	 */
	private void createSizeAttr(){
		chkBox[ChkBox.SIZE.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.SIZE.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.SIZE.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					sizeLbl.setEnabled(true);
					 sizeSpinnerSlider.setEnabled(true);
				}
				else {
					sizeLbl.setEnabled(false);
					 sizeSpinnerSlider.setEnabled(false);
				}
			}

		}); 

		sizeLbl = new Label(top, SWT.LEFT);
		sizeLbl.setText("Size:");

		GridLayout gl = new GridLayout( 3, false );	
		Group sizeGrp = new Group( top, SWT.NONE ) ;	    
		sizeGrp.setLayout( gl );		
/*		
		sizeSlider = new Slider(sizeGrp, SWT.HORIZONTAL);
		sizeSlider.setValues(20,1,100+20,20,1,10);//2, 1, 20+2, 2, 1, 3);      
		
		sizeText = new Text(sizeGrp, SWT.SINGLE|SWT.BORDER);
		sizeText.setLayoutData(new GridData(30,10));
		sizeText.setEditable(true);
		sizeText.setText(""+0.1);
		gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.SliderTxtKeyLtnVry listener = 
			new gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.SliderTxtKeyLtnVry(sizeSlider, sizeText,0.1,10.0);
		sizeText.addKeyListener(listener);
		sizeText.addVerifyListener(listener);
*/		
		
///*        
        sizeSpinnerSlider = 
        	new gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider(sizeGrp, SWT.HORIZONTAL,1);
        sizeSpinnerSlider.setLayoutData(new GridData(180,30));
        sizeSpinnerSlider.setMinimum(1);            
        sizeSpinnerSlider.setMaximum(100);
        sizeSpinnerSlider.setIncrement(1);
        sizeSpinnerSlider.setPageIncrement(10);        
        sizeSpinnerSlider.setDigits(1);
        //default size
		sizeSpinnerSlider.setSelection( (int) (1 * Math.pow(10, sizeSpinnerSlider.getDigits())) );		
//*/        
	}
	
	/**
	 * Create widgets for the Latitude field
	 */
	private void createLatAttr(){
		chkBox[ChkBox.LAT.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.LAT.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.LAT.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					latitudeLabel.setEnabled(true);
					latitudeText.setEnabled(true);
				}
				else {
					latitudeLabel.setEnabled(false);
					latitudeText.setEnabled(false);
				}
			}

		});

		chkBox[ChkBox.LAT.ordinal()].setVisible(false);

		latitudeLabel = new Label(top, SWT.NONE);
		latitudeLabel.setText("Latitude:");

		Composite latGroup = new Composite(top, SWT.NONE);
		latGroup.setLayout(new RowLayout(SWT.HORIZONTAL));

		latitudeText = new Text(latGroup, SWT.SINGLE | SWT.RIGHT
				| SWT.BORDER );
		latitudeText.setTextLimit(8);
		latitudeText.setLayoutData(new RowData(new Point(80,20)));

		placeBtn = new Button(latGroup, SWT.PUSH);
		placeBtn.setText("Place Symbol");
		placeBtn.setEnabled(false);
		placeBtn.setLayoutData(new RowData(new Point(120,28)));
		placeBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {

				DrawableElementFactory def = new DrawableElementFactory();

				/*
				 * Check if new element should be a Symbol or ComboSymbol
				 */
				DrawableType which = DrawableType.SYMBOL;
				if ( pgenCategory.equals("Combo") ) which = DrawableType.COMBO_SYMBOL;

				DrawableElement elem = (DrawableElement)def.create(which, SymbolAttrDlg.this, pgenCategory,
						pgenType, new Coordinate(Double.parseDouble(longitudeText.getText()),
								Double.parseDouble(latitudeText.getText())),
								drawingLayer.getActiveLayer());

				if ( drawingLayer.getSelectedDE() != null ){
					prevLoc = ((ISymbol)drawingLayer.getSelectedDE()).getLocation();
					drawingLayer.replaceElement(drawingLayer.getSelectedDE(), elem);
					drawingLayer.setSelected(elem);
				}
				else if (SymbolAttrDlg.this.labelEnabled()){
            		DECollection dec = new DECollection("labeledSymbol");
            		dec.setPgenCategory(pgenCategory);
            		dec.setPgenType(pgenType);
            		dec.addElement(elem);
            		drawingLayer.addElement(dec);
            		
            		String defaultTxt = "";
            		if ( SymbolAttrDlg.this instanceof VolcanoAttrDlg ){
            			defaultTxt = ((VolcanoAttrDlg)SymbolAttrDlg.this).getVolText();
            			dec.setCollectionName("Volcano");
            		}
            		PgenUtil.setDrawingTextMode( true, ((LabeledSymbolAttrDlg)SymbolAttrDlg.this).useSymbolColor(), defaultTxt, dec );
				}
				else {
					drawingLayer.addElement(elem);
					placeBtn.setEnabled(false);
					undoBtn.setEnabled(true);
				}

				mapEditor.refresh();

			} 

		});
	}

	/**
	 * Create widgets for the Longitude field
	 */
	private void createLonAttr(){
		chkBox[ChkBox.LON.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.LON.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.LON.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					longitudeLabel.setEnabled(true);
					longitudeText.setEnabled(true);
				}
				else {
					longitudeLabel.setEnabled(false);
					longitudeText.setEnabled(false);
				}
			}

		});

		chkBox[ChkBox.LON.ordinal()].setVisible(false);

		longitudeLabel = new Label( top, SWT.None );
		longitudeLabel.setText("Longitude:");

		Composite lonGroup = new Composite(top, SWT.NONE);
		lonGroup.setLayout(new RowLayout(SWT.HORIZONTAL));
		longitudeText = new Text(lonGroup, SWT.SINGLE | SWT.RIGHT | SWT.BORDER );
		longitudeText.setTextLimit(8);
		longitudeText.setLayoutData(new RowData(new Point(80,20)));

		latitudeText.addKeyListener( new LatLonKeyListener() );
		latitudeText.addModifyListener(new LatModifyListener());
		latitudeText.addListener(SWT.Verify, new LatLonVerifyListener());

		longitudeText.addKeyListener( new LatLonKeyListener() ); 
		longitudeText.addModifyListener(new LonModifyListener());
		longitudeText.addListener(SWT.Verify, new LatLonVerifyListener());

		undoBtn = new Button(lonGroup, SWT.PUSH);
		undoBtn.setText("Undo Symbol");
		undoBtn.setEnabled(false);
		undoBtn.setLayoutData(new RowData(new Point(120,28)));


		undoBtn.addListener(SWT.MouseDown, new Listener(){

			@Override
			public void handleEvent(Event event) {

				if ( undoBtn.getText().equalsIgnoreCase("Undo Symbol")){
					
					undoBtn.setText("Redo Symbol");
					drawingLayer.getCommandMgr().undo();
					
					//placeBtn.setEnabled(true);
					//undoBtn.setEnabled(false);
				}
				else if  ( undoBtn.getText().equalsIgnoreCase("Redo Symbol") ) {
					undoBtn.setText("Undo Symbol");
					drawingLayer.getCommandMgr().redo();
					
				}
				
				if ( drawingLayer.getSelectedDE() != null ){

					drawingLayer.setSelected(drawingLayer.getNearestElement( prevLoc ));

				}

				mapEditor.refresh();

			} 
		});

	}
	
	public void enableUndoBtn( boolean flag ){
		undoBtn.setEnabled( flag );
	}

	@Override
	public String getPatternName() {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
    public Coordinate getLocation(){
    	return null;
    }
}
