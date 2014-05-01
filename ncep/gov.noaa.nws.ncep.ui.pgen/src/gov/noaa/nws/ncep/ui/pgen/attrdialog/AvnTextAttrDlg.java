/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.AvnTextAttrDlg
 * 
 * 29 July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.HashSet;
import java.util.Set;
import java.awt.Color;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.display.IAvnText;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

/**
 * Singleton attribute dialog for Aviation Text elements.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/09		104			S. Gilbert   	Initial Creation.
 * 09/09		149			B. Yin			Added check boxes for multi-selection
 * 03/10        231         Archana         Altered the dialog for  
 *                                          aviation text elements to display 
 *                                          only a button showing the selected 
 *                                          color instead of displaying 
 *                                          the complete color matrix.
 * 04/11		#?			B. Yin			Re-factor IAttribute
 * 03/13		#928		B. Yin			Added a separator above the button bar.
 * </pre>
 * 
 * @author	J. Wu
 */

public class AvnTextAttrDlg extends AttrDlg implements IAvnText{
	
	public static enum FontSizeName { TINY, SMALL, MEDIUM, LARGE, HUGE, GIANT };
	public static int[] FontSizeValue =  { 10, 12, 14, 18, 24, 34 };
	//public static final String[] FontName = new String[]{ "Courier", "Helvetica", "Times" };
	public static final String[] FontName = new String[]{ "Courier", "Nimbus Sans L", "Liberation Serif" };

	public static final String[] BoxName = new String[]{ "Normal", "Boxed", "Blanked", "Outline" };
	
	private static final String[] ICING_LIST = new String[] { "ICING_00", "ICING_01", "ICING_02",
															  "ICING_03", "ICING_04", "ICING_05",
															  "ICING_06", "ICING_07", "ICING_08",
															  "ICING_09", "ICING_10" };

	private static final String[] TURB_LIST = new String[] { "TURBULENCE_0", "TURBULENCE_1", "TURBULENCE_2",
		   													 "TURBULENCE_3", "TURBULENCE_4", 
		   													 "TURBULENCE_4|TURBULENCE_6", "TURBULENCE_5",
		   													 "TURBULENCE_6", "TURBULENCE_6|TURBULENCE_7",
		   													 "TURBULENCE_7", "TURBULENCE_8" };

	private static enum ChkBox { TYPE, TOP, BOTTOM, SIZE, FONT, STYLE, JUST, COLOR, SYMBOL };
	
	static AvnTextAttrDlg INSTANCE = null;
       
	private Composite top = null;
	
	private Label colorLbl;
    private ColorButtonSelector cs = null;
    
    private Label topLabel;
    private Text topValue = null;
    
    private Text bottomValue = null;
    private Label bottomLabel = null;
    
    private Label typeLabel;
    private Combo typeCombo = null;
    
    private Label sizeLbl;
    private Combo sizeCombo = null;
    
    private Label fontLbl;
    private Combo fontCombo = null;
    
    private Label styleLbl;
    private Combo styleCombo = null;
    
    private Label justLbl;
    private Combo justCombo = null;
    
    private SymbolCombo symbolCombo = null;
    private Label symbolLabel = null;

    private Set<Control> enableList;  
    
    private Button chkBox[];

    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private AvnTextAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);

    }
	
	/**
	 * Creates an aviation text attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static AvnTextAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new AvnTextAttrDlg( parShell );
				
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
	        initializeComponents();

	        return top;
	        
	}
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 * 
	 */
	private void initializeComponents() {
		
		enableList = new HashSet<Control>();
		
        this.getShell().setText("Aviation Text Attributes");
        
        chkBox = new Button[9];

        /*
         * Type of Aviation Text element
         */
        createTypeAttr();
        
        /*
         * Top flight level
         */
        createTopAttr();
        
        /*
         * bottom flight level
         */
        createBottomAttr();

        /*
         * Text font size
         */
        createSizeAttr();
        
 
        /*
         * Text font
         */
        createFontAttr();
       
        /*
         * Font style
         */
        createStyleAttr();
        
        /*
         * Text justification
         */
        createJustAttr();
  
        /*
         * Element color
         */
       createColorAttr();

        /*
         * for icing or turbulence symbol if/when needed
         */
       createSymbolAttr();
       
		if ( PgenSession.getInstance().getPgenPalette().getCurrentAction()
				.equalsIgnoreCase("MultiSelect")){
			enableChkBoxes(true);
			enableAllWidgets(false);
		}
		else {
			enableChkBoxes(false);
			
			// Set dialog based on Aviation Text type
			updateDialog(AviationTextType.values()[0]);
		}
  
        addSeparator(top.getParent());
        
	}	
	
	/*
	 * Configures the AvnTextAttrDlg dialog based on the selected 
	 * Aviation Text Type
	 */
	private void updateDialog(AviationTextType atype) {

		// Set Avn Text Type
        typeCombo.setText(atype.toString());

        /*
         * Enable all widgets and empty symbol combo 
         */
		for ( Control widget : enableList ) {
    		widget.setEnabled(true);
    	}
    	symbolCombo.removeAll();
    	
    	/*
    	 * Disable inappropriate widgets based on avn text type
    	 */
    	Set<Control> disableList = getDisableList( atype );
    	for ( Control widget : disableList ) {
    		widget.setEnabled(false);
    	}
    	
    	/*
    	 * When multi-selecting, disable bottom and symbol if 
    	 * their check boxes are not set. 
    	 */
    	if ( !chkBox[ChkBox.BOTTOM.ordinal()].getSelection() ){
    		bottomLabel.setEnabled(false);
    		bottomValue.setEnabled(false);
    	}
    	
    	if ( !chkBox[ChkBox.SYMBOL.ordinal()].getSelection() ){
    		symbolLabel.setEnabled(false);
    		symbolCombo.setEnabled(false);
    	}
    	
    	/*
    	 * Set Symbols in symbolCombo widget, if appropriate
    	 */
    	if ( atype == AviationTextType.MID_LEVEL_ICING ) {
    		setIcingSymbols();
    	}
    	else if ( atype == AviationTextType.LOW_LEVEL_TURBULENCE ||
    			  atype == AviationTextType.HIGH_LEVEL_TURBULENCE ) {
    		setTurbulenceSymbols();
    	}
    	//  re-pack dialog
    	top.pack();
		
	}
	
	/**
	 * Add the set of Turbulence symbols to the symbolCombo selector 
	 */
	protected void setTurbulenceSymbols() {
		symbolCombo.setItems(TURB_LIST);
		symbolCombo.select(4);
	}

	/**
	 * Add the set of Icing symbols to the symbolCombo selector 
	 */
	protected void setIcingSymbols() {
		symbolCombo.setItems(ICING_LIST);
		symbolCombo.select(5);
	}

	/**
	 * Get a list of widgets that should be disabled for the fgiven avn text type
	 * @param Aviation Text type
	 * @return widgets to be diabled 
	 */
	protected Set<Control> getDisableList(AviationTextType type) {
	    Set<Control> disableList = new HashSet<Control>();

	    switch (type) {
	    case LOW_PRESSURE_BOX:
	    case HIGH_PRESSURE_BOX:
	    case FLIGHT_LEVEL:
	    case FREEZING_LEVEL:
	    	disableList.add(bottomLabel);
	    	disableList.add(bottomValue);
	    	disableList.add(symbolLabel);
	    	disableList.add(symbolCombo);
	    	return disableList;
	    	
	    case CLOUD_LEVEL:
	    	disableList.add(symbolLabel);
	    	disableList.add(symbolCombo);
	    	return disableList;
	    }
	    
		return disableList;
	}

	/**
	 * Return font size from the font size combo
	 */	
	public float getFontSize(){
		if ( chkBox[ChkBox.SIZE.ordinal()].getSelection() ){
			return ( FontSizeValue[ sizeCombo.getSelectionIndex() ] );
		}
		else {
			return java.lang.Float.NaN;
		}
	}

	/**
	 * Return font name from the font combo
	 */	
	public String getFontName(){
		if ( chkBox[ChkBox.FONT.ordinal()].getSelection() ){
			return fontCombo.getText();
		}
		else {
			return null;
		}
	}
	
	/**
	 * Return font style from the style combo
	 */	
	public FontStyle getStyle(){
		if ( chkBox[ChkBox.STYLE.ordinal()].getSelection() ){
			return FontStyle.values()[ styleCombo.getSelectionIndex() ];
		}
		else {
			return null;
		}
	}
	
	/**
	 * Return TextJustification from the justification combo
	 */	
	public TextJustification getJustification(){
		if ( chkBox[ChkBox.JUST.ordinal()].getSelection() ){
			return TextJustification.values()[ justCombo.getSelectionIndex() ];
		}
		else {
			return null;
		}
	}
		
	/**
	 * Return color from the color picker of the dialog
	 */
	public Color[] getColors(){
		if ( chkBox[ChkBox.COLOR.ordinal()].getSelection() ){
		  // IAttribute requires to return an array of colors
		  // Only the first color is used at this time.
	      Color[] colors = new Color[1];
          
          colors[0] =new java.awt.Color( cs.getColorValue().red,
				cs.getColorValue().green, cs.getColorValue().blue );         

          return colors;
		}
		else {
			return null;
		}
	
	}

	/**
	 * Update the dialog with the given avn text type
	 * @param type
	 */
	public void setTextType( AviationTextType type ) {
		updateDialog(type);
	}

	/**
	 * Set font size
	 */	
	public void setFontSize( float size ){
		
		int index = 0;
		for ( int ii = 0; ii < FontSizeValue.length; ii++ ) {
			if ( (int)size == FontSizeValue[ ii ] ) {
				index = ii;
				break;
			}
		}

		sizeCombo.select( index );

	}
	
	/**
	 * Set top flight level
	 * @param value
	 */
	public void setTopValue(String value) {
		topValue.setText(value);
	}

	/**
	 * Set bottom flight level
	 * @param value
	 */
	public void setBottomValue(String value) {
		bottomValue.setText(value);
	}

	/**
	 * Set font name
	 */	
	public void setFontName( String name ){
		for ( String st:FontName ) {
			if ( st.equalsIgnoreCase( name ) ) {
				fontCombo.setText( st );
				break;
			}
		}		
	}
	
	/**
	 * set font style
	 */	
	public void setStyle( FontStyle style ){
        for ( FontStyle fs : FontStyle.values() ) {
            if ( fs == style ) {
            	styleCombo.setText( fs.name() );
            	break;
            }
        }
	}
	
	/**
	 * Return TextJustification from the justification combo
	 */	
	public void setJustification( TextJustification just ){
		for ( TextJustification js : TextJustification.values() ) {
	        if ( js == just ) {
	        	justCombo.setText( js.name() );	
	        	break;
	        }
	    }		
	}
		
	/**
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */
	private void setColor( Color clr ){
		
		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
		
	}

	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute iattr ){
	
		if ( iattr instanceof IAvnText ){
			IAvnText attr = (IAvnText) iattr;
			this.setTextType ( attr.getAvnTextType() );
			this.setTopValue( attr.getTopValue() );
			this.setBottomValue( attr.getBottomValue() );
			this.setFontName( attr.getFontName() );
			this.setFontSize( attr.getFontSize() );
			this.setJustification( attr.getJustification() );
			this.setStyle( attr.getStyle() );
			this.setSymbolPatternName( attr.getSymbolPatternName() );

			Color clr = attr.getColors()[0];
			if ( clr != null ) this.setColor( clr );
		}
		
	    top.pack();
									
	}

	/*
	 * Set the current symbol
	 */
	private void setSymbolPatternName(String symbolPatternName) {

		symbolCombo.setSelectedText(symbolPatternName);
	}

	@Override
	public AviationTextType getAvnTextType() {
		if ( chkBox[ChkBox.TYPE.ordinal()].getSelection() ){
			return AviationTextType.valueOf(typeCombo.getText());
		}
		else {
			return null;
		}
	}

	@Override
	public String getBottomValue() {
		if ( chkBox[ChkBox.BOTTOM.ordinal()].getSelection() ){
			return bottomValue.getText();
		}
		else {
			return null;
		}
	}

	@Override
	public String getSymbolPatternName() {
		if ( chkBox[ChkBox.SYMBOL.ordinal()].getSelection() ){
			//System.out.println("SYMNAME is: "+symbolCombo.getSelectedText());
			return symbolCombo.getSelectedText();
		}
		else {
			return null;
		}
	}

	@Override
	public String getTopValue() {
		if ( chkBox[ChkBox.TOP.ordinal()].getSelection() ){
			return topValue.getText();
		}
		else {
			return null;
		}
	}

	@Override
	public boolean hasBottomValue() {
		AviationTextType atype = getAvnTextType();
	    switch (atype) {
	    case LOW_PRESSURE_BOX:
	    case HIGH_PRESSURE_BOX:
	    case FLIGHT_LEVEL:
	    case FREEZING_LEVEL:
	    	return false;
	    default:
	    	return true;
	    }
		
	}

	@Override
	public boolean hasSymbolPattern() {
		AviationTextType atype = getAvnTextType();
	    switch (atype) {
	    case MID_LEVEL_ICING:
	    case HIGH_LEVEL_TURBULENCE:
	    case LOW_LEVEL_TURBULENCE:
	    	return true;
	    default:
	    	return false;
	    }
	}

	/**
	 * Create widgets for the type attribute
	 */	
	private void createTypeAttr(){
		
		chkBox[ChkBox.TYPE.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.TYPE.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
		chkBox[ChkBox.TYPE.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					typeLabel.setEnabled(true);
					typeCombo.setEnabled(true);
				}
				else {
					typeLabel.setEnabled(false);
					typeCombo.setEnabled(false);

				}
			}

		});

        typeLabel = new Label( top, SWT.NONE );
        typeLabel.setText( "Text Type:" );
        
        typeCombo = new Combo( top, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( AviationTextType type : AviationTextType.values() ) {
            typeCombo.add( type.toString() );
        }
        
        /*
         * Add selection listener for avn text type combo box
         */
        typeCombo.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent e ) {
                
            	Combo ctype = (Combo)e.getSource();
            	// reset - 
            	updateDialog(AviationTextType.valueOf(ctype.getText()));
                
            }
        });
        
	}

	/**
	 * Create widgets for the Top attribute
	 */	
	private void createTopAttr(){

		chkBox[ChkBox.TOP.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.TOP.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
		chkBox[ChkBox.TOP.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					topLabel.setEnabled(true);
					topValue.setEnabled(true);
				}
				else {
					topLabel.setEnabled(false);
					topValue.setEnabled(false);

				}
			}

		});
		topLabel = new Label( top, SWT.NONE );
		topLabel.setText( "Top Value:" );

		topValue = new Text( top, SWT.SINGLE | SWT.BORDER );                        
		topValue.setEditable( true );
		topValue.setText("XXX");

	}
	
	/**
	 * Create widgets for the Bottom attribute
	 */	
	private void createBottomAttr(){

		chkBox[ChkBox.BOTTOM.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.BOTTOM.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
		chkBox[ChkBox.BOTTOM.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					Set<Control> disableList = getDisableList( getAvnTextType() );
					if ( !disableList.contains(bottomLabel)){
						bottomLabel.setEnabled(true);
					}
					if ( !disableList.contains(bottomValue)){
						bottomValue.setEnabled(true);
					}
				}
				else {
					bottomLabel.setEnabled(false);
					bottomValue.setEnabled(false);

				}
			}

		});
		bottomLabel = new Label( top, SWT.NONE );
		bottomLabel.setText( "Bottom Value:" );
		enableList.add(bottomLabel);

		bottomValue = new Text( top, SWT.SINGLE | SWT.BORDER );                        
		bottomValue.setEditable( true );
		bottomValue.setText("XXX");
		enableList.add(bottomValue);
	}

	/**
	 * Create widgets for the Size attribute
	 */	
	private void createSizeAttr(){

		chkBox[ChkBox.SIZE.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.SIZE.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
		chkBox[ChkBox.SIZE.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					sizeLbl.setEnabled(true);
					sizeCombo.setEnabled(true);
				}
				else {
					sizeLbl.setEnabled(false);
					sizeCombo.setEnabled(false);

				}
			}

		});
		sizeLbl = new Label(top, SWT.LEFT);
		sizeLbl.setText("Size:");

		sizeCombo = new Combo( top, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( FontSizeName fs : FontSizeName.values() ) {
			sizeCombo.add( fs.name() );
		}
		sizeCombo.select( 2 );

	}
	
	/**
	 * Create widgets for the Font attribute
	 */
	private void createFontAttr(){

		chkBox[ChkBox.FONT.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.FONT.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
		chkBox[ChkBox.FONT.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					fontLbl.setEnabled(true);
					fontCombo.setEnabled(true);
				}
				else {
					fontLbl.setEnabled(false);
					fontCombo.setEnabled(false);

				}
			}

		});

		fontLbl = new Label(top, SWT.LEFT);
		fontLbl.setText("Font:");

		fontCombo = new Combo( top, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( String st : FontName ) {
			fontCombo.add( st );
		}
		fontCombo.select( 0 );

	}
	
	/**
	 * Create widgets for the Style attribute
	 */
	private void createStyleAttr(){
		chkBox[ChkBox.STYLE.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.STYLE.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
		chkBox[ChkBox.STYLE.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					styleLbl.setEnabled(true);
					styleCombo.setEnabled(true);
				}
				else {
					styleLbl.setEnabled(false);
					styleCombo.setEnabled(false);

				}
			}

		});

		styleLbl = new Label(top, SWT.LEFT);
		styleLbl.setText("Style:");

		styleCombo = new Combo( top, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( FontStyle fs : FontStyle.values() ) {
			styleCombo.add( fs.name() );
		}
		styleCombo.select( 0 );
	}
	
	/**
	 * Create widgets for Justification attribute
	 */
	private void createJustAttr(){
		chkBox[ChkBox.JUST.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.JUST.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT+5));
		chkBox[ChkBox.JUST.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					justLbl.setEnabled(true);
					justCombo.setEnabled(true);
				}
				else {
					justLbl.setEnabled(false);
					justCombo.setEnabled(false);
				}
			}
		});

		justLbl = new Label(top, SWT.LEFT);
		justLbl.setText("Just:");

		justCombo = new Combo( top, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( TextJustification js : TextJustification.values() ) {
			justCombo.add( js.name() );
		}
		justCombo.select( 1 );

	}
	
	/**
	 * Create widgets for Color attribute
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
	 * Create widgets for Symbol attribute
	 */
	private void createSymbolAttr(){
		chkBox[ChkBox.SYMBOL.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.SYMBOL.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.SYMBOL.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection() && (getAvnTextType() != null) ){

					Set<Control> disableList = getDisableList( getAvnTextType() );
					if ( !disableList.contains(symbolLabel)){
						symbolLabel.setEnabled(true);
					}
					if ( !disableList.contains(symbolCombo)){
						symbolCombo.setEnabled(true);
					}


				}
				else {
					symbolLabel.setEnabled(false);
					symbolCombo.setEnabled(false);

				}
			}

		});

		symbolLabel = new Label(top, SWT.LEFT);
		symbolLabel.setText("Symbol:");
		enableList.add(symbolLabel);

		symbolCombo = new SymbolCombo( top );
		symbolCombo.setLayoutData(new GridData(10, 1));
		enableList.add(symbolCombo);
		// Add listeners to repack dialog when symbolCombo changes
		symbolCombo.addControlListener( new ControlListener() {
			@Override
			public void controlMoved(ControlEvent e) {
				top.pack();
			}
			@Override
			public void controlResized(ControlEvent e) {
				top.pack();
			}
		});

	}
	
	/**
	 * Set multi-selection check boxes visible/invisible
	 * @param flag
	 */
	private void enableChkBoxes(boolean flag){
		
		if(!flag) {
			setAllChkBoxes();
		}	
		for ( ChkBox chk : ChkBox.values()){
			chkBox[chk.ordinal()].setVisible(flag);
		}
	}

	/**
	 * Set all multi-selection check boxes to true
	 */
	private void setAllChkBoxes(){
		for ( ChkBox chk : ChkBox.values()){
			chkBox[chk.ordinal()].setSelection(true);
		}
	}

	/**
	 * Enable/Disable all widgets in the attribute dialog
	 * @param flag
	 */
	private void enableAllWidgets(boolean flag){
		
		colorLbl.setEnabled(flag);
		
		typeLabel.setEnabled(flag);
		typeCombo.setEnabled(flag);
		
		topLabel.setEnabled(flag);
		topValue.setEnabled(flag);
		
		bottomLabel.setEnabled(flag);
		bottomValue.setEnabled(flag);
		
		sizeLbl.setEnabled(flag);
		sizeCombo.setEnabled(flag);
		
		fontLbl.setEnabled(flag);
		fontCombo.setEnabled(flag);
		
		styleLbl.setEnabled(flag);
		styleCombo.setEnabled(flag);

		justLbl.setEnabled(flag);
		justCombo.setEnabled(flag);
		
		symbolLabel.setEnabled(flag);
		symbolCombo.setEnabled(flag);
	}

	@Override
	public Coordinate getPosition() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Color getTextColor() {
		// TODO Auto-generated method stub
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

	/**
	 *  Interface for IText.
	 */
	@Override
	public String[] getString() {
		return new String[] { new String("") };
	}

	@Override
	public double getRotation() {
		return 0.0;
	}
	
	@Override
	public TextRotation getRotationRelativity() {
		return TextRotation.SCREEN_RELATIVE;
	}
	
	@Override
	public DisplayType getDisplayType() {
		return null;
	}
	
	@Override
	public Boolean maskText() {
		return false;
	}

	@Override
	public Boolean getHide() {
		return false;
	}
	
	@Override
	public Boolean getAuto() {
		return false;
	}
	
	@Override
	public int getXOffset() {
		return 0;
	}

	@Override
	public int getYOffset() {
		return 0;
	}
}
