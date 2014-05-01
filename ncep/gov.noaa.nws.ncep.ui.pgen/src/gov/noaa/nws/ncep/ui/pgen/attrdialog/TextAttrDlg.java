/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.TextAttrDlg
 * 
 * 15 April 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.awt.Color;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
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

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.display.IText;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

/**
 * Singleton attribute dialog for text.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/09					J. Wu   	Initial Creation.
 * 09/09		#149		B. Yin		Added check boxes for multi-selection
 * 03/10        #231        Archana     Altered the dialog for text 
 *                                      to display only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 03/13		#928		B. Yin 		Added a separator above the button bar.
 * </pre>
 * 
 * @author	J. Wu
 */

public class TextAttrDlg extends AttrDlg implements IText{
	
	public static enum FontSizeName { TINY, SMALL, MEDIUM, LARGE, HUGE, GIANT };
	public static int[] FontSizeValue =  { 10, 12, 14, 18, 24, 34 };
	//public static String[] FontName = new String[]{ "Courier", "Helvetica", "Times" };
	public static  String[] FontName = new String[]{ "Courier", "Nimbus Sans L", "Liberation Serif" };

	private static final String bgmask = " w/ bg mask";
	
	private static enum ChkBox { TEXT, BOX, SIZE, FONT, STYLE, JUSTIFICATION, COLOR, ROTATION };
	
	private static final int MIN_ROTATION = 0;
	private static final int MAX_ROTATION = 360;
	private static final int START_ROTATION = 0;
	private static final int INC_ROTATION = 1;
	
	private static TextAttrDlg INSTANCE = null;
       
    private final int TEXT_WIDTH = 160;      
    private final int TEXT_HEIGHT = 40;    

	private Composite top = null;
	
	private Label colorLbl;
    private ColorButtonSelector cs = null;
    
    protected Text text = null;
    protected Label textLabel;
    
    protected Label boxLbl;
    private Combo boxCombo = null; 
    
    private Label sizeLbl;
    protected Combo sizeCombo = null;
    
    private Label fontLbl;
    private Combo fontCombo = null;
    
    private Label styleLbl;
    private Combo styleCombo = null;
    
    private Label justLbl;
    private Combo justCombo = null;
    
    protected Label rotLbl; 
    private Group rotGroup;
    private Group relGroup;
    protected Slider rotSlider = null;  
    protected Text rotText = null;
    
    protected Button screenBtn = null;
    protected Button northBtn = null;
    
    //Check boxes for multi-selection
    private Button chkBox[];
    

    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	protected TextAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);

    }
	
	/**
	 * Creates a text attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static TextAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new TextAttrDlg( parShell );
				
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
	        mainLayout.horizontalSpacing = 3;
	        top.setLayout(mainLayout);

	        // Initialize all of the menus, controls, and layouts
	        initializeComponents();

	        return top;
	        
	}
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 * @param listener 
	 */
	private void initializeComponents() {
		
        this.getShell().setText("Text Attributes");
        
        chkBox = new Button[8];
 
        createTextAttr();
        createBoxAttr();
        createSizeAttr();
        createFontAttr();
        createStyleAttr();
        createJustAttr();
        createColorAttr();
        createRotationAttr();
        addSeparator(top.getParent());

	}	
	
	/**
	 * Gets the text
	 */
	public String[] getString(){

		if ( chkBox[ChkBox.TEXT.ordinal()].getSelection() ){
			return text.getText().split( "\n" );	  		
		}
		else {
			return null;
		}

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
		if ( chkBox[ChkBox.JUSTIFICATION.ordinal()].getSelection() ){
			return TextJustification.values()[ justCombo.getSelectionIndex() ];
		}
		else {
			return null;
		}
	}
		
	/**
	 * Return text rotation from the rotation slider
	 */	
	public double getRotation() {
		if ( chkBox[ChkBox.ROTATION.ordinal()].getSelection() ){
			return rotSlider.getSelection();
		}
		else{
			return java.lang.Double.NaN;
		}
		
	}

    /**
	 * Return text rotation relativity from the rotation radio boxes
	 */	
	public TextRotation getRotationRelativity() {
		
		if ( chkBox[ChkBox.ROTATION.ordinal()].getSelection() ){

			if ( screenBtn.getSelection() ) { 
				return TextRotation.SCREEN_RELATIVE;
			}
			else {
				return TextRotation.NORTH_RELATIVE;	
			}
		}
		else {
			return null;
		}
	}

	/**
	 * Return text mask/outline from the box combo
	 */	
	@Override
	public DisplayType getDisplayType() {
		if ( chkBox[ChkBox.BOX.ordinal()].getSelection() ){

			for ( DisplayType type : DisplayType.values() ) {
				if ( boxCombo.getText().startsWith(type.name()) ) {
					return type;		
				}
			}
			return null;
		}
		else {
			return null;
		}
	}
	
	public Boolean maskText() {
		if ( chkBox[ChkBox.BOX.ordinal()].getSelection() ){

			if ( boxCombo.getText().contains(bgmask)  ) {
				return true;		
			}
			else {
				return false;
			}
		}
		else {
			return null;
		}
	}

	/**
	 * Return text offset
	 */		
	public int getXOffset() {
		return 0;
	}

	public int getYOffset() {
		return 0;
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
	 * Sets the text
	 * @param txt
	 */
	public void setText( String[] txt ){
		StringBuilder result = new StringBuilder( "" );
		for ( String st:txt ) {
			result.append( st + "\n" );
		}
		
		int length =  result.length();
		if ( length > 0 ) {
		     result.delete( length-1, length - 1 );
	    }
	
		text.setText( result.toString() );	  		
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
	 * set text rotation
	 */	
	public void setRotation( double rot ) {
		rotSlider.setSelection( (int) rot );
		rotText.setText( "" + (int) rot );
	}

    /**
	 * set text rotation relativity
	 */	
	public void setRotationRelativity( TextRotation trt ) {
		if ( trt == TextRotation.SCREEN_RELATIVE ) { 
			screenBtn.setSelection( true );
		}
		else {
			northBtn.setSelection( true );	
		}
	}

	/**
	 * set text box from the mask/outline flag
	 */	
	public void setBoxText( boolean mask, DisplayType outline ) {
		
		StringBuilder sb = new StringBuilder( outline.name() );
		
		if ( mask ) {
			sb.append( bgmask );
		}

		boxCombo.setText( sb.toString() );			
		
	}
	
	/**
	 * set text offset
	 */		
	public void setXOffset( int xoff ) {
	}

	public void  setYOffset( int yoff ) {
	}

	
	/**
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */
	public void setColor( Color clr ){
		
		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
		
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute iattr ){
	
		if ( iattr instanceof IText ){
			IText attr = (IText)iattr;
			this.setText( attr.getString() );           
			this.setFontName( attr.getFontName() );
			this.setFontSize( attr.getFontSize() );
			this.setJustification( attr.getJustification() );
			this.setRotation( attr.getRotation() );
			this.setRotationRelativity( attr.getRotationRelativity() );
			this.setStyle( attr.getStyle() );
			this.setXOffset( attr.getXOffset() );
			this.setYOffset( attr.getYOffset() );
			this.setBoxText( attr.maskText(), attr.getDisplayType() );

			Color clr = attr.getColors()[0];
			if ( clr != null ) this.setColor( clr );
		}
	}
	
	/**
	 * Set the check boxes visible/invisible
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
	 * Enable/disable all widgets in the attribute dialog
	 * @param flag
	 */
	private void enableAllWidgets(boolean flag){
		textLabel.setEnabled(flag);
		text.setEnabled(flag);
		
		boxLbl.setEnabled(flag);
		boxCombo.setEnabled(flag);
		
		sizeLbl.setEnabled(flag);
		sizeCombo.setEnabled(flag);
		
		fontLbl.setEnabled(flag);
		fontCombo.setEnabled(flag);	
		
		styleLbl.setEnabled(flag);
		styleCombo.setEnabled(flag);	
		
		justLbl.setEnabled(flag);
		justCombo.setEnabled(flag);	
		
		colorLbl.setEnabled(flag);
		
		rotLbl.setEnabled(flag);
		rotGroup.setEnabled(flag);	
		rotSlider.setEnabled(flag);
		rotText.setEnabled(flag);
		
		relGroup.setEnabled(flag);
		screenBtn.setEnabled(flag);
		northBtn.setEnabled(flag);
	}
	
	/**
	 * Set all check boxes to true 
	 */
	private void setAllChkBoxes(){
	
		for ( ChkBox chk : ChkBox.values()){
			chkBox[chk.ordinal()].setSelection(true);
		}
	}
	
	@Override
	public int open(){

		this.create();
		
		if ( PgenSession.getInstance().getPgenPalette().getCurrentAction()
				.equalsIgnoreCase("MultiSelect")){
			enableChkBoxes(true);
			enableAllWidgets(false);
		}
		else {
			enableChkBoxes(false);
		}
		
   	    return super.open();
	}
	
	/**
	 * create widgets for the text attribute
	 */
	private void createTextAttr(){

		chkBox[ChkBox.TEXT.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.TEXT.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.TEXT.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					System.out.println("Text checked?");
					textLabel.setEnabled(true);
					text.setEnabled(true);
				}
				else {
					System.out.println("Text un-checked!!!!!!!!!!!!");
					textLabel.setEnabled(false);
					text.setEnabled(false);

				}
			}

		});

		textLabel = new Label( top, SWT.LEFT );
		textLabel.setText("Text:");

		int style = SWT.MULTI | SWT.BORDER | SWT.V_SCROLL |SWT.H_SCROLL;
		text = new Text( top, style );                        
		text.setLayoutData( new GridData( TEXT_WIDTH, TEXT_HEIGHT ) );
		text.setEditable( true );  

	}

	/**
	 * create widgets for the box attribute
	 */
	private void createBoxAttr(){

		chkBox[ChkBox.BOX.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.BOX.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.BOX.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					boxLbl.setEnabled(true);
					boxCombo.setEnabled(true);
				}
				else {
					boxLbl.setEnabled(false);
					boxCombo.setEnabled(false);

				}
			}
		});

		boxLbl = new Label(top, SWT.LEFT);
		boxLbl.setText("Box:");

		boxCombo = new Combo( top, SWT.DROP_DOWN | SWT.READ_ONLY );       
		for ( DisplayType type : DisplayType.values() ) {
			boxCombo.add( type.name() );
			boxCombo.add( type.name() + bgmask );
		}

		boxCombo.select( 0 );
	}
	
	/**
	 * create widgets for the size attribute
	 */
	private void createSizeAttr(){

		chkBox[ChkBox.SIZE.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.SIZE.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
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
	 * create widgets for the font attribute
	 */
	private void createFontAttr(){
		chkBox[ChkBox.FONT.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.FONT.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
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
	 * create widgets for the style attribute
	 */
	private void createStyleAttr(){
		chkBox[ChkBox.STYLE.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.STYLE.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
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
	 * create widgets for the Justification attribute
	 */
	private void createJustAttr(){

		chkBox[ChkBox.JUSTIFICATION.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.JUSTIFICATION.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.JUSTIFICATION.ordinal()].addSelectionListener(new SelectionAdapter(){

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
	 * create widgets for the Color attribute
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
	 * create widgets for the Rotation attribute
	 */	
	private void createRotationAttr(){

		chkBox[ChkBox.ROTATION.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.ROTATION.ordinal()].setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.ROTATION.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					rotLbl.setEnabled(true);
					rotGroup.setEnabled(true);
					rotSlider.setEnabled(true);
					rotText.setEnabled(true);

					relGroup.setEnabled(true);
					screenBtn.setEnabled(true);
					northBtn.setEnabled(true);
				}
				else {
					rotLbl.setEnabled(false);
					rotGroup.setEnabled(false);
					rotSlider.setEnabled(false);
					rotText.setEnabled(false);

					relGroup.setEnabled(false);
					screenBtn.setEnabled(false);
					northBtn.setEnabled(false);
				}
			}

		});  

		rotLbl = new Label( top, SWT.LEFT );
		rotLbl.setText("Rot:");


		rotGroup = new Group( top, SWT.NONE ) ;
		GridLayout gl = new GridLayout( 2, false );
		rotGroup.setLayout( gl );

		rotSlider = new Slider( rotGroup, SWT.HORIZONTAL );
		rotSlider.setValues( START_ROTATION, MIN_ROTATION, MAX_ROTATION, 1, INC_ROTATION, 5 );

		rotSlider.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent e ) {
				rotText.setText( "" + rotSlider.getSelection() );
			}
		});

		rotText = new Text( rotGroup,  SWT.SINGLE | SWT.BORDER );                        
		rotText.setLayoutData( new GridData( 20, 10 ) );
		rotText.setEditable( true );   
		rotText.setText( "" + START_ROTATION );
		rotText.addKeyListener( new KeyAdapter() {
			public void keyReleased( KeyEvent e ) {
				int value = 0;
				try {
					value = Integer.parseInt( rotText.getText() );
					if ( value >= MIN_ROTATION && value < MAX_ROTATION ) {
						rotSlider.setSelection( value );
						rotText.setToolTipText( "" );
					}
					else {
						rotText.setToolTipText( "Only integer values between 0 and 360 are accepted." );
					}
				} catch ( NumberFormatException e1 ) {
					rotText.setToolTipText( "Only integer values between 0 and 360 are accepted." );
				}
			}
		});


		Button scnChkBox = new Button(top, SWT.CHECK);
		scnChkBox.setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		scnChkBox.setVisible(false);

		Label grpLbl = new Label( top, SWT.LEFT );
		grpLbl.setText("   ");

		relGroup = new Group(top, SWT.NONE);
		relGroup.setLayout( gl );

		screenBtn  = new Button(relGroup, SWT.RADIO);
		screenBtn.setText("Screen");
		screenBtn.setSelection(true);

		northBtn = new Button( relGroup, SWT.RADIO );
		northBtn.setText( "North" );     		
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

	@Override
	public Boolean getHide() {
		return false;
	}
	
	@Override
	public Boolean getAuto() {
		return false;
	}

}
