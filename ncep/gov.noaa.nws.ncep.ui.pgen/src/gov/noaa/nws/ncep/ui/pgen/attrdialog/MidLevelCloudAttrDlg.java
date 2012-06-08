/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.idLevelCloudAttrDlg
 * 
 * 29 July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.display.IMidCloudText;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Singleton attribute dialog for Aviation Mid Level Cloud Text elements.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		104?		S. Gilbert  Initial Creation.
 * 03/10        231         Archana     Altered the dialog for aviation
 *                                      mid-level cloud text elements 
 *                                      to display only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix .
 * 11/10		?			B. Yin		Added flag for two columns
 * 										Removed cloud amount  
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * </pre>
 * 
 * @author	J. Wu
 */

public class MidLevelCloudAttrDlg extends AttrDlg implements IMidCloudText {
	
	private static enum CloudTypes { CU, ST, SC, NS, AS, AC, CS, CC, CI };
	//private static enum CloudAmounts { SKC, SCT, BKN, OVC, LYR };
	private static enum TstormTypes { ISOL, OCNL, FRQ, EMBD };
	
	public static enum FontSizeName { TINY, SMALL, MEDIUM, LARGE, HUGE, GIANT };
	public static int[] FontSizeValue =  { 10, 12, 14, 18, 24, 34 };
	//public static final String[] FontName = new String[]{ "Courier", "Helvetica", "Times" };
	public static final String[] FontName = new String[]{ "Courier", "Nimbus Sans L", "Liberation Serif" };

	
	private static final String[] ICING_LIST = new String[] { "ICING_00", "ICING_01", "ICING_02",
															  "ICING_03", "ICING_04", "ICING_05",
															  "ICING_06", "ICING_07", "ICING_08",
															  "ICING_09", "ICING_10" };

	private static final String[] TURB_LIST = new String[] { "TURBULENCE_0", "TURBULENCE_1", "TURBULENCE_2",
		   													 "TURBULENCE_3", "TURBULENCE_4", 
		   													 "TURBULENCE_4|TURBULENCE_6", "TURBULENCE_5",
		   													 "TURBULENCE_6", "TURBULENCE_6|TURBULENCE_7",
		   													 "TURBULENCE_7", "TURBULENCE_8" };

	private static enum ChkBox { SIZE, FONT, STYLE, JUST, COLOR };
	
	static MidLevelCloudAttrDlg INSTANCE = null;
       
	private Composite top = null;
	Composite multigroup = null;
	
	private Label colorLbl;
    private ColorButtonSelector cs = null;
    
    private List<Button> cloudTypeButtons;
    //private List<Button> cloudAmountButtons;

    private Text turbLevel, icingLevel, tstormLevel;
    
    private SymbolCombo turbCombo, iceCombo;
    
    private List<Button> tstormTypeButtons;
 
    private Label typeLabel;
    
    private Label sizeLbl;
    private Combo sizeCombo = null;
    
    private Label fontLbl;
    private Combo fontCombo = null;
    
    private Label styleLbl;
    private Combo styleCombo = null;
    
    private Label justLbl;
    private Combo justCombo = null;
    
    private Button chkBox[];
    private boolean multiselectMode;

    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	protected MidLevelCloudAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);

    }
	
	/**
	 * Creates an aviation mid level cloud text attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static MidLevelCloudAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new MidLevelCloudAttrDlg( parShell );
				
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
	        initializeComponents();

	        return top;
	        
	}
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 * 
	 */
	private void initializeComponents() {
		
        this.getShell().setText("Mid Level Cloud Attributes");
        
        chkBox = new Button[5];

        /*
         * Create cloud, turbulence, icing, and tstorm widgets, if not in multiselect mode
         */
        multiselectMode = true;
		if ( ! PgenSession.getInstance().getPgenPalette().getCurrentAction()
				.equalsIgnoreCase("MultiSelect")){
			createCloudTypes();
		//	createCloudAmounts();
			createTurbulenceSection();
			createIcingSection();
			createThunderstormSection();
	        multiselectMode = false;
		}
		
    	multigroup = new Composite( top, SWT.NONE );
        multigroup.setLayout(new GridLayout(3,false) );
        
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

		if ( PgenSession.getInstance().getPgenPalette().getCurrentAction()
				.equalsIgnoreCase("MultiSelect")){
			enableChkBoxes(true);
			enableAllWidgets(false);
		}
		else {
			enableChkBoxes(false);
		}
  
      top.pack();
        
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
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */
	public void setColor( Color clr ){
		
		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
		
	}

	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute attr ){

		if ( attr instanceof IMidCloudText ) {
			IMidCloudText mid = (IMidCloudText)attr;
			this.setButtons( cloudTypeButtons, mid.getCloudTypes() );
		//	this.setButtons( cloudAmountButtons, mid.getCloudAmounts() );
			this.setTurbulencePattern( mid.getTurbulencePattern() );
			this.setTurbulenceLevels( mid.getTurbulenceLevels() );
			this.setIcingPattern( mid.getIcingPattern() );
			this.setIcingLevels( mid.getIcingLevels() );
			this.setButtons( tstormTypeButtons, mid.getTstormTypes() );
			this.setTstormLevels( mid.getTstormLevels() );

			this.setFontName( mid.getFontName() );
			this.setFontSize( mid.getFontSize() );
			this.setJustification( mid.getJustification() );
			this.setStyle( mid.getStyle() );

			Color clr = attr.getColors()[0];
			if ( clr != null ) this.setColor( clr );
			top.pack();
		}
									
	}

	private void setButtons(List<Button> checkboxes, String types) {
		
		for ( Button btn : checkboxes ) {
			if ( types.contains( btn.getText() ) ) {
				btn.setSelection(true);
			}
			else {
				btn.setSelection(false);
			}
		}
		
	}

	/**
	 * Create widgets for the Cloud type attribute
	 */	
	private void createCloudTypes(){
		
		cloudTypeButtons = new ArrayList<Button>();
		
        typeLabel = new Label( top, SWT.NONE );
        typeLabel.setText( "Cloud Type:" );
        
        Composite c1 = new Composite( top, SWT.NONE );
        c1.setLayout(new GridLayout(4,true) );
        
        for ( CloudTypes type : CloudTypes.values() ) {
            Button btn = new Button( c1, SWT.CHECK  );
            btn.setText(type.toString());
            cloudTypeButtons.add(btn);
        }
        
        Label sep1 = new Label( top, SWT.SEPARATOR | SWT.HORIZONTAL );
        sep1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
	}
	
	/*
	 * create widgets for cloud amount attributes
	 */
/*	private void createCloudAmounts(){
    
		cloudAmountButtons = new ArrayList<Button>();
		
		typeLabel = new Label( top, SWT.NONE );
        typeLabel.setText( "Cloud Amount:" );
        
        Composite c2 = new Composite( top, SWT.NONE );
        c2.setLayout(new GridLayout(4,true) );
        
        for ( CloudAmounts type : CloudAmounts.values() ) {
            Button btn = new Button( c2, SWT.CHECK  );
            btn.setText(type.toString());
            cloudAmountButtons.add(btn);
        }
        
        Label sep2 = new Label( top, SWT.SEPARATOR | SWT.HORIZONTAL);
        sep2.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

	}
*/	
	/*
	 * create widgets for turbulence attributes
	 */
    private void createTurbulenceSection() {
        
    	Composite c3 = new Composite( top, SWT.NONE );
        c3.setLayout(new GridLayout(2,false) );
        
    	Label turbLabel = new Label( c3, SWT.NONE );
        turbLabel.setText( "Turb Type:" );
		
        turbCombo = new SymbolCombo( c3 );
        turbCombo.setLayoutData(new GridData(10, 1));
		turbCombo.setItems(TURB_LIST);
		turbCombo.select(4);
        
        Label turbLevelText = new Label( c3, SWT.NONE );
        turbLevelText.setText( "Top/Base:" );
        
        turbLevel = new Text( c3, SWT.SINGLE | SWT.BORDER );
        
        Label sep3 = new Label( top, SWT.SEPARATOR | SWT.HORIZONTAL);
        sep3.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
    }

    /*
     * create widgets for icing attributes
     */
    private void createIcingSection() {
        
    	Composite c4 = new Composite( top, SWT.NONE );
        c4.setLayout(new GridLayout(2,true) );
        
    	Label iceLabel = new Label( c4, SWT.NONE );
        iceLabel.setText( "Icing Type:" );
		
        
		iceCombo = new SymbolCombo( c4 );
		iceCombo.setLayoutData(new GridData(10, 1));
		iceCombo.setItems(ICING_LIST);
	       
        Label iceLevelText = new Label( c4, SWT.NONE );
        iceLevelText.setText( "Top/Base:" );
        
        icingLevel = new Text( c4, SWT.SINGLE | SWT.BORDER );
        
        Label sep4 = new Label( top, SWT.SEPARATOR | SWT.HORIZONTAL);
        sep4.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
    }
    
    /*
     * create widgets for tstorm attributes
     */
    private void createThunderstormSection() {
    	
    	tstormTypeButtons = new ArrayList<Button>();
    	
    	Label tstormLabel = new Label( top, SWT.NONE );
    	tstormLabel.setText("Thunderstorm Type:");
    	
    	Composite c5 = new Composite( top, SWT.NONE );
        c5.setLayout(new GridLayout(4,true) );
        
        for ( TstormTypes type : TstormTypes.values() ) {
            Button btn = new Button( c5, SWT.CHECK  );
            btn.setText(type.toString());
            tstormTypeButtons.add(btn);
        }
        
    	Composite c6 = new Composite( top, SWT.NONE );
        c6.setLayout(new GridLayout(2,true) );
        
        Label tstormText = new Label( c6, SWT.NONE );
        tstormText.setText( "Top/Base:" );
        
        tstormLevel = new Text( c6, SWT.SINGLE | SWT.BORDER );
        
        Label sep5 = new Label( top, SWT.SEPARATOR | SWT.HORIZONTAL);
        sep5.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
    }
    
	/**
	 * Create widgets for the Size attribute
	 */	
	private void createSizeAttr(){
		
        chkBox[ChkBox.SIZE.ordinal()] = new Button(multigroup, SWT.CHECK);
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
		sizeLbl = new Label(multigroup, SWT.LEFT);
		sizeLbl.setText("Size:");

		sizeCombo = new Combo( multigroup, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( FontSizeName fs : FontSizeName.values() ) {
			sizeCombo.add( fs.name() );
		}
		sizeCombo.select( 2 );

	}
	
	/**
	 * Create widgets for the Font attribute
	 */
	private void createFontAttr(){

		chkBox[ChkBox.FONT.ordinal()] = new Button(multigroup, SWT.CHECK);
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

		fontLbl = new Label(multigroup, SWT.LEFT);
		fontLbl.setText("Font:");

		fontCombo = new Combo( multigroup, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( String st : FontName ) {
			fontCombo.add( st );
		}
		fontCombo.select( 0 );

	}
	
	/**
	 * Create widgets for the Style attribute
	 */
	private void createStyleAttr(){
		chkBox[ChkBox.STYLE.ordinal()] = new Button(multigroup, SWT.CHECK);
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

		styleLbl = new Label(multigroup, SWT.LEFT);
		styleLbl.setText("Style:");

		styleCombo = new Combo( multigroup, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( FontStyle fs : FontStyle.values() ) {
			styleCombo.add( fs.name() );
		}
		styleCombo.select( 0 );
	}
	
	/**
	 * Create widgets for Justification attribute
	 */
	private void createJustAttr(){
		chkBox[ChkBox.JUST.ordinal()] = new Button(multigroup, SWT.CHECK);
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

		justLbl = new Label(multigroup, SWT.LEFT);
		justLbl.setText("Just:");

		justCombo = new Combo( multigroup, SWT.DROP_DOWN | SWT.READ_ONLY );

		for ( TextJustification js : TextJustification.values() ) {
			justCombo.add( js.name() );
		}
		justCombo.select( 1 );

	}
	
	/**
	 * Create widgets for Color attribute
	 */
	private void createColorAttr(){

		chkBox[ChkBox.COLOR.ordinal()] = new Button(multigroup, SWT.CHECK);
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

		colorLbl = new Label( multigroup, SWT.LEFT );
		colorLbl.setText("Color:");
		cs = new ColorButtonSelector( multigroup );
		cs.setColorValue( new RGB( 0,255,0 ) );
		
        Label sep6 = new Label( top, SWT.SEPARATOR | SWT.HORIZONTAL);
        sep6.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
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
		
		sizeLbl.setEnabled(flag);
		sizeCombo.setEnabled(flag);
		
		fontLbl.setEnabled(flag);
		fontCombo.setEnabled(flag);
		
		styleLbl.setEnabled(flag);
		styleCombo.setEnabled(flag);

		justLbl.setEnabled(flag);
		justCombo.setEnabled(flag);
		
	}

	@Override
	public String getCloudAmounts() {
		return null;
		//if ( multiselectMode ) return null;
		//return assembleString(cloudAmountButtons);
	}

	@Override
	public String getCloudTypes() {
		if ( multiselectMode ) return null;
		return assembleString(cloudTypeButtons);
	}

	@Override
	public String getIcingLevels() {
		if ( multiselectMode ) return null;
		return icingLevel.getText();
	}
	
	public void setIcingLevels( String levels ) {
		icingLevel.setText(levels);
	}
	
	@Override
	public String getIcingPattern() {
		if ( multiselectMode ) return null;
		return iceCombo.getSelectedText();
	}

	public void setIcingPattern( String pattern ) {
		iceCombo.setSelectedText(pattern);
	}

	@Override
	public String getTstormLevels() {
		if ( multiselectMode ) return null;
		return tstormLevel.getText();
	}
	
	public void setTstormLevels( String levels ) {
		tstormLevel.setText(levels);
	}
	
	@Override
	public String getTstormTypes() {
		if ( multiselectMode ) return null;
		return assembleString(tstormTypeButtons);
	}

	@Override
	public String getTurbulenceLevels() {
		if ( multiselectMode ) return null;
		return turbLevel.getText();
	}
	
	public void setTurbulenceLevels( String levels ) {
		turbLevel.setText(levels);
	}

	@Override
	public String getTurbulencePattern() {
		if ( multiselectMode ) return null;
		return turbCombo.getSelectedText();
	}

	public void setTurbulencePattern( String pattern ) {
		turbCombo.setSelectedText(pattern);
	}

	@Override
	public boolean hasIcing() {
		if ( icingLevel.getText()==null || icingLevel.getText().isEmpty() ) return false;
		else return true;
	}

	@Override
	public boolean hasTstorm() {
		if ( tstormLevel.getText()==null || tstormLevel.getText().isEmpty() ) return false;
		else return true;
	}

	@Override
	public boolean hasTurbulence() {
		if ( turbLevel.getText()==null || turbLevel.getText().isEmpty() ) return false;
		else return true;
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
	
	/*
	 * create a string containing the name of all selected buttons in the list
	 * delimited with a vertical bar "|"
	 */
	private String assembleString( List<Button> list ) {
		StringBuilder sb = new StringBuilder();
		for ( Button btn : list ) {
			if ( btn.getSelection() ) {
				sb.append( btn.getText() );
				sb.append('|');
			}
		}
		if ( sb.length() > 0 ) sb.deleteCharAt( sb.length()-1 );
		return sb.toString();
	}

	@Override
	public boolean isTwoColumns() {
		// TODO Auto-generated method stub
		return true;
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
