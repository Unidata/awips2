/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.LineAttrDlg
 * 
 * 29 April 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.awt.Color;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Label;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IArc;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

/**
 * Singleton attribute dialog for lines.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/09		89			J. Wu   	Initial Creation.
 * 09/09		149			B. Yin		Added check boxes for multi-selection
 * 03/10        231         Archana     Altered the Arc attribute dialog
 *                                      to display only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix .
 * 04/11		?			B. Yin		Re-factor IAttribute
 * 04/13		TTR399		J. Wu		make the dialog smaller.
 * </pre>
 * 
 * @author	J. Wu
 */

public class ArcAttrDlg  extends AttrDlg implements IArc{
	
	private static enum ChkBox { COLOR, WIDTH, AXIS_RATIO, START_ANGLE, END_ANGLE };
	
	private static ArcAttrDlg INSTANCE;

    private Composite top;
    
    private Label colorLbl;
    private ColorButtonSelector cs;
    
    private Label widthLbl;
    private Slider lineWidthSlider;
    private Text lineWidthText;
    
    protected Label axisRatioLbl;  
    protected Slider axisRatioSlider;   
    protected Text axisRatioText;
    
    protected Label startAngleLbl;
    protected Slider startAngleSlider;   
    protected Text startAngleText;
    
    protected Label endAngleLbl;
    protected Slider endAngleSlider;   
    protected Text endAngleText;  
    
    private Button chkBox[];


	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	protected ArcAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);
       
    }
	
	/**
	 * Creates a Arc attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static ArcAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				INSTANCE = new ArcAttrDlg( parShell );
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
	        top.setLayout( getGridLayout( 1, false, 0, 0 , 0, 0 ) );

	        // Initialize all of the menus, controls, and layouts
	        initializeComponents();

	        return top;
	    }   
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 */
	private void initializeComponents() {
		
        this.getShell().setText("Arc Attributes");
        chkBox = new Button[5];

        createColorAttr();
        createWidthAttr();
        createRatioAttr();
        createStartAngleAttr();
        createEndAngleAttr();
        addSeparator(top.getParent());
        
	}

	/**
	 * Return color from the color picker of the dialog
	 */
	public Color[] getColors() {
		
		if ( chkBox[ChkBox.COLOR.ordinal()].getSelection() ){  
			// IAttribute requires to return an array of colors
			// Only the first color is used at this time.		
			Color[] colors = new Color[ 1 ];

			colors[0] = new java.awt.Color( cs.getColorValue().red,
					cs.getColorValue().green, cs.getColorValue().blue );

			return colors;          
		}
		else {
			return null;
		}
	}
	
	/**
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */	
	private void setColor( Color clr ){

		cs.setColorValue( new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()) );

	}
	
	/**
	 * Returns the line width from the dialog.
	 */	
	public float getLineWidth(){
		if ( chkBox[ChkBox.WIDTH.ordinal()].getSelection() ){  

			return lineWidthSlider.getSelection();
		}
		else {
			return java.lang.Float.NaN;
		}
		
	}
	
	/**
	 * Sets the line width value of the dialog.
	 * @param lw
	 */	
	private void setLineWidth( float lw ){
		
		lineWidthSlider.setSelection( (int)lw );
		lineWidthText.setText( "" + (int)lw );
		
	}

	/**
	 * Returns the axis ratio from the dialog.
	 */	
	public double getAxisRatio(){
		if ( chkBox[ChkBox.AXIS_RATIO.ordinal()].getSelection() ){  

		return ( axisRatioSlider.getSelection()/100.0 );
		}
		else {
			return java.lang.Double.NaN;
		}
		
	}
	
	/**
	 * set axis ratio
	 */	
	public void setAxisRatio( double ar ) {
		axisRatioSlider.setSelection( (int) ( ar * 100 ) );
		axisRatioText.setText( "" + ar );
	}
	
	/**
	 * Returns the start angle from the dialog.
	 */	
	public double getStartAngle(){
		if ( chkBox[ChkBox.START_ANGLE.ordinal()].getSelection() ){  

			return ( startAngleSlider.getSelection() );
		}
		else {
			return java.lang.Double.NaN;
		}
		
	}
	
	/**
	 * set the start angle
	 */	
	public void setStartAngle( double sa ) {
		startAngleSlider.setSelection( (int)sa );
		startAngleText.setText( "" + sa );
	}
	
	/**
	 * Returns the end angle from the dialog.
	 */	
	public double getEndAngle(){
		if ( chkBox[ChkBox.END_ANGLE.ordinal()].getSelection() ){  

			return ( endAngleSlider.getSelection() );
		}
		else {
			return java.lang.Double.NaN;
		}
		
	}
	
	/**
	 * set the start angle
	 */	
	public void setEndAngle( double ea ) {
		endAngleSlider.setSelection( (int)ea );
		endAngleText.setText( "" + ea );
	}
	
	/**
	 * Returns the Close flag of the dialog.
	 */
	public boolean isClosed(){
		
		return true;
		
	}
	
	/**
	 * Returns the fill pattern from the dialog.
	 */		
	public FillPattern getFillPattern(){
		
		return FillPattern.SOLID;
		
	}
	
	/**
	 * Returns the Filled flag of the dialog.
	 */	
	public Boolean isFilled(){
		
		return false;
		
	}
	
	
	/**
	 * Returns the smooth level of the dialog.
	 */		
	public int getSmoothFactor(){
		
		return 2;
		
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute iattr ){

		if ( iattr instanceof IArc ){
			IArc attr = (IArc) iattr;
			Color clr = attr.getColors()[0];
			if ( clr != null ) this.setColor(clr);

			float lw =  attr.getLineWidth();
			if ( lw > 0 ) this.setLineWidth( lw );

			double ar =  attr.getAxisRatio();
			if ( ar > 0 ) this.setAxisRatio( ar );

			double sa =  attr.getStartAngle();
			if ( sa > 0 ) this.setStartAngle( sa );

			double ea =  attr.getEndAngle();
			if ( ea > 0 ) this.setEndAngle( ea );
		}
				
	}

	/**
	 * Create widgets for the color attribute
	 */
	private void createColorAttr(){

		Composite inCmp = new Composite( top, SWT.NONE);
        inCmp.setLayout( getGridLayout( 3, false, 0, 0, 0, 0 ) );
		
        chkBox[ChkBox.COLOR.ordinal()] = new Button(inCmp, SWT.CHECK);
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
     
        colorLbl = new Label( inCmp, SWT.LEFT );
        colorLbl.setText("Color:");

		cs = new ColorButtonSelector( inCmp, 20, 15 );
        cs.setColorValue( new RGB( 0,255,0 ) );
	}
	
	/**
	 * Create widgets for the wid th attribute
	 */
	private void createWidthAttr(){
	    
		Composite inCmp = new Composite( top, SWT.NONE);
        inCmp.setLayout( getGridLayout( 3, false, 0, 0, 0, 0 ) );
        
        chkBox[ChkBox.WIDTH.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.WIDTH.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.WIDTH.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					widthLbl.setEnabled(true);
					lineWidthSlider.setEnabled(true);
					lineWidthText.setEnabled(true);

				}
				else {
					widthLbl.setEnabled(false);
					lineWidthSlider.setEnabled(false);
					lineWidthText.setEnabled(false);
				}
			}
        	
        });  
        widthLbl = new Label(inCmp, SWT.LEFT);
        widthLbl.setText("Line Width ");
        
        Group lineWidthGrp = new Group( inCmp, SWT.NONE ) ;
        lineWidthGrp.setLayout( getGridLayout( 2, false, 0, 0, 0, 0 ) );

        lineWidthSlider = new Slider( lineWidthGrp, SWT.HORIZONTAL);
        lineWidthSlider.setValues( 2, 1, 11, 1, 1, 1 );
        lineWidthSlider.setLayoutData( new GridData( 90, 15 ) );
        lineWidthSlider.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent e ) {
            	lineWidthText.setText( "" + lineWidthSlider.getSelection() );
            }
        });
            
        lineWidthText = new Text(lineWidthGrp,  SWT.SINGLE | SWT.BORDER );                        
        lineWidthText.setLayoutData( new GridData( 30, 8 ) );
        lineWidthText.setEditable( true );   
        lineWidthText.setText( "2" );
        lineWidthText.addKeyListener( new KeyAdapter() {
            public void keyReleased( KeyEvent e ) {
                int value = 0;
            	try {
                    value = Integer.parseInt( lineWidthText.getText() );
                	if ( value >= 1 && value <= 10 ) {
                		lineWidthSlider.setSelection( value );
                		lineWidthText.setToolTipText( "" );
                	}
                	else {
                		lineWidthText.setToolTipText( "Only integer values between 1 and 10 are accepted." );
                	}
                } catch ( NumberFormatException e1 ) {
                	lineWidthText.setToolTipText( "Only integer values between 1 and 10 are accepted." );
                }
            }
        });
	}
	
	/**
	 * Create widgets for the ration attribute
	 */
	private void createRatioAttr(){
		Composite inCmp = new Composite( top, SWT.NONE);
        inCmp.setLayout( getGridLayout( 3, false, 0, 0, 0, 0 ) );

        chkBox[ChkBox.AXIS_RATIO.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.AXIS_RATIO.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.AXIS_RATIO.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					axisRatioLbl.setEnabled(true);
					axisRatioSlider.setEnabled(true);
					axisRatioText.setEnabled(true);

				}
				else {
					axisRatioLbl.setEnabled(false);
					axisRatioSlider.setEnabled(false);
					axisRatioText.setEnabled(false);
				}
			}
        	
		});  

		axisRatioLbl = new Label( inCmp, SWT.LEFT );
		axisRatioLbl.setText("Axis Ratio ");

		Group axisRatioGrp = new Group( inCmp, SWT.NONE ) ;
		axisRatioGrp.setLayout( getGridLayout( 2, false, 0, 0, 0, 0 ) );

		axisRatioSlider = new Slider( axisRatioGrp, SWT.HORIZONTAL );
		axisRatioSlider.setValues( 100, 0, 101, 1, 1, 1 );
		axisRatioSlider.setLayoutData( new GridData( 92, 15 ) );
		axisRatioSlider.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent e ) {
				axisRatioText.setText( "" + axisRatioSlider.getSelection()/100.0 );
			}
		});

		axisRatioText = new Text( axisRatioGrp,  SWT.SINGLE | SWT.BORDER );                        
		axisRatioText.setLayoutData( new GridData( 30, 10 ) );
		axisRatioText.setEditable( true );   
		axisRatioText.setText( "1.0" );
		axisRatioText.addKeyListener( new KeyAdapter() {
			public void keyReleased( KeyEvent e ) {
				double value = 0.0;
				try {
					value = Double.parseDouble( axisRatioText.getText() );
					if ( value >= 0.0 && value <= 100.0 ) {
						axisRatioSlider.setSelection( (int)(value*100) );
						axisRatioText.setToolTipText( "" );
					}
					else {
						axisRatioText.setToolTipText( "Only values between 0.0 and 1.0 are accepted." );
					}
				} catch ( NumberFormatException e1 ) {
					axisRatioText.setToolTipText( "Only values between 0.0 and 1.0 are accepted." );
				}
			}
		});

	}
	
	/**
	 * Create widgets for the start-angle atribute
	 */
	private void createStartAngleAttr(){
		Composite inCmp = new Composite( top, SWT.NONE);
        inCmp.setLayout( getGridLayout( 3, false, 0, 0, 0, 0 ) );

		chkBox[ChkBox.START_ANGLE.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.START_ANGLE.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.START_ANGLE.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					startAngleLbl.setEnabled(true);
					startAngleSlider.setEnabled(true);
					startAngleText.setEnabled(true);

				}
				else {
					startAngleLbl.setEnabled(false);
					startAngleSlider.setEnabled(false);
					startAngleText.setEnabled(false);
				}
			}
        	
		});
		 
        startAngleLbl = new Label( inCmp, SWT.LEFT );
        startAngleLbl.setText("Start Angle ");
        
        Group startAngleGrp = new Group( inCmp, SWT.NONE ) ;
        startAngleGrp.setLayout( getGridLayout( 2, false, 0, 0, 0, 0 ) );
        
        startAngleSlider = new Slider( startAngleGrp, SWT.HORIZONTAL );
        startAngleSlider.setValues( 0, 0, 361, 1, 1, 5 );
        startAngleSlider.setLayoutData( new GridData( 87, 15 ) );
        startAngleSlider.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent e ) {
            	startAngleText.setText( "" + startAngleSlider.getSelection() );
            }
        });
            
        startAngleText = new Text( startAngleGrp,  SWT.SINGLE | SWT.BORDER );                        
        startAngleText.setLayoutData( new GridData( 30, 10 ) );
        startAngleText.setEditable( true );   
        startAngleText.setText( "0" );
        startAngleText.addKeyListener( new KeyAdapter() {
            public void keyReleased( KeyEvent e ) {
                int value = 0;
            	try {
                    value = Integer.parseInt( startAngleText.getText() );
                	if ( value >= 0 && value <= 360 ) {
                		startAngleSlider.setSelection( value );
                		startAngleText.setToolTipText( "" );
                	}
                	else {
                		startAngleText.setToolTipText( "Only integer values between 0 and 360 are accepted." );
                	}
                } catch ( NumberFormatException e1 ) {
                	startAngleText.setToolTipText( "Only integer  values between 0 and 360 are accepted." );
                }
            }

        });
	}
	
	/**
	 * Create widgets for the end angle attribute
	 */
	private void createEndAngleAttr(){
		
		Composite inCmp = new Composite( top, SWT.NONE);
        inCmp.setLayout( getGridLayout( 3, false, 0, 0, 0, 0 ) );

		chkBox[ChkBox.END_ANGLE.ordinal()] = new Button(inCmp, SWT.CHECK);
		chkBox[ChkBox.END_ANGLE.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.END_ANGLE.ordinal()].addSelectionListener(new SelectionAdapter(){

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					endAngleLbl.setEnabled(true);
					endAngleSlider.setEnabled(true);
					endAngleText.setEnabled(true);

				}
				else {
					endAngleLbl.setEnabled(false);
					endAngleSlider.setEnabled(false);
					endAngleText.setEnabled(false);
				}
			}

		});

		endAngleLbl = new Label( inCmp, SWT.LEFT );
		endAngleLbl.setText("End Angle ");

		Group endAngleGrp = new Group( inCmp, SWT.NONE ) ;
		endAngleGrp.setLayout( getGridLayout( 2, false, 0, 0, 0, 0) );

		endAngleSlider = new Slider( endAngleGrp, SWT.HORIZONTAL );
		endAngleSlider.setValues( 360, 0, 361, 1, 1, 5 );
	    endAngleSlider.setLayoutData( new GridData( 88, 15 ) );		
		endAngleSlider.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent e ) {
				endAngleText.setText( "" + endAngleSlider.getSelection() );
			}
		});

		endAngleText = new Text( endAngleGrp,  SWT.SINGLE | SWT.BORDER );                        
		endAngleText.setLayoutData( new GridData( 33, 10 ) );
		endAngleText.setEditable( true );   
		endAngleText.setText( "360" );
		endAngleText.addKeyListener( new KeyAdapter() {
			public void keyReleased( KeyEvent e ) {
				int value = 0;
				try {
					value = Integer.parseInt( endAngleText.getText() );
					if ( value >= 0 && value <= 360 ) {
						endAngleSlider.setSelection( value );
						endAngleText.setToolTipText( "" );
					}
					else {
						endAngleText.setToolTipText( "Only integer values between 0 and 360 are accepted." );
					}
				} catch ( NumberFormatException e1 ) {
					endAngleText.setToolTipText( "Only integer  values between 0 and 360 are accepted." );
				}
			}
		});
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
	 * Enable/Disable all widgets in the dialog
	 * @param flag
	 */
	private void enableAllWidgets(boolean flag){
		
		colorLbl.setEnabled(flag);
		
		widthLbl.setEnabled(flag);
		lineWidthSlider.setEnabled(flag);
		lineWidthText.setEnabled(flag);
		
		axisRatioLbl.setEnabled(flag);
		axisRatioSlider.setEnabled(flag);
		axisRatioText.setEnabled(flag);
		
		startAngleLbl.setEnabled(flag);
		startAngleSlider.setEnabled(flag);
		startAngleText.setEnabled(flag);
		
		endAngleLbl.setEnabled(flag);
		endAngleSlider.setEnabled(flag);
		endAngleText.setEnabled(flag);
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
	 *  Interface for IArc.
	 */
	public Coordinate getCenterPoint() {
		return null;
	}
	
	public Coordinate getCircumferencePoint() {
		return null;
	}

}
