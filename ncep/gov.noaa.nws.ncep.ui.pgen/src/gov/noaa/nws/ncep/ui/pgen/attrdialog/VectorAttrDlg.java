/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.VectorAttrDlg
 * 
 * 14 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.HashMap;
import java.awt.Color;

import org.eclipse.swt.SWT;
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
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.display.IVector;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

/**
 * Singleton attribute dialog for text.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09		#111		J. Wu   	Initial Creation.
 * 09/09		#149		B. Yin		Added check boxes for multi-selection
 * 03/10        #231        Archana     Altered the dialog for the vector attribute
 *                                      to show only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 03/13		#928		B. Yin 		Added a separator above the button bar.
 * </pre>
 * 
 * @author	J. Wu
 */

public class VectorAttrDlg extends AttrDlg implements IVector{
		
	protected static enum ChkBox { COLOR, CLEAR, DIRECTION, SPEED, SIZE, WIDTH, HEADSIZE };

	static VectorAttrDlg INSTANCE = null;
       
	private Composite top = null;
	
	private Label colorLbl;
    private ColorButtonSelector cs = null;
    
    private Label clearLbl;
	private Button	clearBtn1 = null;
	private Button	clearBtn2 = null;
	
	protected Label dirLbl;
    protected Slider	dirSlider = null;  
    protected Text	dirText = null;
    
    protected Label   spdLbl = null;
    protected Slider	spdSlider = null;  
    protected Text	spdText = null;
    
    private Label sizeLbl;
    protected Slider	sizeSlider = null;  
    protected Text	sizeText = null;
    
    private Label widthLbl;
    protected Slider	widthSlider = null;  
    protected Text	widthText = null;
 
    private Label   arwHeadSizeLbl = null;
    private Slider	arwHeadSizeSlider = null;  
    private Text	arwHeadSizeText = null;
	
    //Check boxes for multi-selection
    protected Button chkBox[];
    
    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	protected VectorAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);

    }
	
	/**
	 * Creates a text attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static VectorAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new VectorAttrDlg( parShell );
				
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
	 * @param listener 
	 */
	private void initializeComponents() {
		
        this.getShell().setText("Vector Attributes");
        chkBox = new Button[7];
   
        createColorAttr();
        createClearAttr();
        createDirectionAttr();
        createSpeedAttr();
        createSizeAttr();
        createWidthAttr();
        createHeadSizeAttr();
 
        addSeparator(top.getParent());

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
	 * Sets the color of the color picker of the dialog.
	 * @param clr
	 */
	private void setColor( Color clr ){
		
		cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
		
	}
	
	/**
	 * Gets vector object type
	 * @return type of object
	 */
	public VectorType getVectorType() {
		return null;
	}

	/**
	 * Gets the wind direction
	 * @return direction from which the wind is blowing. North is considered 0 degrees 
	 * and direction increases clockwise.
	 */
	public double getDirection() {
		if ( chkBox[ChkBox.DIRECTION.ordinal()].getSelection() ){
			return (double)dirSlider.getSelection();
		}
		else {
			return java.lang.Double.NaN;
		}
	}
		
	/**
	 * Gets the wind speed 
	 * @return wind speed
	 */
	public double getSpeed() {
		if ( chkBox[ChkBox.SPEED.ordinal()].getSelection() ){
			return (double)spdSlider.getSelection();
		}
		else {
			return java.lang.Double.NaN;
		}
	}
	
	/**
	 * Gets the width of the vector object
	 * @return line width
	 */
	public float getLineWidth() {
		if ( chkBox[ChkBox.WIDTH.ordinal()].getSelection() ){
			return (float)widthSlider.getSelection();
		}
		else {
			return java.lang.Float.NaN;
		}
	}
			
	/**
	 * Gets the size scale factor for the object
	 * @return size scale factor
	 */
	public double getSizeScale() {
		if ( chkBox[ChkBox.SIZE.ordinal()].getSelection() ){
			return (double)(sizeSlider.getSelection()/10.0);
		}
		else {
			return java.lang.Double.NaN;
		}
	}
	
	/**
	 * Gets the size scale for the arrow head.
	 * @return returns arrow head size
	 */
	public double getArrowHeadSize() {
		if ( chkBox[ChkBox.HEADSIZE.ordinal()].getSelection() ){
			return (double)(arwHeadSizeSlider.getSelection()/10.0);
		}
		else {
			return java.lang.Double.NaN;
		}
	}
	
	/**
	 * Gets boolean flag indication whether arrow has an associated speed,
	 * or indicates direction only
	 * @return direction only flag
	 */
	public boolean hasDirectionOnly() {
		return false;
	}

	/**
	 * Checks whether the background of the object should be cleared.
	 * @return true, if background should be cleared
	 */
	public Boolean isClear() {
		if ( chkBox[ChkBox.CLEAR.ordinal()].getSelection() ){
			return clearBtn1.getSelection();
		}
		else {
			return null;
		}
	}
	
	/**
	 * Checks whether the background of the object should be cleared.
	 * @return true, if background should be cleared
	 */
	public Boolean hasBackgroundMask() {
		return isClear();
	}
	
	/**
	 * Sets the wind speed 
	 */
	public void setSpeed( double spd ) {
		spdSlider.setSelection( (int)spd );
		spdText.setText( "" + (int)spd );
	}
	
	/**
	 * Sets the wind direction
	 */
	public void setDirection( double dir ) {
		dirSlider.setSelection( (int)dir );
		dirText.setText( "" + (int)dir );
	}

	/**
	 * Sets the width of the vector object
	 * @return line width
	 */
	public void setLineWidth( float width ) {
		widthSlider.setSelection( (int)width );
		widthText.setText( "" +  width );		
	}
			
	/**
	 * Sets the size scale factor for the object
	 * @return size scale factor
	 */
	public void setSizeScale( double size ) {
		sizeSlider.setSelection( (int)(size*10) );
		sizeText.setText( "" +  size );		

	}

	/**
	 * Sets the size scale for the arrow head.
	 */
	public void setArrowHeadSize( double ahs ) {
		arwHeadSizeSlider.setSelection( (int)(ahs*10) );
		arwHeadSizeText.setText( "" +  ahs );		
	}
	
	/**
	 * Sets whether the background of the object should be cleared.
	 */
	public void setClear( boolean clr ) {
		clearBtn1.setSelection( clr );
	}
		

	/**
	 * Gets values of all attributes of the dialog.
	 */
	public HashMap<String, Object> getAttrFromDlg(){
		
	 	HashMap<String, Object> attr = new HashMap<String, Object>( );
   
    	attr.put("speed", this.getSpeed());
    	attr.put("direction", this.getDirection());
    	attr.put("arrowHeadSize", this.getArrowHeadSize());
    	attr.put("sizeScale", this.getSizeScale());
    	attr.put("lineWidth", this.getLineWidth());
    	attr.put("clear", this.isClear());
   	    attr.put("color", this.getColor());

    	return attr;
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute iattr ){
		if ( iattr instanceof IVector ){
			IVector attr = (IVector)iattr;
			adjustAttrForDlg( ((Vector)attr).getPgenType() );

			Color clr = attr.getColors()[0];
			if ( clr != null ) this.setColor( clr );

			this.setSpeed( attr.getSpeed() );           
			this.setClear( attr.isClear() );
			this.setDirection( attr.getDirection() );
			this.setSizeScale( attr.getSizeScale() );
			this.setLineWidth( attr.getLineWidth() );
			this.setArrowHeadSize( attr.getArrowHeadSize() );
		}
		
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void adjustAttrForDlg( String pgenType ){
	        		
		if ( pgenType.equalsIgnoreCase( "Barb" ) ) {
	        
			this.getShell().setText("Wind Barb Attributes");
			
	        arwHeadSizeLbl.setEnabled( false );
			arwHeadSizeSlider.setEnabled( false );
			arwHeadSizeText.setEnabled( false );
		
			spdLbl.setEnabled( true );
			spdSlider.setEnabled( true );
			spdText.setEnabled( true );	
	        	
			spdSlider.setValues( 100, 0, 405, 5, 5, 5 );	               
			spdText.setText( "100" );	
		
		}
		else if ( pgenType.equalsIgnoreCase( "Hash" ) ) {
			
	        this.getShell().setText("Hash Attributes");
	        
			spdLbl.setEnabled( false );
			spdSlider.setEnabled( false );
			spdText.setEnabled( false );	
			
			arwHeadSizeLbl.setEnabled( false );
			arwHeadSizeSlider.setEnabled( false );
			arwHeadSizeText.setEnabled( false );
		}
		else if ( pgenType.equalsIgnoreCase( "directional" ) ) {
	        
			this.getShell().setText("Directional Arrow Attributes");
			
			spdLbl.setEnabled( false );
			spdSlider.setEnabled( false );
			spdText.setEnabled( false );			
		
		}
		else {
			
	        this.getShell().setText("Wind Arrow Attributes");
	        
			spdLbl.setEnabled( true );
			spdSlider.setEnabled( true );
			spdText.setEnabled( true );	
			
	        spdSlider.setValues( 10, 0, 401, 1, 1, 1 );	               
			
			arwHeadSizeLbl.setEnabled( true );
			arwHeadSizeSlider.setEnabled( true );
			arwHeadSizeText.setEnabled( true );			
		}
									
	}

	/**
	 * Create widgets for the Color attribute
	 */
	private void createColorAttr(){
		chkBox[ChkBox.COLOR.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.COLOR.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.COLOR.ordinal()].addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

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

		cs = new ColorButtonSelector ( top ) ;
		cs.setColorValue( new RGB( 0,255,0 ) );
	}

	/**
	 * Create widgets for the Clear attribute
	 */
	private void createClearAttr(){
		chkBox[ChkBox.CLEAR.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.CLEAR.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.CLEAR.ordinal()].addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

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
	 * Create widgets foe the Direction attribute
	 */
	private void createDirectionAttr(){
		chkBox[ChkBox.DIRECTION.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.DIRECTION.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.DIRECTION.ordinal()].addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					dirLbl.setEnabled(true);
					dirText.setEnabled(true);
					dirSlider.setEnabled(true);
				}
				else {
					dirLbl.setEnabled(false);
					dirText.setEnabled(false);
					dirSlider.setEnabled(false);
				}
			}

		}); 
		dirLbl = new Label( top, SWT.LEFT );
		dirLbl.setText("Direction:");
		GridLayout gl = new GridLayout(2, false);

		Group dirGroup = new Group( top, SWT.NONE ) ;
		dirGroup.setLayout( gl );

		dirSlider = new Slider( dirGroup, SWT.HORIZONTAL );
		dirSlider.setValues( 360, 0, 365, 5, 5, 5 );
		dirSlider.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent e ) {
				dirText.setText( "" + dirSlider.getSelection() );
			}
		});

		dirText = new Text( dirGroup,  SWT.SINGLE | SWT.BORDER );                        
		dirText.setLayoutData( new GridData( 25, 10 ) );
		dirText.setEditable( true );   
		dirText.setText( "360" );
		dirText.addKeyListener( new KeyAdapter() {
			public void keyReleased( KeyEvent e ) {
				int value = 0;
				try {
					value = Integer.parseInt( dirText.getText() );
					if ( value >= 0 && value < 361 ) {
						dirSlider.setSelection( value/5 * 5 );
						dirText.setToolTipText( "" );
					}
					else {
						dirText.setToolTipText( "Only integer values between 0 and 360 are accepted." );
					}
				} catch ( NumberFormatException e1 ) {
					dirText.setToolTipText( "Only integer values between 0 and 360 are accepted." );
				}
			}
		});

	}

	/**
	 * Create widgets for the Speed attribute
	 */
	private void createSpeedAttr(){
		chkBox[ChkBox.SPEED.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.SPEED.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.SPEED.ordinal()].addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					spdLbl.setEnabled(true);
					spdText.setEnabled(true);
					spdSlider.setEnabled(true);
				}
				else {
					spdLbl.setEnabled(false);
					spdText.setEnabled(false);
					spdSlider.setEnabled(false);
				}
			}

		}); 

		spdLbl = new Label( top, SWT.LEFT );
		spdLbl.setText("Speed:");
		GridLayout gl = new GridLayout(2, false);

		Group spdGroup = new Group( top, SWT.NONE ) ;
		spdGroup.setLayout( gl );

		spdSlider = new Slider( spdGroup, SWT.HORIZONTAL );
		spdSlider.setValues( 10, 0, 401, 1, 1, 1 );
		spdSlider.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent e ) {
				spdText.setText( "" + spdSlider.getSelection() );
			}
		});

		spdText = new Text( spdGroup,  SWT.SINGLE | SWT.BORDER );                        
		spdText.setLayoutData( new GridData( 25, 10 ) );
		spdText.setEditable( true );   
		spdText.setText( "10" );
		spdText.addKeyListener( new KeyAdapter() {
			public void keyReleased( KeyEvent e ) {
				int value = 0;
				try {
					value = Integer.parseInt( spdText.getText() );
					if ( value >= 0 && value < 401 ) {
						spdSlider.setSelection( value );
						spdText.setToolTipText( "" );
					}
					else {
						spdText.setToolTipText( "Only integer values between 0 and 400 are accepted." );
					}
				} catch ( NumberFormatException e1 ) {
					spdText.setToolTipText( "Only integer values between 0 and 400 are accepted." );
				}
			}
		});			
	}

	/**
	 * Create widgets for the Size attribute
	 */
	private void createSizeAttr(){
		chkBox[ChkBox.SIZE.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.SIZE.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.SIZE.ordinal()].addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					sizeLbl.setEnabled(true);
					sizeText.setEnabled(true);
					sizeSlider.setEnabled(true);
				}
				else {
					sizeLbl.setEnabled(false);
					sizeText.setEnabled(false);
					sizeSlider.setEnabled(false);
				}
			}

		}); 	


		sizeLbl = new Label( top, SWT.LEFT );
		sizeLbl.setText("Size:");
		GridLayout gl = new GridLayout(2, false);

		Group sizeGroup = new Group( top, SWT.NONE ) ;
		sizeGroup.setLayout( gl );

		sizeSlider = new Slider( sizeGroup, SWT.HORIZONTAL );
		sizeSlider.setValues( 10, 1, 101, 1, 1, 1 );
		sizeSlider.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent e ) {
				sizeText.setText( "" + sizeSlider.getSelection()/10.0 );
			}
		});

		sizeText = new Text( sizeGroup,  SWT.SINGLE | SWT.BORDER );                        
		sizeText.setLayoutData( new GridData( 25, 10 ) );
		sizeText.setEditable( true );   
		sizeText.setText( "1.0" );
		sizeText.addKeyListener( new KeyAdapter() {
			public void keyReleased( KeyEvent e ) {
				double value = 0;
				try {
					value = Double.parseDouble( sizeText.getText() );
					if ( value >= 0.1 && value < 10.0 ) {
						sizeSlider.setSelection( (int)(value*10) );
						sizeText.setToolTipText( "" );
					}
					else {
						sizeText.setToolTipText( "Only values between 0.1 and 10.0 are accepted." );
					}
				} catch ( NumberFormatException e1 ) {
					sizeText.setToolTipText( "Only values between 0.1 and 10.0 are accepted." );
				}
			}
		});	
	}

	/**
	 * Create widgets for the line width attribute 
	 */
	private void createWidthAttr(){
		chkBox[ChkBox.WIDTH.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.WIDTH.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.WIDTH.ordinal()].addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					widthLbl.setEnabled(true);
					widthText.setEnabled(true);
					widthSlider.setEnabled(true);
				}
				else {
					widthLbl.setEnabled(false);
					widthText.setEnabled(false);
					widthSlider.setEnabled(false);
				}
			}

		}); 


		widthLbl = new Label( top, SWT.LEFT );
		widthLbl.setText("Width:");

		GridLayout gl = new GridLayout(2, false);

		Group widthGroup = new Group( top, SWT.NONE ) ;
		widthGroup.setLayout( gl );

		widthSlider = new Slider( widthGroup, SWT.HORIZONTAL );
		widthSlider.setValues( 2, 1, 11, 1, 1, 1 );
		widthSlider.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent e ) {
				widthText.setText( "" + widthSlider.getSelection() );
			}
		});

		widthText = new Text( widthGroup,  SWT.SINGLE | SWT.BORDER );                        
		widthText.setLayoutData( new GridData( 25, 10 ) );
		widthText.setEditable( true );   
		widthText.setText( "2" );
		widthText.addKeyListener( new KeyAdapter() {
			public void keyReleased( KeyEvent e ) {
				int value = 0;
				try {
					value = Integer.parseInt( widthText.getText() );
					if ( value >= 1 && value < 11 ) {
						widthSlider.setSelection( value );
						widthText.setToolTipText( "" );
					}
					else {
						widthText.setToolTipText( "Only values between 1.0 and 10.0 are accepted." );
					}
				} catch ( NumberFormatException e1 ) {
					widthText.setToolTipText( "Only values between 1.0 and 10.0 are accepted." );
				}
			}
		});
	}

	/**
	 * Create widgets for the head size attribute 
	 */
	private void createHeadSizeAttr(){
		chkBox[ChkBox.HEADSIZE.ordinal()] = new Button(top, SWT.CHECK);
		chkBox[ChkBox.HEADSIZE.ordinal()] .setLayoutData(new GridData(CHK_WIDTH,CHK_HEIGHT));
		chkBox[ChkBox.HEADSIZE.ordinal()].addSelectionListener(new SelectionListener(){

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				Button btn = (Button)e.widget;
				if(btn.getSelection()){
					arwHeadSizeLbl.setEnabled(true);
					arwHeadSizeText.setEnabled(true);
					arwHeadSizeSlider.setEnabled(true);
				}
				else {
					arwHeadSizeLbl.setEnabled(false);
					arwHeadSizeText.setEnabled(false);
					arwHeadSizeSlider.setEnabled(false);
				}
			}

		}); 

		arwHeadSizeLbl = new Label( top, SWT.LEFT );
		arwHeadSizeLbl.setText("Head Size:");
		GridLayout gl = new GridLayout(2, false);

		Group arwHeadSizeGroup = new Group( top, SWT.NONE ) ;
		arwHeadSizeGroup.setLayout( gl );

		arwHeadSizeSlider = new Slider( arwHeadSizeGroup, SWT.HORIZONTAL );
		arwHeadSizeSlider.setValues( 10, 1, 101, 1, 1, 1 );
		arwHeadSizeSlider.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent e ) {
				arwHeadSizeText.setText( "" + arwHeadSizeSlider.getSelection()/10.0 );
			}
		});

		arwHeadSizeText = new Text( arwHeadSizeGroup,  SWT.SINGLE | SWT.BORDER );                        
		arwHeadSizeText.setLayoutData( new GridData( 25, 10 ) );
		arwHeadSizeText.setEditable( true );   
		arwHeadSizeText.setText( "1.0" );
		arwHeadSizeText.addKeyListener( new KeyAdapter() {
			public void keyReleased( KeyEvent e ) {
				double value = 0;
				try {
					value = Double.parseDouble( arwHeadSizeText.getText() );
					if ( value >= 0.1 && value < 10.0 ) {
						arwHeadSizeSlider.setSelection( (int)(value*10) );
						arwHeadSizeText.setToolTipText( "" );
					}
					else {
						arwHeadSizeText.setToolTipText( "Only values between 0.1 and 10.0 are accepted." );
					}
				} catch ( NumberFormatException e1 ) {
					arwHeadSizeText.setToolTipText( "Only values between 0.1 and 10.0 are accepted." );
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

		int rt = super.open();
		Point shellSizeInPoint = this.getShell().getSize();
		shellSizeInPoint.x += 20;
		this.getShell().setSize(shellSizeInPoint);
		return rt;
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

		clearLbl.setEnabled(flag);
		clearBtn1.setEnabled(flag);
		clearBtn2.setEnabled(flag);

		widthLbl.setEnabled(flag);
		widthText.setEnabled(flag);
		widthSlider.setEnabled(flag);

		sizeLbl.setEnabled(flag);
		sizeText.setEnabled(flag);
		sizeSlider.setEnabled(flag);

		dirLbl.setEnabled(flag);
		dirText.setEnabled(flag);
		dirSlider.setEnabled(flag);

		spdLbl.setEnabled(flag);
		spdText.setEnabled(flag);
		spdSlider.setEnabled(flag);

		arwHeadSizeLbl.setEnabled(flag);
		arwHeadSizeText.setEnabled(flag);
		arwHeadSizeSlider.setEnabled(flag);

	}

	/**
	 * Set all multi-selection check boxes to true. 
	 */
	private void setAllChkBoxes(){
		for ( ChkBox chk : ChkBox.values()){
			chkBox[chk.ordinal()].setSelection(true);
		}
	}

	@Override
    public Coordinate getLocation(){
    	return null;
    }

	@Override
	public Color getColor() {
		return this.getColors()[0];
	}

}

