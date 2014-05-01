/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenExtrapDlg
 * 
 * June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;

import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;

import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;


/**
 * Create a dialog for PGEN extrapolation action.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09		#130		Jun Wu		Initial creation
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class PgenExtrapDlg extends AttrDlg {
   	
	static PgenExtrapDlg INSTANCE = null;	    
	
	public static final double S2HR	= ( 3600.0F );
	public static final double HR2S	= ( 2.778e-4F );		/* Seconds <--> Hours		*/
	public static final double SM2M	= ( 1609.34F );
	public static final double M2SM	= ( 6.21e-4F );			/* Statute miles <--> Meters	*/
	public static final double MS2SMH	= ( S2HR * M2SM );
	public static final double SMH2MS	= ( HR2S * SM2M );	/* Meters/Second <--> Miles/Hour*/
	public static final double NM2M	= ( 1852.0F );
	public static final double M2NM	= ( 5.4e-4F );			/* Nautical miles <--> Meters	*/
	public static final double MS2NMH = ( S2HR * M2NM );
	public static final double NMH2MS = ( HR2S * NM2M );	/* Nautical miles/hour <-> Meters/Sec */
		
	
	public static String[] SpeedUnit = new String[]{ "KTS", "MPH", "M/S" };
	public static String[] DurationOption = new String[]{ "00:15", "00:30", "01:00", "02:00",
														  "06:00", "12:00", "Other" };    
	
	private Composite top = null;
	
	private Text speedText = null;
    private Combo speedUnitCombo = null; 
	private Text directionText = null;

	private Text durationText = null;
    private Combo durationOptionCombo = null; 
    
    private Button copyBtn = null;
    private Button moveBtn = null;	  
    
    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private PgenExtrapDlg( Shell parShell ) throws VizException {
		
        super( parShell );

    }
	
	/**
	 * Creates an extrapolation dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static PgenExtrapDlg getInstance( Shell parShell ) {
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new PgenExtrapDlg( parShell );
				
			} catch (VizException e) {
				
				e.printStackTrace();
				
			}	
		}
		
		return INSTANCE;
		
	} 
                 
    /*
     * (non-Javadoc)
     * Create all of the widgets on the Dialog
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public Control createDialogArea( Composite parent ) {
        
        top = (Composite) super.createDialogArea( parent );

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout( 3, false );
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout( mainLayout );

        // Initialize all of the menus, controls, and layouts
        initializeComponents();

        return top;
        
     }
    
    /*
     *  Remove "Ok/Cancel" buttons by creating no buttons for button bar.
     */
    @Override
    public void createButtonsForButtonBar( Composite parent ) {          	
    }

	
    /**
	 * Creates buttons, menus, and other controls in the dialog area
	 * @param listener 
	 */
	private void initializeComponents() {
		
        this.getShell().setText( "Extrapolation" );
                
    	/*
    	 *  Create label, text, and unit for speed.      
    	 */
        GridData textGridData = new GridData( 40, 20 );
        
        Label speedLbl = new Label( top, SWT.LEFT );
        speedLbl.setText("Speed:");
        
        speedText = new Text( top,  SWT.SINGLE | SWT.BORDER );                        
        speedText.setLayoutData( textGridData );
        speedText.setEditable( true );   
        speedText.setText( "30.0" );

        speedUnitCombo = new Combo( top, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : SpeedUnit ) {
            speedUnitCombo.add( st );
        }
       
        speedUnitCombo.select( 1 );
        
        Label directionLbl = new Label( top, SWT.LEFT );
        directionLbl.setText("Direction:");
                               
    	/*
    	 *  Create label, and text for direction.      
    	 */
        directionText = new Text( top,  SWT.SINGLE | SWT.BORDER );                        
        directionText.setLayoutData( textGridData );
        directionText.setEditable( true );   
        directionText.setText( "270.0" );
        
        Label dummyLbl = new Label( top, SWT.LEFT );
        dummyLbl.setText("");
        
    	/*
    	 *  Create label, text, option for duration.      
    	 */
        Label durationLbl = new Label( top, SWT.LEFT );
        durationLbl.setText("Duration:");
        
        int defaultDuration = 4;
        
        durationText = new Text( top,  SWT.SINGLE | SWT.BORDER );                        
        durationText.setLayoutData( textGridData ); 
        durationText.setText( DurationOption[ defaultDuration ] );
        durationText.setEnabled( false );  
        
        durationOptionCombo = new Combo( top, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : DurationOption ) {
        	durationOptionCombo.add( st );
        }
        durationOptionCombo.select( defaultDuration );
        
        durationOptionCombo.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent e ) {
            	String selected = DurationOption[durationOptionCombo.getSelectionIndex()];
            	
            	if ( selected.equalsIgnoreCase("Other")) {
                    durationText.setEnabled( true ); 	
            	}
            	else {
            		durationText.setEnabled( false ); 
                	durationText.setText( selected );
            	}
            }
        } );
         
    	/*
    	 *  Create radio buttons for copy/move option.      
    	 */      
        copyBtn  = new Button( top, SWT.RADIO);
        copyBtn.setText( "Copy" );
        copyBtn.setSelection( true );

        Label grpLbl = new Label( top, SWT.LEFT );
        grpLbl.setText("");
      
        moveBtn = new Button( top, SWT.RADIO );
        moveBtn.setText( "Move" );                

	}	
	
	/**
	 * Gets values of all attributes of the dialog.
	 */
	public HashMap<String, Object> getAttrFromDlg(){
		
	 	HashMap<String, Object> attr = new HashMap<String, Object>( );
   
    	return attr;
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute attr ){	
	}

    /**
	 * @return the speed in unit of meter/second
	 */
	public double getSpeed() {
		
		String unit = SpeedUnit[ speedUnitCombo.getSelectionIndex() ];
		
        double speed = Double.parseDouble( speedText.getText() );
		
		if ( unit.equalsIgnoreCase( "KTS" ) ) {
			speed *= NMH2MS;
		}
		else if ( unit.equalsIgnoreCase( "MPH" )) {
			speed *= SMH2MS;
		}
		
		return speed;
	}

	/**
	 * @return the direction 
	 * 
	 * Note:  The direction is the ind direction from 0 - 360.
	 *        0  - North wind,  180 - South wind
	 *        90 - East wind,   270 - West wind
	 */
	public double getDirection() {		
		
		double dir = Double.parseDouble( directionText.getText() );
		
	    dir = dir - ((int)dir)/360 * 360;
	    
	    if ( dir < 0 )  dir += 360.0;

		directionText.setText( "" + dir );			
		
		return dir;
		
	}

	/**
	 * @return the distance in unit of meter
	 */
	public double getDistance() {		
		
		return this.getSpeedInMeter() * this.getDuration();
	}

	/**
	 * @return the copy/move flag
	 */
	public boolean isCopy() {		
		
		return copyBtn.getSelection(); 
	
	}

    /**
	 * @return the speed in unit of meter/second
	 */
	private double getSpeedInMeter() {
		
		String unit = SpeedUnit[ speedUnitCombo.getSelectionIndex() ];
		
        double speed = Double.parseDouble( speedText.getText() );
		
		if ( unit.equalsIgnoreCase( "KTS" ) ) {
			speed *= NMH2MS;
		}
		else if ( unit.equalsIgnoreCase( "MPH" )) {
			speed *= SMH2MS;
		}
		
		/*
		 * Speed should be >= 0.
		 */		
		if ( speed < 0 ) {
			speed *= -1;
			speedText.setText( "" + speed );			
		}
		
		return speed;
	}

	
	/**
	 * @return the duration in unit of second
	 */
	private double getDuration() {
		
		String[] st = durationText.getText().split( ":" );
		
		double duration = 0.0;
		
		if ( st.length > 0 ) {
			duration += ( 3600.0 * Integer.parseInt( st[0] ) );
		}
		
		if ( st.length > 1 ) {
			duration += ( 60.0 * Integer.parseInt( st[1] ) );
		}
		
		/*
		 *  Set to "00:30" if duration < 0.
		 */
		if ( duration < 0 ) { 
			
			int durationIndex = 1;
			
			durationText.setText( DurationOption[ durationIndex ] );
	        durationOptionCombo.select( durationIndex );			
			
	        duration = 60.0 * 30;
		}

		
		return duration;
	
	}


}
