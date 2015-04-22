/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.PgenDistanceDlg
 * 
 * August 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.HashMap;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;

import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;

import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;


/**
 * Create a dialog to specify the Distance Display Options in PGEN.  These options are used by
 * the multipoint drawing tool to display the distance and direction to the first point while drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/1		#318		S. Gilbert	Initial creation from PgenInterpDlg
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class PgenDistanceDlg extends AttrDlg {
   	
	static PgenDistanceDlg INSTANCE = null;	
	
	public static final String STATUTE_MILES = "sm";
	public static final String NAUTICAL_MILES = "nm";
	public static final String KILOMETERS = "km";
	public static final String COMPASS_16_PT = "16-pt";
	public static final String DIR_DEGREES = "DEG";
	
	/*
	 * Class to hold/remember the distance display properties
	 */
	public static class DistanceDisplayProperties {
		public boolean displayDistance;
		public String distanceUnits;
		public String directionUnits;
		
		public DistanceDisplayProperties(boolean displayDistance,
				String distanceUnits, String directionUnits) {
			this.displayDistance = displayDistance;
			this.distanceUnits = distanceUnits;
			this.directionUnits = directionUnits;
		}

	}
	
	private DistanceDisplayProperties distProps = new DistanceDisplayProperties(false, 
														NAUTICAL_MILES, COMPASS_16_PT);
	
	private static final String DIALOG_LABEL = "Distance Options";
	private static final String CLOSE_LABEL = "Close";
	
	private Composite top = null;
	
	private Button distanceDisplay = null;
	private Combo distanceUnits = null;
	private Combo directionUnits = null;

    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private PgenDistanceDlg( Shell parShell ) throws VizException {
		
        super( parShell );

    }
	
	/**
	 * Creates an extrapolation dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static PgenDistanceDlg getInstance( Shell parShell ) {
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new PgenDistanceDlg( parShell );
				
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
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout( mainLayout );

        // Initialize all of the menus, controls, and layouts
        initializeComponents();

        return top;
        
     }
    
    /*
     *  Add an Interpolate button on the button bar.
     */
    @Override
    public void createButtonsForButtonBar( Composite parent ) {   
    	createButton(parent, IDialogConstants.CLOSE_ID, CLOSE_LABEL, true);
    }

	
    /**
	 * Creates buttons, menus, and other controls in the dialog area
	 * @param listener 
	 */
	private void initializeComponents() {
		
        this.getShell().setText( DIALOG_LABEL );
        
        distanceDisplay = new Button(top, SWT.CHECK);
        distanceDisplay.setText( "Display Distance");
        distanceDisplay.setSelection( distProps.displayDistance );
        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        distanceDisplay.setLayoutData(gd);
        distanceDisplay.addSelectionListener( new DistancePropertiesListener() );
        
        Composite distanceUnitsGroup = new Composite( top, SWT.NONE);
        RowLayout rl1 = new RowLayout();
        rl1.justify = true;
        rl1.pack = false;
        distanceUnitsGroup.setLayout( rl1 );
        
		/*
		 * Distance Units label
		 */
		Label du = new Label( distanceUnitsGroup, SWT.NONE);
		du.setText("Distance Units:");

		/*
		 * Distance Units Option
		 */
		distanceUnits = new Combo( distanceUnitsGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
		distanceUnits.add(STATUTE_MILES);
		distanceUnits.add(NAUTICAL_MILES);
		distanceUnits.add(KILOMETERS);
		int index = distanceUnits.indexOf( distProps.distanceUnits );
		distanceUnits.select( index );
		
		distanceUnits.setEnabled( distProps.displayDistance );
        distanceUnits.addSelectionListener( new DistancePropertiesListener() );

        Composite directionUnitsGroup = new Composite( top, SWT.NONE);
        RowLayout rl2 = new RowLayout();
        rl2.justify = true;
        rl2.pack = false;
        directionUnitsGroup.setLayout( rl2 );
        
		/*
		 * Direction Units label
		 */
		Label dir = new Label( directionUnitsGroup, SWT.NONE);
		dir.setText("Direction Units:");

		/*
		 * Direction Units Option
		 */
		directionUnits = new Combo( directionUnitsGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
		directionUnits.add(COMPASS_16_PT);
		directionUnits.add(DIR_DEGREES);
		int idx = directionUnits.indexOf( distProps.directionUnits );
		directionUnits.select( idx );

		directionUnits.setEnabled( distProps.displayDistance );
        directionUnits.addSelectionListener( new DistancePropertiesListener() );
	}	
	
	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		
		if (IDialogConstants.CLOSE_ID == buttonId) {
			//System.out.println("INETRP PRESSED~!~~!~~");
			PgenUtil.setSelectingMode();
		}
		
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

	public DistanceDisplayProperties getDistanceProperties() {
		return distProps;
	}
	
	/**
	 * An SWT SelectionListener that saves selection changes to a DistanceDisplayProperties for
	 * later query.
	 * @author sgilbert
	 *
	 */
	class DistancePropertiesListener implements SelectionListener {

		@Override
		public void widgetSelected(SelectionEvent e) {
			
			if ( e.getSource() instanceof Button ) {
				Button b = (Button)e.getSource();
				if ( b.getSelection() ) {
					distanceUnits.setEnabled(true);
					directionUnits.setEnabled(true);
					distProps.displayDistance = true;
				}
				else {
					distanceUnits.setEnabled(false);
					directionUnits.setEnabled(false);
					distProps.displayDistance = false;
				}
			}
			
			else if ( e.getSource() == distanceUnits ) {
				distProps.distanceUnits = distanceUnits.getText();
			}

			else if ( e.getSource() == directionUnits ) {
				distProps.directionUnits = directionUnits.getText();
			}

		}

		@Override
		public void widgetDefaultSelected(SelectionEvent e) {
		}

		
	}

}
