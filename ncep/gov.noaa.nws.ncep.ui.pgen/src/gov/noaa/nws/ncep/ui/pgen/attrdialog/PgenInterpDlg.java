/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenInterpDlg
 * 
 * August 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.HashMap;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;

import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;

import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenInterpolationTool;


/**
 * Create a dialog for PGEN interpolation action.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/09		#142		S. Gilbert	Initial creation from PgenExtrapDlg
 * 11/12		#?			J. Wu		Allow using Gfa forecast hour
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class PgenInterpDlg extends AttrDlg {
   	
	static PgenInterpDlg INSTANCE = null;	
	
	PgenInterpolationTool interpolationTool = null;
	
	private static final int INTERP_ID = IDialogConstants.CLIENT_ID + 7585;
	private static final String INTERP_LABEL = "Interpolate";
	
	private Composite top = null;
	
	private Text startTimeText = null;
	private Text endTimeText = null;
	private Spinner interval = null;

    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private PgenInterpDlg( Shell parShell ) throws VizException {
		
        super( parShell );

    }
	
	/**
	 * Creates an extrapolation dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static PgenInterpDlg getInstance( Shell parShell ) {
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new PgenInterpDlg( parShell );
				
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
        FormLayout mainLayout = new FormLayout();
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
    	createButton(parent, INTERP_ID, INTERP_LABEL,	true);
    	getButton(INTERP_ID).setEnabled(false);
    }

	
    /**
	 * Creates buttons, menus, and other controls in the dialog area
	 * @param listener 
	 */
	private void initializeComponents() {
		
        this.getShell().setText( "Interpolation" );
        
		/*
		 * Starting Time label
		 */
		Label startTime = new Label( top, SWT.NONE);
		startTime.setText("Starting Time:");
		FormData fd = new FormData();
		fd.left = new FormAttachment(0,10);
		fd.top = new FormAttachment(0,10);
		startTime.setLayoutData(fd);

		/*
		 * Text field for start time
		 */
		startTimeText = new Text( top, SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.right = new FormAttachment(100,-10);
		fd.top = new FormAttachment(0,10);
		fd.left = new FormAttachment(startTime, 50, SWT.RIGHT);
		startTimeText.setLayoutData(fd);
		
		// Add listener to make sure only numeric digits are input
		startTimeText.addVerifyListener(new DigitVerifyListener() );

		/*
		 * Ending Time label
		 */
		Label endTime = new Label( top, SWT.NONE);
		endTime.setText("Ending Time:");
		fd = new FormData();
		fd.left = new FormAttachment(0,10);
		endTime.setLayoutData(fd);

		/*
		 * Text field for end time
		 */
		endTimeText = new Text( top, SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.right = new FormAttachment(100,-10);
		fd.top = new FormAttachment(startTimeText,10);
		fd.left = new FormAttachment(startTimeText, 0, SWT.LEFT);
		endTimeText.setLayoutData(fd);

		// Add listener to make sure only numeric digits are input
		endTimeText.addVerifyListener(new DigitVerifyListener() );

		// reposition top of end time label
		fd = (FormData)endTime.getLayoutData();
		fd.top = new FormAttachment(endTimeText, 0, SWT.TOP);

		/*
		 * Ending Time interval label
		 */
		Label intervalLabel = new Label( top, SWT.NONE);
		intervalLabel.setText("Interval (hrs):");
		fd = new FormData();
		fd.left = new FormAttachment(0,10);
		fd.top = new FormAttachment(endTime,10,SWT.BOTTOM);
		intervalLabel.setLayoutData(fd);

		/*
		 * Create spinner for time interval
		 */
		interval = new Spinner(top, SWT.BORDER);
		interval.setMinimum(1);
		interval.setMaximum(500);
		fd = new FormData();
		fd.right = new FormAttachment(100,-10);
		fd.top = new FormAttachment(endTimeText,10);
		fd.left = new FormAttachment(endTimeText, 0, SWT.LEFT);
		interval.setLayoutData(fd);
		
		// reposition top of interval label
		fd = (FormData)intervalLabel.getLayoutData();
		fd.top = new FormAttachment(interval, 0, SWT.TOP);

	}	
	
	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		
		if (INTERP_ID == buttonId) {
			//System.out.println("INETRP PRESSED~!~~!~~");
			if ( startTimeText.getText().isEmpty() || endTimeText.getText().isEmpty() ) {
				String msg = "Please provide both a valid Start Time and End Time.";
		    	MessageDialog messageDlg = new MessageDialog( 
		        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
		        		"Warning", null, msg,
		        		MessageDialog.WARNING, new String[]{"OK"}, 0);
		        messageDlg.open();
			} 
			else {
				interpolationTool.performInterpolation();
			}
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

	/**
	 * Returns the start time value from the Text widget
	 */
	public int getStartTime() {	
		if ( startTimeText.getText().isEmpty() ) return 0;
		else return Integer.parseInt( startTimeText.getText() );
	}

	/**
	 * Returns the end time value from the Text widget
	 * @return
	 */
	public int getEndTime() {
		if ( endTimeText.getText().isEmpty() ) return 0;
		else return Integer.parseInt( endTimeText.getText() );
	}
	
	/**
	 * Returns the interval value from the Spinner
	 * @return
	 */
	public int getInterval() {
		return interval.getSelection();
	}
	
	/**
	 * Specifies the interpolation tool to use and enables the "Interpolate" button
	 * @param tool an Interpolation tool
	 */
	public void arm(PgenInterpolationTool tool) {
		interpolationTool = tool;
		getButton(INTERP_ID).setEnabled(true);
	}
	
	/**
	 * Disables the "Interpolate" button
	 */
	public void disarm() {
		interpolationTool = null;
		getButton(INTERP_ID).setEnabled(false);
	}
	
	/**
	 * An SWT VerifyListener that checks to make sure the only text input allowed
	 * is a numeric digit, backspace, or delete character.
	 * @author sgilbert
	 *
	 */
	class DigitVerifyListener implements VerifyListener {

		@Override
		public void verifyText(VerifyEvent e) {
			
			final char BACKSPACE = 0x08;
			final char DELETE = 0x7F;
			
			if ( Character.isDigit(e.character) || 
				  e.character == BACKSPACE || e.character == DELETE ) e.doit = true;
			else {
				e.doit = false;
				Display.getCurrent().beep();
			}
		}
		
	}

	/**
	 * Sets the end time value from the Text widget
	 */
	public void setStartTime( String startTime ) {
		if ( startTime != null ) {
			for ( Listener ls : this.startTimeText.getListeners( SWT.Verify ) ) {
			    this.startTimeText.removeListener( SWT.Verify, ls );
			}
			
			this.startTimeText.setText( startTime  );
			
			startTimeText.addVerifyListener(new DigitVerifyListener() );					
		}
	}

	/**
	 * Sets the end time value from the Text widget
	 */
	public void setEndTime( String endTime ) {
		if ( endTime != null ) {
			for ( Listener ls : this.endTimeText.getListeners( SWT.Verify ) ) {
			    this.endTimeText.removeListener( SWT.Verify, ls );
			}
			
			this.endTimeText.setText( endTime  );
			
			endTimeText.addVerifyListener(new DigitVerifyListener() );
					
		}
	}

}
