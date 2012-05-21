/*
 * gov.noaa.nws.ncep.ui.pgen.tools.IncDecDlg
 * 
 * August 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenIncDecTool;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenIncDecTool.PgenIncDecHandler;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenInterpolationTool;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.core.exception.VizException;


/**
 * Create a dialog for PGEN Inc/Dec action.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/11			#?		B. Yin		Initial creation.
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class IncDecDlg  extends AttrDlg {
	   	
		static IncDecDlg INSTANCE = null;	
		
		private Composite top = null;
		
		private Spinner interval = null;
		private int intervalValue = 1;
		
		private PgenIncDecTool tool;
		
	    /**
		 * Private constructor
		 * @param parShell
		 * @throws VizException
		 */
		private IncDecDlg( Shell parShell ) throws VizException {
			
	        super( parShell );

	    }
		
		/**
		 * Creates an Inc/Dec dialog if the dialog does not exist 
		 * and returns the instance. If the dialog exists, return the instance.
		 *  
		 * @param parShell
		 * @return
		 */
		public static IncDecDlg getInstance( Shell parShell ) {
			
			if ( INSTANCE == null ){
						
				try {
					
					INSTANCE = new IncDecDlg( parShell );
					
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
	        GridLayout mainLayout = new GridLayout();
	        mainLayout.marginTop = 5;
	        mainLayout.marginWidth = 1;
	        top.setLayout( mainLayout );

	        // Initialize all of the menus, controls, and layouts
	        initializeComponents();

	        return top;
	        
	     }
	    
	    /**
	     *  Put the 'Deselect' and the 'Exit' buttons on the button bar.
	     */
	    @Override
	    public void createButtonsForButtonBar( Composite parent ) {   
	    	super.createButtonsForButtonBar(parent);
	    	
	  		this.getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
	  		this.getButton(IDialogConstants.OK_ID).setEnabled(true);	    
	  		this.getButton(IDialogConstants.CANCEL_ID).setText("Deselect");
	  		this.getButton(IDialogConstants.OK_ID).setText("Exit");
	    }
	    
	    /**
		 * Creates buttons, menus, and other controls in the dialog area
		 * @param listener 
		 */
		private void initializeComponents() {
			
	        this.getShell().setText( "Inc/Dec Editor" );
	        
			/*
			 * Create spinner for Inc/Dec interval
			 */
	        interval = new Spinner(top, SWT.BORDER|SWT.CENTER);
	        interval.setMinimum(-10000);
	        interval.setMaximum(10000);
	        interval.setSelection(intervalValue);
	        
	        interval.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, true, true, 1, 1));
	      
		}	
		
		/*
		 * 
		 * (non-Javadoc)
		 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
		 */
		@Override
		protected void buttonPressed(int buttonId) {

			((PgenIncDecHandler)tool.getMouseHandler()).cleanup();
			
			if ( buttonId == IDialogConstants.OK_ID ){
				PgenUtil.setSelectingMode();
				this.close();
			}
		}
		
		/**
		 * Sets values of all attributes of the dialog.
		 */
		@Override
		public void setAttrForDlg( IAttribute attr ){	
		}

		/**
		 * Returns the interval value from the Spinner
		 * @return
		 */
		public int getInterval() {
			return interval.getSelection();
		}
		
		@Override
		public boolean close(){
			if ( this.getShell() != null )intervalValue = interval.getSelection();
			return super.close();
		}
		
		public void setTool(PgenIncDecTool tool) {
			this.tool = tool;
		}

		public PgenIncDecTool getTool() {
			return tool;
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
}
