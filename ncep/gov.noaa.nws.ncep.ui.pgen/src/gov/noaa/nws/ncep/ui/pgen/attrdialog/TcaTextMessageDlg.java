/*
 * TcaTextMessageDlg
 * 
 * Date created: 15 SEPTEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.file.FileTools;

import java.io.File;
import java.io.FileWriter;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * @author sgilbert
 *
 */
public class TcaTextMessageDlg extends CaveJFACEDialog {

	private Composite top = null;
	
	private final int SAVE_ID = IDialogConstants.CLIENT_ID + 7589;
	private final String SAVE_LABEL = "Save";
	
	private String filename;
	private String message;
	
	private final int NUM_LINES = 25;
	private final int NUM_COLUMNS = 80;
	
	/*
	 * no-arg constructor
	 */
	protected TcaTextMessageDlg(Shell parentShell) {
		super(parentShell);
	}

    /*
     *  Add Save and Cancel buttons on the dialog's button bar.
     */
    @Override
    public void createButtonsForButtonBar( Composite parent ) {   
    	
    	createButton(parent, SAVE_ID, SAVE_LABEL,	true);
    	createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL,	true);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
       */
    @Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText("TCA Text Message");
    }
    
	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {

		top = (Composite) super.createDialogArea(parent);
		
        /*
         *  Create the main layout for the dialog area.
         */
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);

        /*
         *  Create a text box for the VTEC message
         */
        Text messageBox = new Text(top, SWT.MULTI | SWT.BORDER | SWT.READ_ONLY | SWT.V_SCROLL);
        messageBox.setFont(new Font(messageBox.getDisplay(),"Courier",12, SWT.NORMAL) );
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		
		//  Calculate approximate size of text box to display 25 lines at 80 characters each
		gd.heightHint = NUM_LINES * messageBox.getLineHeight();       
		GC gc = new GC (messageBox);
		FontMetrics fm = gc.getFontMetrics ();
		gd.widthHint = NUM_COLUMNS * fm.getAverageCharWidth ();

        messageBox.setLayoutData(gd);
        messageBox.setText(message);
        
        //  Make sure to dispose of font
        messageBox.addDisposeListener(new DisposeListener() {

			@Override
			public void widgetDisposed(DisposeEvent e) {
				Text w = (Text)e.widget;
				w.getFont().dispose();
				//System.out.println("DISPOSINGFONT");
			}
        	
        });
        
        /*
         * Add horizontal separator
         */
        Label sep = new Label(top, SWT.HORIZONTAL | SWT.SEPARATOR);
        sep.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
        /*
         * Set label to display output file name
         */
		Label filelabel = new Label(top, SWT.NONE );
		filelabel.setText("Text File Name:  " + filename);
		
        /*
         * Add horizontal separator
         */
        Label sep2 = new Label(top, SWT.HORIZONTAL | SWT.SEPARATOR);
        sep2.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		return top;
	}
	
	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		
		/*
		 * Save TCV button pressed.  Save TCV Message to a file
		 */
		if ( buttonId == SAVE_ID ) {
			
			FileTools.writeFile(filename, message);
			
			super.buttonPressed(IDialogConstants.OK_ID);
			
		}
		else {
			super.buttonPressed(buttonId);
		}
		
	}
			
	/**
	 * @param filename the name of the output file
	 */
	public void setOutputFilename(String filename) {
		this.filename = filename;
	}
	
	/**
	 * 
	 * @param vtec The VTEC message
	 */
	public void setMessage(String vtec) {
		this.message = vtec;
		//this.message = vtec.replace((char)0x0a, Text.DELIMITER.charAt(0));
	}
	
	
}
