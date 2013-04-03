/*
 * WatchWCCDlg
 * 
 * Date created: 27 January 2010
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
 * Watch Box WCC SAVE dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#159		B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */
public class WatchWCCDlg extends CaveJFACEDialog {

	private Composite top = null;
	
	private final String filename = "KNCFNIMNAT";
	private String wccText;
	private String launchText;
	private String outputPath;

	private final int NUM_LINES = 25;
	private final int NUM_COLUMNS = 68;
	
	/*
	 * no-arg constructor
	 */
	protected WatchWCCDlg(Shell parentShell) {
		super(parentShell);
      //  this.setShellStyle(SWT.TITLE | SWT. | SWT.CLOSE );

	}

	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
		getShell().setText("Save WCC");
		top = (Composite) super.createDialogArea(parent);
		
        /*
         *  Create the main layout for the dialog area.
         */
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);

        /*
         *  Create a text box for the WCL message
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
        messageBox.setText(wccText);
        
        //  Make sure to dispose of font
        messageBox.addDisposeListener(new DisposeListener() {

			@Override
			public void widgetDisposed(DisposeEvent e) {
				Text w = (Text)e.widget;
				w.getFont().dispose();
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
		filelabel.setText("WCC File Name:  " + filename);
		
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
	protected void okPressed() {
		
		/*
		 * Save WCC button pressed.  Save WCC Message to a file
		 */
		
		FileTools.writeFile(outputPath + filename, wccText);
		FileTools.writeFile(outputPath + filename+".launch", launchText);
			
		super.okPressed();
	}
			
	/**
	 * Set WCC text
	 * @param wcc WCC message
	 */
	public void setMessage(String wcc) {
		this.wccText = wcc;
	}
	
	/**
	 * Set WCC launch text
	 * @param launchTxt
	 */
	public void setWCCLaunchText(String launchTxt ) {
		this.launchText = launchTxt;
	}
	
	@Override
	/**
	 * Set the location of the dialog
	 */
	public int open(){

		if ( this.getShell() == null ){
			this.create();
		}
   	   // this.getShell().setLocation(this.getShell().getParent().getLocation());
  	    this.getButton(IDialogConstants.OK_ID).setText("Save");
  	    this.getButtonBar().pack();
   	    return super.open();
		
	}

	public void setOutputPath(String outputPath) {
		this.outputPath = outputPath;
	}

	public String getOutputPath() {
		return outputPath;
	}

}
