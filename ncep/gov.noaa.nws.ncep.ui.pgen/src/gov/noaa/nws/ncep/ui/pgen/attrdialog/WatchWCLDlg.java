/*
 * WatchWCLDlg
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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Implementation of WCL dialog for watch box element.
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

public class WatchWCLDlg extends CaveJFACEDialog {

	private Composite top = null;
	
	private String fileName = "WCL_report";
	private String wclText;
	private String outputPath;
	
	private final int NUM_LINES = 25;
	private final int NUM_COLUMNS = 68;
	
	/*
	 * no-arg constructor
	 */
	protected WatchWCLDlg(Shell parentShell) {
		super(parentShell);
      //  this.setShellStyle(SWT.TITLE | SWT. | SWT.CLOSE );

	}

	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
		getShell().setText("Save WCL");
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
        messageBox.setText(wclText);
        
        //  Make sure to dispose of font
        messageBox.addDisposeListener(new DisposeListener() {

			@Override
			public void widgetDisposed(DisposeEvent e) {
				Text w = (Text)e.widget;
				w.getFont().dispose();
			}
        	
        });
		
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
		 * Save WCL button pressed.  Save WCL Message to a file
		 */

		FileTools.writeFile(outputPath + fileName, wclText);

		super.okPressed();
	}

	/**
	 * Set WCL text
	 * @param wcl The wcl message
	 */
	public void setMessage(String wcl) {
		this.wclText = wcl;
	}
	
	/**
	 * Set WCL file name
	 * @param file The WCL file name
	 */
	public void setWCLFileNmae(String file) {
		this.fileName = file;
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
