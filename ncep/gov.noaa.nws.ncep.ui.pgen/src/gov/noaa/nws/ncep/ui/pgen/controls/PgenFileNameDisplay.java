/* PgenFileNameDisplay
 * 
 * Date Created (December 2011)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.controls;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;


/**
 * Contribution item added to the CAVE status bar in the National Centers Perspective
 * to display the current PGEN file name
 *<p>
 * <pre>
 * SOFTWARE HISTORY
 *    Date       Ticket#	 Engineer	    Description
 * -----------------------------------------------------------
 * 12/2011                   J. Wu      	Initial creation
 * 03/2012                   J. Wu      	Moved here from ncep.viz.tools
 *                                       
 * </pre>
 * 
 * @author	J. Wu
 * @version 1.0
 */
public class PgenFileNameDisplay extends ContributionItem { 

	private static PgenFileNameDisplay instance = null;

	private Shell shell=null;       
    private Label fileNameLabel = null;
    private String fileName = "";
		
	/**
	 * Access method
	 * 
	 * @return	PgenFileNameDisplay
	 */
	public static PgenFileNameDisplay getInstance() {
	    if ( instance == null ) {
	        instance = new PgenFileNameDisplay();
	    }
	    return instance;
	}
	
	private PgenFileNameDisplay() {
		super();		
	}	

	/***
	 * Creates the composite to display active PGEN file name.
	 * @param the parent status bar manager, where the contribution item should be added.	
	 */
    @Override
	public void fill( Composite parent ) {

    	shell = parent.getShell();
    	
    	Composite fileNameComposite = new Composite( parent, SWT.NONE );
		fileNameComposite.setLayout( new GridLayout(1, false) );
		
		fileNameLabel = new Label( fileNameComposite, SWT.NONE );

		fileNameLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, true) );
		fileNameLabel.setText( "");
				
		setVisible( true ); 
		
	}
	
	@Override
	public void update() {		
		if ( fileName != null && fileNameLabel != null ) {
			fileNameLabel.setText( fileName );
		    shell.layout(true, true);
		    fileNameLabel.pack( true );
		}
	}

    @Override
    public void dispose() {
        super.dispose();                
		instance = null;
    }


	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
		update();
	}
    
}