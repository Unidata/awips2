/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.PgenHelpDialog
 * 
 * October 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * This class creates a dialog to show help information for PGEN.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/09  		#151      	J. Wu 		Initial creation. 
 * 07/11  		#450      	G. Hull     NcPathManager 
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */

public class PgenHelpDialog extends ProductDialog {
	
	private static PgenHelpDialog INSTANCE = null;
    
    private static String helpContent = null;
	    	               
    private final static int TEXT_WIDTH = 600;
    private final static int TEXT_HEIGHT = 600;
    
	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	protected PgenHelpDialog( Shell parShell ) throws VizException {
		
        super( parShell );
              
    }
	
	/**
	 * Creates a product configuration dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static PgenHelpDialog getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				INSTANCE = new PgenHelpDialog( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}
			
		}
		
		return INSTANCE;
		
	}

    
    /**
     *  Sets the title of the dialog.
     */
    public void setTitle() {    	
        shell.setText( "Product Generation Help" );        
    }
    
    /**
     *  Creates the main layout for the shell.
     */
    public void setLayout() {
        
        GridLayout mainLayout = new GridLayout( 1, true );
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout( mainLayout );

    }
    
    
    /**
     *  Set the default location.
     * @param parent
     */
    public void setDefaultLocation( Shell parent ) {
        
		if ( shellLocation == null ) {
	        Point pt = parent.getLocation();
	        shell.setLocation( pt.x + 500,  pt.y + 150 );
		} else {
			shell.setLocation(shellLocation);
		}

    }
    

    /**
     * Initialize the dialog components.
     */
    public void initializeComponents() {
    	
    	/*
    	 *  Load the help file.
    	 */
        try {
        	readHelpFile();
        }
        catch ( FileNotFoundException e ) {
        	System.out.println( "Cannot find file - productHelp.txt.");
        }
        
        /*
         * Create a text area to display the help content
         */
   	    Composite mainComp = new Composite( shell, SWT.NONE );
		GridLayout gl = new GridLayout( 1, false );
        mainComp.setLayout( gl );
        
		int style = SWT.MULTI | SWT.BORDER | SWT.V_SCROLL |SWT.WRAP;

		Text text = new Text( mainComp, style );                        
		text.setLayoutData( new GridData( TEXT_WIDTH, TEXT_HEIGHT ) );
		text.setEditable( false );  
        
		if ( helpContent != null ) {
		    text.setText( helpContent );
		}
		
		/*
		 * Create a "Close" button to exit the dialog
		 */
        GridData gd = new GridData( SWT.CENTER, SWT.DEFAULT, true, false );
        
        Button closeBtn = new Button( mainComp, SWT.NONE );
        closeBtn.setText( "Close"  );
        closeBtn.setLayoutData( gd );
        closeBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {                           	
            	close();
            	helpContent = null;
            }
        });
        
    } 

    /**
     *  Set the default location.
     * @param parent
     * @throws FileNotFoundException 
     */
    private void readHelpFile() throws FileNotFoundException {
        
   	    if ( helpContent == null ) {
    	    
    		/*
    		 *  Get the help file from the localization
    		 */    		
   	    	helpContent = "";
   	    	File prdHelpFile = PgenStaticDataProvider.getProvider().getStaticFile( 
   	    			PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "PgenHelp.txt" );   	    
    		
     		if( prdHelpFile != null && prdHelpFile.exists() && prdHelpFile.canRead() ) {

     			Scanner scanner = new Scanner( prdHelpFile );
    	        try {
                    //first use a Scanner to get each line
    	            while ( scanner.hasNextLine() ){
    	                helpContent += scanner.nextLine() + "\n";    	                
    	            }
    	        }
    	        finally {
    	            //ensure the underlying stream is always closed
    	            scanner.close();
    	        }   	    

    	    }    
    	}
    }
    
}



