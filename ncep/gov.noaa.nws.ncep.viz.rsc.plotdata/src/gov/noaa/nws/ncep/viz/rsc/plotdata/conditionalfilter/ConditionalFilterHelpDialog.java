package gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;


/**
 * 
 * This class displays the ConditionalFilterHelp.txt file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2012      #615       S. Gurung   Initial Creation
 *                       
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
public class ConditionalFilterHelpDialog extends Dialog {

	private Shell shell;
	private Point dlgLocation=null;
     
	private static ConditionalFilterHelpDialog INSTANCE = null;
    
    private static String helpContent = null;
     
	private final static int TEXT_WIDTH = 600;
	private final static int TEXT_HEIGHT = 600;
	    
	public ConditionalFilterHelpDialog(Shell parent) {
		super(parent);
		// TODO Auto-generated constructor stub
	}
	
	/**
     * Open method used to display the product control dialog.
     * 
     * @return Return object (can be null).
     */
    public Object open() {
    	 
        // Create the main shell; 
    	Shell parent = this.getParent();
        shell = new Shell( parent, SWT.DIALOG_TRIM | SWT.MODELESS );
        Display display = parent.getDisplay();

        // Set the title of the dialog.
        setTitle();
		
        // Create the main layout for the shell.
        setLayout();
 
        // Set the default location.
        setDefaultLocation( parent );
        
        // Create and initialize all of the controls and layouts
        initializeComponents();
                
        if( dlgLocation == null ) {
    		dlgLocation = new Point( parent.getLocation().x+100, 
    								 parent.getLocation().y+100 );
    	}
    	shell.setLocation( parent.getLocation().x+100, 
    					   parent.getLocation().y+100);
    	shell.setMinimumSize(400, 300);

    	shell.pack();
    	shell.open();
    	
    	while( !shell.isDisposed() ) {
    		if( !display.readAndDispatch() ) {
    			display.sleep();
    		}
    	}
       
        return null;
    }
        
	
	/**
	 * Creates a product configuration dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static ConditionalFilterHelpDialog getInstance( Shell parShell){
		
		if ( INSTANCE == null ){					
			INSTANCE = new ConditionalFilterHelpDialog( parShell );
		}
		
		return INSTANCE;
		
	}

    
    /**
     *  Sets the title of the dialog.
     */
    public void setTitle() {    	
        shell.setText( "Conditional Filtering Help" );        
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
        
		if ( dlgLocation == null ) {
	        Point pt = parent.getLocation();
	        shell.setLocation( pt.x + 500,  pt.y + 150 );
		} else {
			shell.setLocation(dlgLocation);
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
        	System.out.println( "Cannot find file - ConditionalFilterHelp.txt.");
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
   	    	File condFilterHelpFile = NcPathManager.getInstance().getStaticFile( 
    				NcPathConstants.CONDITIONAL_FILTER_HELP_FILE );   	    
    		
     		if( condFilterHelpFile != null && condFilterHelpFile.exists() && condFilterHelpFile.canRead() ) {

     			Scanner scanner = new Scanner( condFilterHelpFile );
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
   
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }  

    /*
     *  Close the dialog
     */
    public void close() {
        
		if ( shell != null && !shell.isDisposed() ){
			Rectangle bounds = shell.getBounds();
			dlgLocation = new Point(bounds.x, bounds.y);
			shell.dispose();
		}

    }  
    

}
