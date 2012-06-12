/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.ProductFileNameDialog
 * 
 * Oct. 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.FileDialog;

/**
 * This class allows the user to edit the input/output file name
 * and path of a PGEN product.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/10  		#151      	J. Wu 		Initial creation. 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */
public class ProductFileNameDialog extends ProductDialog {
    
	private static final int TEXT_BOX_LENGTH = 300;
	
    private ProductManageDialog prdManageDlg = null;
    
//    private Text inputFileTxt = null;
    private Text outputFileTxt = null; 
       
//	private String initialInput = null;
	private String initialOutput = null;
   
	/**
     * Constructor.
     */
	public ProductFileNameDialog( Shell parentShell, ProductManageDialog dlg ) {
		
		super( parentShell );
		
		this.prdManageDlg = dlg;
		
	}

	/**
     *  Sets the title of the dialog.
     */
    public void setTitle() {    	
/*        
    	String title = prdManageDlg.getPrdNameForFileInOut();
    	
   	    if ( title == null )	{
            title = "Product Input/Output File";       
   	    }
   	    else {
   	    	title = title + " Input/Output File";
   	    }
   	   
   	    shell.setText( title ); 
*/   	    
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
	        shell.setLocation( pt.x + 475,  pt.y + 146 );
		} else {
			shell.setLocation(shellLocation);
		}

    }

    
	/**
     * Initialize the dialog components.
     */
    public void initializeComponents() {    	               

//    	initialInput = prdManageDlg.getProductForPrdFileInOut().getInputFile();
//		initialOutput = prdManageDlg.getProductForPrdFileInOut().getOutputFile();       
 				
		// Create a composite for layer input file name
        Composite infileComp = new Composite( shell, SWT.NONE );
        GridLayout gl0 = new GridLayout( 3, false );
        gl0.marginWidth = 3;
        infileComp.setLayout( gl0 );

/*       
        Label inputLbl = new Label( infileComp, SWT.LEFT );
        inputLbl.setText("Input:");
        
        inputFileTxt = new Text( infileComp,  SWT.SINGLE | SWT.BORDER );                        
        inputFileTxt.setLayoutData( new GridData( TEXT_BOX_LENGTH, 15 ) );
        inputFileTxt.setEditable( true );   
        if ( initialInput != null ) {
        	inputFileTxt.setText( initialInput  );
       }
        else {
        	inputFileTxt.setText( "" );
    	}

        Button nameBtn = new Button( infileComp, SWT.PUSH );
        nameBtn.setText( "Browse");
        
        nameBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent event ) {
            	createFileText( inputFileTxt, initialInput );
            }
        });      
*/       
        //Create a composite for layer output file name
        Composite outfileComp = new Composite( shell, SWT.NONE );
        outfileComp.setLayout( gl0 );
        
        Label outputLbl = new Label( outfileComp, SWT.LEFT );
        outputLbl.setText("Output:");
      
        outputFileTxt = new Text( outfileComp,  SWT.SINGLE | SWT.BORDER );                        
        outputFileTxt.setLayoutData( new GridData( TEXT_BOX_LENGTH, 15 ) );
        outputFileTxt.setEditable( true );   
        if ( initialOutput != null ) {
        	outputFileTxt.setText( initialOutput  );
        }
        else {
        	outputFileTxt.setText( "" );
        }

        Button browseBtn = new Button( outfileComp, SWT.PUSH );
        browseBtn.setText( "Browse");
        
        browseBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected( SelectionEvent event ) {            	
               	createFileText( outputFileTxt, initialOutput );
            }
        });      

    	//Create a composite for control buttons.
        Composite centeredComp = new Composite( shell, SWT.NONE );
        GridLayout gl2 = new GridLayout( 2, true );
        centeredComp.setLayout( gl2 );
        GridData gd = new GridData( SWT.FILL, SWT.DEFAULT, true, false );
        centeredComp.setLayoutData( gd );

        Button acceptBtn = new Button( centeredComp, SWT.NONE );
        acceptBtn.setText( "Accept" );
        acceptBtn.setLayoutData( gd );
        acceptBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {                           	
            	
//           	    prdManageDlg.updateProductFileAttr( null, 
//	                        outputFileTxt.getText() );

           	    close();
            }
        });
        
        Button cancelBtn = new Button( centeredComp, SWT.NONE );
        cancelBtn.setText( "Cancel" );
        cancelBtn.setLayoutData( gd );
        cancelBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

    
    }    
	    
	/**
	 * Create a file/path input dialog with a Text and a "Browse" button.
	 * 
	 * @param txt
	 * @return 
	 */
	private void createFileText( Text txt, String initialFile ) {
		
  	    String[] filterNames = new String[] { "*.xml", "All Files (*)" };
        String[] filterExtensions = new String[] {"*.xml", "*" };
//   	    String filterPath = PgenUtil.CURRENT_WORKING_DIRECTORY;
   	    String filterPath = PgenUtil.getWorkingDirectory();
   	    String defaultFile = new String( "default.xml" );
   	    
   	    if ( initialFile != null ) {
   	    	int index = initialFile.lastIndexOf('/');
   	    	if ( index >= 0 ) {
   	    	    defaultFile = initialFile.substring( index+1, initialFile.length() );
   	    	    filterPath = initialFile.substring( 0, index);
   	    	}
   	    	else {
   	    		defaultFile = new String( initialFile );
   	    	}
   	    }
   	    
    	String selectedFile = selectFile( shell, SWT.SAVE, filterNames,
    			    filterExtensions, filterPath, defaultFile, true );
    	if ( selectedFile != null ) {
    	    txt.setText(  selectedFile );
    	}           	
       
	}
    
	/**
	 * Create a file selection dialog
	 * 
	 * @param sh
	 * @param mode
	 * @param nameFilter
	 * @param extensionFilter
	 * @param pathFilter
	 * @param defaultFile
	 * @param overWrite
	 * @return dialog.open()
	 */
	private String selectFile( Shell sh, int mode, String[] nameFilter,
								String[] extensionFilter, String pathFilter,
								String defaultFile, boolean overWrite ) {
		
        FileDialog dialog = new FileDialog( sh, mode );
        dialog.setFilterNames( nameFilter );
        dialog.setFilterExtensions( extensionFilter );
        dialog.setFilterPath( pathFilter );
        if ( defaultFile != null ) {
            dialog.setFileName ( defaultFile );
        }
        dialog.setOverwrite( overWrite );               
                
        return dialog.open();
            	               
	}
	
}


