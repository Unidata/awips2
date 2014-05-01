/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenLayeringNameDialog
 * 
 * July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.layering;

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

/**
 * This class allows the user to edit a PGEN layer name.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/09  		#131      	J. Wu 		Initial creation. 
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */

public class PgenLayeringNameDialog extends PgenLayeringDialog {

    private Text   nameText = null;    
    private PgenLayeringControlDialog layeringDlg = null;
    
    
	/**
     * Constructor.
     */
	public PgenLayeringNameDialog( Shell parentShell, PgenLayeringControlDialog dlg ) {
		
		super( parentShell );		
		
		layeringDlg = dlg;
			
	}
    
    /**
     *  Sets the title of the dialog.
     */
    public void setTitle() {    	
        shell.setText( "Layer Name" );        
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
        Point pt = parent.getLocation();
        shell.setLocation( pt.x + 400,  pt.y + 380 );
    }
    

    /**
     * Initialize the dialog components.
     */
    public void initializeComponents() {
    	        
    	Composite top = new Composite( shell, SWT.NONE );
        GridLayout gl = new GridLayout( 1, false );
        GridData gd = new GridData( SWT.FILL, SWT.DEFAULT, true, false );
        top.setLayoutData( gd );
        top.setLayout( gl );
        
        nameText = new Text( top, SWT.SINGLE | SWT.BORDER );                        
        nameText.setLayoutData( new GridData( 95, 20 ) );
        nameText.setEditable( true );   
        nameText.setText( layeringDlg.getActiveLayer().getName() );
		
        Composite centeredComp = new Composite( shell, SWT.NONE );
        GridLayout gl2 = new GridLayout( 2, true );
        centeredComp.setLayout( gl2 );
        centeredComp.setLayoutData( gd );

        Button acceptBtn = new Button( centeredComp, SWT.NONE );
        acceptBtn.setText( "Accept" );
        acceptBtn.setLayoutData( gd );
        acceptBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {                           	
            	updateName( nameText.getText() );            	
            	shell.dispose();
            }
        });
        
        Button cancelBtn = new Button( centeredComp, SWT.NONE );
        cancelBtn.setText( "  Close " );
        cancelBtn.setLayoutData( gd );
        cancelBtn.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    
    }

    

	/*
     *  Update the name in the layering control window.
     */
    private void updateName( String txt ) {
    	
        if ( layeringDlg != null ) {
            layeringDlg.updateActiveLayerName( txt );
    	}
    } 
       
}

