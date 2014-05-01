/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.LayeringDisplayDialog
 * 
 * Sept 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

import java.awt.Color;

import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;


/**
 * This class allows the user to edit the color and fill mode of a PGEN layer.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/09  		#151      	J. Wu 		Initial creation. 
 * 03/11        231         Archana     Altered the PGEN layering dialog
 *                                      to display only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix . 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */

public class LayeringDisplayDialog extends ProductDialog {

    private ProductManageDialog layeringDlg = null;
    
    private Button colorBtn = null;
    private Button fillBtn = null;
    private ColorButtonSelector cs = null;   
           	         
    
	/**
     * Constructor.
     */
	public LayeringDisplayDialog( Shell parentShell, ProductManageDialog dlg ) {
		
		super( parentShell );
		
		this.layeringDlg = dlg;
			
	}

    /**
     *  Sets the title of the dialog.
     */
    public void setTitle() {    	
        
    	String title = layeringDlg.getColorModeLayerName();
    	
   	    if ( title == null )	{
            title = "Layer Display";       
   	    }
   	   
   	    shell.setText( title ); 
   	    
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
	        shell.setLocation( pt.x + 400,  pt.y + 146 );
		} else {
			shell.setLocation(shellLocation);
		}

    }

    
    /**
     * Initialize the dialog components.
     */
    public void initializeComponents() {    	               

    	Group grp = new Group( shell, SWT.NONE);
        grp.setText( "Mono Color" );
        grp.setLayout( new GridLayout( 2, true ) );
        colorBtn = new Button( grp, SWT.CHECK );
        colorBtn.setSelection( layeringDlg.getLayerForColorMode().isMonoColor() );

        cs = new ColorButtonSelector( grp );
        Color clr = layeringDlg.getLayerForColorMode().getColor();
        cs.setColorValue( new RGB( clr.getRed(),  clr.getGreen(), clr.getBlue() ) );       
        
        Composite fillComp = new Composite( shell, SWT.NONE );
        fillComp.setLayout( new GridLayout( 1, true ) );
        
        fillBtn = new Button( fillComp, SWT.CHECK );
        fillBtn.setText( "Filled" );
        fillBtn.setSelection( layeringDlg.getLayerForColorMode().isFilled() );
       
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
            	
            	layeringDlg.updateDisplayAttr( colorBtn.getSelection(), 
            	           new Color( cs.getColorValue().red, cs.getColorValue().green, 
            	  				      cs.getColorValue().blue,
            	  				      layeringDlg.getLayerForColorMode().getColor().getAlpha() ),
            	  		   fillBtn.getSelection()  );         	
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
 
}


