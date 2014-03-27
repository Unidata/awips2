package gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd;


import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.ResourceSelectionControl.IResourceSelectedListener;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;


/**
 *  Dialog displayed 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02/10/10      #226       Greg Hull    Initial Creation.
 * 06/13/10      #273       Greg Hull    
 * 04/20/11                 Greg Hull    args to set the style and title
 * 10/25/11      #467       Greg Hull    add Replace Resource 
 * 02/22/13      #972       G. Hull     Only show resources for given NcDisplayType
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class ResourceSelectionDialog extends Dialog { 
   
    private Shell shell;
    private Point dlgLocation=null;
    private boolean replaceEnabled = false;
    
    private ResourceSelectionControl sel_rsc_cntrl = null;
        
    private Set<IResourceSelectedListener> rscSelListeners = 
    	                   new HashSet<IResourceSelectedListener>();
    
    public ResourceSelectionDialog( Shell parShell )  {
    	super(parShell);
    }
    
	Boolean replaceBtnVisible;
	Boolean replaceBtnEnabled;
	ResourceName initRscName;


    public Object open( Boolean replaceVisible,
    					Boolean replaceEnabled,
    					ResourceName initRsc, 
    			        Boolean multiPane,
    			        NcDisplayType dispType,
    			        int style ) {
    	
        replaceBtnVisible =replaceVisible;
    	replaceBtnEnabled = replaceEnabled;
    	initRscName =initRsc;
    	
    	String title = ( replaceBtnVisible ? "Replace Resource" : "Add Resource" );
    	title = "Select New Resource";
    	
    	Shell parent = getParent();
    	Display display = parent.getDisplay();
    	
    	shell = new Shell( parent, style );
    	shell.setText( title );
//    	shell.setSize( 800, 520 ); // pack later

    	GridLayout mainLayout = new GridLayout(1, true);
    	mainLayout.marginHeight = 1;
    	mainLayout.marginWidth = 1;
    	
    	shell.setLayout(mainLayout);

    	Group sel_rscs_grp = new Group( shell, SWT.SHADOW_NONE );
    	sel_rscs_grp.setText("Select Resources");
    	sel_rscs_grp.setLayout( new GridLayout(1,true) );
    	
    	GridData gd = new GridData();
    	gd.grabExcessHorizontalSpace = true;
    	gd.grabExcessVerticalSpace = true;
    	gd.horizontalAlignment = SWT.FILL;
    	gd.verticalAlignment = SWT.FILL;
    	sel_rscs_grp.setLayoutData( gd );

    	try {
			sel_rsc_cntrl = new ResourceSelectionControl( sel_rscs_grp, 
					replaceBtnVisible, 
					replaceBtnEnabled, 
					initRscName, 
					multiPane,
					dispType );
		} catch (VizException e) {
			e.printStackTrace();
			close();
		} catch ( Exception e ) {
			e.printStackTrace();
			close();
		}
		
        Button can_btn = new Button( shell, SWT.PUSH );
        can_btn.setText("  Close  ");
    	gd = new GridData();
    	gd.grabExcessHorizontalSpace = false;
    	gd.grabExcessVerticalSpace = false;
    	gd.horizontalAlignment = SWT.END;
    	gd.verticalAlignment = SWT.END;
    	can_btn.setLayoutData( gd );

        can_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			shell.dispose();
       		}
        });

    	initWidgets();
    	
    	if( dlgLocation == null ) {
    		dlgLocation = new Point( parent.getLocation().x+100, 
    								 parent.getLocation().y+100 );
    	}
    	shell.setLocation( parent.getLocation().x+100, 
    					   parent.getLocation().y+100);
    	shell.setMinimumSize(500, 300);

    	shell.pack();
    	shell.open();
    	
    	while( !shell.isDisposed() ) {
    		if( !display.readAndDispatch() ) {
    			display.sleep();
    		}
    	}
    	
//    	dlgLocation = shell.getLocation();
    	
    	return null;
    }
    
    private void initWidgets() {
    	
    	for( IResourceSelectedListener lstnr : rscSelListeners ) {
    		sel_rsc_cntrl.addResourceSelectionListener( lstnr );
    	}
    }
    
    public void setSelectedResource( ResourceName rscName ) {
    	initRscName = rscName;
    	sel_rsc_cntrl.initWidgets( initRscName );
    }
    
	public void addResourceSelectionListener( IResourceSelectedListener lstnr ) {
		rscSelListeners.add( lstnr );
	}
    
	// this isn't necessary if we remove the 'Replace' (Modify) button from Create Rbd
	// and have it on the Select Resource instead.
	//
	public boolean replaceOptionEnabled() {
		return replaceEnabled;
	}
	
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }  

    public void close() {
    	shell.dispose();
    }

    public void setMultiPaneEnabled( Boolean multPaneEnable ) {
    	sel_rsc_cntrl.setMultiPaneEnabled( multPaneEnable );
    }
    
    public void setReplaceEnabled( Boolean replaceEnabled ) {
    	if( sel_rsc_cntrl != null ) 
    		sel_rsc_cntrl.setReplaceEnabled( replaceEnabled );
    }
    
    public ResourceName getPrevSelectedResource() {
    	
    	return (sel_rsc_cntrl != null ?  sel_rsc_cntrl.getPrevSelectedResource() : new ResourceName());
    }
}

