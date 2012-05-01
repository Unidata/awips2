package gov.noaa.nws.ncep.viz.resourceManager.ui;

import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.CreateRbdControl;
import gov.noaa.nws.ncep.viz.resourceManager.ui.loadRbd.LoadRbdControl;
import gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources.ManageResourceControl;
import gov.noaa.nws.ncep.viz.resourceManager.ui.spf.ManageMultiSPFControl;
import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;


/**
 *  Main Dialog to manage and load RBDs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/25/10	      #226		Greg Hull	 Initial creation
 * 06/10/10       #273      Greg Hull    Added ManageResource tab
 * 08/18/10       #273      Greg Hull    use mode to set the initial tab
 * 12/17/10       #365      Greg Hull    update Manage Control
 * 01/26/11                 Greg Hull    don't set the dialog size.
 * 02/16/11       #408      Greg Hull    Change shell to Modeless and have hotkey 
 *                                       bring to the front.
 * 11/07/11                 Chin Chen    fixed a null pointer exception bug                                      
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class ResourceManagerDialog extends Dialog {

	private RscBundleDisplayMngr rbd_mngr;
	
    private static Shell  shell;
    private String dlgTitle;
    private static boolean isOpen = false;

	private TabFolder mngrTabFolder = null;

    protected Control activeMngr = null;
    
    protected  CreateRbdControl createRbdCntrl = null;
    
    protected  LoadRbdControl loadRbdCntrl = null;

    protected  ManageResourceControl manageRscCntrl = null;

    protected ManageMultiSPFControl manageMultiSPFControl; 
    
    private Point prevSize = new Point( 750, 860 );
    private Point prevLocation = new Point(0,0);
    
    public ResourceManagerDialog(Shell parShell, String title,
    		          RscBundleDisplayMngr mngr, String mode )   throws VizException {
    	super(parShell);
    	rbd_mngr = mngr;
    	this.dlgTitle = title;

    	shell = new Shell( SWT.SHELL_TRIM | SWT.MODELESS );
    	shell.setText( dlgTitle);

    	GridLayout mainLayout = new GridLayout(1, true);
    	mainLayout.marginHeight = 1;
    	mainLayout.marginWidth = 1;
    	shell.setLayout(mainLayout);
    	prevLocation = new Point( parShell.getLocation().x, 0);
//    	shell.setLocation( parShell.getLocation().x, 0);
    	    	
    	mngrTabFolder = new TabFolder( shell, SWT.NONE );
    	GridData gd = new GridData();
    	gd.grabExcessHorizontalSpace = true;
    	gd.grabExcessVerticalSpace = true;
    	gd.horizontalAlignment = SWT.FILL;
    	gd.verticalAlignment = SWT.FILL;
    	mngrTabFolder.setLayoutData( gd );

    	final TabItem loadTabItem = new TabItem( mngrTabFolder, SWT.NONE );
    	loadTabItem.setText( "   Load RBD   " );

    	loadRbdCntrl = new LoadRbdControl( mngrTabFolder );

    	final TabItem mngrTabItem = new TabItem( mngrTabFolder, SWT.NONE );
    	mngrTabItem.setText( "   Create RBD   " );

    	rbd_mngr.init();
    	
    	// get the active Display and set the rbd_mngr with it
   		NCMapEditor currEditor = NmapUiUtils.getActiveNatlCntrsEditor();

		if( currEditor != null ) { 
			RbdBundle rbdBndl = new RbdBundle();
//			rbdBndl.setNcEditor( currEditor );
			rbdBndl.initFromEditor(currEditor);

			rbd_mngr.initFromRbdBundle( rbdBndl );
		}        

    	createRbdCntrl = new CreateRbdControl( mngrTabFolder, rbd_mngr );
    	
    	final TabItem cnfgTabItem = new TabItem( mngrTabFolder, SWT.NONE );
    	cnfgTabItem.setText( "   Manage Resources  " );

    	manageRscCntrl = new ManageResourceControl( mngrTabFolder );

    	final TabItem manageSPFTabItem = new TabItem( mngrTabFolder, SWT.NONE );
    	manageSPFTabItem.setText( "   Manage SPFs  " );

    	manageMultiSPFControl = new ManageMultiSPFControl( mngrTabFolder );

    	mngrTabItem.setControl( createRbdCntrl );
    	loadTabItem.setControl( loadRbdCntrl );
    	cnfgTabItem.setControl( manageRscCntrl );
    	manageSPFTabItem.setControl( manageMultiSPFControl );

    	
    	Button closeBtn = new Button( shell, SWT.PUSH );
    	closeBtn.setText( "    Close    " );
    	gd = new GridData( );
    	gd.horizontalAlignment = SWT.END;
    	gd.verticalAlignment = SWT.END;
    	closeBtn.setLayoutData( gd );
    	closeBtn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			close();
       		}
        });
    	
    	shell.setMinimumSize(600,550);
    	
    	mngrTabFolder.layout();    	

    	shell.pack();

    	if( mode == null || mode.equals("LOAD") ) {
    		mngrTabFolder.setSelection(0);
    		loadRbdCntrl.updateDialog();
    	}
    	else if( mode.equals("CREATE") ) {
    		mngrTabFolder.setSelection(1);
    		createRbdCntrl.updateDialog();
    	}
    	else if( mode.equals("MANAGE") ) {
    		mngrTabFolder.setSelection(2);
		    manageRscCntrl.updateDialog();
    	}
    	else if( mode.equals("MULTI-SPF") ) {
    		mngrTabFolder.setSelection(3);
//    		manageMultiSPFControl.updateDialog();
    	}
    	else {
    		mngrTabFolder.setSelection(0);
    	}
    	
    	// 
    	mngrTabFolder.addSelectionListener( new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			TabItem[] seldTab = mngrTabFolder.getSelection();
       			
       			if( seldTab[0].getControl() instanceof LoadRbdControl ) {       				
       				((LoadRbdControl)seldTab[0].getControl()).updateDialog();
       			}
       			else if( seldTab[0].getControl() instanceof CreateRbdControl ) {       				
       				((CreateRbdControl)seldTab[0].getControl()).updateDialog();
       			}
       			else if( seldTab[0].getControl() instanceof ManageResourceControl ) {       				
       				((ManageResourceControl)seldTab[0].getControl()).updateDialog();
       			}

       		}
        });    	
    }
    
    
   	public boolean isOpen() {
        return isOpen; // shell != null && !shell.isDisposed();
    }

    public Object open() {
    	Shell parent = getParent();
    	Display display = parent.getDisplay();

    	shell.setSize( prevSize );
    	shell.setLocation( prevLocation );
    	shell.open();
    	
    	isOpen = true;
    	
//    	AbstractVizPerspectiveManager pMngr = VizPerspectiveListener.getInstance().getActivePerspectiveManager();
//
//    	if( pMngr instanceof NCPerspectiveManager ) {
//    		
//    	}
    	
    	while( !shell.isDisposed() ) {
    		if( !display.readAndDispatch() ) {
    			display.sleep();
    		}
    	}

//    	prevSize = shell.getSize(); 
//    	prevLocation = shell.getLocation();
    	
    	isOpen = false;
    	
    	return null;
    }
    
    
    public static void close() {
    	// if there is a preview editor up then close it
    	if(shell!=null){
    		if( !shell.isDisposed() ) {
    			shell.dispose();	
    		}
    	}
    	isOpen = false;
    }
    
    public void setActiveTab( String mode ) {
    	shell.setFocus();
    	shell.setActive();
    	shell.setMaximized(false); // will cause the dialog to be brought to the front. 
    	
    	if( mode == null ) {
    		return;
    	}    	
    	if( mode.equals("LOAD") ) {
    		mngrTabFolder.setSelection(0);
    	}
    	else if( mode.equals("CREATE") ) {
    		mngrTabFolder.setSelection(1);
    	}
    }
//    public void updateManager( ) {
//    	loadRbdCntrl.updateDialog();
//    }


}