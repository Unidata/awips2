package gov.noaa.nws.ncep.viz.cloudHeight.ui;


import gov.noaa.nws.ncep.viz.cloudHeight.CloudHeightProcesser;
import gov.noaa.nws.ncep.viz.rsc.satellite.rsc.ICloudHeightCapable;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNCModalMapTool;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.vividsolutions.jts.geom.Coordinate;


/**
 * Cloud Height Dialog
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/30/09					Greg Hull		Created
 * 09/27/09      #169	    Greg Hull     AbstractNCModalMapTool
 * 03/07/11     migration   Greg Hull     use Raytheons ISelectedPanesChangedListener
 * 03/01/12     524/TTR11   B. Hebbard    Various changes to allow mutual operation of
 *                                        'Take Control' button with other Modal Tools
 * 06/01/12		747			B. Yin		  Made the pan tool work when the shift is held down.
 * 06/21/12     826         Archana       Updated the activateTool() method to remove the cloudheight tool from the 
 *                                        tool manager when there is no IR image loaded. 
 *                                        Instead, the default Pan tool is loaded. 
 *                                        
 * 
 * </pre>
 * 
 * @version 1
 */
public class CloudHeightAction extends AbstractNCModalMapTool {

	protected IInputHandler mouseHndlr = null;
	
	protected static CloudHeightDialog cldHghtDlg = null;
	private CloudHeightProcesser cldHghtProcessor = null;
	private ICloudHeightCapable satResource = null;
	
	ISelectedPanesChangedListener paneChangeListener = new ISelectedPanesChangedListener() {

		@Override
		public void selectedPanesChanged(String id, IDisplayPane[] seldPanes) {
			if( !id.equals( NCPaneManager.NC_PANE_SELECT_ACTION ) ) {
				return;
			}
			
			// NOTE: can only use cloud height on one pane at a time.
			//
			if( cldHghtProcessor != null && seldPanes != null && seldPanes.length > 0 ) {
				if( seldPanes[0] instanceof NCDisplayPane ) {
					cldHghtProcessor.setPane( (NCDisplayPane) seldPanes[0] );
				}
			}			
		}
	};

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    protected void activateTool() {

		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		if( cldHghtDlg == null ) {
			cldHghtDlg = CloudHeightDialog.getInstance(shell, this);
		}
    	
    	mapEditor = NmapUiUtils.getActiveNatlCntrsEditor();
    	mapEditor.addSelectedPaneChangedListener( paneChangeListener );
    	
    	/*
         * Register mouse handler. 
         */
		if( mouseHndlr == null ) {
			mouseHndlr = new MouseHandler();
		}
		if (mapEditor != null) {
			mapEditor.registerMouseHandler( this.mouseHndlr, InputPriority.LOWEST );
		}

        NCDisplayPane[] seldPanes = (NCDisplayPane[]) mapEditor.getSelectedPanes();
        if( seldPanes.length > 1 ) {
        	System.out.println("Cloud Height will only work on one selected pane.");
        	//? return;
        }
        
    	try {
			cldHghtProcessor = new CloudHeightProcesser( seldPanes[0], cldHghtDlg );
		} catch (VizException e) {
        	System.out.println("Exception from CloudHeightProcessor: "+e.getMessage() );
    		MessageDialog errDlg = new MessageDialog(
    				NmapUiUtils.getCaveShell(), 
    				"Error Starting Cloud Height (Processor):\n"+e.getMessage(), null, 
    				"\n",
			MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
			return;
		}

		satResource = cldHghtProcessor.getSatResource();
		if ( satResource == null ) {
			issueAlert();
			AbstractVizPerspectiveManager mgr = VizPerspectiveListener
			.getCurrentPerspectiveManager();
			if (mgr != null) {
				mgr.getToolManager().deselectModalTool(this);
                NmapUiUtils.setPanningMode();

			}
			return;
		}
		
		/*
		 * Pop up Cloud Height result window (if not already open)
		 */
		try {
			if( !cldHghtDlg.isOpen() ) {
				cldHghtDlg.open();
			}
		} catch( Exception ex ) {
			System.out.println("Exception from CloudHeightDialog: "+ex.getMessage() );
			MessageDialog errDlg = new MessageDialog( 
					NmapUiUtils.getCaveShell(), 
					"Error Starting Cloud Height (Dialog):\n"+ex.getMessage(), null, 
					"\n",
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();			
		}
		
		/*
		 * Note that CloudHeightDialog.open() will, after the dialog is closed,
		 * call ModalToolManager.deselectModalTool(...) to shut things down, which
		 * in turn eventually calls back to deactivateTool() here.
		 */
    }
    
    private void issueAlert() {
		
		String msg = "Unable to invoke CLOUD HEIGHT tool.\nPlease load an IR Satellite image!";
    	MessageDialog messageDlg = new MessageDialog( 
        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
        		"Warning", null, msg,
        		MessageDialog.WARNING, new String[]{"OK"}, 0);
        messageDlg.open();

	}
  
    // TODO : make cloud height work as a modal tool 
    @Override
    public void setEnabled( boolean enable ) {
    	super.setEnabled( enable );
    	if( isEnabled() ) {
//    		cldHghtProcessor.setMapEditor( editor );
    	}
    }
    
    /*
     * (non-Javadoc)
     * org.osgi.framework.BundleContext
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
    	if( mapEditor != null ) {
    		
    		mapEditor.removeSelectedPaneChangedListener( paneChangeListener );
    		
    		if( mouseHndlr != null ) {
    			mapEditor.unregisterMouseHandler( mouseHndlr );
    		}
        }
    		
    	if( cldHghtProcessor != null ) {
    		cldHghtProcessor.close();
    	}        
    }
    
    public class MouseHandler extends InputAdapter {
    	
    	boolean preempt = false;
    	private boolean shiftDown;

    	/*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
    	@Override
    	public boolean handleMouseDown(int x, int y, int button) {
    		
    		preempt = false;
    		
    		if ( shiftDown ) return false;
    		
    		if( button == 1 ) {
    			Coordinate ll = mapEditor.translateClick(x, y);
    			if ( ll == null || satResource == null ) return false;
    			
        		Double value = satResource.getSatIRTemperature(ll);
        		if ( value != null && !value.isNaN() ) {
    				cldHghtProcessor.processCloudHeight( ll, true );
    				preempt = false;
        		} 
    			
    		}

    		return preempt;
    	}

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         *      int, int)
         */
    	@Override
    	public boolean handleMouseDownMove(int x, int y, int button) {
    		return preempt;
    	}
        
    	@Override
    	public boolean handleKeyDown(int keyCode) {
    		if ( keyCode == SWT.SHIFT) {
    			shiftDown = true;
    		}

    		return true;
    	}

    	@Override
    	public boolean handleKeyUp(int keyCode) {
    		if ( keyCode == SWT.SHIFT) {
    			shiftDown = false;
    		}    		

    		return true;
    	}
    	
    }   

	public String getCommandId() {
		return commandId;
	}
}