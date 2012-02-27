package gov.noaa.nws.ncep.viz.cloudHeight.ui;


import gov.noaa.nws.ncep.viz.cloudHeight.CloudHeightProcesser;
import gov.noaa.nws.ncep.viz.rsc.satellite.rsc.ICloudHeightCapable;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNCModalMapTool;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
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
    	
    	mapEditor = NmapUiUtils.getActiveNatlCntrsEditor();
    	
    	mapEditor.addSelectedPaneChangedListener( paneChangeListener );

    	
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

        if( cldHghtDlg == null ) {
        	cldHghtDlg = new CloudHeightDialog( shell, "Cloud Height");
        }

        NCDisplayPane[] seldPanes = (NCDisplayPane[]) mapEditor.getSelectedPanes();
        
        if( seldPanes.length > 1 ) {
        	System.out.println("Cloud Height will only work on one selected pane.");
        }
        
        try {
        	cldHghtProcessor = new CloudHeightProcesser( seldPanes[0], cldHghtDlg );
        	satResource = cldHghtProcessor.getSatResource();

        	// if the dialog is already open then reset it with 
        	//
        	if( cldHghtDlg.isOpen() ) {         	 
        		mapEditor.registerMouseHandler( this.mouseHndlr );
        		return;
        	}

        	if( !cldHghtDlg.isOpen() ) {

        		if( mouseHndlr == null ) {
        			mouseHndlr = new MouseHandler();
        		}
        		mapEditor.registerMouseHandler( this.mouseHndlr );

        		if ( satResource != null )
        			cldHghtDlg.open();
        		else
        			issueAlert();
        		
        		cldHghtDlg = null;

        		//deactivateTool();
        		AbstractVizPerspectiveManager mgr = VizPerspectiveListener.getCurrentPerspectiveManager();
        		if (mgr != null) {
        			mgr.getToolManager().deselectModalTool(this);
        		}
        		
        	}
        } catch( Exception ex ) {
        	System.out.println("Exception from CloudHeightDialog: "+ex.getMessage() );
    		MessageDialog errDlg = new MessageDialog( 
    				NmapUiUtils.getCaveShell(), 
    				"Error Starting Cloud Height:\n"+ex.getMessage(), null, 
    				"\n",
			MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();			
        }
        finally {
        	cldHghtDlg = null;
        }
        cldHghtProcessor = null;
        
		return;
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
    	
        //  close the Cloud Height dialog
        if ( cldHghtDlg != null ) {
        	cldHghtDlg.close();
        	cldHghtDlg = null;
        }
        
    }
    
    public class MouseHandler extends InputAdapter {
    	
    	boolean preempt = false;
    	
    	/*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
    	@Override
    	public boolean handleMouseDown(int x, int y, int button) {
    		
    		preempt = false;
    		
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

    }   
}