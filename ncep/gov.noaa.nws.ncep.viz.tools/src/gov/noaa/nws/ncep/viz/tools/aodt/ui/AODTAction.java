package gov.noaa.nws.ncep.viz.tools.aodt.ui;

import gov.noaa.nws.ncep.viz.gempak.nativelib.LibraryLoader;
import gov.noaa.nws.ncep.viz.rsc.satellite.rsc.ICloudHeightCapable;
import gov.noaa.nws.ncep.viz.tools.aodt.AODTProcesser;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNCModalMapTool;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
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
 * 09/30/09					M. Li		Created
 * 10/05/09      169        Greg Hull   integrate with NCMapEditor,
 *                                      AbstractNCModalMapTool and InputHandlerDefaultImpl
 * 
 * </pre>
 * 
 * @version 1
 */
public class AODTAction extends AbstractNCModalMapTool {

	protected IInputHandler mouseHndlr = null;
	
	protected static AODTDialog aodtDlg = null;

	private AODTProcesser aodtProcessor = null;

	private ICloudHeightCapable satResource = null;
	
    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    protected void activateTool() {
        LibraryLoader.load("aodtv64");
    	
    	mapEditor = NmapUiUtils.getActiveNatlCntrsEditor();
    	
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        if( aodtDlg == null ) {
        	
        	String aodtVersion = null;
        	try {
        		aodtVersion = event.getCommand().getName();
			} catch (NotDefinedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			aodtDlg = new AODTDialog( shell, aodtVersion);
			satResource = aodtDlg.getSatResource();
        }

        aodtProcessor = new AODTProcesser( aodtDlg );
        
        if( !aodtDlg.isOpen() ) {

        	if( mouseHndlr == null ) {
                mouseHndlr = new MouseHandler();
            }
            mapEditor.registerMouseHandler( this.mouseHndlr );
            
            if ( satResource != null )
            	aodtDlg.open();
            else
            	issueAlert();
        	
        	aodtDlg = null;

        	//deactivateTool();
            AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                    .getCurrentPerspectiveManager();
    		if (mgr != null) {
    			mgr.getToolManager().deselectModalTool(this);
    		}
        }

        aodtProcessor = null;
        
		return;
    }
    
    private void issueAlert() {
		
		String msg = "Unable to invoke AODT tool.\nPlease load an IR Satellite image!";
        MessageDialog messageDlg = new MessageDialog(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell(), "Warning", null, msg,
        		MessageDialog.WARNING, new String[]{"OK"}, 0);
        messageDlg.open();

	}

	/*
     * (non-Javadoc) org.osgi.framework.BundleContext
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {        
    	if( mapEditor != null && mouseHndlr != null ) {
            mapEditor.unregisterMouseHandler( mouseHndlr );
        }
    	if( aodtProcessor != null ) {
    		aodtProcessor.close();
    	}
    	
        //  close the Cloud Height dialog
        if ( aodtDlg != null ) {
        	aodtDlg.close();
        	aodtDlg = null;
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
                if (ll == null || satResource == null)
                    return false;
    			
        		Double value = satResource.getSatIRTemperature(ll);
        		if ( value != null && !value.isNaN() ) {
        			aodtProcessor.processAODT( ll );
        			preempt = false;
        		}

    		}

    		return preempt;
    	}
    	
        @Override
        public boolean handleMouseDownMove(int aX, int aY, int button) {
        	return preempt;
        }
        
    }   
}