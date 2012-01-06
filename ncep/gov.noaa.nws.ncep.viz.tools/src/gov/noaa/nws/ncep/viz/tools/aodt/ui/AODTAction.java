package gov.noaa.nws.ncep.viz.tools.aodt.ui;


import gov.noaa.nws.ncep.viz.ui.display.AbstractNCModalMapTool;
import gov.noaa.nws.ncep.viz.tools.aodt.AODTProcesser;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.input.InputAdapter;
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
	
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    protected void activateTool() {
    	
    	mapEditor = NmapUiUtils.getActiveNatlCntrsEditor();
    	
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

        if( aodtDlg == null ) {
        	
        	String aodtVersion = null;
        	try {
        		aodtVersion = event.getCommand().getName();
			} catch (NotDefinedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			aodtDlg = new AODTDialog( shell, aodtVersion);
        }

        aodtProcessor = new AODTProcesser( aodtDlg );
        
        if( !aodtDlg.isOpen() ) {

        	if( mouseHndlr == null ) {
                mouseHndlr = new MouseHandler();
            }
            mapEditor.registerMouseHandler( this.mouseHndlr );
            
        	aodtDlg.open();
        	aodtDlg = null;

        	deactivateTool();
        }

        aodtProcessor = null;
        
		return;
    }
    
    /*
     * (non-Javadoc)
     * org.osgi.framework.BundleContext
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
        if ( aodtDlg != null ) aodtDlg.close();
        
    }
    
    public class MouseHandler extends InputAdapter {
    	/*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
    	@Override
    	public boolean handleMouseDown(int x, int y, int button) {
    		if( button == 1 ) {
    			Coordinate ll = mapEditor.translateClick(x, y);
    			aodtProcessor.processAODT( ll );
    		}

    		return false;
    	}
    }   
}