package gov.noaa.nws.ncep.viz.cloudHeight.ui;


import gov.noaa.nws.ncep.viz.cloudHeight.CloudHeightProcesser;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNCModalMapTool;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
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
        
        cldHghtProcessor = new CloudHeightProcesser( seldPanes[0], cldHghtDlg );
        
        // if the dialog is already open then reset it with 
		//
        if( cldHghtDlg.isOpen() ) {         	 
    		mapEditor.registerMouseHandler( this.mouseHndlr );
    		return;
    	}

        try {
        	if( !cldHghtDlg.isOpen() ) {

        		if( mouseHndlr == null ) {
        			mouseHndlr = new MouseHandler();
        		}
        		mapEditor.registerMouseHandler( this.mouseHndlr );

        		cldHghtDlg.open();
        		cldHghtDlg = null;

        		deactivateTool();
        	}
        } catch( Exception ex ) {
        	System.out.println("Exception from CloudHeightDialog: "+ex.getMessage() );
        }
        finally {
        	cldHghtDlg = null;
        }
        cldHghtProcessor = null;
        
		return;
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
        if ( cldHghtDlg != null ) cldHghtDlg.close();
        
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
    			cldHghtProcessor.processCloudHeight( ll, true );
    		}

    		return false;
    	}

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         *      int, int)
         */
    	@Override
    	public boolean handleMouseDownMove(int x, int y, int button) {
    		if( button == 1 ) {
    			Coordinate ll = mapEditor.translateClick(x, y);
    			cldHghtProcessor.processCloudHeight( ll, false );
    		}

    		return false;
    	}

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         *      int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int button) {
    		if( button == 1 ) {
    			Coordinate ll = mapEditor.translateClick(x, y);
    			cldHghtProcessor.processCloudHeight( ll, false );
    		}

    		return true;
        }

    }   
}