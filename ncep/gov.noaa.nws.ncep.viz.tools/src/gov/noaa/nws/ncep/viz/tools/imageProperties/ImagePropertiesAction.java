package gov.noaa.nws.ncep.viz.tools.imageProperties;

import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
//import com.raytheon.viz.ui.dialogs.ImagingDialog;

/**
 * Get imaging dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/09/2009   187         Q. Zhou     Initial. com.raytheon.viz.ui.dialog
 * 12/16/2009               G. Hull     Only one at a time.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ImagePropertiesAction extends AbstractHandler {

	private static ImagingDialog dlg = null;
	

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

    	if( dlg == null ) {
    		try {
    			dlg = new ImagingDialog( NcDisplayMngr.getCaveShell(),
    					"Image Properties" );

    			dlg.open();
    			
    		} catch ( Exception e ) {
    			System.out.println("Imaging Properties:"+ e.getMessage() );
    		}
    		finally {
    			dlg = null;
    		}
    	}

        return null;
    }
}

