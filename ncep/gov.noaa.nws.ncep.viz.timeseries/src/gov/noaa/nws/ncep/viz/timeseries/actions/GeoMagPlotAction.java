package gov.noaa.nws.ncep.viz.timeseries.actions;

import gov.noaa.nws.ncep.viz.timeseries.GeoMagPlotDialog;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/**
 * Popup Logos display controls dialog in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 2009  	105        M. Li    	Initial creation. 
 * 09/09        #169       G. Hull      NCMapEditor
 * 02/11/13      #972        G. Hull     AbstractEditor instead of NCMapEditor
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */
public class GeoMagPlotAction extends AbstractHandler {

    protected GeoMagPlotDialog dialog;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        // AbstractEditor mapEditor = NcDisplayMngr.getActiveNatlCntrsEditor();

        /*
         * Pop up Dialog
         */
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            dialog = new GeoMagPlotDialog(shell);
            dialog.setBlockOnOpen(false);
            dialog.open();
        } else {
            dialog.bringToTop();
        }

        return null;
    }

}
