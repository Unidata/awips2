/*
 * gov.noaa.nws.ncep.viz.ui.remotescript.RemoteScripDialogtHandler
 * 
 * March 2014
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.ui.remotescript;

import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.remotescript.dialog.RemoteScriptDialog;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

/**
 * Handler to pop up the remote script window in CAVE.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/14        #?          B. Yin       Initial creation.
 * 
 * </pre>
 * 
 * @author byin
 * @version 1.0
 * 
 */

public class RemoteScriptDialogHandler extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        RemoteScriptDialog dlg = RemoteScriptDialog.getInstance(NcDisplayMngr
                .getCaveShell());
        dlg.open();
        return null;

    }

}
