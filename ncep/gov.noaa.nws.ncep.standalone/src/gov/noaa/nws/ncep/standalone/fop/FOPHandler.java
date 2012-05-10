/*
 * gov.noaa.nws.ncep.standalone.fop.FOPHandler
 * 
 * Date created (as Jan 29, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.fop;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * Flood Outlook Product (FOP) Handler
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer      Description
 * ------------ ----------  ------------  --------------------------
 * 01/29/2010   220         mlaryukhin    Initial created
 * 
 * </pre>
 * 
 * @author mlaryukhin
 */
public class FOPHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		FOPDialog dialog = new FOPDialog(HandlerUtil.getActiveWorkbenchWindow(event).getShell());
		dialog.open();

		return null;
	}
}
