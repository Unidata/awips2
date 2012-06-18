/*
 * gov.noaa.nws.ncep.standalone.rfop.RFOPHandler
 * 
 * Date created (as Feb 01, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.rfop;

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
 * 
 * </pre>
 * 
 * @author mlaryukhin
 */
public class RFOPHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		RFOPDialog dialog = new RFOPDialog(HandlerUtil.getActiveWorkbenchWindow(event).getShell());
		dialog.open();

		return null;
	}
}
