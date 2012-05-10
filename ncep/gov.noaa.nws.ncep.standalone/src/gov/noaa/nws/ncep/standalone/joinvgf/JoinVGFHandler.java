/*
 * gov.noaa.nws.ncep.standalone.clipvgf.ClipVGFHandler
 * 
 * Date created (as Jan 12, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.joinvgf;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * ClipVGF Handler
 * 
 * @author mlaryukhin
 */
public class JoinVGFHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		JoinVGFDialog dialog = new JoinVGFDialog(HandlerUtil.getActiveWorkbenchWindow(event).getShell());
		dialog.open();

		return null;
	}
}
