package gov.noaa.nws.ncep.standalone.vgfConverter;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * ColorMapConvert
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/30/2009   203         Q. Zhou     Initial created
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class VgfConvertHandler extends AbstractHandler { //implements IHandler {
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		VgfConvertDialog cd = new VgfConvertDialog(HandlerUtil.getActiveWorkbenchWindow(event).getShell());
		cd.open();
		
		return null;
	}
}
