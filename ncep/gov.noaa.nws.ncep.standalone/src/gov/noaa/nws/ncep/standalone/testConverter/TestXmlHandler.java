package gov.noaa.nws.ncep.standalone.testConverter;

import gov.noaa.nws.ncep.standalone.vgfConverter.VgfConvertDialog;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * TestXmlHandler
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/21/2010   271         Q. Zhou     Initial created
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class TestXmlHandler extends AbstractHandler  {
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		TestXmlDialog cd = new TestXmlDialog(HandlerUtil.getActiveWorkbenchWindow(event).getShell());
		cd.open();
		
		return null;
	}
}

