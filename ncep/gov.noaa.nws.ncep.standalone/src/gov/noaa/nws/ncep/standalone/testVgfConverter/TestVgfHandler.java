package gov.noaa.nws.ncep.standalone.testVgfConverter;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * TestVgfHandler
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 6/21/2010   271         Q. Zhou     Initial created
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class TestVgfHandler extends AbstractHandler  {
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		TestVgfDialog cd = new TestVgfDialog(HandlerUtil.getActiveWorkbenchWindow(event).getShell());
		cd.open();
		
		return null;
	}
}


