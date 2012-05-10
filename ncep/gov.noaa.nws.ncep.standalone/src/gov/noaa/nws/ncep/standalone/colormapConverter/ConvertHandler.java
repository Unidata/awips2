package gov.noaa.nws.ncep.standalone.colormapConverter;

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
 * 11/30/2009   197         Q. Zhou     Initial created
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class ConvertHandler extends AbstractHandler { //implements IHandler {
	
	//static ConvertDialog cd = null;
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		ConvertDialog cd = new ConvertDialog(HandlerUtil.getActiveWorkbenchWindow(event).getShell());
		cd.open();
//		// TODO Auto-generated method stub	
//		if( cd != null ) {
//    		return null;
//    	}
//		
//			cd = new ConvertDialog(HandlerUtil.getActiveWorkbenchWindow(event).getShell());
//          cd.open();
//          cd = null;
			
		return null;
	}
}
