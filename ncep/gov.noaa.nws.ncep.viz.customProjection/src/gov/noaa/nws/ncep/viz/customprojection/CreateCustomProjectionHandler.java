package gov.noaa.nws.ncep.viz.customprojection;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;


import com.raytheon.viz.ui.tools.AbstractTool;

public class CreateCustomProjectionHandler extends AbstractTool implements IHandler {

	private CreateCustomProjectionDialog dialog; 
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        if(dialog == null)
        	dialog = new CreateCustomProjectionDialog(shell);

        dialog.open();

        return null;
    }

}
