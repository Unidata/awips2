package gov.noaa.nws.ncep.viz.tools.colorMapEditor;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;


public class ColormapEditAction extends AbstractHandler {

	ColormapEditDialog ced = null;

	public Object execute(ExecutionEvent arg0) throws ExecutionException {
		if( ced != null ) {
			return null;
		}
		try {
			Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			ColormapEditDialog ced = new ColormapEditDialog(shell, null);
			ced.open();
			ced = null;
		} catch (Exception e) {
			throw new ExecutionException(
					"ColorMap Editor Error", e);
		}
		finally {
			ced = null;
		}
		return null;
	}
	
}
