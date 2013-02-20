/**
 * 
 */
package com.raytheon.viz.hydro.timeseries;

import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Action for Time Series Configuration Plug-in
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 24, 2011	7797		bkowal	    Initial creation.
 * Jan 30, 2013 15264       wkwock      Get the correct group_definition.cfg file.
 * 
 * </pre>
 * 
 * @author bkowal
 * 
 */
public class TimeSeriesconfigAction extends AbstractHandler {
    private TimeSeriesDlg timeSeriesDialog;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        if (this.timeSeriesDialog == null || this.timeSeriesDialog.isDisposed()) {
            this.timeSeriesDialog = new TimeSeriesDlg(shell,
                    locateGroupDefinitionFile());
            this.timeSeriesDialog.open();
            if (this.timeSeriesDialog != null && this.timeSeriesDialog.isOpen()) {
                this.timeSeriesDialog.disposeDialogTS();
            }
            this.timeSeriesDialog = null;
        } else {
            this.timeSeriesDialog.setFocus();
        }

        return null;
    }

    protected static File locateGroupDefinitionFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        File file = pm.getStaticFile(HydroConstants.GROUP_DEFINITION);

        return file;
    }
}
