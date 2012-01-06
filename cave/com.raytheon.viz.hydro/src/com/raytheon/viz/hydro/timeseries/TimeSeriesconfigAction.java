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

/**
 * Action for Time Series Configuration Plug-in
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jan 24, 2011	7797		bkowal	    Initial creation.
 * 
 * </pre>
 * 
 * @author bkowal
 * 
 */
public class TimeSeriesconfigAction extends AbstractHandler {
    private TimeSeriesDlg timeSeriesDialog;

    private static final String ENV_WHFS_CONFIG_DIR = "whfs_config_dir";

    private static final String GROUP_DEFINITION_FILE_NAME = "group_definition.cfg";

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
        String configDir = System
                .getenv(TimeSeriesconfigAction.ENV_WHFS_CONFIG_DIR);
        if (!configDir.endsWith("/")) {
            configDir = configDir + "/";
        }
        File file = new File(configDir
                + TimeSeriesconfigAction.GROUP_DEFINITION_FILE_NAME);

        return file;
    }
}
