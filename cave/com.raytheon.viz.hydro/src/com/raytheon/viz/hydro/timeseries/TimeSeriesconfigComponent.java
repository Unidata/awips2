/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.hydro.timeseries;

import java.io.File;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.personalities.awips.AbstractCAVEDialogComponent;

/**
 * Class to create a stand alone Time Series Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2011            mschenke     Initial creation
 * Feb 05, 2013 1578       rferrel     Changes for non-blocking singleton TimeSeriesDlg.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TimeSeriesconfigComponent extends AbstractCAVEDialogComponent {

    private static final String ENV_WHFS_CONFIG_DIR = "whfs_config_dir";

    private static final String GROUP_DEFINITION_FILE_NAME = "group_definition.cfg";

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#startInternal
     * (java.lang.String)
     */
    @Override
    protected void startInternal(String componentName) throws Exception {
        TimeSeriesDlg timeSeriesDialog = new TimeSeriesDlg(new Shell(
                Display.getCurrent()),
                TimeSeriesconfigComponent.locateGroupDefinitionFile());
        timeSeriesDialog.open();
        blockUntilClosed(timeSeriesDialog);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#getRuntimeModes
     * ()
     */
    @Override
    protected int getRuntimeModes() {
        return ALERT_VIZ;
    }

    protected static File locateGroupDefinitionFile() {
        String configDir = System
                .getenv(TimeSeriesconfigComponent.ENV_WHFS_CONFIG_DIR);
        if (configDir == null) {
            configDir = "";
        }

        if (!configDir.endsWith("/")) {
            configDir = configDir + "/";
        }
        File file = new File(configDir
                + TimeSeriesconfigComponent.GROUP_DEFINITION_FILE_NAME);

        return file;
    }

}
