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
package com.raytheon.uf.viz.alertviz.ui;

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.alertviz.AlertService;
import com.raytheon.uf.viz.alertviz.ui.dialogs.AlertVisualization;

/**
 * Class consumes {@link AlertService} service and starts the
 * {@link AlertVisualization} UI and stops when the service shut down
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2015             mschenke    Initial creation
 * Jun 3, 2015   4473      njensen     Hooked up embedded and exit status
 *
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AlertVizUILauncher {

    private AlertService service;

    private AlertVisualization alertViz;

    public void setAlertService(AlertService service) {
        if (this.service == null) {
            this.service = service;
            Display display = Display.getDefault();
            display.syncExec(new Runnable() {
                @Override
                public void run() {
                    AlertVizUILauncher.this.alertViz = new AlertVisualization(
                            !AlertVizUILauncher.this.service.isEmbedded(),
                            Display.getCurrent());
                }
            });
        }
    }

    public void unsetAlertService(AlertService service) {
        if (this.service == service) {
            this.service.setExitStatus(this.alertViz.getExitStatus());
            this.service = null;

            Display display = Display.getDefault();
            display.asyncExec(new Runnable() {
                @Override
                public void run() {
                    AlertVizUILauncher.this.alertViz.dispose();
                }
            });
        }
    }

}
