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
package com.raytheon.uf.viz.personalities.cave.workbench;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.application.component.IStandaloneComponent;
import com.raytheon.uf.viz.core.VizApp;

/**
 * {@link WorkbenchAdvisor} to use which allows for the workbench to be started
 * but not visible. This is usedby {@link IStandaloneComponent}s that need the
 * workbench to start but do not utilize it
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class HiddenWorkbenchAdvisor extends WorkbenchAdvisor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HiddenWorkbenchAdvisor.class);

    private IStandaloneComponent component;

    private String componentName;

    public HiddenWorkbenchAdvisor(String componentName,
            IStandaloneComponent component) {
        this.component = component;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.application.WorkbenchAdvisor#getInitialWindowPerspectiveId
     * ()
     */
    @Override
    public String getInitialWindowPerspectiveId() {
        return null;
    }

    @Override
    public WorkbenchWindowAdvisor createWorkbenchWindowAdvisor(
            IWorkbenchWindowConfigurer configurer) {
        return new HiddenWorkbenchWindowAdvisor(configurer);
    }

    @Override
    public boolean openWindows() {
        boolean rval = super.openWindows();
        VizApp.runAsync(new Runnable() {
            public void run() {
                try {
                    component.startComponent(componentName);
                    PlatformUI.getWorkbench().close();
                } catch (Exception e) {
                    statusHandler.handle(Priority.CRITICAL,
                            "error running component", e);
                }
            }
        });
        return rval;
    }
}
