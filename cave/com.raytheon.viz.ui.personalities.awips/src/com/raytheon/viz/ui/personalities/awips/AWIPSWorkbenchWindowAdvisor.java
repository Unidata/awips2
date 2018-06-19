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
package com.raytheon.viz.ui.personalities.awips;

import org.eclipse.e4.ui.model.application.ui.MUIElement;
import org.eclipse.e4.ui.model.application.ui.basic.MWindow;
import org.eclipse.e4.ui.workbench.modeling.EModelService;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.services.IServiceLocator;

import com.raytheon.uf.viz.personalities.cave.workbench.VizWorkbenchWindowAdvisor;
import com.raytheon.uf.viz.ui.menus.widgets.tearoff.TearOffMenuListener;
import com.raytheon.viz.ui.statusline.VizActionBarAdvisor;

/**
 * AWIPS window advisor, doesn't show perspective bar when -perspective argument
 * is used to launch the application
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 15, 2013           mschenke  Initial creation
 * Sep 23, 2016  5897     bsteffen  Disable rendering of perspective switcher
 *                                  when in single perspective mode.
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class AWIPSWorkbenchWindowAdvisor extends VizWorkbenchWindowAdvisor {

    private boolean singlePerspective;

    public AWIPSWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer,
            boolean singlePerspective) {
        super(configurer);
        this.singlePerspective = singlePerspective;
    }

    @Override
    public void preWindowOpen() {
        super.preWindowOpen();
        IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
        if (singlePerspective) {
            /*
             * In Eclipse 4.5.1 there seems to be a bug in
             * WorkbenchWindow.populateTopTrimContributions(). When the model
             * already contains a perspective switcher that is not supposed to
             * be there then the switcher is removed from the model but the
             * renderer does not remove the UI so the user still sees the
             * switcher. This block will attempt to find an existing switcher
             * and disable rendering.
             */
            IServiceLocator services = configurer.getWindow();
            MWindow window = services.getService(MWindow.class);
            EModelService modelService = services
                    .getService(EModelService.class);
            MUIElement switcherControl = modelService
                    .find("PerspectiveSwitcher", window);
            if (switcherControl != null) {
                switcherControl.setToBeRendered(false);
            }

        }
        configurer.setShowPerspectiveBar(!singlePerspective);
    }

    @Override
    public void postWindowOpen() {
        super.postWindowOpen();
        new TearOffMenuListener(VizActionBarAdvisor.getInstance(
                getWindowConfigurer().getWindow()).getMenuManager());
    }

}
