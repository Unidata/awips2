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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;

import com.raytheon.uf.viz.ui.menus.widgets.tearoff.TearOffMenuListener;
import com.raytheon.viz.ui.dialogs.ModeListener;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.raytheon.viz.ui.statusline.VizActionBarAdvisor;

/**
 * Workbench window advisor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * Oct 21, 2008   #1450     randerso    Fixed to support multipane editors
 * Mar 20, 2013    1638     mschenke    Removed call to DiscoverMenuContributions as now handled in VizWorkbenchAdvisor
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class VizWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor {

    private boolean singlePerspective;

    private VizPerspectiveListener listener;

    private boolean firstTime = true;

    /**
     * Constructor
     * 
     * @param configurer
     */
    public VizWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer,
            boolean singlePerspective) {
        super(configurer);
        this.singlePerspective = singlePerspective;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.application.WorkbenchWindowAdvisor#preWindowOpen()
     */
    @Override
    public void preWindowOpen() {
        super.preWindowOpen();
        IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
        configurer.setShowProgressIndicator(true);
        configurer.setInitialSize(new Point(1024, 768));
        // Don't show perspective bar if running a specific perspective?
        configurer.setShowPerspectiveBar(!singlePerspective);
        configurer.setShowCoolBar(true);
        configurer.setShowStatusLine(true);

        OpenPerspectiveList.getInstance(configurer.getWindow());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.application.WorkbenchWindowAdvisor#createActionBarAdvisor
     * (org.eclipse.ui.application.IActionBarConfigurer)
     */
    @Override
    public ActionBarAdvisor createActionBarAdvisor(
            IActionBarConfigurer configurer) {
        return new VizActionBarAdvisor(configurer);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.application.WorkbenchWindowAdvisor#postWindowOpen()
     */
    @Override
    public void postWindowOpen() {
        super.postWindowOpen();

        final IWorkbenchWindow window = super.getWindowConfigurer().getWindow();

        window.getShell().setMinimumSize(new Point(400, 400));

        if (firstTime) {
            // Maximize if this is the first time CAVE has started.
            window.getShell().setMaximized(true);
            firstTime = false;
        }

        listener = new VizPerspectiveListener(window, VizActionBarAdvisor
                .getInstance(window).getStatusLine());
        window.addPerspectiveListener(listener);
        window.addPageListener(AbstractVizPerspectiveManager.pageListener);
        IWorkbenchPage page = window.getActivePage();
        page.addPartListener(AbstractVizPerspectiveManager.partListener);

        IPerspectiveDescriptor perspective = page.getPerspective();
        if (perspective != null) {
            listener.perspectiveActivated(page, perspective);
        }

        new TearOffMenuListener(VizActionBarAdvisor.getInstance(window)
                .getMenuManager());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.application.WorkbenchWindowAdvisor#createWindowContents
     * (org.eclipse.swt.widgets.Shell)
     */
    @Override
    public void createWindowContents(Shell shell) {
        super.createWindowContents(shell);

        // Gets the main shell and colors the shell if the mode is set to
        // practice or training
        new ModeListener(shell);
    }

    @Override
    public IStatus restoreState(IMemento memento) {
        // If we have state to restore then this isn't our first time
        firstTime = false;
        return super.restoreState(memento);
    }

}
