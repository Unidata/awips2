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

package com.raytheon.viz.ui.panes;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.preferences.PreferenceConstants;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2007            chammack    Initial Creation.
 * Aug 2,  2013       2190 mschenke    Changed "panes" to Map to simplify code
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DrawCoordinatorJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DrawCoordinatorJob.class);

    /** The rate at which the screen should be drawn */
    private final float fpsThrottle = VizApp
            .getCorePreferenceInt(PreferenceConstants.P_FPS);

    /** The amount of time to sleep between frames */
    private final long sleepTime = (long) (1E3 * (1.0f / fpsThrottle));

    /**
     * Amount of time to track skipped frames for
     */
    private final long segmentCheckTime = 60000;

    /**
     * If we skip more than 10% of the frames in a minute, print an error.
     */
    private final int frameSkipLimit = (int) (segmentCheckTime / sleepTime * 0.2);

    /**
     * When system is thrashing, give small breaks to the system in increments
     * of this (ms)
     */
    private final long smallSleep = 5;

    private Map<IDisplayPaneContainer, DrawCoordinatedPane> panes = new IdentityHashMap<IDisplayPaneContainer, DrawCoordinatedPane>();

    private boolean shouldRun = true;

    private static DrawCoordinatorJob instance = new DrawCoordinatorJob();

    private DrawCoordinatorJob() {
        super("Draw Coordinator");
        setSystem(true);
        schedule();
    }

    public static DrawCoordinatorJob getInstance() {
        return instance;

    }

    /**
     * Unregister a displayPane from a display pane container.
     * 
     * @param container
     *            the container to unregister the pane from
     * @param displayPane
     */
    public void unregisterPane(IDisplayPaneContainer container,
            VizDisplayPane displayPane) {
        synchronized (panes) {
            DrawCoordinatedPane pane = panes.get(container);
            if (pane != null) {
                pane.remove(displayPane);
                if (pane.getPanes().isEmpty()) {
                    panes.remove(container);
                }
            }
        }
    }

    /**
     * Register a pane for drawing
     * 
     * @param container
     *            the container of the pane
     * @param displayPane
     *            the pane itself
     */
    public void registerPane(IDisplayPaneContainer container,
            VizDisplayPane displayPane) {
        DrawCoordinatedPane pane;
        synchronized (panes) {
            pane = panes.get(container);
            if (pane == null) {
                pane = new DrawCoordinatedPane(container);
                panes.put(container, pane);
            }
        }
        pane.add(displayPane);
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {

        long beforeTime = System.currentTimeMillis();
        long afterTime = 0L;
        long renderTime = 0L;
        long sleepDelta = 0L;
        boolean isDroppingFrames = false;
        final ArrayList<DrawCoordinatedPane> listToDraw = new ArrayList<DrawCoordinatedPane>();
        int framesSkippedInLastSegment = 0;
        long segmentStartTime = System.currentTimeMillis();

        while (shouldRun) {
            try {
                beforeTime = System.currentTimeMillis();
                if (beforeTime - segmentStartTime > segmentCheckTime) {
                    if (framesSkippedInLastSegment > frameSkipLimit) {
                        System.out.println("Skipped "
                                + framesSkippedInLastSegment
                                + " frames in last "
                                + (beforeTime - segmentStartTime) + "ms");
                        framesSkippedInLastSegment = 0;
                        segmentStartTime = afterTime;
                    }

                    framesSkippedInLastSegment = 0;
                    segmentStartTime = beforeTime;
                }

                listToDraw.clear();
                Set<DrawCoordinatedPane> managed;
                synchronized (panes) {
                    managed = new HashSet<DrawCoordinatedPane>(panes.values());
                }
                for (DrawCoordinatedPane pane : managed) {
                    if (pane.needsRefresh()) {
                        listToDraw.add(pane);
                    }
                }

                if (listToDraw.size() > 0) {
                    final boolean draw = !isDroppingFrames;

                    beforeTime = System.currentTimeMillis();
                    VizApp.runSync(new Runnable() {
                        public void run() {
                            for (DrawCoordinatedPane pane : listToDraw) {
                                pane.draw(draw);
                            }
                        }
                    });

                    afterTime = System.currentTimeMillis();
                    renderTime = afterTime - beforeTime;
                    sleepDelta = sleepTime - renderTime;

                    if (sleepDelta > 0) {
                        isDroppingFrames = false;
                        try {
                            Thread.sleep(sleepDelta);
                        } catch (InterruptedException e) {
                            // ignore
                        }
                    } else {
                        isDroppingFrames = true;
                        framesSkippedInLastSegment += (int) (-sleepDelta / sleepTime);

                        // yield a little to not swamp the cpu
                        try {
                            Thread.sleep(smallSleep);
                        } catch (InterruptedException e) {
                            // ignore
                        }
                    }
                } else {
                    try {
                        Thread.sleep(sleepTime);
                    } catch (InterruptedException e) {
                        // ignore
                    }
                }
            } catch (Throwable e) {
                statusHandler.error("Caught exception in DrawCoordinatorJob:",
                        e);
            }
        }

        return Status.OK_STATUS;
    }

    public void shutdown() {
        shouldRun = false;
    }
}
