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
package com.raytheon.uf.viz.volumebrowser.dataplugin;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.derivparam.inv.AbstractInventory;

/**
 * 
 * Job for querying the {@link AbstractInventory} to determine what possible
 * Sources, Parameters, and Levels have available data. The full set of all
 * available sources, parameters, and levels must be provided. If anything has
 * already been selected then it should be provided to narrow down the possible
 * results. If no selection is provided then all items will be considered.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Feb 01, 2016  5275     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
final class InventoryUpdateJob extends Job {

    private final LevelFactory levelFactory = LevelFactory.getInstance();

    private final AbstractInventory inventory;

    private final BlockingQueue<String> sourceQueue = new LinkedBlockingQueue<String>();

    private final BlockingQueue<String> parameterQueue = new LinkedBlockingQueue<String>();

    private final BlockingQueue<String> levelQueue = new LinkedBlockingQueue<String>();

    private Collection<String> allPossibleSources;

    private Collection<String> selectedSources;

    private Collection<String> planeFilteredSources;

    private Collection<String> allPossibleParameters;

    private Collection<String> selectedParameters;

    private Collection<Level> allPossibleLevels;

    private Collection<Level> selectedLevels;

    public InventoryUpdateJob(AbstractInventory inventory) {
        super("Loading available inventory");
        setSystem(true);
        this.inventory = inventory;
    }

    /**
     * Set all possible sources, this must be called before the job is
     * scheduled.
     */
    public void setAllPossibleSources(Collection<String> allPossibleSources) {
        this.allPossibleSources = allPossibleSources;
    }

    /**
     * Set the selected sources, if this is not called then all possible sources
     * will be used when determining available parameters and levels.
     */
    public void setSelectedSources(Collection<String> selectedSources) {
        this.selectedSources = selectedSources;
    }

    /**
     * When the volume browser is in a mode where the planes menus allow the
     * user to select a point or line instead of a level then this job needs to
     * know what sources are available for the selected point or line. For
     * example if a point is in Hawaii then sources only over Alsaka should not
     * be in this set. This will be used when determining what sources or
     * parameters are available, this is not necessary when the planes menu in
     * the volume browser contain levels.
     */
    public void setPlaneFilteredSources(Collection<String> planeFilteredSources) {
        this.planeFilteredSources = planeFilteredSources;
    }

    /**
     * Set all possible parameters, this must be called before the job is
     * scheduled.
     */
    public void setAllPossibleParameters(
            Collection<String> allPossibleParameters) {
        this.allPossibleParameters = allPossibleParameters;
    }

    /**
     * Set the selected parameters, if this is not called then all possible
     * parameters will be used when determining available sources and levels.
     */
    public void setSelectedParameters(Collection<String> selectedParameters) {
        this.selectedParameters = selectedParameters;
    }

    /**
     * Set all possible levels, this must be called before the job is scheduled.
     */
    public void setAllPossibleLevels(Collection<Level> allPossibleLevels) {
        this.allPossibleLevels = allPossibleLevels;
    }

    /**
     * Set the selected levels, if this is not called then all possible levels
     * will be used when determining available sources and parameters.
     */
    public void setSelectedLevels(Collection<Level> selectedLevels) {
        this.selectedLevels = selectedLevels;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        if (selectedSources == null) {
            selectedSources = allPossibleSources;
        }
        if (selectedParameters == null) {
            selectedParameters = allPossibleParameters;
        }
        if (selectedLevels == null) {
            selectedLevels = allPossibleLevels;
        }
        if (planeFilteredSources == null) {
            planeFilteredSources = allPossibleSources;
        }

        Collection<String> planeFilteredSelectedSources = new HashSet<>(
                selectedSources);
        planeFilteredSelectedSources.retainAll(planeFilteredSources);

        /*
         * The query methods on the inventory will modify the input lists so
         * make a copy since the original might be in use elsewhere.
         */
        planeFilteredSources = new LinkedList<>(planeFilteredSources);
        allPossibleParameters = new LinkedList<>(allPossibleParameters);
        allPossibleLevels = new LinkedList<>(allPossibleLevels);

        try {
            inventory.checkSources(planeFilteredSources, selectedParameters,
                    selectedLevels, sourceQueue);
            if (monitor.isCanceled()) {
                return Status.CANCEL_STATUS;
            }
            inventory.checkParameters(planeFilteredSelectedSources,
                    allPossibleParameters, selectedLevels, false,
                    parameterQueue);
            if (monitor.isCanceled()) {
                return Status.CANCEL_STATUS;
            }
            inventory.checkLevels(selectedSources, selectedParameters,
                    allPossibleLevels, levelQueue);
        } catch (InterruptedException e) {
            // no-op
        }
        return Status.OK_STATUS;
    }

    /**
     * Attempt to interrupt the current query and cancel the job.
     */
    public void interrupt() {
        cancel();
        Thread thread = getThread();
        if (thread != null) {
            thread.interrupt();
        }
    }

    /**
     * @return true when all queries are complete and all the results have been
     *         read.
     */
    public boolean isDone() {
        return getResult() != null && sourceQueue.isEmpty()
                && parameterQueue.isEmpty() && levelQueue.isEmpty();
    }

    /**
     * @return a single available source or null if no new sources are
     *         available. Even if null is returned it is possible for a new
     *         source to be found as available if the job is still running.
     */
    public String pollSource() {
        return sourceQueue.poll();
    }

    /**
     * @return a single available parameter or null if no new parameters are
     *         available. Even if null is returned it is possible for a new
     *         parameter to be found as available if the job is still running.
     */
    public String pollParameter() {
        return parameterQueue.poll();
    }

    /**
     * @return a single available level or null if no new levels are available.
     *         Even if null is returned it is possible for a new level to be
     *         found as available if the job is still running.
     */
    public Level pollLevel() {
        String levelStr = levelQueue.poll();
        if (levelStr == null) {
            return null;
        } else {
            return levelFactory.getLevel(levelStr);
        }
    }
}