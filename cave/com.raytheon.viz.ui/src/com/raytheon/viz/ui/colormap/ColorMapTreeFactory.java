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
package com.raytheon.viz.ui.colormap;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;

/**
 * Factory which can provide cached versions of {@link ColorMapTree} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 18, 2013  2421     bsteffen    Initial creation
 * Aug 28, 2014  3516     rferrel     Getting treesByLevel no longer
 *                                     on the UI thread.
 *                                    Converted to singleton.
 *                                    Added localized file observer.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ColorMapTreeFactory {

    /**
     * The only allowed instance of this class.
     */
    private final static ColorMapTreeFactory instance = new ColorMapTreeFactory();

    /**
     * 
     * @return instance
     */
    public static ColorMapTreeFactory getInstance() {
        return instance;
    }

    /**
     * BASE localization tree must be handled differently from the other
     * localization levels. Its items are placed directly on the top menu while
     * the other levels are in their own pop up menu.
     */
    private ColorMapTree baseTree;

    /**
     * Lock when working on baseTree.
     */
    private Object baseTreeLock = new Object();

    /**
     * Trees for non-BASE localization levels.
     */
    private final Map<LocalizationLevel, ColorMapTree> treesByLevel = new HashMap<LocalizationLevel, ColorMapTree>();

    /**
     * Listeners needing treesByLevel information
     */
    private List<ILevelMapsCallback> tlcListeners = new ArrayList<ILevelMapsCallback>();

    /**
     * Non-BASE localization levels.
     */
    private final LocalizationLevel[] treesLevelLocalization;

    /**
     * Thread safe list of listeners to notify when localized colormaps
     * directories have a file change. Since the list is modified more often
     * then it is traversed CopyOnWriterArrayList is not recommended.
     * 
     */
    private final List<IRefreshColorMapTreeListener> refreshListeners = Collections
            .synchronizedList(new ArrayList<IRefreshColorMapTreeListener>());

    /**
     * Singleton constructor.
     */
    private ColorMapTreeFactory() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationLevel[] allLevels = pm.getAvailableLevels();
        // Remove BASE
        treesLevelLocalization = Arrays.copyOfRange(allLevels, 1,
                allLevels.length);

        // Start creating trees off the UI-thread
        Job job = new Job("Find ColorMapTree base") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                ColorMapTree base = getBaseTree();
                base.optimizeIsEmpty();
                return Status.OK_STATUS;
            }

        };
        job.setSystem(true);
        job.schedule();
        initTreeByLevel();
    }

    /**
     * Get a tree for the BASE localization context. This tree is treated
     * differently from the other localization context trees. The tree items are
     * placed directly on the top menu while the other level trees are put in
     * their own pop up menu.
     */
    public ColorMapTree getBaseTree() {
        synchronized (baseTreeLock) {
            if (baseTree == null) {
                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationContext baseContext = pm.getContext(
                        LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

                /*
                 * Useful for testing delay in getting base tree.
                 */
                // try {
                // baseTreeLock.wait(8000L);
                // } catch (InterruptedException e) {
                // e.printStackTrace();
                // }

                baseTree = new ColorMapTree(pm, baseContext,
                        ColorMapLoader.DIR_NAME);
            }
            return baseTree;
        }
    }

    /**
     * This immediately performs the call back for each non-BASE localization
     * level sending a null tree when a level's tree has not yet been created.
     * This allows the call back to generate a place holder for the level. When
     * needed the call back routine will be called as any missing trees are
     * generated.
     * 
     * @param listener
     */
    public void updateLevelMapsCallack(ILevelMapsCallback listener) {
        synchronized (treesByLevel) {

            /*
             * Immediately perform the action for all levels. When treeByLevel
             * entry is null the tree for the level has not yet been created;
             * invoke the listener with a null tree. This allows a listener
             * creating menu items to generate a place holder entry such as
             * "USER ???".
             */
            for (LocalizationLevel level : getTreesLevelLocalization()) {
                ColorMapTree tree = treesByLevel.get(level);
                listener.treeCreated(level, tree);
            }

            /*
             * Still missing trees. Must call the callback again as each missing
             * tree is created.
             */
            if (isMissingLevelTrees()) {
                tlcListeners.add(listener);
            }
        }
    }

    /**
     * 
     * @return true if still generating trees
     */
    private boolean isMissingLevelTrees() {
        return tlcListeners != null;
    }

    /**
     * Return a {@link ColorMapTree}Tree for the provided level. The tree will
     * have the same name as the level and will have a subtree for each context
     * that exists at that level.
     */
    public ColorMapTree getTreeForLevel(LocalizationLevel level) {
        synchronized (treesByLevel) {
            ColorMapTree tree = treesByLevel.get(level);
            if (tree == null) {
                IPathManager pm = PathManagerFactory.getPathManager();
                tree = new ColorMapTree(pm, level, ColorMapLoader.DIR_NAME);
                treesByLevel.put(level, tree);
            }
            return tree;
        }
    }

    /**
     * 
     * @return treesLevelLocalization
     */
    public LocalizationLevel[] getTreesLevelLocalization() {
        return treesLevelLocalization;
    }

    /**
     * This creates the trees for treesByLevel off of the UI thread. It informs
     * all listeners when a tree is created.
     */
    private void initTreeByLevel() {
        synchronized (treesByLevel) {
            treesByLevel.clear();

            Job job = new Job("Finding Colormaps") {

                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    IPathManager pm = PathManagerFactory.getPathManager();
                    LocalizationLevel[] levels = getTreesLevelLocalization();
                    for (final LocalizationLevel level : levels) {
                        ColorMapTree tree = new ColorMapTree(pm, level,
                                ColorMapLoader.DIR_NAME);
                        tree.optimizeIsEmpty();

                        synchronized (treesByLevel) {
                            /*
                             * Use for debugging. Simulates a long delay in
                             * getting tree level's color map. Allows testing to
                             * see if menu items for the tree levels are
                             * properly handled.
                             */
                            // try {
                            // treesByLevel.wait(3000L);
                            // } catch (InterruptedException e) {
                            // e.printStackTrace();
                            // }

                            treesByLevel.put(level, tree);
                            for (ILevelMapsCallback listener : tlcListeners) {
                                listener.treeCreated(level, tree);
                            }
                        }
                    }

                    synchronized (treesByLevel) {
                        /*
                         * Free up tlcListeners space and indicate all level
                         * trees are now generated.
                         */
                        tlcListeners.clear();
                        tlcListeners = null;
                    }
                    return Status.OK_STATUS;
                }
            };
            job.schedule();
        }
    }

    /**
     * Method used to inform listeners of any changes to any ColorMapTree.
     * 
     * @param level
     */
    public void refresh() {

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                IRefreshColorMapTreeListener[] rListeners = refreshListeners
                        .toArray(new IRefreshColorMapTreeListener[0]);
                for (IRefreshColorMapTreeListener listener : rListeners) {
                    listener.refreshColorMapTree();
                }
            }
        });
    }

    /**
     * Thread safe removal of listener.
     * 
     * @param listener
     */
    public void removeRefreshItemsListener(IRefreshColorMapTreeListener listener) {
        refreshListeners.remove(listener);
    }

    /**
     * Thread safe adding listener.
     * 
     * @param listener
     */
    public void addRefreshItemsListener(IRefreshColorMapTreeListener listener) {
        refreshListeners.add(listener);
    }
}
