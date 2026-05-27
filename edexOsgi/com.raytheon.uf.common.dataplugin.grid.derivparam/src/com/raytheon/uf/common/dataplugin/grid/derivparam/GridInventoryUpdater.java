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
package com.raytheon.uf.common.dataplugin.grid.derivparam;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.GridCacheUpdater.GridUpdateListener;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.GridTimeCache;
import com.raytheon.uf.common.dataplugin.grid.derivparam.tree.GridRequestableNode;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.tree.OrLevelNode;
import com.raytheon.uf.common.inventory.tree.LevelNode;

/**
 * Listens for updates to grid data and generates alerts for derived parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------
 * Mar 25, 2010  3547     bsteffen  Initial creation
 * Aug 30, 2013  2298     rjpeter   Make getPluginName abstract
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * Mar 03, 2016  5439     bsteffen  Allow grid derived parameters from edex
 * AUg 23, 2017  6125     bsteffen  Split common updating code from viz GridUpdater.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridInventoryUpdater implements GridUpdateListener {

    protected final CommonGridInventory inventory;

    public GridInventoryUpdater(CommonGridInventory inventory) {
        this.inventory = inventory;
    }

    @Override
    public void update(GridRecord record) {
        GridMapKey updateKey = new GridMapKey(record);
        GridTimeCache.getInstance().clearTimes(updateKey);
        Level level = LevelFactory.getInstance().getLevel(updateKey.masterLevel,
                updateKey.levelone, updateKey.leveltwo);
        LevelNode lNode = inventory.getNode(updateKey.modelName,
                updateKey.parameter, level);

        if (lNode == null) {
            inventory.reinitTree();
        } else if (!(lNode instanceof GridRequestableNode)) {
            if (lNode instanceof OrLevelNode) {
                DerivParamMethod method = ((OrLevelNode) lNode).getMethod();
                /*
                 * Null means it is an alias model and supplement means there
                 * exists a true GridRequestableNode buried under the or node
                 */
                if ((method == null)
                        || !"Supplement".equals(method.getName())) {
                    inventory.reinitTree();
                }
            } else {
                inventory.reinitTree();
            }
        }

    }

    @Override
    public void enableUpdates() {
        inventory.reinitTree();
    }

    @Override
    public void disableUpdates() {
        /*
         * No different behavior if updates are disabled. The inventory may
         * become out of sync with the database but there is no way to achieve
         * usable performance without the in memory inventory.
         */
    }

}
