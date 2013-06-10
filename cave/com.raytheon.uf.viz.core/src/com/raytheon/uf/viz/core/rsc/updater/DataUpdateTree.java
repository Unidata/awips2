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

package com.raytheon.uf.viz.core.rsc.updater;

import java.util.Map;

import com.raytheon.uf.common.dataquery.DecisionTree;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;

/**
 * Update tree for {@link AbstractVizResource} objects, resources inserted will
 * automatically be removed once disposed
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 5, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DataUpdateTree extends DecisionTree<AbstractVizResource<?, ?>>
        implements IDisposeListener {

    private static DataUpdateTree instance;

    public static synchronized DataUpdateTree getInstance() {
        if (instance == null) {
            instance = new DataUpdateTree();
        }

        return instance;
    }

    protected static synchronized void setCustomInstance(DataUpdateTree tree) {
        if (instance != null) {
            for (DataPair pair : instance.getDataPairs()) {
                tree.insertCriteria(pair.metadata, pair.data, false);
            }
            tree.rebuildTree();
        }
        instance = tree;
    }

    protected DataUpdateTree() {
        super();
    }

    @Override
    public void insertCriteria(Map<String, RequestConstraint> searchCriteria,
            AbstractVizResource<?, ?> item, boolean rebuild) {
        if (item != null) {
            super.insertCriteria(searchCriteria, item, rebuild);
            item.registerListener(this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IDisposeListener#disposed(com.raytheon.uf
     * .viz.core.rsc.AbstractVizResource)
     */
    @Override
    public void disposed(AbstractVizResource<?, ?> rsc) {
        remove(rsc);
    }

    @Override
    public void remove(AbstractVizResource<?, ?> item) {
        item.unregisterListener(this);
        super.remove(item);
    }

}
