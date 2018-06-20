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
package com.raytheon.viz.grid;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.GridMapKey;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.derivparam.inv.AbstractInventory;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.CubeLevel;
import com.raytheon.uf.common.inventory.tree.DataTree;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.inventory.tree.SourceNode;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.grid.inv.GridUpdater;

/**
 * 
 * Provides a plugin based mechanism to add custom data to grid derived
 * parameters.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 15, 2017  6332     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public interface GridExtension {

    /**
     * This method can be used to provide custom data based off the information
     * in the descriptor. For example it is possible to return data that is
     * based off of other resources that are loaded. This method can return null
     * indicating that no descriptor specific customizations are necessary, in
     * which case the data will be loaded through the {@link DataCubeContainer}.
     */
    public IDataRecord[] loadCustomData(GridRecord record,
            IDescriptor descriptor) throws VizException;

    /**
     * Modify the base grid {@link DataTree}. This method will generally add new
     * {@link AbstractBaseDataNode}s to the tree for a custom data type.
     */
    public void addToBaseTree(DataTree dataTree,
            Map<String, DerivParamDesc> derParLibrary);

    /**
     * This method needs to handle any {@link AbstractBaseDataNode}s that were
     * added in {@link #addToBaseTree(DataTree, Map)}. It should return a
     * {@link GridMapKey}that will be used by the {@link GridUpdater} to ensure
     * that updates work for derived parameters. If the node type was not
     * created by this extension then null should be returned.
     */
    public GridMapKey getUpdateKey(AbstractBaseDataNode node);

    /**
     * Handle a time query for a particular data source, implementation should
     * check the query to see if it applies to this extension and return null if
     * it does not.
     * 
     * @see AbstractInventory#timeAgnosticQuery(Map)
     */
    public Set<DataTime> timeInvariantQuery(
            Map<String, RequestConstraint> query) throws VizException;

    /**
     * Get the appropriate {@link MasterLevel} name to use for 3D data queries
     * against a particular source. If the source is not managed by a particular
     * extension then null should be returned.
     */
    public String get3DMasterLevel(String model);

    /**
     * Generate a cube node for a specific model.
     */
    public LevelNode getCubeNode(String modelName,
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> cubeLevels);

    /**
     * Resolve a static parameter.
     */
    public Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level);
}
