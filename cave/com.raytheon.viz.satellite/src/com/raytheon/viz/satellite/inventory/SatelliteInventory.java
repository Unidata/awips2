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
package com.raytheon.viz.satellite.inventory;

import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.inv.AbstractInventory;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.DataTree;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.inventory.tree.ParameterNode;
import com.raytheon.uf.common.inventory.tree.SourceNode;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * Inventory of available satellite data. sectorID is used for source and
 * physicalElement for parameter, the level is always the entire atmosphere.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 09, 2014  2947     bsteffen    Initial creation
 * May 06, 2014  3117     bsteffen    Update for new data.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SatelliteInventory extends AbstractInventory implements
        IAlertObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatelliteInventory.class);

    public static final String SATELLITE = "satellite";

    public static final String SECTOR_ID = "sectorID";

    public static final String PHYSICALELEMENT = "physicalElement";

    public static final String GID = "coverage.gid";

    private SatelliteCoverageCache coverages;

    private Level level;

    public SatelliteInventory() {
        ProductAlertObserver.addObserver(SATELLITE, this);
    }

    @Override
    public List<DataTime> timeAgnosticQuery(Map<String, RequestConstraint> query) {
        /* Returning null means no data will be time agnostic. */
        return null;
    }

    @Override
    public synchronized void initTree(Map<String, DerivParamDesc> derParLibrary)
            throws DataCubeException {
        try {
            level = LevelFactory.getInstance().getLevel("EA", 0.0);
        } catch (CommunicationException e) {
            throw new DataCubeException(e);
        }
        coverages = new SatelliteCoverageCache();
        super.initTree(derParLibrary);
    }

    @Override
    protected DataTree createBaseTree() {
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(SatelliteRecord.class);
        request.setDistinct(true);
        request.addRequestField(SECTOR_ID);
        request.addRequestField(PHYSICALELEMENT);

        DbQueryResponse response;
        try {
            response = (DbQueryResponse) RequestRouter.route(request);
        } catch (Exception e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return null;
        }

        String levelId = Long.toString(level.getId());

        DataTree tree = new DataTree();
        for (Map<String, Object> result : response.getResults()) {
            String sectorID = (String) result.get(SECTOR_ID);
            String physicalElement = (String) result.get(PHYSICALELEMENT);
            SourceNode sourceNode = tree.getSourceNode(sectorID);
            if (sourceNode == null) {
                sourceNode = new SourceNode();
                sourceNode.setValue(sectorID);
                tree.getSourceNodes().put(sectorID, sourceNode);
            }

            ParameterNode paramNode = sourceNode.getChildNode(physicalElement);
            if (paramNode == null) {
                paramNode = new ParameterNode();
                paramNode.setValue(physicalElement);
                paramNode.setParameterName(physicalElement);
                sourceNode.addChildNode(paramNode);
            }

            LevelNode levelNode = paramNode.getChildNode(levelId);
            if (levelNode == null) {
                Map<String, RequestConstraint> requestConstraints = new HashMap<String, RequestConstraint>();
                requestConstraints.put(SECTOR_ID, new RequestConstraint(
                        sectorID));
                requestConstraints.put(PHYSICALELEMENT, new RequestConstraint(
                        physicalElement));
                requestConstraints.put(PluginDataObject.PLUGIN_NAME_ID,
                        new RequestConstraint(SATELLITE));
                levelNode = new SatelliteRequestableLevelNode(coverages,
                        requestConstraints);
                levelNode.setValue(levelId);
                paramNode.addChildNode(levelNode);
            }
        }
        return tree;
    }

    @Override
    protected LevelNode getCubeNode(SourceNode sNode, DerivParamField field,
            Deque<StackEntry> stack, Set<StackEntry> nodata) {
        /* Returning null means cubes is not supported. */
        return null;
    }

    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableData nodeToImport, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level) {
        /* Returning null means import is not supported. */
        return null;
    }

    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableNode nodeToImport,
            String nodeToImportSourceName, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level) {
        /* Returning null means import is not supported. */
        return null;
    }

    @Override
    protected Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level) {
        /* Returning null means static data is not supported. */
        return null;
    }

    protected List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> constraints) {
        Collection<String> sources = getAllSources();
        RequestConstraint sectorLimiter = constraints.get(SECTOR_ID);
        if (sectorLimiter != null) {
            Iterator<String> it = sources.iterator();
            while (it.hasNext()) {
                if (!sectorLimiter.evaluate(it.next())) {
                    it.remove();
                }
            }
        }
        Collection<String> parameters = getAllParameters();
        RequestConstraint peLimiter = constraints.get(PHYSICALELEMENT);
        if (peLimiter != null) {
            Iterator<String> it = parameters.iterator();
            while (it.hasNext()) {
                if (!peLimiter.evaluate(it.next())) {
                    it.remove();
                }
            }
        }
        Collection<Level> levels = Collections.singleton(level);
        try {
            List<AbstractRequestableNode> result = walkTree(null, sources,
                    parameters, levels, true, true, null);
            return result;
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return Collections.emptyList();
        }
    }

    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        if (dataTree == null) {
            return;
        }
        for (AlertMessage message : alertMessages) {
            String sector = message.decodedAlert.get(SECTOR_ID).toString();
            String pe = message.decodedAlert.get(PHYSICALELEMENT).toString();
            if (dataTree.getParameterNode(sector, pe) == null) {
                /*
                 * When a sector or element arrives that is not known reinit the
                 * tree to ensure no nodes are missing.
                 */
                try {
                    initTree(derParLibrary);
                } catch (DataCubeException e) {
                    statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
                }
                return;
            }
        }
    }
}
