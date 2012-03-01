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
package com.raytheon.uf.viz.derivparam.tree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.CubeRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * Provided a mechanism for requesting data for an entire 3D cube. If a Level is
 * set it will request records for all Standard levels within that composite
 * level, otherwise it will request all MB records. It will respond to time
 * queries with the Union of all levels it represents, although in the future
 * this may need to be changed to the intersection, or a limited intersection
 * when at least 3 levels are available. It returns all the GribRecords from all
 * the level nodes it represents, these should be sorted by the requesting node.
 * Finally it attempts to merge any requests to avoid the overhead of multiple
 * requests to EDEX.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010 #4473      rjpeter      Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractCubeLevelNode extends AbstractDerivedLevelNode {

    protected List<CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode>> levels;

    public AbstractCubeLevelNode(AbstractCubeLevelNode that) {
        super(that);
        this.levels = that.levels;
        this.setValue("3D");
    }

    public AbstractCubeLevelNode(
            List<CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode>> levels,
            String modelName) {
        this.levels = levels;
        this.modelName = modelName;
        this.setValue("3D");
    }

    public AbstractCubeLevelNode(
            Level level,
            String modelName,
            List<CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode>> levels) {
        this.setLevel(level);
        this.levels = levels;
        this.modelName = modelName;
        this.setValue("3D");
    }

    public AbstractCubeLevelNode(
            Level level,
            DerivParamDesc desc,
            DerivParamMethod method,
            String modelName,
            List<CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode>> levels) {
        super(level, desc, method, modelName);
        this.levels = levels;
        this.setValue("3D");
    }

    @Override
    public Level getLevel() {
        return null;
    }

    @Override
    public List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        List<AbstractRequestableLevelNode> requests = new ArrayList<AbstractRequestableLevelNode>(
                levels.size());
        List<AbstractRequestableLevelNode> timeAgnosticRequests = new ArrayList<AbstractRequestableLevelNode>();
        List<AbstractRequestableData> rawRecords = new ArrayList<AbstractRequestableData>(
                levels.size());
        List<AbstractRequestableData> timeAgnosticRecords = new ArrayList<AbstractRequestableData>();

        for (CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode> level : levels) {
            AbstractRequestableLevelNode node = level.getParam();
            if (cache.containsKey(node)) {
                List<AbstractRequestableData> cachedRecords = cache.get(node);
                if (node.isTimeAgnostic()) {
                    timeAgnosticRecords.addAll(cachedRecords);
                } else {
                    boolean foundMissingTime = false;
                    Set<DataTime> selectedTimes = new HashSet<DataTime>(
                            Arrays.asList(property.getSelectedEntryTime()));
                    for (AbstractRequestableData cRec : cachedRecords) {
                        if (!selectedTimes.contains(cRec.getDataTime())) {
                            foundMissingTime = true;
                            break;
                        }
                    }
                    if (foundMissingTime) {
                        requests.add(node);
                    } else {
                        rawRecords.addAll(cachedRecords);
                    }
                }
            } else {
                if (node.isTimeAgnostic()) {
                    timeAgnosticRequests.add(node);
                } else {
                    requests.add(node);
                }
            }
        }

        retrieveRecords(requests, property, timeOut, cache, rawRecords);
        retrieveRecords(timeAgnosticRequests, property, timeOut, cache,
                timeAgnosticRecords);
        Map<DataTime, CubeRequestableData> bins = new HashMap<DataTime, CubeRequestableData>(
                (int) (property.getSelectedEntryTime().length * 1.3));
        processRecords(rawRecords, property, true, false, bins);
        processRecords(timeAgnosticRecords, property, true, true, bins);
        requests.clear();
        timeAgnosticRequests.clear();
        rawRecords.clear();
        timeAgnosticRecords.clear();
        for (CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode> level : levels) {
            AbstractRequestableLevelNode node = level.getPressure();
            if (cache.containsKey(node)) {
                List<AbstractRequestableData> cachedRecords = cache.get(node);
                if (node.isTimeAgnostic()) {
                    timeAgnosticRecords.addAll(cachedRecords);
                } else {
                    rawRecords.addAll(cachedRecords);
                }
            } else {
                if (node.isTimeAgnostic()) {
                    timeAgnosticRequests.add(node);
                } else {
                    requests.add(node);
                }
            }
        }
        retrieveRecords(requests, property, timeOut, cache, rawRecords);
        retrieveRecords(timeAgnosticRequests, property, timeOut, cache,
                timeAgnosticRecords);
        processRecords(rawRecords, property, false, false, bins);
        processRecords(timeAgnosticRecords, property, false, true, bins);

        List<AbstractRequestableData> records = new ArrayList<AbstractRequestableData>(
                bins.size());
        for (Entry<DataTime, CubeRequestableData> entry : bins.entrySet()) {
            CubeRequestableData record = entry.getValue();
            record.validate();
            if (record.size() >= 2) {
                record.setDataTime(entry.getKey());
                modifyRequest(record);
                records.add(record);
            }
        }
        return records;
    }

    private void retrieveRecords(
            List<AbstractRequestableLevelNode> requests,
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache,
            List<AbstractRequestableData> retrievedRecords) throws VizException {
        retrievedRecords.addAll(mergedGetData(merge(requests), property,
                timeOut));
        for (AbstractRequestableLevelNode request : requests) {
            retrievedRecords.addAll(request.getData(property, timeOut, cache));
        }
    }

    private void processRecords(List<AbstractRequestableData> records,
            LayerProperty property, boolean isParam, boolean isTimeAgnostic,
            Map<DataTime, CubeRequestableData> bins) {
        DataTime[] keys = new DataTime[1];
        if (isTimeAgnostic) {
            keys = property.getSelectedEntryTime();
        }

        for (AbstractRequestableData record : records) {
            if (!isTimeAgnostic) {
                keys[0] = record.getDataTime();
            }

            for (DataTime dt : keys) {
                if (dt != null) {
                    CubeRequestableData bin = bins.get(dt);
                    if (bin == null) {
                        bin = new CubeRequestableData(record);
                        bins.put(dt, bin);
                    }
                    if (isParam) {
                        bin.addParam(record);
                    } else {
                        bin.addPressure(record);
                    }
                }
            }
        }
    }

    @Override
    public boolean isTimeAgnostic() {
        boolean timeAgnostic = true;

        for (CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode> level : levels) {
            if (!level.getPressure().isTimeAgnostic()
                    || !level.getParam().isTimeAgnostic()) {
                timeAgnostic = false;
                break;
            }
        }
        return timeAgnostic;
    }

    /**
     * Perform any filtering on the requestContraintsToFilter based on the
     * baseRequestConstraints.
     * 
     * @param baseRequestConstraints
     * @param requestContraintsToFilter
     */
    protected abstract void filter(
            Map<String, RequestConstraint> baseRequestConstraints,
            Map<String, RequestConstraint> requestContraintsToFilter);

    @Override
    public Set<DataTime> timeQueryInternal(TimeQueryRequest originalRequest,
            boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        Set<DataTime> results = new HashSet<DataTime>();
        List<AbstractRequestableLevelNode> requests = new ArrayList<AbstractRequestableLevelNode>(
                levels.size() * 2);

        for (CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode> level : levels) {
            AbstractRequestableLevelNode node = level.getPressure();
            if (cache.containsKey(node)) {
                results.addAll(cache.get(node));
            } else if (!node.isTimeAgnostic()) {
                requests.add(node);
            }
            node = level.getParam();
            if (cache.containsKey(node)) {
                results.addAll(cache.get(node));
            } else if (!node.isTimeAgnostic()) {
                requests.add(node);
            }
        }

        if (requests.size() == 0 && results.size() == 0) {
            return TIME_AGNOSTIC;
        }

        results.addAll(mergedTimeQuery(merge(requests), latestOnly));
        for (AbstractRequestableLevelNode request : requests) {
            results.addAll(request.timeQuery(originalRequest, latestOnly,
                    cache, latestOnlyCache));
        }
        return results;
    }

    /**
     * A merged time query performs a single time query from multiple requests
     * when possible and removes those requests from the list.
     * 
     * @param requests
     * @param latestOnly
     * @return
     * @throws VizException
     */
    protected Set<DataTime> mergedTimeQuery(
            List<Map<String, RequestConstraint>> requests, boolean latestOnly)
            throws VizException {
        Set<DataTime> results = new HashSet<DataTime>();
        for (Map<String, RequestConstraint> mergeMap : requests) {
            DataTime[] resultsArr = CatalogQuery.performTimeQuery(mergeMap,
                    latestOnly, null);

            if (resultsArr != null) {
                for (int i = 0; i < resultsArr.length; i++) {
                    results.add(resultsArr[i]);
                }
            }
        }
        return results;
    }

    protected List<AbstractRequestableData> mergedGetData(
            List<Map<String, RequestConstraint>> requests,
            LayerProperty property, int timeOut) throws VizException {
        List<Object> results = new ArrayList<Object>();
        Map<String, RequestConstraint> oldQuery = property
                .getEntryQueryParameters(false);
        int numberOfImages = property.getNumberOfImages();
        if (numberOfImages < 9999) {
            property.setNumberOfImages(9999);
        }
        try {
            for (Map<String, RequestConstraint> mergeMap : requests) {
                Map<String, RequestConstraint> newQuery = new HashMap<String, RequestConstraint>(
                        mergeMap);
                filter(oldQuery, newQuery);
                property.setEntryQueryParameters(newQuery, false);
                // TODO: replace
                String scriptToExecute = ScriptCreator.createScript(property);

                results.addAll(Loader.loadScripts(
                        new String[] { scriptToExecute }, timeOut));
            }
        } finally {
            property.setNumberOfImages(numberOfImages);
            property.setEntryQueryParameters(oldQuery, false);
        }

        return wrapRawRecord(results);
    }

    protected abstract List<AbstractRequestableData> wrapRawRecord(
            List<Object> objs) throws VizException;

    protected List<Map<String, RequestConstraint>> merge(
            List<AbstractRequestableLevelNode> requests) {
        List<Map<String, RequestConstraint>> mergeMaps = new ArrayList<Map<String, RequestConstraint>>(
                requests.size());
        Iterator<AbstractRequestableLevelNode> iter = requests.iterator();
        while (iter.hasNext()) {
            AbstractRequestableLevelNode request = iter.next();
            if (request.hasRequestConstraints()) {
                mergeMaps.add(request.getRequestConstraintMap());
                iter.remove();
            }
        }
        return mergeConstraints(mergeMaps);
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    public List<Dependency> getDependencies() {
        List<Dependency> dependencies = new ArrayList<Dependency>(
                levels.size() * 2);
        for (CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode> level : levels) {
            dependencies.add(new Dependency(level.getPressure(), 0));
            dependencies.add(new Dependency(level.getParam(), 0));
        }
        return dependencies;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((levels == null) ? 0 : levels.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        AbstractCubeLevelNode other = (AbstractCubeLevelNode) obj;
        if (levels == null) {
            if (other.levels != null)
                return false;
        } else if (!levels.equals(other.levels))
            return false;
        return true;
    }

}
