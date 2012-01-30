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
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AliasRequestableData;
import com.raytheon.uf.viz.derivparam.data.DerivedRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamField;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterRequest;
import com.raytheon.uf.viz.derivparam.library.IDerivParamField;

/**
 * The default node for all Derived Parameters, Responsible for building
 * DerivedParameterRequests that will eventually be passed to the
 * DerivedParamterGenerator.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 14, 2009            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DerivedLevelNode extends AbstractDerivedLevelNode {

    private static final int TIME_QUERY_CACHE_TIME = 30000;

    private Map<IDerivParamField, AbstractRequestableData> fieldStaticData = null;

    private Map<DerivParamField, AbstractRequestableLevelNode> fields = null;

    /**
     * Time cache should be reset every time a time query is performed and then
     * it can be used to correlate times when requesting data.
     */
    private Map<DerivParamField, Set<DataTime>> timeCache = null;

    private long lastTimeQuery = 0;

    private int dt;

    public DerivedLevelNode(Level level, DerivParamDesc desc,
            DerivParamMethod method, String modelName, int dt) {
        super(level, desc, method, modelName);
        this.dt = dt;
    }

    public DerivedLevelNode(DerivedLevelNode that) {
        super(that);
        if (that.fields != null) {
            fields = new HashMap<DerivParamField, AbstractRequestableLevelNode>(
                    that.fields);
        }
        if (that.fieldStaticData != null) {
            fieldStaticData = new HashMap<IDerivParamField, AbstractRequestableData>(
                    that.fieldStaticData);
        }
        this.dt = that.dt;
    }

    /**
     * @return the fields
     */
    public Map<IDerivParamField, AbstractRequestableData> getFieldsStaticData() {
        return fieldStaticData;
    }

    /**
     * @return the fields
     */
    public Map<DerivParamField, AbstractRequestableLevelNode> getFields() {
        return fields;
    }

    /**
     * This should be used for strings which the DataCubeAdapter knows how to
     * handle
     * 
     * @param field
     * @param string
     */
    public void putStaticField(IDerivParamField field,
            AbstractRequestableData requestableData) {
        if (fieldStaticData == null) {
            fieldStaticData = new HashMap<IDerivParamField, AbstractRequestableData>();
        }
        this.fieldStaticData.put(field, requestableData);
    }

    public void putField(DerivParamField field,
            AbstractRequestableLevelNode object) {
        if (fields == null) {
            fields = new HashMap<DerivParamField, AbstractRequestableLevelNode>();
        }
        this.fields.put(field, object);
    }

    public int getFieldsSize() {
        int size = 0;
        size += fields == null ? 0 : fields.size();
        size += fieldStaticData == null ? 0 : fieldStaticData.size();
        return size;
    }

    public boolean isPopulated() {
        return getFieldsSize() == method.getFields().size();
    }

    @Override
    public Set<DataTime> timeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        this.lastTimeQuery = System.currentTimeMillis();
        Map<DerivParamField, Set<DataTime>> timeCache = new HashMap<DerivParamField, Set<DataTime>>();
        // We have a derived parameter for the requested grid
        Set<DataTime> availableDataTimes = null;
        if (fields == null) {
            timeCache.put(null, TIME_AGNOSTIC);
            this.timeCache = timeCache;
            return TIME_AGNOSTIC;
        }
        Collection<DerivParamField> fieldsKeys = fields.keySet();
        if (method.isDtime()) {
            // Attempt to use 0 time shift data first.
            List<DerivParamField> fieldsList = new ArrayList<DerivParamField>(
                    fieldsKeys);
            for (int i = 1; i < fieldsList.size(); i++) {
                if (fieldsList.get(i).getTimeShift() == 0) {
                    fieldsList.set(i, fieldsList.set(0, fieldsList.get(i)));
                }
            }
            fieldsKeys = fieldsList;
        }
        for (DerivParamField field : fieldsKeys) {
            AbstractRequestableLevelNode node = fields.get(field);
            Set<DataTime> queryDataTimes = node.timeQuery(false, cache,
                    latestOnlyCache);
            timeCache.put(field, queryDataTimes);
            if (queryDataTimes == TIME_AGNOSTIC) {
                if (availableDataTimes == null) {
                    availableDataTimes = TIME_AGNOSTIC;
                }
                continue;
            }
            if (queryDataTimes != null && queryDataTimes.size() > 0) {

                if (availableDataTimes != null
                        && availableDataTimes != TIME_AGNOSTIC) {
                    availableDataTimes.retainAll(timeMatch(availableDataTimes,
                            queryDataTimes, field).keySet());
                } else {
                    // populate initial data times

                    if (method.isDtime()) {
                        // if dT required handle now
                        int dTimeInSeconds = getDTimeInSeconds(field);

                        if (dTimeInSeconds == -1) {
                            return new HashSet<DataTime>(0);
                        }

                        // handle valid time shift
                        availableDataTimes = new HashSet<DataTime>((64));

                        // generate all possible valid times
                        for (DataTime dataTime : queryDataTimes) {
                            if (field.getTimeShift() == 0) {
                                // Strip any validPeriodInfo
                                availableDataTimes.add(new DataTime(dataTime
                                        .getRefTime(), dataTime.getFcstTime()));
                            } else {
                                availableDataTimes
                                        .addAll(generatePossibleDataTimes(
                                                dataTime, field));
                            }
                        }
                    } else if (method.isFtime()) {
                        // handle forecast time shift
                        availableDataTimes = new HashSet<DataTime>();
                        int fcstShift = field.getTimeShift();
                        for (DataTime dataTime : queryDataTimes) {
                            int fcstTime = dataTime.getFcstTime() - fcstShift;
                            if (fcstTime >= 0) {
                                availableDataTimes.add(new DataTime(dataTime
                                        .getRefTime(), fcstTime));
                            }
                        }
                    } else {
                        availableDataTimes = new HashSet<DataTime>();
                        for (DataTime time : queryDataTimes) {
                            if (time.getUtilityFlags().contains(FLAG.FCST_USED))
                                availableDataTimes.add(new DataTime(time
                                        .getRefTime(), time.getFcstTime()));
                            else
                                availableDataTimes.add(new DataTime(time
                                        .getRefTime()));
                        }
                    }
                }
            } else {
                // no data for this query, clear times and return
                availableDataTimes = null;
                break;
            }

            if (availableDataTimes == null || availableDataTimes.size() == 0) {
                break;
            }
        }// FIELD_LOOP

        if (availableDataTimes != null) {
            timeCache.put(null, availableDataTimes);
            this.timeCache = timeCache;
        } else {
            timeCache = new HashMap<DerivParamField, Set<DataTime>>();
            availableDataTimes = new HashSet<DataTime>(0);
            timeCache.put(null, availableDataTimes);
            this.timeCache = timeCache;
        }
        return availableDataTimes;
    }

    @Override
    public boolean isTimeAgnostic() {
        boolean timeAgnostic = true;

        for (AbstractRequestableLevelNode node : fields.values()) {
            if (!node.isTimeAgnostic()) {
                timeAgnostic = false;
                break;
            }
        }

        return timeAgnostic;
    }

    public List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        if (this.timeCache == null
                || this.lastTimeQuery + TIME_QUERY_CACHE_TIME < System
                        .currentTimeMillis()) {
            this.timeQuery(false);
        }

        // keep a reference for scope of method
        Map<DerivParamField, Set<DataTime>> myTimeCache = this.timeCache;
        Set<DataTime> thisAvailableTimes = myTimeCache.get(null);
        if (thisAvailableTimes == TIME_AGNOSTIC) {
            thisAvailableTimes = new HashSet<DataTime>();
            thisAvailableTimes.add(null);
        } else {
            if (property.getSelectedEntryTime() != null) {
                thisAvailableTimes = new HashSet<DataTime>(thisAvailableTimes);
                thisAvailableTimes.retainAll(Arrays.asList(property
                        .getSelectedEntryTime()));
            }
        }
        Map<DataTime, DerivedParameterRequest> mapOfRequests = new HashMap<DataTime, DerivedParameterRequest>(
                thisAvailableTimes.size());
        List<DerivedRequestableData> initResponses = new ArrayList<DerivedRequestableData>(
                thisAvailableTimes.size());

        for (DataTime time : thisAvailableTimes) {
            DerivedParameterRequest derivRequest = new DerivedParameterRequest();
            DerivedRequestableData newRecord = new DerivedRequestableData(
                    derivRequest);
            newRecord.setDataTime(time);
            derivRequest.setParameterAbbreviation(desc.getAbbreviation());
            derivRequest.setMethod(method.getName());
            // get data time for this field
            derivRequest.setBaseTime(time);
            initResponses.add(newRecord);
            mapOfRequests.put(time, derivRequest);
        }

        for (IDerivParamField ifield : method.getFields()) {
            if (fieldStaticData != null && fieldStaticData.containsKey(ifield)) {
                AbstractRequestableData data = fieldStaticData.get(ifield);
                if (ifield instanceof DerivParamField) {
                    data = checkFieldUnits((DerivParamField) ifield,
                            Arrays.asList(data)).get(0);
                }
                for (DerivedParameterRequest request : mapOfRequests.values()) {
                    request.addBaseParam(data);
                }
            } else if (fields != null && fields.containsKey(ifield)) {
                DerivParamField field = (DerivParamField) ifield;
                AbstractRequestableLevelNode fieldNode = fields.get(field);

                Collection<DataTime> availableTimes = myTimeCache.get(field);
                if (availableTimes == TIME_AGNOSTIC) {
                    List<AbstractRequestableData> responses = fieldNode
                            .getData(property, timeOut, cache);
                    responses = checkFieldUnits(field, responses);
                    for (DerivedParameterRequest request : mapOfRequests
                            .values()) {
                        for (AbstractRequestableData response : responses) {
                            if (response.getDataTime() == null
                                    || response.getDataTime().equals(
                                            response.getDataTime())) {
                                request.addBaseParam(response);
                                break;
                            }
                        }
                    }
                } else {

                    DataTime[] oldEntryTimes = property.getSelectedEntryTime();
                    Map<DataTime, DataTime> matchTimes = timeMatch(
                            mapOfRequests.keySet(), availableTimes, field);
                    Set<DataTime> newEntryTimes = new HashSet<DataTime>(
                            matchTimes.values());
                    property.setSelectedEntryTimes(newEntryTimes
                            .toArray(new DataTime[newEntryTimes.size()]));
                    List<AbstractRequestableData> responses = fieldNode
                            .getData(property, timeOut, cache);
                    property.setSelectedEntryTimes(oldEntryTimes);
                    responses = checkFieldUnits(field, responses);
                    Map<DataTime, AbstractRequestableData> responseMap = new HashMap<DataTime, AbstractRequestableData>();
                    for (AbstractRequestableData record : responses) {
                        responseMap.put(record.getDataTime(), record);
                    }
                    Iterator<Entry<DataTime, DerivedParameterRequest>> it = mapOfRequests
                            .entrySet().iterator();
                    while (it.hasNext()) {
                        Entry<DataTime, DerivedParameterRequest> entry = it
                                .next();
                        DataTime requestTime = matchTimes.get(entry.getKey());
                        AbstractRequestableData data = responseMap
                                .get(requestTime);
                        if (data != null) {
                            entry.getValue().addBaseParam(data);
                        } else {
                            it.remove();
                        }
                    }

                }
            } else {
                throw new VizException("Error processing Derived parameter:"
                        + desc.getAbbreviation() + ":" + method.getName() + ":"
                        + ifield.toString());
            }
        }

        List<AbstractRequestableData> finalResponses = new ArrayList<AbstractRequestableData>();
        for (DerivedRequestableData record : initResponses) {
            if (record.getRequest().getBaseParams().size() == method
                    .getFields().size()) {
                modifyRequest(record);
                finalResponses.add(record);
            }

        }
        return finalResponses;
    }

    /**
     * Given a set of times to load, and the available times for a field
     * generate a map from one to the other, taking into account dTime and fTime
     * 
     * @param timesToLaod
     * @param availableTimes
     * @param field
     * @return
     */
    private Map<DataTime, DataTime> timeMatch(Collection<DataTime> timesToLaod,
            Collection<DataTime> availableTimes, DerivParamField field) {
        // First step is to sort the available times by valid time
        Map<Long, Set<DataTime>> validMap = new HashMap<Long, Set<DataTime>>();
        for (DataTime time : availableTimes) {
            Long validTime = time.getMatchValid();
            Set<DataTime> timeSet = validMap.get(validTime);
            if (timeSet == null) {
                timeSet = new HashSet<DataTime>();
                validMap.put(validTime, timeSet);
            }
            timeSet.add(time);
        }
        // For each requested time we find the best available time
        Map<DataTime, DataTime> results = new HashMap<DataTime, DataTime>();
        int dTimeInSeconds = getDTimeInSeconds(field);

        if (dTimeInSeconds == -1) {
            return results;
        }
        for (DataTime entryTime : timesToLaod) {
            Long validTime = entryTime.getMatchValid() + dTimeInSeconds * 1000;
            if (!validMap.containsKey(validTime)) {
                // This means we have no data for this field at this
                // time
                continue;
            }
            DataTime latest = null;
            for (DataTime time : validMap.get(validTime)) {
                if (time.getMatchRef() == entryTime.getMatchRef()) {
                    // Prefer a matching ref time
                    latest = time;
                    break;
                } else if (!method.isFtime()
                        && (latest == null || latest.getMatchRef() < time
                                .getMatchRef())) {
                    latest = time;
                }
            }
            if (latest != null) {
                results.put(entryTime, latest);
            }
        }
        return results;
    }

    private int getDTimeInSeconds(DerivParamField field) {
        int dTimeInSeconds = 0;

        if (method.isDtime()) {
            dTimeInSeconds = field.getTimeShift();
            if (dt <= 0) {
                return -1;
            } else if (Math.abs(dTimeInSeconds) < 60) {
                dTimeInSeconds *= dt;
            }
        } else if (method.isFtime()) {
            dTimeInSeconds = field.getTimeShift();
        }
        return dTimeInSeconds;
    }

    private List<AbstractRequestableData> checkFieldUnits(
            DerivParamField field, List<AbstractRequestableData> records) {
        List<AbstractRequestableData> newRecs = new ArrayList<AbstractRequestableData>(
                records.size());
        for (AbstractRequestableData record : records) {
            if (record.getUnit() != null
                    && !record.getUnit().equals(field.getUnit())
                    && record.getUnit().isCompatible(field.getUnit())) {
                AbstractRequestableData alias = new AliasRequestableData(record);
                alias.setUnit(field.getUnit());
                newRecs.add(alias);
            } else {
                newRecs.add(record);
            }
        }
        return newRecs;
    }

    /**
     * Generates the possible data times for a dTime option.
     * 
     * @param dataTime
     *            The original data time
     * @param dTimeInSeconds
     *            The dTime offset in seconds
     * @param dTinSeconds
     *            The intrinsic time spacing of the current model.
     * @return A list of possible data times.
     */
    private List<DataTime> generatePossibleDataTimes(DataTime dataTime,
            DerivParamField field) {
        int dTimeInSeconds = getDTimeInSeconds(field);
        int sign = 1;
        if (dTimeInSeconds < 0) {
            dTimeInSeconds *= -1;
            sign = -1;
        }

        int fTimeInSeconds = dataTime.getFcstTime();
        long vTimeInMillis = dataTime.getValidTime().getTimeInMillis()
                - (dTimeInSeconds * 1000 * sign);

        List<DataTime> rval = new ArrayList<DataTime>(
                (dTimeInSeconds * 2 / dt) + 1);

        for (int fInSeconds = fTimeInSeconds - dTimeInSeconds; fInSeconds <= fTimeInSeconds
                + dTimeInSeconds; fInSeconds += dt) {
            if (fInSeconds >= 0) {
                rval.add(new DataTime(new Date(vTimeInMillis
                        - (fInSeconds * 1000)), fInSeconds));
            }
        }

        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    @Override
    public List<Dependency> getDependencies() {
        List<Dependency> dependencies = new ArrayList<Dependency>();
        if (fields != null) {
            for (Entry<DerivParamField, AbstractRequestableLevelNode> entry : fields
                    .entrySet()) {
                dependencies.add(new Dependency(entry.getValue(),
                        getDTimeInSeconds(entry.getKey())));
            }
        }
        if (fieldStaticData != null) {
            for (Entry<IDerivParamField, AbstractRequestableData> entry : fieldStaticData
                    .entrySet()) {
                StaticDataLevelNode node = new StaticDataLevelNode(level, desc,
                        entry.getValue(), modelName);
                dependencies.add(new Dependency(node, 0));
            }
        }
        return dependencies;
    }

    @Override
    public DerivedLevelNode clone() {
        return new DerivedLevelNode(this);
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
        result = prime * result
                + ((fieldStaticData == null) ? 0 : fieldStaticData.hashCode());
        result = prime * result + ((fields == null) ? 0 : fields.hashCode());
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
        DerivedLevelNode other = (DerivedLevelNode) obj;
        if (fieldStaticData == null) {
            if (other.fieldStaticData != null)
                return false;
        } else if (!fieldStaticData.equals(other.fieldStaticData))
            return false;
        if (fields == null) {
            if (other.fields != null)
                return false;
        } else if (!fields.equals(other.fields))
            return false;
        return true;
    }

}
