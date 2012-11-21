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
import java.util.Collections;
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
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AliasRequestableData;
import com.raytheon.uf.viz.derivparam.data.DerivedRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpaceMatcher;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpaceMatcher.MatchResult;
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

public class DerivedLevelNode extends AbstractDerivedDataNode {

    private Map<IDerivParamField, AbstractRequestableData> fieldStaticData = null;

    private Map<DerivParamField, AbstractRequestableNode> fields = null;

    private int dt;

    public DerivedLevelNode(Level level, DerivParamDesc desc,
            DerivParamMethod method, String modelName, int dt) {
        super(level, desc, method, modelName);
        this.dt = dt;
    }

    public DerivedLevelNode(DerivedLevelNode that) {
        super(that);
        if (that.fields != null) {
            fields = new HashMap<DerivParamField, AbstractRequestableNode>(
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
    public Map<DerivParamField, AbstractRequestableNode> getFields() {
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

    public void putField(DerivParamField field, AbstractRequestableNode object) {
        if (fields == null) {
            fields = new HashMap<DerivParamField, AbstractRequestableNode>();
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

    private TimeAndSpaceMatcher getMatcher() {
        TimeAndSpaceMatcher matcher = new TimeAndSpaceMatcher();
        matcher.setIgnoreRange(true);
        if (method.isFtime()) {
            matcher.setMatchValid(false);
        } else {
            matcher.setMatchValid(true);
        }
        return matcher;
    }

    private Map<TimeAndSpace, TimeAndSpace> shiftTime(DerivParamField field,
            Set<TimeAndSpace> dataTimes) {
        int timeShift = getDTimeInSeconds(field);
        if (timeShift != 0) {
            Map<TimeAndSpace, TimeAndSpace> result = new HashMap<TimeAndSpace, TimeAndSpace>();
            for (TimeAndSpace t : dataTimes) {
                if (t.isTimeAgnostic()) {
                    result.put(t, t);
                } else {
                    DataTime time = t.getTime();
                    long rTime = time.getRefTime().getTime();
                    int fTime = time.getFcstTime();
                    // Shift valid time back rather than having a
                    // negative forecast time.
                    fTime -= timeShift;
                    while (method.isDtime() && fTime < 0) {
                        rTime -= dt * 1000;
                        fTime += dt;
                    }
                    time = new DataTime(new Date(rTime), fTime);
                    result.put(new TimeAndSpace(time, t.getSpace()), t);
                }
            }
            return result;
        }
        return null;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws VizException {
        TimeAndSpaceMatcher matcher = getMatcher();
        // We have a derived parameter for the requested grid
        Set<TimeAndSpace> availableDataTimes = null;
        if (fields == null) {
            availableDataTimes = AvailabilityContainer.AGNOSTIC_SET;
            return availableDataTimes;
        }
        for (DerivParamField field : fields.keySet()) {
            AbstractRequestableNode node = fields.get(field);
            Set<TimeAndSpace> queryDataTimes = availability.get(node);
            if (queryDataTimes != null && queryDataTimes.size() > 0) {
                Map<TimeAndSpace, TimeAndSpace> shiftMap = shiftTime(field,
                        queryDataTimes);
                if (shiftMap != null) {
                    queryDataTimes = shiftMap.keySet();
                }
                if (availableDataTimes == null) {
                    availableDataTimes = AvailabilityContainer.AGNOSTIC_SET;
                }
                availableDataTimes = matcher.match(availableDataTimes,
                        queryDataTimes).keySet();
            } else {
                // no data for this query, clear times and return
                availableDataTimes = null;
                break;
            }

            if (availableDataTimes == null || availableDataTimes.size() == 0) {
                break;
            }
        }// FIELD_LOOP

        if (availableDataTimes == null) {
            availableDataTimes = new HashSet<TimeAndSpace>(0);
        }
        return availableDataTimes;
    }

    @Override
    public Map<AbstractRequestableNode, Set<TimeAndSpace>> getDataDependency(
            Set<TimeAndSpace> availability,
            AvailabilityContainer availabilityContainer) throws VizException {
        TimeAndSpaceMatcher matcher = getMatcher();
        availability = matcher.match(availability,
                availabilityContainer.getAvailability(this)).keySet();
        if (availability.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<AbstractRequestableNode, Set<TimeAndSpace>> result = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>();
        if (fields != null) {
            for (Entry<DerivParamField, AbstractRequestableNode> field : fields
                    .entrySet()) {
                Set<TimeAndSpace> queryTimes = availabilityContainer
                        .getAvailability(field.getValue());
                Map<TimeAndSpace, TimeAndSpace> shiftMap = shiftTime(
                        field.getKey(), queryTimes);
                if (shiftMap != null) {
                    queryTimes = shiftMap.keySet();
                }
                Set<TimeAndSpace> fieldResult = TimeAndSpaceMatcher
                        .getAll2(matcher.match(availability, queryTimes));
                if (shiftMap != null) {
                    Set<TimeAndSpace> newFieldResult = new HashSet<TimeAndSpace>();
                    for (TimeAndSpace t : fieldResult) {
                        newFieldResult.add(shiftMap.get(t));
                    }
                    fieldResult = newFieldResult;
                }
                Set<TimeAndSpace> oldSet = result.put(field.getValue(),
                        fieldResult);
                if (oldSet != null) {
                    fieldResult.addAll(oldSet);
                }
            }
        }
        if (fieldStaticData != null) {
            for (Entry<IDerivParamField, AbstractRequestableData> entry : fieldStaticData
                    .entrySet()) {
                StaticDataLevelNode node = new StaticDataLevelNode(level, desc,
                        entry.getValue(), modelName);
                result.put(node, AvailabilityContainer.AGNOSTIC_SET);
            }
        }
        return result;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        Map<AbstractRequestableNode, Set<TimeAndSpace>> availCache = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>(
                (int) (dependencyData.size() / 0.75) + 1, 0.75f);
        for (AbstractRequestableNode node : fields.values()) {
            Set<AbstractRequestableData> dataSet = dependencyData.get(node);
            Set<TimeAndSpace> tas = new HashSet<TimeAndSpace>(
                    (int) (dataSet.size() / 0.75) + 1, 0.75f);
            for (AbstractRequestableData data : dataSet) {
                tas.add(data.getTimeAndSpace());
            }
            availCache.put(node, tas);
        }
        TimeAndSpaceMatcher matcher = getMatcher();
        availability = matcher.match(availability, getAvailability(availCache))
                .keySet();
        if (availability.isEmpty()) {
            return Collections.emptySet();
        }
        Map<TimeAndSpace, DerivedParameterRequest> mapOfRequests = new HashMap<TimeAndSpace, DerivedParameterRequest>(
                availability.size());
        List<DerivedRequestableData> initResponses = new ArrayList<DerivedRequestableData>(
                availability.size());

        for (TimeAndSpace ast : availability) {
            DerivedParameterRequest derivRequest = new DerivedParameterRequest();
            DerivedRequestableData newRecord = new DerivedRequestableData(
                    derivRequest);
            newRecord.setDataTime(ast.getTime());
            newRecord.setSpace(ast.getSpace());
            derivRequest.setParameterAbbreviation(desc.getAbbreviation());
            derivRequest.setMethod(method.getName());
            // get data time for this field
            derivRequest.setBaseTime(ast.getTime());
            initResponses.add(newRecord);
            mapOfRequests.put(ast, derivRequest);
        }

        for (IDerivParamField ifield : method.getFields()) {
            if (fieldStaticData != null && fieldStaticData.containsKey(ifield)) {
                AbstractRequestableData data = fieldStaticData.get(ifield);
                if (ifield instanceof DerivParamField) {
                    data = checkFieldUnits((DerivParamField) ifield,
                            Arrays.asList(data)).iterator().next();
                }
                for (DerivedParameterRequest request : mapOfRequests.values()) {
                    request.addBaseParam(data);
                }
            } else if (fields != null && fields.containsKey(ifield)) {
                DerivParamField field = (DerivParamField) ifield;
                AbstractRequestableNode fieldNode = fields.get(field);

                Set<TimeAndSpace> fieldAvailability = availCache.get(fieldNode);
                Map<TimeAndSpace, TimeAndSpace> shiftMap = shiftTime(field,
                        fieldAvailability);
                if (shiftMap != null) {
                    fieldAvailability = shiftMap.keySet();
                }
                Map<TimeAndSpace, MatchResult> matchTimes = matcher.match(
                        availability, fieldAvailability);
                Collection<AbstractRequestableData> responses = dependencyData
                        .get(fieldNode);
                responses = checkFieldUnits(field, responses);
                Map<TimeAndSpace, AbstractRequestableData> responseMap = new HashMap<TimeAndSpace, AbstractRequestableData>();
                for (AbstractRequestableData record : responses) {
                    responseMap.put(record.getTimeAndSpace(), record);
                }
                Iterator<Entry<TimeAndSpace, DerivedParameterRequest>> it = mapOfRequests
                        .entrySet().iterator();
                while (it.hasNext()) {
                    Entry<TimeAndSpace, DerivedParameterRequest> entry = it
                            .next();
                    TimeAndSpace requestTime = matchTimes.get(entry.getKey())
                            .get2();
                    if (shiftMap != null) {
                        requestTime = shiftMap.get(requestTime);
                    }
                    AbstractRequestableData data = responseMap.get(requestTime);
                    if (data != null) {
                        entry.getValue().addBaseParam(data);
                    } else {
                        it.remove();
                    }
                }

            } else {
                throw new VizException("Error processing Derived parameter:"
                        + desc.getAbbreviation() + ":" + method.getName() + ":"
                        + ifield.toString());
            }
        }

        Set<AbstractRequestableData> finalResponses = new HashSet<AbstractRequestableData>();
        for (DerivedRequestableData record : initResponses) {
            if (record.getRequest().getBaseParams().size() == method
                    .getFields().size()) {
                modifyRequest(record);
                finalResponses.add(record);
            }

        }
        return finalResponses;
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

    private Collection<AbstractRequestableData> checkFieldUnits(
            DerivParamField field, Collection<AbstractRequestableData> records) {
        Set<AbstractRequestableData> newRecs = new HashSet<AbstractRequestableData>(
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
            for (Entry<DerivParamField, AbstractRequestableNode> entry : fields
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
