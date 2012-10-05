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
package com.raytheon.viz.grid.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.CombinedGribRecord;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.time.CombinedDataTime;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.d2d.core.map.IDataScaleResource;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.uf.viz.d2d.core.time.TimeMatcher;
import com.raytheon.viz.core.rsc.ICombinedResourceData;
import com.raytheon.viz.grid.inv.GribDataCubeAlertMessageParser;
import com.raytheon.viz.grid.inv.GridInventory;
import com.raytheon.viz.grid.rsc.general.D2DGribGridResource;
import com.raytheon.viz.grid.rsc.general.DifferenceGridResourceData;
import com.raytheon.viz.grid.util.TiltRequest;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource data for grids from GribRecords
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GridResourceData extends AbstractRequestableResourceData implements
        ICombinedResourceData {

    protected GribRecord[] records;

    @XmlElement
    protected GridResourceData secondaryResourceData;

    @XmlAttribute
    protected CombineOperation combinationOperation;

    @XmlAttribute
    protected String customLegend;

    @XmlAttribute
    protected Boolean sampling;

    @XmlAttribute
    protected boolean spatial = false;

    private DataTime[] dataTimes;

    private GribModel modelInfo;

    public GridResourceData() {
        setAlertParser(new GribDataCubeAlertMessageParser());
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        DisplayType displayType = loadProperties.getCapabilities()
                .getCapability(this, DisplayTypeCapability.class)
                .getDisplayType();
        if (secondaryResourceData != null
                && (displayType == DisplayType.BARB
                        || displayType == DisplayType.ARROW
                        || displayType == DisplayType.DUALARROW || displayType == DisplayType.STREAMLINE)) {
            // GribGridResource does not support diff through a secondary
            // resource, instead it must use a DifferenceGridResource.
            ResourcePair one = new ResourcePair();
            one.setResourceData(this);
            one.setLoadProperties(loadProperties);
            ResourcePair two = new ResourcePair();
            two.setResourceData(secondaryResourceData);
            two.setLoadProperties(loadProperties);
            this.secondaryResourceData = null;
            return new DifferenceGridResourceData(one, two).construct(
                    loadProperties, descriptor);
        }
        return super.construct(loadProperties, descriptor);
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        records = new GribRecord[objects.length];
        for (int i = 0; i < objects.length; i++) {
            records[i] = (GribRecord) objects[i];
        }
        switch (loadProperties.getCapabilities()
                .getCapability(this, DisplayTypeCapability.class)
                .getDisplayType()) {
        case IMAGE:
            sampling = sampling == null ? true : sampling;
            return new GridResource(this, loadProperties);
        case ICON:
            sampling = sampling == null ? false : sampling;
            return new GridIconResource(this, loadProperties);
        case BARB:
            sampling = sampling == null ? false : sampling;
        case ARROW:
        case DUALARROW:
        case STREAMLINE:
            // TODO eventually contour and image should also use
            // D2DGribGridResource so that all data requesta nd transform of
            // grib data in D2D is in one location. There are only a few
            // products that do not work correctly, contours of vector
            // direction, and data mapped images.
            sampling = sampling == null ? true : sampling;
            return new D2DGribGridResource(this, loadProperties);
        case CONTOUR:
        default:
            sampling = sampling == null ? false : sampling;
            return new GridVectorResource(this, loadProperties);
        }
    }

    /**
     * @return the records
     */
    public GribRecord[] getRecords() {
        return records;
    }

    /**
     * @param records
     *            the records to set
     */
    public void setRecords(GribRecord[] records) {
        this.records = records;
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof GridResourceData == false) {
            return false;
        }

        GridResourceData otherObj = (GridResourceData) obj;

        if (!isObjectsEqual(this.secondaryResourceData,
                otherObj.secondaryResourceData)) {
            return false;
        }

        return true;
    }

    /**
     * @return the customLegend
     */
    public String getCustomLegend() {
        return customLegend;
    }

    /**
     * @param customLegend
     *            the customLegend to set
     */
    public void setCustomLegend(String customLegend) {
        this.customLegend = customLegend;
    }

    /**
     * @return the sampling
     */
    public boolean isSampling() {
        return sampling == null ? false : sampling;
    }

    /**
     * @return the spatial
     */
    public boolean isSpatial() {
        return spatial;
    }

    /**
     * @param spatial
     *            the spatial to set
     */
    public void setSpatial(boolean spatial) {
        this.spatial = spatial;
    }

    /**
     * @param sampling
     *            the sampling to set
     */
    public void setSampling(boolean sampling) {
        this.sampling = sampling;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.ICombinedResourceData#getCombineOperation()
     */
    @Override
    public CombineOperation getCombineOperation() {
        return combinationOperation;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.ICombinedResourceData#getSecondaryData()
     */
    @Override
    public AbstractResourceData getSecondaryData() {
        return secondaryResourceData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.ICombinedResourceData#setCombineOperation(com
     * .raytheon.viz.core.rsc.ICombinableResource.CombineOperation)
     */
    @Override
    public void setCombineOperation(CombineOperation operation) {
        combinationOperation = operation;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.ICombinedResourceData#setSecondaryData(com.
     * raytheon.uf.viz.core.rsc.AbstractResourceData)
     */
    @Override
    public void setSecondaryData(AbstractResourceData data) {
        secondaryResourceData = (GridResourceData) data;
        ((ICombinedResourceData) secondaryResourceData)
                .setCombineOperation(CombineOperation.NONE);
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        if (!spatial) {
            if (secondaryResourceData != null) {
                DataTime[] secondaryTimes = secondaryResourceData
                        .getAvailableTimes();
                DataTime[] primaryTimes = super.getAvailableTimes();
                if (primaryTimes == null || secondaryTimes == null) {
                    return null;
                }
                if (primaryTimes.length > secondaryTimes.length) {
                    secondaryTimes = TimeMatcher.doValTimOverlay(
                            secondaryTimes, primaryTimes, 0,
                            LoadMode.VALID_TIME_SEQ, null, 0.5F);

                } else {
                    primaryTimes = TimeMatcher.doValTimOverlay(primaryTimes,
                            secondaryTimes, 0, LoadMode.VALID_TIME_SEQ, null,
                            0.5F);

                }
                List<DataTime> availDataTimes = new ArrayList<DataTime>();
                for (int i = 0; i < primaryTimes.length; i++) {
                    if (primaryTimes[i] != null && secondaryTimes[i] != null) {
                        availDataTimes.add(new CombinedDataTime(
                                primaryTimes[i], secondaryTimes[i]));
                    }
                }
                return availDataTimes.toArray(new DataTime[availDataTimes
                        .size()]);
            }

            return super.getAvailableTimes();
        }

        DataTime[] times = super.getAvailableTimes();
        Set<Level> levels = ((GridInventory) DataCubeContainer
                .getInventory("grib")).getAvailableLevels(metadataMap);
        List<DataTime> timesWithLevels = new ArrayList<DataTime>();
        for (int i = 0; i < times.length; ++i) {
            for (Level l : levels) {
                DataTime time = times[i].clone();
                time.setLevelValue(l.getLevelonevalue());
                if (time.isSpatial()) {
                    timesWithLevels.add(time);
                }
            }
        }
        return timesWithLevels.toArray(new DataTime[timesWithLevels.size()]);
    }

    @Override
    public PluginDataObject[] getLatestPluginDataObjects(DataTime[] desired,
            DataTime[] current) throws VizException {
        if (!spatial) {
            if (secondaryResourceData != null) {
                return getCombinedPluginDataObjects(desired, current);
            }
            return super.getLatestPluginDataObjects(desired, current);
        }
        Set<DataTime> stripped = new HashSet<DataTime>(desired.length);
        Double levelValue = null;
        for (int i = 0; i < desired.length; ++i) {
            if (desired[i] == null) {
                continue;
            }
            boolean found = false;
            for (int j = 0; j < current.length; ++j) {
                if (desired[i].equals(current[j])) {
                    found = true;
                    break;
                }
            }
            if (found) {
                continue;
            }
            DataTime strip = desired[i].clone();
            strip.setLevelValue(null);
            stripped.add(strip);
            if (levelValue == null) {
                levelValue = desired[i].getLevelValue();
            } else if (levelValue != desired[i].getLevelValue()) {
                levelValue = -1.0;
            }
        }

        HashMap<String, RequestConstraint> originalMetadataMap = this.metadataMap;
        if (levelValue != null && levelValue != -1) {
            this.metadataMap = new HashMap<String, RequestConstraint>(
                    this.metadataMap);
            this.metadataMap.put(GridInventory.LEVEL_ONE_QUERY,
                    new RequestConstraint(levelValue.toString()));
        }
        PluginDataObject[] objs;
        if (secondaryResourceData != null) {
            objs = getCombinedPluginDataObjects(desired, current);
        } else {
            objs = super.getLatestPluginDataObjects(
                    stripped.toArray(new DataTime[0]), new DataTime[0]);
        }
        this.metadataMap = originalMetadataMap;
        for (PluginDataObject obj : objs) {
            GribRecord record = (GribRecord) obj;
            DataTime time = obj.getDataTime().clone();
            time.setLevelValue(record.getModelInfo().getLevelOneValue());
            obj.setDataTime(time);
        }
        return objs;
    }

    private CombinedGribRecord[] getCombinedPluginDataObjects(
            DataTime[] desired, DataTime[] current) throws VizException {

        DataTime[] currentPrimaryDataTimes = new DataTime[current.length];
        DataTime[] desiredPrimaryDataTimes = new DataTime[desired.length];

        DataTime[] currentSecondaryDataTimes = new DataTime[current.length];
        DataTime[] desiredSecondaryDataTimes = new DataTime[desired.length];

        for (int i = 0; i < current.length; i++) {
            if (!(current[i] instanceof CombinedDataTime)) {
                current = dataTimes;
                break;
            }
        }
        for (int i = 0; i < desired.length; i++) {
            if (!(desired[i] instanceof CombinedDataTime)) {
                desired = getAvailableTimes();
                break;
            }
        }

        for (int i = 0; i < current.length; i++) {
            currentPrimaryDataTimes[i] = ((CombinedDataTime) current[i])
                    .getPrimaryDataTime();
            currentSecondaryDataTimes[i] = ((CombinedDataTime) current[i])
                    .getAdditionalDataTime();
        }

        for (int i = 0; i < desired.length; i++) {
            desiredPrimaryDataTimes[i] = ((CombinedDataTime) desired[i])
                    .getPrimaryDataTime();
            desiredSecondaryDataTimes[i] = ((CombinedDataTime) desired[i])
                    .getAdditionalDataTime();
        }

        PluginDataObject[] primaryPdos = super.getLatestPluginDataObjects(
                desiredPrimaryDataTimes, currentPrimaryDataTimes);

        secondaryResourceData.dataTimes = desiredSecondaryDataTimes;
        PluginDataObject[] secondaryPdos = secondaryResourceData
                .getLatestPluginDataObjects(desiredSecondaryDataTimes,
                        currentSecondaryDataTimes);
        // Combine
        List<CombinedGribRecord> combinedGribRecords = new ArrayList<CombinedGribRecord>();
        for (int i = 0; i < desired.length; i++) {
            boolean found = false;
            for (PluginDataObject primaryPdo : primaryPdos) {
                if (found) {
                    break;
                }
                if (((CombinedDataTime) desired[i]).equals(primaryPdo
                        .getDataTime())) {
                    for (PluginDataObject secondaryPdo : secondaryPdos) {
                        if (((CombinedDataTime) desired[i])
                                .getAdditionalDataTime().equals(
                                        secondaryPdo.getDataTime())) {
                            // primary and secondary match the datatimes
                            // now combine them
                            CombinedGribRecord combinedGribRecord = new CombinedGribRecord(
                                    (GribRecord) primaryPdo,
                                    (GribRecord) secondaryPdo);
                            combinedGribRecord.setDataTime(desired[i]);
                            combinedGribRecords.add(combinedGribRecord);
                            found = true;
                            break;
                        }
                    }
                }
            }
        }
        return combinedGribRecords
                .toArray(new CombinedGribRecord[combinedGribRecords.size()]);
    }

    /**
     * Gets the model information
     * 
     * @return The model information
     */
    public GribModel getModelInfo() {
        if (this.modelInfo != null) {
            return this.modelInfo;
        }
        for (GribRecord record : records) {
            if (record.getModelInfo() != null) {
                this.modelInfo = record.getModelInfo();
                return this.modelInfo;
            }
        }
        return null;
    }

    @Override
    public AbstractVizResource<?, ?> getSecondaryResource() {
        // TODO Auto-generated method stub
        return null;
    }

    public static IDataRecord[] getDataRecordsForTilt(GribRecord record,
            IDescriptor descriptor) throws VizException {
        if (record.getModelInfo().getLevel().getMasterLevel().getName()
                .equals("TILT")) {
            Coordinate tiltLoc = findTiltLocation(descriptor.getResourceList());
            if (tiltLoc != null) {
                TiltRequest request = new TiltRequest();
                request.setType(Request.Type.ALL);
                request.setTiltLocation(tiltLoc);
                return DataCubeContainer.getDataRecord(record, request, null);
            }
        }
        return null;

    }

    private static Coordinate findTiltLocation(ResourceList resourceList) {
        for (ResourcePair rp : resourceList) {
            AbstractResourceData resourceData = rp.getResourceData();
            AbstractVizResource<?, ?> resource = rp.getResource();
            if (resource instanceof IDataScaleResource) {
                return ((IDataScaleResource) resource).getCenterLocation();
            }
            if (resourceData instanceof IResourceGroup) {
                Coordinate tiltLoc = findTiltLocation(((IResourceGroup) resourceData)
                        .getResourceList());
                if (tiltLoc != null) {
                    return tiltLoc;
                }
            }
        }
        return null;
    }
}
