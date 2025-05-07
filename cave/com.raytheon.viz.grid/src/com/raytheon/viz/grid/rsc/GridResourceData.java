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

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.util.LevelUtilities;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.IUpdateHandlingResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.grid.inv.GribDataCubeAlertMessageParser;
import com.raytheon.viz.grid.inv.VizGridInventory;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;
import com.raytheon.viz.grid.rsc.general.DifferenceGridResourceData;

/**
 * Resource data for grids from GridRecords
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 12, 2009  1960     njensen   Initial creation
 * Jun 17, 2013  2107     bsteffen  Enable sampling by default for several
 *                                  display types.
 * Sep 03, 2015  4779     njensen   Removed DataScale references
 * Mar 03, 2016  5439     bsteffen  Rename inventory class
 * Aug 15, 2017  6332     bsteffen  Move radar specific logic to extension
 * Sep 09, 2021  8651     njensen   Implemented IUpdateHandlingResourceData
 *                                  and added keepDataWhileRetrievingUpdate
 * Oct 29, 2022  8959     mapeters  Update how data time levels are set
 * Nov 02, 2022  8963     mapeters  Prevent getAvailableTimes from returning
 *                                  duplicates
 * Feb 09, 2023  9011     mapeters  Update getLatestPluginDataObjects to return
 *                                  early if no data should be requested
 * Dec 20, 2023  2036519  mapeters  Records are now passed into resource constructor
 *                                  instead of being stored in resource data
 * Feb 29, 2024  2036955  mapeters  Prevent getLatestPluginDataObjects() from returning
 *                                  PDOs that don't match the requested times
 * Jul 15, 2024  2037624  mapeters  Add getExtraLegendText, make constructResource
 *                                  return type more specific
 *
 * </pre>
 *
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GridResourceData extends AbstractRequestableResourceData
        implements ICombinedResourceData, IUpdateHandlingResourceData {

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

    @XmlAttribute
    protected boolean keepDataWhileRetrievingUpdate = true;

    public GridResourceData() {
        setAlertParser(new GribDataCubeAlertMessageParser());
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if (secondaryResourceData != null) {
            // GridResource does not support diff through a secondary
            // resource, instead it must use a DifferenceGridResource.
            ResourcePair one = new ResourcePair();
            one.setResourceData(this);
            one.setLoadProperties(loadProperties);
            ResourcePair two = new ResourcePair();
            two.setResourceData(secondaryResourceData);
            two.setLoadProperties(loadProperties);
            this.secondaryResourceData = null;
            return new DifferenceGridResourceData(one, two)
                    .construct(loadProperties, descriptor);
        }
        return super.construct(loadProperties, descriptor);
    }

    @Override
    protected AbstractGridResource<GridResourceData> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        GridRecord[] records = new GridRecord[objects.length];
        for (int i = 0; i < objects.length; i++) {
            if (objects[i] instanceof GridRecord) {
                records[i] = (GridRecord) objects[i];
            } else {
                throw new IllegalArgumentException(
                        "GridResourceData only supports data for the grid plugin, the "
                                + objects[i].getPluginName()
                                + " plugin is not supported.");
            }
        }
        if (sampling == null) {
            if (loadProperties.getCapabilities()
                    .hasCapability(DisplayTypeCapability.class)) {
                DisplayType dType = loadProperties.getCapabilities()
                        .getCapability(this, DisplayTypeCapability.class)
                        .getDisplayType();
                if (dType == DisplayType.BARB || dType == DisplayType.CONTOUR
                        || dType == DisplayType.ICON) {
                    sampling = false;
                } else {
                    sampling = true;
                }
            } else {
                sampling = true;
            }
        }
        return new D2DGridResource(this, loadProperties, records);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((secondaryResourceData == null) ? 0
                : secondaryResourceData.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (this.getClass() != obj.getClass()) {
            return false;
        }

        GridResourceData otherObj = (GridResourceData) obj;

        if (!Objects.equals(this.secondaryResourceData,
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

    @Override
    public CombineOperation getCombineOperation() {
        return combinationOperation;
    }

    @Override
    public AbstractResourceData getSecondaryData() {
        return secondaryResourceData;
    }

    @Override
    public void setCombineOperation(CombineOperation operation) {
        combinationOperation = operation;
    }

    @Override
    public void setSecondaryData(AbstractResourceData data) {
        secondaryResourceData = (GridResourceData) data;
        ((ICombinedResourceData) secondaryResourceData)
                .setCombineOperation(CombineOperation.NONE);
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        if (!spatial) {
            return super.getAvailableTimes();
        }

        DataTime[] times = super.getAvailableTimes();
        Set<Level> levels = ((VizGridInventory) DataCubeContainer
                .getInventory(GridConstants.GRID))
                        .getAvailableLevels(metadataMap);
        /*
         * Using a set here because the super method can return times without
         * levels and the same times with levels. That would result in duplicate
         * times here which can cause time matching to use the same time/level
         * for multiple frames. The mixture of times occurs because the
         * superclass queries times that don't have levels and throws them in
         * cachedAvailableTimes, and then its update() methods add times with
         * levels to cachedAvailableTimes. This specifically occurred when
         * toggling Vertical Interaction on/off for a radar resource with
         * altitude (FHAG) levels.
         */
        Set<DataTime> timesWithLevels = new LinkedHashSet<>();
        for (DataTime time : times) {
            for (Level l : levels) {
                time = time.clone();
                LevelUtilities.setDataTimeLevel(time, l.getLevelonevalue(), l);
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
            return super.getLatestPluginDataObjects(desired, current);
        }

        if (desired == null || desired.length == 0 || !isRetrieveData()
                || !isRequeryNecessaryOnTimeMatch()) {
            return new PluginDataObject[0];
        }

        Set<DataTime> desiredSet = new HashSet<>(desired.length);
        Set<DataTime> stripped = new HashSet<>(desired.length);
        Double levelValue = null;
        for (DataTime desiredTime : desired) {
            if (desiredTime == null) {
                continue;
            }
            boolean found = false;
            for (DataTime currentTime : current) {
                if (desiredTime.equals(currentTime)) {
                    found = true;
                    break;
                }
            }
            if (found) {
                continue;
            }
            desiredSet.add(desiredTime);
            DataTime strip = desiredTime.clone();
            strip.clearLevel();
            stripped.add(strip);
            if (levelValue == null) {
                levelValue = desiredTime.getLevelValue();
            } else if (!levelValue.equals(desiredTime.getLevelValue())) {
                levelValue = Level.INVALID_VALUE;
            }
        }

        HashMap<String, RequestConstraint> originalMetadataMap = this.metadataMap;
        if (levelValue != null && !levelValue.equals(Level.INVALID_VALUE)) {
            this.metadataMap = new HashMap<>(this.metadataMap);
            this.metadataMap.put(GridConstants.LEVEL_ONE,
                    new RequestConstraint(levelValue.toString()));
        }
        PluginDataObject[] objs = super.getLatestPluginDataObjects(
                stripped.toArray(new DataTime[0]), new DataTime[0]);
        this.metadataMap = originalMetadataMap;
        for (PluginDataObject obj : objs) {
            GridRecord record = (GridRecord) obj;
            DataTime time = obj.getDataTime().clone();
            Level level = record.getLevel();
            LevelUtilities.setDataTimeLevel(time, level.getLevelonevalue(),
                    level);
            obj.setDataTime(time);
        }

        /*
         * We may not want all levels for all times, so filter to only the
         * desired time/level combinations
         */
        objs = Arrays.stream(objs)
                .filter(pdo -> desiredSet.contains(pdo.getDataTime()))
                .toArray(PluginDataObject[]::new);

        return objs;
    }

    @Override
    public AbstractVizResource<?, ?> getSecondaryResource() {
        return null;
    }

    @Override
    public void handleUpdate(AlertMessage message) {
        DataTime time = (DataTime) message.decodedAlert.get("dataTime");
        if (this.keepDataWhileRetrievingUpdate) {
            this.fireChangeListeners(ChangeType.DATA_UPDATE, time);
        } else {
            this.fireChangeListeners(ChangeType.DATA_REMOVE, time);
        }
    }

    /**
     * Get text to append to the legend for the given grid record.
     *
     * @param record
     *            grid record
     * @return extra legend text
     */
    public String getExtraLegendText(GridRecord record) {
        return "";
    }

    public boolean isKeepDataWhileRetrievingUpdate() {
        return keepDataWhileRetrievingUpdate;
    }

    public void setKeepDataWhileRetrievingUpdate(
            boolean keepDataWhileRetrievingUpdate) {
        this.keepDataWhileRetrievingUpdate = keepDataWhileRetrievingUpdate;
    }
}