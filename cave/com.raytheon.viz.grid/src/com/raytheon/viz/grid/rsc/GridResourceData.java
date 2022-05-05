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
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
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
 * 
 * </pre>
 * 
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GridResourceData extends AbstractRequestableResourceData
        implements ICombinedResourceData {

    protected GridRecord[] records;

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
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        records = new GridRecord[objects.length];
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
        return new D2DGridResource(this, loadProperties);
    }

    /**
     * @return the records
     */
    public GridRecord[] getRecords() {
        return records;
    }

    /**
     * @param records
     *            the records to set
     */
    public void setRecords(GridRecord[] records) {
        this.records = records;
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
            return super.getAvailableTimes();
        }

        DataTime[] times = super.getAvailableTimes();
        Set<Level> levels = ((VizGridInventory) DataCubeContainer
                .getInventory(GridConstants.GRID))
                        .getAvailableLevels(metadataMap);
        List<DataTime> timesWithLevels = new ArrayList<>();
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
            return super.getLatestPluginDataObjects(desired, current);
        }
        Set<DataTime> stripped = new HashSet<>(desired.length);
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
            time.setLevelValue(record.getLevel().getLevelonevalue());
            obj.setDataTime(time);
        }
        return objs;
    }

    @Override
    public AbstractVizResource<?, ?> getSecondaryResource() {
        // TODO Auto-generated method stub
        return null;
    }

}
