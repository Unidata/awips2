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
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridIconResource extends
        AbstractVizResource<GridResourceData, MapDescriptor> implements
        IResourceDataChanged, IGridNameResource {

    private static final int imageSize = 80;

    private Map<DataTime, GriddedIconDisplay> displays = new HashMap<DataTime, GriddedIconDisplay>();

    private Map<DataTime, GribRecord> records = new HashMap<DataTime, GribRecord>();

    private DataTime currTime;

    public GridIconResource(GridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        this.dataTimes = new ArrayList<DataTime>();
        for (GribRecord record : resourceData.getRecords()) {
            addRecord(record);
        }
        if (resourceData.getNameGenerator() == null) {
            resourceData.setNameGenerator(new GridNameGenerator());
        }
        resourceData.addChangeListener(this);
    }

    @Override
    protected void disposeInternal() {
        for (GriddedIconDisplay display : displays.values()) {
            display.dispose();
        }
        displays.clear();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        currTime = paintProps.getDataTime();
        GriddedIconDisplay display = displays.get(currTime);
        if (display == null) {
            GribRecord record = records.get(paintProps.getDataTime());
            if (record == null) {
                return;
            }
            if (!(record.getMessageData() instanceof float[])) {
                IDataRecord[] dataRecord = DataCubeContainer
                        .getDataRecord(record);
                record.setMessageData(dataRecord[0].getDataObject());
            }
            float[] values = (float[]) record.getMessageData();
            display = new GriddedIconDisplay(values, descriptor, record
                    .getModelInfo().getLocation().getGridGeometry(), imageSize);
            display.setColor(getCapability(ColorableCapability.class)
                    .getColor());
            display.setDensity(getCapability(DensityCapability.class)
                    .getDensity());
            display.setMagnification(getCapability(
                    MagnificationCapability.class).getMagnification());
            displays.put(currTime, display);
        }

        display.paint(target, paintProps);
    }

    public LegendParameters getLegendParameters() {
        GribRecord record = records.get(currTime);
        if (record == null) {
            record = resourceData.getRecords()[0];
        }
        LegendParameters legendParams = new LegendParameters();
        legendParams.model = resourceData.getModelInfo();
        legendParams.unit = resourceData.getModelInfo().getParameterUnit();
        legendParams.dataTime = record.getDataTime();
        legendParams.type = "Icons";
        return legendParams;
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject pdo : pdos) {
                addRecord(pdo);
            }
        } else if (type == ChangeType.CAPABILITY) {
            if (object instanceof DensityCapability) {
                Double density = ((DensityCapability) object).getDensity();
                for (GriddedIconDisplay display : displays.values()) {
                    display.setDensity(density);
                }
            } else if (object instanceof ColorableCapability) {
                RGB color = ((ColorableCapability) object).getColor();
                for (GriddedIconDisplay display : displays.values()) {
                    display.setColor(color);
                }
            } else if (object instanceof MagnificationCapability) {
                double mag = ((MagnificationCapability) object)
                        .getMagnification();
                for (GriddedIconDisplay display : displays.values()) {
                    display.setMagnification(mag);
                }
            }
        }
        issueRefresh();
    }

    private void addRecord(PluginDataObject pdo) {
        if (pdo instanceof GribRecord) {
            records.put(pdo.getDataTime(), (GribRecord) pdo);
            if (!this.dataTimes.contains(pdo.getDataTime())) {
                this.dataTimes.add(pdo.getDataTime());
            }
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        for (GriddedIconDisplay display : displays.values()) {
            display.reproject();
        }
        super.project(crs);
    }

    @Override
    public void remove(DataTime dataTime) {
        records.remove(dataTime);
        GriddedIconDisplay display = displays.remove(dataTime);
        if (display != null) {
            display.dispose();
        }
        dataTimes.remove(dataTime);
    }

}