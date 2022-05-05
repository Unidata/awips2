/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.viz.dataplugin.nswrc;

import java.awt.Rectangle;
import java.io.File;
import java.io.FileNotFoundException;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCConstants;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCDataRetriever;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCRadialRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension.RadialMeshData;
import org.locationtech.jts.geom.Coordinate;

/**
 * Provides a NetCDF Radar implementation for creating visualizations.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013            ekladstrup  Initial creation
 * Apr 21, 2014 3048       mweeks      Updates for peer review and 13.5.4 baseline.
 * Aug 25, 2014 3555       mweeks      Updates for 14.3.1 baseline.
 * May 28, 2015 4524       bsteffen    Updates for 14.4.1 baseline
 * Dec 01, 2017 5863       mapeters    Change dataTimes to a NavigableSet
 *
 * </pre>
 *
 * @author ekladstrup
 */

public class NSWRCVizResource
        extends AbstractVizResource<NSWRCVizResourceData, MapDescriptor> {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NSWRCVizResource.class);

    private static final String NO_DATA = "No Data";

    protected RadialDataInterrogator interrogator = null;

    protected Map<DataTime, DrawableImage> images = new ConcurrentHashMap<DataTime, DrawableImage>();

    protected NSWRCVizResource(NSWRCVizResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) updateObject;
            for (PluginDataObject pdo : pdos) {
                this.dataTimes.add(pdo.getDataTime());
            }
        }
    }

    @Override
    protected void disposeInternal() {
        synchronized (this.images) {
            for (DrawableImage image : images.values()) {
                disposeImage(image);
            }
            images.clear();
        }
    }

    protected void disposeImage(DrawableImage image) {
        if (image != null) {
            image.dispose();
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        final DataTime newPaintTime = paintProps.getDataTime();
        if (newPaintTime == null) {
            return;
        }

        NSWRCRadialRecord record = this.resourceData.getRecord(newPaintTime);
        if (record == null) {
            return;
        }

        if (interrogator == null) {
            interrogator = new RadialDataInterrogator(record);
        } else {
            interrogator.setRecord(record);
        }

        DrawableImage drawable = null;
        synchronized (this.images) {
            if (images.containsKey(newPaintTime)) {
                drawable = images.get(newPaintTime);
            } else {
                drawable = buildImage(record, target, paintProps);
                if (drawable != null) {
                    images.put(newPaintTime, drawable);
                }
            }
        }

        if (drawable != null) {
            ImagingCapability cap = getCapability(ImagingCapability.class);
            drawable.getImage().setBrightness(cap.getBrightness());
            drawable.getImage().setContrast(cap.getContrast());
            drawable.getImage().setInterpolated(cap.isInterpolationState());
            target.drawRasters(paintProps, drawable);
        } else {
            issueRefresh();
        }
    }

    protected DrawableImage buildImage(NSWRCRadialRecord record,
            IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        try {
            File loc = HDF5Util.findHDF5Location(record);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);

            NSWRCDataRetriever.populateRadarRecord(dataStore, record);
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to open data file",
                    e);
        } catch (StorageException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to get radar data",
                    e);
        }

        ColorMapParameters params = this.getColorMapParameters(target, record);

        Rectangle rect = new Rectangle(0, 0, record.getNumBins(),
                record.getNumRadials());

        IColormappedImage raster = target
                .getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new FilteredRadarRadialDataRetriever(record, rect),
                        params);

        PixelCoverage coverage = this.buildCoverage(target, record);

        DrawableImage image = new DrawableImage(raster, coverage);

        return image;
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        for (Map.Entry<DataTime, DrawableImage> entry : this.images
                .entrySet()) {
            entry.getValue().dispose();
        }
        this.images.clear();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // Nothing to do
    }

    /**
     * Get the colormap parameters, expects a radar record populated with data
     *
     * @param target
     * @param record
     * @return
     * @throws VizException
     */
    protected ColorMapParameters getColorMapParameters(IGraphicsTarget target,
            NSWRCRadialRecord record) throws VizException {
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        String colorMapName = "";
        IColorMap colorMap = null;
        if (params != null && params.getDataUnit() != null) {
            return params;
        } else if (params != null) {
            colorMapName = params.getColorMapName();
            colorMap = params.getColorMap();
        }

        try {
            // Setup the ColorMap settings
            params = ColorMapParameterFactory.build(record.getData(),
                    record.getProductName(), record.getUnitObject(),
                    (SingleLevel) null, (String) null);

            if (params != null && params.getDisplayUnit() == null) {
                params.setDisplayUnit(record.getUnitObject());
            }
        } catch (StyleException se) {
            throw new VizException(se);
        }

        getCapability(ColorMapCapability.class).setColorMapParameters(params);

        if (colorMap != null) {
            params.setColorMap(colorMap);
            params.setColorMapName(colorMapName);
        }

        if (params.getColorMap() == null) {
            if (colorMapName.isEmpty()) {
                colorMapName = params.getColorMapName();
            }
            if (colorMapName == null) {
                colorMapName = "Radar/OSF/256 Level Reflectivity";
                params.setColorMapMax(215);
                params.setColorMapMin(-40);
            }

            try {
                params.setColorMap(ColorMapLoader.loadColorMap(colorMapName));
            } catch (ColorMapException e) {
                throw new VizException("Cannot load colorMap: " + colorMapName,
                        e);
            }
        }

        return params;
    }

    protected PixelCoverage buildCoverage(IGraphicsTarget target,
            NSWRCRadialRecord timeRecord) throws VizException {
        // should we store coverage?
        PixelCoverage coverage = new PixelCoverage(new Coordinate(0, 0), 0, 0);

        // we are essentially in Radial mode, not Raster
        RadialMeshData meshData = new RadialMeshData();
        meshData.setLatitude(timeRecord.getLatitude());
        meshData.setLongitude(timeRecord.getLongitude());
        meshData.setNumBins(timeRecord.getNumBins());
        meshData.setNumRadials(timeRecord.getNumRadials());
        meshData.setBinWidth(timeRecord.getBinWidth());
        meshData.setTiltAngle(timeRecord.getElevationAngle().floatValue());
        meshData.setFirstBin(timeRecord.getjStart());
        meshData.setAngleData(timeRecord.getAngleData());
        IMesh mesh = target.getExtension(IRadialMeshExtension.class)
                .constructMesh(meshData, descriptor.getGridGeometry());
        coverage.setMesh(mesh);
        return coverage;
    }

    @Override
    public String getName() {
        NSWRCRadialRecord record = this.resourceData
                .getRecord(descriptor.getFramesInfo().getTimeForResource(this));
        if (null == record) {
            return NO_DATA;
        }
        DecimalFormat df = new DecimalFormat("0.0");
        String elevationAngle = df.format(record.getElevationAngle());
        return record.getLocationName() + " " + elevationAngle + "  "
                + record.getProductName() + " (" + record.getUnit() + ")";
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String rval = "";

        if (this.interrogator != null) {
            try {
                float[] dataValues = interrogator
                        .getValueNoisePower(coord.asLatLon());
                DecimalFormat format = new DecimalFormat("0.0#");

                // fill value is -999999.9 from interrogator
                if (dataValues[0] > NSWRCConstants.FILL_VALUE
                        && dataValues[1] >= NSWRCConstants.NOISE_THRESHOLD
                        && dataValues[2] >= NSWRCConstants.POWER_THRESHOLD) {
                    if (dataValues[0] == 0.0) {
                        rval = null;
                    } else {
                        rval = format.format(dataValues[0]) + " "
                                + interrogator.getRecord().getUnit();
                    }
                } else {
                    rval = NO_DATA;
                }
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return rval;
    }

    protected StyleRule getStyleRule(NSWRCRadialRecord record) {
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        match.setParameterName(Arrays.asList(record.getProductName()));
        match.setCreatingEntityNames(null);
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance()
                    .getStyleRule(StyleManager.StyleType.IMAGERY, match);
        } catch (StyleException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error processing radar data Unit from Style Rules", e);

        }

        return sr;
    }

    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        // remove record from resourceData map
        this.resourceData.remove(dataTime);
        synchronized (this.images) {
            disposeImage(this.images.remove(dataTime));
        }
    }
}
