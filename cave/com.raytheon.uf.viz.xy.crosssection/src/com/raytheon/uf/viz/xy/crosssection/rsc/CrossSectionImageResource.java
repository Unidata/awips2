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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.measure.converter.UnitConverter;

import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatArrayWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource for displaying cross sections as images
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2007            njensen     Initial creation
 * 02/17/09                njensen     Refactored to new rsc architecture
 * Dec 11, 2013 DR 16795   D. Friedman Transform pixel coordinate in inspect
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CrossSectionImageResource extends AbstractCrossSectionResource
        implements IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrossSectionImageResource.class);

    private Map<DataTime, IImage> imageMap = new HashMap<DataTime, IImage>();

    private boolean needsColorMapInit = true;

    private CrossSectionResourceData data = null;

    private ColorMapParameters coloParams = null;

    public CrossSectionImageResource(CrossSectionResourceData data,
            LoadProperties props, AbstractCrossSectionAdapter<?> adapter) {
        super(data, props, adapter);
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(resourceData.getParameter());
        match.setParameterName(paramList);
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY, match);
        } catch (StyleException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting contour style rule", e);
        }
        if (sr != null) {
            prefs = sr.getPreferences();
        }
        this.data = data;
        data.addChangeListener(this);
        if (!this.hasCapability(ImagingCapability.class)) {
            this.getCapability(ImagingCapability.class).setInterpolationState(
                    true);
            this.getCapability(ImagingCapability.class).setBrightness(0.5f);
        }
    }

    @Override
    protected void disposeInternal() {
        this.data.removeChangeListener(this);
        super.disposeInternal();
        disposeImages();
    }

    private void disposeImages() {
        for (IImage image : imageMap.values()) {
            image.dispose();
        }
        imageMap.clear();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);

        // defaults
        if (!hasCapability(ImagingCapability.class)) {
            ImagingCapability imageCap = getCapability(ImagingCapability.class);
            imageCap.setInterpolationState(true);
            imageCap.setBrightness(1.0f);
            imageCap.setContrast(1.0f);
        }
    }

    private IImage constructImage(float[] floatData, IGraphicsTarget target)
            throws VizException {
        // re-init the colormap params because the mins/maxes are likely to
        // change
        ColorMapParameters prevParams = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        ColorMapParameters colorMapParams;
        try {
            colorMapParams = ColorMapParameterFactory.build(floatData,
                    resourceData.getParameter(), getUnit(), null);
        } catch (StyleException e) {
            throw new VizException(e.getLocalizedMessage(), e);
        }
        if (prevParams != null) {
            colorMapParams.setColorMap(prevParams.getColorMap());
            colorMapParams.setColorMapName(prevParams.getColorMapName());
        }
        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParams);

        int[] dims = new int[] { geometry.getGridRange().getSpan(0),
                geometry.getGridRange().getSpan(1) };
        final ColorMapData data = new ColorMapData(FloatBuffer.wrap(floatData),
                dims);
        IColorMapDataRetrievalCallback callback = new IColorMapDataRetrievalCallback() {

            @Override
            public ColorMapData getColorMapData() throws VizException {
                return data;
            }
        };
        IImage image = target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(callback, colorMapParams);
        image.setInterpolated(getCapability(ImagingCapability.class)
                .isInterpolationState());

        image.setBrightness(getCapability(ImagingCapability.class)
                .getBrightness());

        image.setContrast(getCapability(ImagingCapability.class).getContrast());

        return image;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);
        DataTime currentTime = paintProps.getDataTime();
        if (sliceMap.get(currentTime) == null) {
            return;
        }

        IImage image = imageMap.get(currentTime);
        if (image == null) {
            image = constructImage(sliceMap.get(currentTime).get(0), target);
            imageMap.put(currentTime, image);
        }

        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        if (needsColorMapInit && params != null) {
            needsColorMapInit = false;
            String colorMap = params.getColorMapName();
            if (colorMap == null) {
                colorMap = "Grid/gridded data";
            }

            params.setColorMap(ColorMapLoader.loadColorMap(colorMap));
            params.setColorMapName(colorMap);
        }

        target.setupClippingPlane(descriptor.getGraph(this).getExtent());

        Envelope2D env = geometry.getEnvelope2D();
        Coordinate ul = new Coordinate(env.getMinX(), env.getMaxY());
        Coordinate ur = new Coordinate(env.getMaxX(), env.getMaxY());
        Coordinate ll = new Coordinate(env.getMinX(), env.getMinY());
        Coordinate lr = new Coordinate(env.getMaxX(), env.getMinY());

        PixelCoverage cov = new PixelCoverage(ul, ur, lr, ll);
        target.drawRaster(image, cov, paintProps);
    }

    @Override
    public void setDescriptor(CrossSectionDescriptor descriptor) {
        super.setDescriptor(descriptor);
        disposeImages();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ImagingCapability) {
                ImagingCapability imgcap = (ImagingCapability) object;
                for (IImage image : imageMap.values()) {
                    image.setBrightness(imgcap.getBrightness());
                    image.setContrast(imgcap.getContrast());
                    image.setInterpolated(imgcap.isInterpolationState());
                }
            } else if (object instanceof ColorMapCapability) {
                ColorMapCapability colocap = this
                        .getCapability(ColorMapCapability.class);
                if (this.coloParams == null) {
                    this.coloParams = colocap.getColorMapParameters();
                } else if (!(this.coloParams.getColorMapName().equals(colocap
                        .getColorMapParameters().getColorMapName()))) {
                    this.coloParams = colocap.getColorMapParameters();
                    // dispose all images
                    disposeImages();
                    this.issueRefresh();
                }
            }
        }
    }

    protected void updateTime(DataTime time) {
        imageMap.remove(time);
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        DataTime currentTime = descriptor.getTimeForResource(this);
        if (sliceMap.get(currentTime) == null) {
            return "NO DATA";
        }
        float[] sliceData = sliceMap.get(currentTime).get(0);
        if (sliceData == null) {
            return null;
        }

        FloatArrayWrapper source = new FloatArrayWrapper(sliceData, geometry);
        source.setValidRange(-9998, Double.POSITIVE_INFINITY);
        GridSampler sampler = new GridSampler(source,
                new BilinearInterpolation());
        GridReprojection reproj = new GridReprojection(geometry,
                descriptor.getGridGeometry());

        IExtent extent = descriptor.getGraph(this).getExtent();


        double val = Double.NaN;
        double[] worldCoord = descriptor.pixelToWorld(new double[] {
                coord.getObject().x, coord.getObject().y });
        if (extent.contains(worldCoord)) {
            try {

                DirectPosition2D dp = new DirectPosition2D(worldCoord[0],
                        worldCoord[1]);
                descriptor.getGridGeometry().getGridToCRS().transform(dp, dp);
                val = reproj.reprojectedGridCell(sampler, (int) dp.x,
                        (int) dp.y);
            } catch (Exception e) {
                throw new VizException(e);
            }
        }
        if (Double.isNaN(val)) {
            return "NO DATA";
        } else {
            ColorMapParameters colorMapParams = getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            if (colorMapParams != null) {
                UnitConverter converter = colorMapParams
                        .getDataToDisplayConverter();
                if (converter != null) {
                    val = converter.convert(val);
                }
            }
            return val + getUnitString();
        }

    }

    @Override
    public void disposeTimeData(DataTime dataTime) {
        super.disposeTimeData(dataTime);
        IImage image = imageMap.remove(dataTime);
        if (image != null) {
            image.dispose();
        }
    }

}
