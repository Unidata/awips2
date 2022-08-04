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
package com.raytheon.uf.viz.xy.timeheight.rsc;

import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.List;

import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;

import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.filter.ValidRangeFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData.CombineOperation;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import org.locationtech.jts.geom.Coordinate;

/**
 * Resource for displaying time heights as images
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Dec 04, 2007  625      njensen      Initial creation
 * Feb 20, 2009  1960     njensen      Refactored to new rsc architecture
 * Dec 11, 2013  16795    D. Friedman  Transform pixel coordinate in inspect
 * Mar 07, 2014  2791     bsteffen     Move Data Source/Destination to numeric
 *                                     plugin.
 * Jun 30, 2014  3165     njensen      Use ColorMapLoader to get ColorMap
 * Aug 13, 2014  3505     mapeters     Replaced deprecated CMDataPreparerManager
 *                                     reference in initializeRaster() call.
 * Aug 12, 2016  5822     bsteffen     implement disposeRenderables
 * Feb 28, 2018  7231     njensen      Cleanup
 * Apr 04, 2018  6889     njensen      Use brightness from ImagePreferences if
 *                                     present but missing in ImagingCapability
 * 
 * </pre>
 * 
 * @author njensen
 */
public class TimeHeightImageResource extends AbstractTimeHeightResource
        implements IResourceDataChanged {

    private IImage image;

    private boolean needsColorMapInit = true;

    public TimeHeightImageResource(TimeHeightResourceData data,
            LoadProperties props, AbstractVarHeightAdapter<?> adapter) {
        super(data, props, adapter);
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        List<String> paramList = new ArrayList<>();
        paramList.add(resourceData.getParameter());
        match.setParameterName(paramList);
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance()
                    .getStyleRule(StyleManager.StyleType.IMAGERY, match);
        } catch (StyleException e) {
            statusHandler.error("Error getting image style rules", e);
        }
        if (sr != null) {
            prefs = sr.getPreferences();
        }

        if (!hasCapability(ImagingCapability.class)) {
            getCapability(ImagingCapability.class).setInterpolationState(true);
        }

        /*
         * If the capability already has a brightness it was most likely loaded
         * from a procedure/bundle and that should take precedent. If brightness
         * is not set, then try style rules to get a brightness.
         */
        ImagingCapability imgCap = getCapability(ImagingCapability.class);
        if (!imgCap.isBrightnessSet()) {
            if (prefs instanceof ImagePreferences) {
                ImagePreferences imgPrefs = (ImagePreferences) prefs;
                if (imgPrefs.getBrightness() != null) {
                    imgCap.setBrightness(imgPrefs.getBrightness());
                }
            }
        }
        if (!imgCap.isBrightnessSet()) {
            imgCap.setBrightness(0.5f);
        }
    }

    @Override
    protected void disposeRenderables() {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                if (image != null) {
                    image.dispose();
                }
                image = null;
            }
        });
        needsColorMapInit = true;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        if (interpolatedData != null) {
            initializeImage(target);
        }

        // defaults
        ImagingCapability imageCap = getCapability(ImagingCapability.class);
        imageCap.setInterpolationState(true);
    }

    private void initializeImage(IGraphicsTarget target) throws VizException {
        if (image == null) {
            // convert NaN to -999999 since we can't handle NaN in the image
            // code right now
            if (interpolatedData != null) {
                for (int i = 0; i < interpolatedData.length; ++i) {
                    if (Float.isNaN(interpolatedData[i])) {
                        interpolatedData[i] = -999999f;
                    }
                }
            }

            // re-init the colormap params because the mins/maxes are likely to
            // change
            ColorMapParameters prevParams = getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            ColorMapParameters colorMapParams;
            try {
                colorMapParams = ColorMapParameterFactory.build(
                        interpolatedData, resourceData.getParameter(),
                        getUnit(), null);
            } catch (StyleException e) {
                throw new VizException(e.getLocalizedMessage(), e);
            }
            if (prevParams != null) {
                colorMapParams.setColorMap(prevParams.getColorMap());
                colorMapParams.setColorMapName(prevParams.getColorMapName());
            }
            getCapability(ColorMapCapability.class)
                    .setColorMapParameters(colorMapParams);
            final int[] dims = new int[] { geometry.getGridRange().getSpan(0),
                    geometry.getGridRange().getSpan(1) };

            float[] sliceData = interpolatedData;

            if (secondaryResource != null
                    && combineOperation != CombineOperation.NONE) {
                sliceData = combineResourceData(sliceData,
                        secondaryResource.interpolatedData);
            }

            final FloatBuffer data = FloatBuffer.wrap(sliceData);
            this.image = target.getExtension(IColormappedImageExtension.class)
                    .initializeRaster(new IColorMapDataRetrievalCallback() {
                        @Override
                        public ColorMapData getColorMapData()
                                throws VizException {
                            return new ColorMapData(data, dims,
                                    ColorMapDataType.FLOAT);
                        }
                    }, colorMapParams);

            ImagingCapability cap = getCapability(ImagingCapability.class);
            image.setBrightness(cap.getBrightness());
            image.setContrast(cap.getContrast());
            image.setInterpolated(cap.isInterpolationState());
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);
        if (interpolatedData == null) {
            return;
        }
        if (secondaryResource != null) {
            secondaryResource.paint(target, paintProps);
            if (secondaryResource.interpolatedData == null) {
                return;
            }
        }
        if (image == null) {
            initializeImage(target);
        }
        ColorMapParameters colorMapParams = getCapability(
                ColorMapCapability.class).getColorMapParameters();
        if (needsColorMapInit && colorMapParams != null) {
            needsColorMapInit = false;
            String colorMap = colorMapParams.getColorMapName();
            if (colorMap == null) {
                colorMap = "Grid/gridded data";
            }

            try {
                colorMapParams
                        .setColorMap(ColorMapLoader.loadColorMap(colorMap));
            } catch (ColorMapException e) {
                throw new VizException(e);
            }
        }

        target.setupClippingPlane(descriptor.getGraph(this).getExtent());

        Envelope2D env = geometry.getEnvelope2D();
        Coordinate ul = new Coordinate(env.getMinX(), env.getMaxY());
        Coordinate ur = new Coordinate(env.getMaxX(), env.getMaxY());
        Coordinate ll = new Coordinate(env.getMinX(), env.getMinY());
        Coordinate lr = new Coordinate(env.getMaxX(), env.getMinY());
        PixelCoverage cov = new PixelCoverage(ul, ur, lr, ll);

        if (combineOperation != CombineOperation.NONE) {
            ImagingCapability cap = getCapability(ImagingCapability.class);
            image.setBrightness(cap.getBrightness());
            image.setContrast(cap.getContrast());
            target.drawRaster(image, cov, paintProps);
        }
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (secondaryResource != null) {
            ((IResourceDataChanged) secondaryResource).resourceChanged(type,
                    object);
        }

        if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ImagingCapability) {
                ImagingCapability imgcap = (ImagingCapability) object;
                image.setBrightness(imgcap.getBrightness());
                image.setContrast(imgcap.getBrightness());
                image.setInterpolated(imgcap.isInterpolationState());
            }
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (interpolatedData == null) {
            return "NO DATA";
        }
        float[] sliceData = interpolatedData;
        if (sliceData == null) {
            return null;
        }
        if (secondaryResource != null
                && combineOperation != CombineOperation.NONE) {
            sliceData = combineResourceData(sliceData,
                    secondaryResource.interpolatedData);
        }
        DataSource source = new FloatBufferWrapper(sliceData,
                geometry.getGridRange2D());
        source = ValidRangeFilter.apply(source, -9998,
                Double.POSITIVE_INFINITY);
        GridSampler sampler = new GridSampler(source,
                new BilinearInterpolation());
        GridReprojection reproj = new GridReprojection(geometry,
                descriptor.getGridGeometry());

        IExtent extent = descriptor.getGraph(this).getExtent();

        double val = Double.NaN;
        double[] worldCoord = descriptor.pixelToWorld(
                new double[] { coord.getObject().x, coord.getObject().y });
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
                val = colorMapParams.getDataToDisplayConverter().convert(val);
            }
            return ((int) (val * 100)) / 100.0 + getUnitString();
        }
    }

    @Override
    public void setDescriptor(TimeHeightDescriptor descriptor) {
        if (image != null) {
            image.dispose();
        }
        image = null;
        super.setDescriptor(descriptor);
    }

}
