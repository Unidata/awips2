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
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters.PersistedParameters;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.filter.ValidRangeFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.IStyleType;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleManager.StyleType;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;

/**
 * Resource for displaying cross sections as images
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 29, 2007           njensen     Initial creation
 * Feb 17, 2009           njensen     Refactored to new rsc architecture
 * Dec 11, 2013  16795    D. Friedman Transform pixel coordinate in inspect
 * Mar 07, 2014  2791     bsteffen    Move Data Source/Destination to numeric
 *                                    plugin.
 * Oct 02, 2015  4914     bsteffen    Create custom style type for rules that
 *                                    apply only to cross section.
 * Apr 12, 2016  5567     bsteffen    Fix conversion in inspect
 * Feb 28, 2018  7231     njensen     Use super's statusHandler
 * Mar 14, 2018  6815     njensen     Add Contour as alternative display type
 * Apr 04, 2018  6889     njensen     Use brightness from ImagePreferences if
 *                                    present but missing in ImagingCapability
 * Apr 09, 2018  6931     njensen     When loading procedures use saved colormap
 * Apr 19, 2018  6760     njensen     Sample only out to 5 decimal places
 * Apr 15, 2019  7596     lsingh      Updated units framework to JSR-363.
 *                                    Handled unit conversion.
 * Sep 10, 2019  7922     bsteffen    Ensure custom colormap is used when
 *                                    loading from bundle.
 * 
 * </pre>
 * 
 * @author njensen
 */

public class CrossSectionImageResource extends AbstractCrossSectionResource {

    /**
     * A custom style type is used for creating rules that apply to only cross
     * section.
     */
    public static final IStyleType STYLE_TYPE = new IStyleType() {

        @Override
        public String[] getExtensions() {
            return new String[] { "ImageryCrossSectionStyleRules.xml" };
        }
    };

    /**
     * One image per frame.
     */
    private Map<DataTime, IImage> imageMap = new HashMap<>();

    /**
     * The colormap is initialized only when the first image is displayed and
     * remains constant for all other frames.
     */
    private boolean needsColorMapInit = true;

    private DecimalFormat decimalFormat = new DecimalFormat("#.#####");

    public CrossSectionImageResource(CrossSectionResourceData resourceData,
            LoadProperties props, AbstractCrossSectionAdapter<?> adapter) {
        super(resourceData, props, adapter);
        StyleRule styleRule = loadStyleRule();
        if (styleRule != null) {
            prefs = styleRule.getPreferences();
        }
        if (!this.hasCapability(ImagingCapability.class)) {
            this.getCapability(ImagingCapability.class)
                    .setInterpolationState(true);
        }
        getCapability(DisplayTypeCapability.class)
                .setAlternativeDisplayTypes(Arrays.asList(DisplayType.CONTOUR));
    }

    private StyleRule loadStyleRule() {
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        List<String> paramList = new ArrayList<>();
        paramList.add(resourceData.getParameter());
        match.setParameterName(paramList);
        try {
            StyleRule rule = StyleManager.getInstance().getStyleRule(STYLE_TYPE,
                    match);
            if (rule == null) {
                /*
                 * If no cross section specific rule is found then attempt to
                 * load a generic imagery rule.
                 */
                rule = StyleManager.getInstance()
                        .getStyleRule(StyleType.IMAGERY, match);
            }
            return rule;
        } catch (StyleException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error finding cross section image style rule for "
                            + getSafeName(),
                    e);
            return null;
        }
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        disposeImages();
    }

    private void disposeImages() {
        for (IImage image : imageMap.values()) {
            image.dispose();
        }
        imageMap.clear();
    }

    private IImage constructImage(DataTime time, IGraphicsTarget target)
            throws VizException {
        ColorMapParameters colorMapParams;

        // if loaded from a procedure, the colormap is already somewhat set up
        IColorMap savedColorMap = null;
        String savedColorMapName = null;
        PersistedParameters savedPersistedParams = null;
        if (needsColorMapInit && hasCapability(ColorMapCapability.class)) {
            ColorMapParameters cmapp = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            savedColorMapName = cmapp.getColorMapName();
            savedColorMap = cmapp.getColorMap();
            savedPersistedParams = cmapp.getPersisted();
        }

        if (needsColorMapInit) {
            float[] floatData = sliceMap.get(time).get(0);
            StyleRule styleRule = loadStyleRule();
            try {
                if (styleRule == null || !(styleRule
                        .getPreferences() instanceof ImagePreferences)) {
                    colorMapParams = ColorMapParameterFactory.build(floatData,
                            resourceData.getParameter(), getUnit(), null);
                } else {
                    colorMapParams = ColorMapParameterFactory.build(styleRule,
                            floatData, null, getUnit());
                    if (styleRule
                            .getPreferences() instanceof ImagePreferences) {
                        ImagePreferences imgPrefs = (ImagePreferences) styleRule
                                .getPreferences();
                        ImagingCapability imgCap = getCapability(
                                ImagingCapability.class);
                        if (!imgCap.isBrightnessSet()
                                && imgPrefs.getBrightness() != null) {
                            imgCap.setBrightness(imgPrefs.getBrightness());
                        }
                    }
                }
            } catch (StyleException e) {
                throw new VizException(
                        "Unable to build color map for cross section of "
                                + getSafeName(),
                        e);
            }

            String colorMap = colorMapParams.getColorMapName();

            if (savedColorMap == null) {
                if (savedColorMapName != null) {
                    colorMap = savedColorMapName;
                } else if (colorMap == null) {
                    colorMap = "Grid/gridded data";
                }
                try {
                    colorMapParams
                            .setColorMap(ColorMapLoader.loadColorMap(colorMap));
                } catch (ColorMapException e) {
                    throw new VizException(e);
                }
            } else {
                colorMapParams.setColorMap(savedColorMap);
                colorMap = savedColorMapName;

            }
            colorMapParams.setColorMapName(colorMap);
            if (savedPersistedParams != null) {
                colorMapParams.applyPersistedParameters(savedPersistedParams);
            }

            getCapability(ColorMapCapability.class)
                    .setColorMapParameters(colorMapParams);
            needsColorMapInit = false;

        } else {
            colorMapParams = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
        }

        IImage image = target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(new ImageDataCallback(time), colorMapParams);
        ImagingCapability imagingCapability = getCapability(
                ImagingCapability.class);
        image.setInterpolated(imagingCapability.isInterpolationState());
        if (!imagingCapability.isBrightnessSet()) {
            imagingCapability.setBrightness(0.5f);
        }
        image.setBrightness(imagingCapability.getBrightness());
        image.setContrast(imagingCapability.getContrast());
        return image;

    }

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
            image = constructImage(currentTime, target);
            imageMap.put(currentTime, image);
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
    public void resourceDataChanged(ChangeType type, Object object) {
        super.resourceDataChanged(type, object);
        if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ImagingCapability) {
                ImagingCapability imgcap = (ImagingCapability) object;
                for (IImage image : imageMap.values()) {
                    image.setBrightness(imgcap.getBrightness());
                    image.setContrast(imgcap.getContrast());
                    image.setInterpolated(imgcap.isInterpolationState());
                }
            }
        }
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
                Unit<?> dataUnit = getUnit();
                Unit<?> displayUnit = colorMapParams.getDisplayUnit();
                if (displayUnit != null && dataUnit != null
                        && dataUnit.isCompatible(displayUnit)) {
                    UnitConverter converter = UnitConv
                            .getConverterToUnchecked(dataUnit, displayUnit);
                    val = converter.convert(val);
                    
                }
            }
            return decimalFormat.format(val) + getUnitString();
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

    /**
     * Simple callback which just copies the data out of the sliceMap.
     */
    private class ImageDataCallback implements IColorMapDataRetrievalCallback {

        private final DataTime time;

        public ImageDataCallback(DataTime time) {
            this.time = time;
        }

        @Override
        public ColorMapData getColorMapData() throws VizException {
            List<float[]> data = sliceMap.get(time);
            if (data == null || data.isEmpty()) {
                throw new VizException("No Image Data available for "
                        + getSafeName() + " at " + time.getDisplayString());
            }
            int[] dims = new int[] { geometry.getGridRange().getSpan(0),
                    geometry.getGridRange().getSpan(1) };
            return new ColorMapData(FloatBuffer.wrap(data.get(0)), dims);
        }

    }

}
