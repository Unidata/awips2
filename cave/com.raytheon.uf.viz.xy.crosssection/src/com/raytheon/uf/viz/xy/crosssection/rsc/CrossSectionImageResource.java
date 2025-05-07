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

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
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
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
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
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameData;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionImage;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;

/**
 * Resource for displaying cross sections as images
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Nov 29, 2007           njensen      Initial creation
 * Feb 17, 2009           njensen      Refactored to new rsc architecture
 * Dec 11, 2013  16795    D. Friedman  Transform pixel coordinate in inspect
 * Mar 07, 2014  2791     bsteffen     Move Data Source/Destination to numeric
 *                                     plugin.
 * Oct 02, 2015  4914     bsteffen     Create custom style type for rules that
 *                                     apply only to cross section.
 * Apr 12, 2016  5567     bsteffen     Fix conversion in inspect
 * Feb 28, 2018  7231     njensen      Use super's statusHandler
 * Mar 14, 2018  6815     njensen      Add Contour as alternative display type
 * Apr 04, 2018  6889     njensen      Use brightness from ImagePreferences if
 *                                     present but missing in ImagingCapability
 * Apr 09, 2018  6931     njensen      When loading procedures use saved
 *                                     colormap
 * Apr 19, 2018  6760     njensen      Sample only out to 5 decimal places
 * Apr 15, 2019  7596     lsingh       Updated units framework to JSR-363.
 *                                     Handled unit conversion.
 * Sep 10, 2019  7922     bsteffen     Ensure custom colormap is used when
 *                                     loading from bundle.
 * May 17, 2021  8452     randerso     Make CrossSectionImageResource respect
 *                                     the interpolate flag if specified in a
 *                                     styleRule.
 * Feb 22, 2023  9021     mapeters     Use getSliceData() to access sliceMap
 * Jul 19, 2023  2035935  mapeters     Add isEmpty() checks to prevent errors
 *                                     when the line isn't near the data
 * Apr 02, 2024  2037091  mapeters     Auto-update and refactor to support new
 *                                     subclass
 * Jun 30, 2024  2037476  bines        Updated to match other two FSI screens
 * Aug 06, 2024  2037698  bines        Added conversion for HC data
 * Aug 20, 2024  2037631  mapeters     Wrap floats and images in new classes, move
 *                                     some paint logic into CrossSectionImage
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
    public static final IStyleType STYLE_TYPE = () -> new String[] {
            "ImageryCrossSectionStyleRules.xml" };

    /**
     * One image per frame.
     */
    protected final Map<DataTime, CrossSectionImage> imageMap = new HashMap<>();

    /**
     * The colormap is initialized only when the first image is displayed and
     * remains constant for all other frames.
     */
    private boolean needsColorMapInit = true;

    private DecimalFormat decimalFormat = new DecimalFormat("#.#####");

    public CrossSectionImageResource(CrossSectionResourceData resourceData,
            LoadProperties props, AbstractCrossSectionAdapter<?> adapter) {
        super(resourceData, props, adapter);

        if (!this.hasCapability(ImagingCapability.class)) {
            this.getCapability(ImagingCapability.class)
                    .setInterpolationState(true);
        }
        getCapability(DisplayTypeCapability.class)
                .setAlternativeDisplayTypes(Arrays.asList(DisplayType.CONTOUR));
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        StyleRule styleRule = loadStyleRule();
        if (styleRule != null) {
            prefs = styleRule.getPreferences();
        }

        super.initInternal(target);
    }

    private StyleRule loadStyleRule() {
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        List<String> paramList = new ArrayList<>();
        paramList.add(resourceData.getParameter());
        List<String> sourceList = new ArrayList<>();
        sourceList.add(adapter.getCreatingEntity());
        match.setParameterName(paramList);
        match.setCreatingEntityNames(sourceList);
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

    protected CrossSectionImage constructImage(DataTime time,
            CrossSectionFrameData frameData, IGraphicsTarget target)
            throws VizException {
        List<float[]> data = frameData.getData();
        if (data.isEmpty()) {
            return new CrossSectionImage(null, this,
                    frameData.getExtraRenderable());
        }

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

        ColorMapParameters colorMapParams;
        if (needsColorMapInit) {
            float[] floatData = data.get(0);
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
                        imgCap.setInterpolationState(imgPrefs.isInterpolate());
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
                .initializeRaster(new ImageDataCallback(time, data),
                        colorMapParams);
        ImagingCapability imagingCapability = getCapability(
                ImagingCapability.class);
        image.setInterpolated(imagingCapability.isInterpolationState());
        if (!imagingCapability.isBrightnessSet()) {
            imagingCapability.setBrightness(0.5f);
        }
        image.setBrightness(imagingCapability.getBrightness());
        image.setContrast(imagingCapability.getContrast());

        Envelope2D env = geometry.getEnvelope2D();
        Coordinate ul = new Coordinate(env.getMinX(), env.getMaxY());
        Coordinate ur = new Coordinate(env.getMaxX(), env.getMaxY());
        Coordinate ll = new Coordinate(env.getMinX(), env.getMinY());
        Coordinate lr = new Coordinate(env.getMaxX(), env.getMinY());
        PixelCoverage cov = new PixelCoverage(ul, ur, lr, ll);

        return new CrossSectionImage(new DrawableImage(image, cov), this,
                frameData.getExtraRenderable());

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);

        CrossSectionImage image = getOrCreateImage(target, paintProps);
        if (image == null) {
            return;
        }
        target.setupClippingPlane(descriptor.getGraph(this).getExtent());
        image.paint(target, paintProps);
    }

    /**
     * Get the image to display for the current time, creating it if it is not
     * available yet but the data is.
     *
     * @param target
     * @param paintProps
     * @return the image
     * @throws VizException
     */
    protected CrossSectionImage getOrCreateImage(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime currentTime = paintProps.getDataTime();
        if (currentTime == null) {
            return null;
        }

        CrossSectionImage image;
        synchronized (lock) {
            image = imageMap.get(currentTime);
            if (image == null) {
                CrossSectionFrameData data = getSliceData(currentTime);
                if (data != null) {
                    image = constructImage(currentTime, data, target);
                    imageMap.put(currentTime, image);
                }
            }
        }
        return image;
    }

    @Override
    protected CrossSectionImage getFrameRenderable(DataTime frameTime) {
        synchronized (lock) {
            return imageMap.get(frameTime);
        }
    }

    @Override
    protected void disposeFrames() {
        super.disposeFrames();
        synchronized (lock) {
            for (CrossSectionImage image : imageMap.values()) {
                image.dispose();
            }
            imageMap.clear();
        }
    }

    /**
     * Get the data to use for inspection for the given time.
     *
     * @param time
     *            the time being inspected
     * @return the data
     * @throws VizException
     */
    protected float[] getInspectData(DataTime time) {
        CrossSectionFrameData data = getSliceData(time);
        if (data == null || !data.hasData()) {
            return null;
        }
        return data.getData().get(0);
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        DataTime currentTime = descriptor.getTimeForResource(this);

        float[] sliceData = getInspectData(currentTime);
        if (sliceData == null) {
            return "NO DATA";
        }

        Interpolation interpolation = (adapter.useNearestNeighbor())
                ? new NearestNeighborInterpolation()
                : new BilinearInterpolation();

        DataSource source = new FloatBufferWrapper(sliceData,
                geometry.getGridRange2D());
        source = ValidRangeFilter.apply(source, -9998,
                Double.POSITIVE_INFINITY);
        GridSampler sampler = new GridSampler(source, interpolation);
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
                    UnitConverter converter;
                    try {
                        converter = dataUnit.getConverterToAny(displayUnit);
                        val = converter.convert(val);
                    } catch (UnconvertibleException
                            | IncommensurableException e) {
                        statusHandler.error("Error converting data unit: "
                                + dataUnit.getName() + " to display unit: "
                                + displayUnit.getName(), e);
                    }
                }
                String mapResult = dataMappingConversion(val);
                if (mapResult != null && !mapResult.isEmpty()) {
                    return mapResult;
                }
            }
            return decimalFormat.format(val) + getUnitString();
        }

    }

    protected String dataMappingConversion(double val) {
        if (hasCapability(ColorMapCapability.class)) {
            ColorMapParameters cmapp = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();

            DataMappingPreferences dataMapping = cmapp.getDataMapping();

            if (dataMapping != null) {
                double imageVal = cmapp.getDisplayToColorMapConverter()
                        .convert(val);
                String mapResult = dataMapping
                        .getSampleOrLabelValueForDataValue(imageVal);
                if (mapResult != null && !mapResult.isEmpty()) {
                    return mapResult;
                }
            }
        }

        return null;
    }

    @Override
    public void disposeFrame(DataTime frameTime, boolean onUpdate) {
        super.disposeFrame(frameTime, onUpdate);
        CrossSectionImage image;
        synchronized (lock) {
            image = imageMap.remove(frameTime);
        }
        if (image != null) {
            image.dispose();
        }
    }

    /**
     * Simple callback which just wraps the given data. Also does display to
     * color map conversions when a converter is present.
     */
    protected class ImageDataCallback
            implements IColorMapDataRetrievalCallback {

        private final DataTime time;

        private final List<float[]> data;

        public ImageDataCallback(DataTime time, List<float[]> data) {
            this.time = time;
            this.data = data;
        }

        @Override
        public ColorMapData getColorMapData() throws VizException {
            if (data == null || data.isEmpty()) {
                throw new VizException("No Image Data available for "
                        + getSafeName() + " at " + time.getDisplayString());
            }
            int[] dims = { geometry.getGridRange().getSpan(0),
                    geometry.getGridRange().getSpan(1) };

            float[] convertedData = convertDataToColorMap(data.get(0));

            return new ColorMapData(FloatBuffer.wrap(convertedData), dims);
        }

        protected float[] convertDataToColorMap(float[] data) {
            float[] convertedData = new float[data.length];

            if (hasCapability(ColorMapCapability.class)) {
                ColorMapParameters cmapp = getCapability(
                        ColorMapCapability.class).getColorMapParameters();

                UnitConverter converter = cmapp.getDisplayToColorMapConverter();
                if (converter != null) {
                    for (int i = 0; i < data.length; i++) {
                        convertedData[i] = (float) converter.convert(data[i]);
                    }
                } else {
                    convertedData = data;
                }
            }
            return convertedData;
        }

    }
}
