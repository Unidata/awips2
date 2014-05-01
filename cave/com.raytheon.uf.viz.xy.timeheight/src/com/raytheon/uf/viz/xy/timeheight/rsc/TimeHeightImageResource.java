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

import java.awt.Rectangle;
import java.util.ArrayList;

import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatArrayWrapper;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.data.prep.CMDataPreparerManager;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource for displaying time heights as images
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2007            njensen     Initial creation
 * Feb 20, 2009            njensen     Refactored to new rsc architecture
 * Dec 11, 2013 DR 16795   D. Friedman Transform pixel coordinate in inspect
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
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
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(resourceData.getParameter());
        match.setParameterName(paramList);
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY, match);
        } catch (StyleException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        if (sr != null) {
            prefs = sr.getPreferences();
        }
        if (!this.hasCapability(ImagingCapability.class)) {
            this.getCapability(ImagingCapability.class).setInterpolationState(
                    true);
            this.getCapability(ImagingCapability.class).setBrightness(0.5f);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        if (secondaryResource != null) {
            secondaryResource.dispose();
        }
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
            getCapability(ColorMapCapability.class).setColorMapParameters(
                    colorMapParams);
            int[] dims = new int[] { geometry.getGridRange().getSpan(0),
                    geometry.getGridRange().getSpan(1) };

            float[] sliceData = interpolatedData;

            if (secondaryResource != null
                    && combineOperation != CombineOperation.NONE) {
                sliceData = combineResourceData(sliceData,
                        secondaryResource.interpolatedData);
            }

            image = target.initializeRaster(CMDataPreparerManager
                    .getDataPreparer(sliceData,
                            new Rectangle(dims[0], dims[1]), null),
                    colorMapParams);

            ImagingCapability cap = getCapability(ImagingCapability.class);
            image.setBrightness(cap.getBrightness());
            image.setContrast(cap.getContrast());
            image.setInterpolated(cap.isInterpolationState());
        }
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

            colorMapParams.setColorMap(target.buildColorMap(colorMap));
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
                val = colorMapParams.getDataToDisplayConverter().convert(val);
            }
            return ((int) (val * 100)) / 100.0 + getUnitString();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.timeheight.rsc.AbstractTimeHeightResource#
     * setDescriptor
     * (com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor)
     */
    @Override
    public void setDescriptor(TimeHeightDescriptor descriptor) {
        if (image != null) {
            image.dispose();
        }
        image = null;
        super.setDescriptor(descriptor);
    }

}
