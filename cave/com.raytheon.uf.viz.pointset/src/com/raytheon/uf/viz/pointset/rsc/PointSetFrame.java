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
package com.raytheon.uf.viz.pointset.rsc;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImage;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImageExtension;
import com.raytheon.uf.viz.pointset.image.PointSetDataCallback;
import com.raytheon.uf.viz.pointset.image.PointSetLocationCallback;

/**
 * Object responsible for rendering a single frame of point set data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 28, 2015  4709     bsteffen  Initial creation
 * Jan 25, 2016  5208     bsteffen  Ensure correct units are used for conversion
 * Jun 07, 2016  5452     bsteffen  Handle null data units
 * Sep 29, 2016  5581     bsteffen  Ensure image is not reused after dispose.
 * Apr 04, 2018  6889     njensen   Set brightness on image create
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class PointSetFrame {

    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(PointSetFrame.class);

    private final PointSetRecord record;

    private final PointSetResource resource;

    private ITriangulatedImage image;

    private PointSetDataCallback dataCallback;

    private volatile boolean staged = false;

    public PointSetFrame(PointSetRecord record, PointSetResource resource) {
        super();
        this.record = record;
        this.resource = resource;
    }

    public PointSetRecord getRecord() {
        return record;
    }

    /**
     * Release any graphics resources associated with this image. This must not
     * be called concurrently with paint.
     */
    public void dispose() {
        if (image != null) {
            image.dispose();
            image = null;
        }
        staged = false;
    }

    public double inspect(double x, double y) {
        ITriangulatedImage image = this.image;
        if (image == null) {
            return Double.NaN;
        }
        ColorMapParameters colorMapParameters = resource
                .getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        Unit<?> displayUnit = colorMapParameters.getDisplayUnit();

        double dataValue = image.getDataValue(x, y);
        if (displayUnit != null) {
            Unit<?> dataUnit = dataCallback.getDataUnit();
            if (dataUnit != null) {
                UnitConverter converter = UnitConv
                        .getConverterToUnchecked(dataUnit, displayUnit);
                dataValue = converter.convert(dataValue);
          
            }
        }
        return dataValue;
    }

    public boolean paint(PaintProperties paintProps, IGraphicsTarget target)
            throws VizException {
        ITriangulatedImage image = this.image;
        if (image == null) {
            ColorMapParameters colorMapParameters = resource
                    .getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            ITriangulatedImageExtension triangleExt = target
                    .getExtension(ITriangulatedImageExtension.class);
            this.dataCallback = new PointSetDataCallback(record);
            this.image = triangleExt
                    .initializeImage(colorMapParameters,
                            new PointSetLocationCallback(
                                    resource.getDescriptor(), record),
                            dataCallback);
            ImagingCapability imgCap = resource
                    .getCapability(ImagingCapability.class);
            this.image.setBrightness(imgCap.getBrightness());
            return false;
        } else if (staged) {
            ITriangulatedImageExtension triangleExt = target
                    .getExtension(ITriangulatedImageExtension.class);
            triangleExt.drawImage(paintProps, image);
            return true;
        } else {
            return false;
        }
    }

    public void stage() {
        ITriangulatedImage image = this.image;
        if (image == null || staged) {
            return;
        }
        try {
            image.stage();
            staged = true;
            resource.issueRefresh();
        } catch (VizException e) {
            logger.error("Unable to stage point set data.", e);
        }
    }

    protected void updateImagingCapability(ImagingCapability imgCap) {
        if (image != null) {
            image.setBrightness(imgCap.getBrightness());
            image.setContrast(imgCap.getContrast());
        }
    }

}