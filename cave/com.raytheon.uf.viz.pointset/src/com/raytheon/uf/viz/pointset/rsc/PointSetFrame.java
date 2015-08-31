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

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
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
 * ------------- -------- --------- --------------------------
 * Aug 28, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointSetFrame {

    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(PointSetFrame.class);

    private final PointSetRecord record;

    private final PointSetResource resource;

    private ITriangulatedImage image;

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
        ITriangulatedImage image = this.image;
        if (image != null) {
            image.dispose();
        }
        image = null;
        staged = false;
    }

    public String inspect(double x, double y) {
        double data_value = image.getDataValue(x, y);
        if (Double.isNaN(data_value)) {
            return "No Data";
        } else {
            return String.format("%4.2f %s", image.getDataValue(x, y), record
                    .getParameter().getUnitString());
        }
    }

    public boolean paint(PaintProperties paintProps, IGraphicsTarget target)
            throws VizException {
        ITriangulatedImage image = this.image;
        if (image == null) {
            ColorMapParameters colorMapParameters = resource.getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            ITriangulatedImageExtension triangleExt = target
                    .getExtension(ITriangulatedImageExtension.class);
            this.image = triangleExt.initializeImage(colorMapParameters,
                    new PointSetLocationCallback(resource.getDescriptor(),
                            record), new PointSetDataCallback(record));
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

}