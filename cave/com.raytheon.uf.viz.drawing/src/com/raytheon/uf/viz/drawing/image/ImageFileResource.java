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
package com.raytheon.uf.viz.drawing.image;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.drawing.image.EditRectangleInputHandler.EditRectangleTarget;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource for displaying a non geolocated image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 15, 2014  2313     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ImageFileResource extends
        AbstractVizResource<ImageFileResourceData, IDescriptor> implements
        EditRectangleTarget {

    private IImage image;

    private EditRectangleInputHandler inputHandler;

    private Rectangle canvasBounds;

    protected ImageFileResource(ImageFileResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        inputHandler = new EditRectangleInputHandler(this);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        image = target.initializeRaster(new RenderedImageFileCallback(
                resourceData.getFile()));
        checkEditable();
        checkImagingProperties();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        canvasBounds = paintProps.getCanvasBounds();
        Rectangle rect = getRectangle();
        Coordinate ul = new Coordinate(rect.x, rect.y);
        Coordinate ur = new Coordinate(rect.x + rect.width, rect.y);
        Coordinate lr = new Coordinate(rect.x + rect.width, rect.y
                + rect.height);
        Coordinate ll = new Coordinate(rect.x, rect.y + rect.height);
        PixelCoverage pc = new PixelCoverage(ul, ur, lr, ll);

        ICanvasRenderingExtension renderer = target
                .getExtension(ICanvasRenderingExtension.class);
        renderer.drawImages(paintProps, new DrawableImage(image, pc));

        EditableCapability eCap = getCapability(EditableCapability.class);
        if (eCap.isEditable()) {
            DrawableLine box = new DrawableLine();
            ColorableCapability cCap = getCapability(ColorableCapability.class);
            box.basics.color = cCap.getColor();
            box.width = 3.0f;
            box.addPoint(ul.x, ul.y);
            box.addPoint(ur.x, ur.y);
            box.addPoint(lr.x, lr.y);
            box.addPoint(ll.x, ll.y);
            box.addPoint(ul.x, ul.y);
            renderer.drawLines(paintProps, box);
        }
    }

    @Override
    protected void disposeInternal() {
        if (image != null) {
            image.dispose();
            image = null;
        }
        inputHandler.disable(getResourceContainer());
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        super.resourceDataChanged(type, updateObject);
        if (type == ChangeType.CAPABILITY) {
            if (updateObject instanceof EditableCapability) {
                checkEditable();
            } else if (updateObject instanceof ImagingCapability) {
                checkImagingProperties();
            }
        }
    }

    @Override
    public String getName() {
        return resourceData.getFile().getName();
    }

    @Override
    public Rectangle getRectangle() {
        if (canvasBounds == null) {
            return null;
        }
        int north = (int) (canvasBounds.y + resourceData.getNorth()
                * canvasBounds.height / 100.0);
        int east = (int) (canvasBounds.x + resourceData.getEast()
                * canvasBounds.width / 100.0);
        int south = (int) (canvasBounds.y + resourceData.getSouth()
                * canvasBounds.height / 100.0);
        int west = (int) (canvasBounds.x + resourceData.getWest()
                * canvasBounds.width / 100.0);
        return new Rectangle(west, north, east - west, south - north);
    }

    @Override
    public void resize(Rectangle rect) {
        resourceData.setNorth((rect.y - canvasBounds.y) * 100.0
                / canvasBounds.height);
        resourceData.setEast((rect.x + rect.width - canvasBounds.x) * 100.0
                / canvasBounds.width);
        resourceData.setSouth((rect.y + rect.height - canvasBounds.y) * 100.0
                / canvasBounds.height);
        resourceData.setWest((rect.x - canvasBounds.x) * 100.0
                / canvasBounds.width);
        issueRefresh();
    }

    protected void checkEditable() {
        EditableCapability eCap = getCapability(EditableCapability.class);
        if (eCap.isEditable()) {
            inputHandler.enable(getResourceContainer());
        } else {
            inputHandler.disable(getResourceContainer());
        }
        issueRefresh();
    }

    protected void checkImagingProperties() {
        if (image != null) {
            ImagingCapability iCap = getCapability(ImagingCapability.class);
            image.setBrightness(iCap.getBrightness());
            image.setContrast(iCap.getContrast());
        }
    }

}
