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

package com.raytheon.viz.awipstools.ui.layer;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.tools.AbstractMovableToolLayer;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.awipstools.ui.action.LapsToolsData;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         bsteffen    Intial creation.
 * 07-21-14     #3412      mapeters    Updated deprecated drawCircle call.
 * 07-29-14     #3465      mapeters    Updated deprecated drawString() calls.
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class LapsToolLayer extends AbstractMovableToolLayer<Coordinate>
        implements IContextMenuContributor {

    private LapsToolsData data;

    public static final String DEFAULT_NAME = "Laps Relocator";

    private final AbstractRightClickAction selectLocationAction;

    private final AbstractRightClickAction moveElementAction;

    private IWireframeShape validShape;

    private IWireframeShape gridShape;

    public LapsToolLayer(GenericToolsResourceData<LapsToolLayer> resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
        selectLocationAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                save(null, lastMouseLoc);
            }
        };
        selectLocationAction.setText("Select Location");
        moveElementAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                makeSelectedLive();
            }
        };
        moveElementAction.setText("Move Entire Element");
        this.rightClickMovesToCoord = true;
        resourceData.addChangeListener(this);
    }

    @Override
    protected void disposeInternal() {
        if (validShape != null) {
            validShape.dispose();
            validShape = null;
        }
        if (gridShape != null) {
            gridShape.dispose();
            gridShape = null;
        }
        super.disposeInternal();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        setObjects(new ArrayList<Coordinate>(
                Arrays.asList(data.getGridCenter())));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.layer.AbstractMovableToolLayer#paintInternal
     * (com.raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);
        if (validShape == null) {
            validShape = target.createWireframeShape(false, descriptor);
            Coordinate[] coords = new Coordinate[5];
            Envelope area = data.getValidArea();
            coords[0] = new Coordinate(area.getMinX(), area.getMinY());
            coords[1] = new Coordinate(area.getMinX(), area.getMaxY());
            coords[2] = new Coordinate(area.getMaxX(), area.getMaxY());
            coords[3] = new Coordinate(area.getMaxX(), area.getMinY());
            coords[4] = coords[0];
            validShape.addLineSegment(coords);
        }
        if (gridShape == null) {
            gridShape = target.createWireframeShape(false, descriptor);
            Coordinate[] coords = new Coordinate[5];
            Envelope area = data.getGridArea();
            coords[0] = new Coordinate(area.getMinX(), area.getMinY());
            coords[1] = new Coordinate(area.getMinX(), area.getMaxY());
            coords[2] = new Coordinate(area.getMaxX(), area.getMaxY());
            coords[3] = new Coordinate(area.getMaxX(), area.getMinY());
            coords[4] = coords[0];
            gridShape.addLineSegment(coords);
        }
        RGB color = getCapability(ColorableCapability.class).getColor();
        target.drawWireframeShape(validShape, color, 1, LineStyle.DASHED_LARGE);
        target.drawWireframeShape(gridShape, color, 1, LineStyle.SOLID);
    }

    @Override
    protected void paint(IGraphicsTarget target, PaintProperties paintProps,
            Coordinate home, SelectionStatus status) throws VizException {
        RGB color = getCapability(ColorableCapability.class).getColor();
        if (status == SelectionStatus.SELECTED) {
            color = GRAY;
        }
        double radius = (MAGIC_CIRCLE_RADIUS * paintProps.getZoomLevel());
        double[] center = descriptor
                .worldToPixel(new double[] { home.x, home.y });
        DrawableCircle circle = new DrawableCircle();
        circle.setCoordinates(center[0], center[1]);
        circle.radius = radius;
        circle.basics.color = color;
        target.drawCircle(circle);
        double labelLoc[] = target.getPointOnCircle(center[0], center[1], 0.0,
                radius, 0);
        DrawableString string = new DrawableString("center point", color);
        string.setCoordinates(labelLoc[0], labelLoc[1]);
        target.drawStrings(string);
    }

    @Override
    public String getDefaultName() {
        return DEFAULT_NAME;
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (isEditable() && selectedObject != null) {
            menuManager.add(moveElementAction);
        }
        if (isEditable() && data.getValidArea().contains(lastMouseLoc)) {
            menuManager.add(selectLocationAction);
        }
    }

    @Override
    protected boolean isClicked(IDisplayPaneContainer container,
            Coordinate mouseLoc, Coordinate object) {
        this.endpointClicked = false;
        double[] pointPixel = container.translateInverseClick(object);
        double distance = (mouseLoc.x - pointPixel[0])
                * (mouseLoc.x - pointPixel[0]) + (mouseLoc.y - pointPixel[1])
                * (mouseLoc.y - pointPixel[1]);
        if (distance < MAGIC_CLICK_DISTANCE * MAGIC_CLICK_DISTANCE) {
            this.endpointClicked = true;
            return true;
        }
        return false;
    }

    @Override
    protected Coordinate makeLive(Coordinate object) {
        return new Coordinate(object);
    }

    @Override
    protected Coordinate move(Coordinate lastMouseLoc, Coordinate mouseLoc,
            Coordinate object) {
        if (mouseLoc != null && data.getValidArea().contains(mouseLoc)) {
            return new Coordinate(mouseLoc);
        } else {
            return object;
        }
    }

    @Override
    protected void save(Coordinate oldCoordinate, Coordinate coordinate) {
        data.setGridCenterLon(coordinate.x);
        data.setGridCenterLat(coordinate.y);
        resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, null);
    }

    public LapsToolsData getData() {
        return data;
    }

    public void setData(LapsToolsData data) {
        this.data = data;
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        super.resourceChanged(type, object);
        if (type == ChangeType.DATA_UPDATE) {
            setObjects(new ArrayList<Coordinate>(Arrays.asList(data
                    .getGridCenter())));
            if (validShape != null) {
                validShape.dispose();
                validShape = null;
            }
            if (gridShape != null) {
                gridShape.dispose();
                gridShape = null;
            }
            issueRefresh();
        }
    }

}
