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
import java.util.Collection;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.awipstools.ui.display.AwipsToolsResourceData;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Implements the points bearing functionality.
 * 
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Oct032007    #463        ebabin      Initial Creation.
 *  26Oct2007    #504        ebabin      Update to use BaselineLoader class.
 *  02Sept2008   #1516       dhladky     de-JIBX baselines.
 *  14Oct2009    #810        bsteffen    Fix for grabbing points.
 *  10-21-09     #1711       bsteffen    Refactor to common MovableTool model
 *  05-27-10     #5362       bkowal      When NULL Is Passed To The 'move'
 *                                       Function As The Last Coordinate,
 *                                       The Current Coordinates Of The Point
 *                                       Passed To The Function Are Used.
 *                                       Constructor Now Sets The
 *                                       'rightClickMovesToCoord' Indicator
 *                                       Owned By The Super Class.
 *  02-08-11     #8214       bkowal      Points now have the Magnification
 *                                       capability.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public class PointsToolLayer extends AbstractMovableToolLayer<Point> implements
        IContextMenuContributor, IPointChangedListener, IResourceDataChanged {

    public static final String DEFAULT_NAME = "Interactive Points";

    private AbstractRightClickAction deleteElementAction;

    private AbstractRightClickAction moveElementAction;

    private PointsDataManager dataManager = PointsDataManager.getInstance();

    private static GeometryFactory gf = new GeometryFactory();

    private IFont font = null;

    public PointsToolLayer(
            AwipsToolsResourceData<PointsToolLayer> resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        deleteElementAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                deleteSelected();
            }
        };
        deleteElementAction.setText("Delete Entire Element");
        moveElementAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                makeSelectedLive();
            }
        };
        moveElementAction.setText("Move Entire Element");

        this.rightClickMovesToCoord = true;
        this.resourceData.addChangeListener(this);
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        dataManager.removePointsChangedListener(this);
        if (font != null) {
            font.dispose();
        }
        this.resourceData.removeChangeListener(this);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        resetPoints();
        dataManager.addPointsChangedListener(this);
    }

    private void resetPoints() {
        Object selectedLabel = null;
        if (selectedObject != null) {
            selectedLabel = selectedObject.getUserData();
        }
        Point selectedPoint = null;
        Collection<Point> points = new ArrayList<Point>();
        for (String name : dataManager.getPointNames()) {
            Coordinate coordinate = dataManager.getPoint(name);
            Point point = gf.createPoint(coordinate);
            point.setUserData(name);
            points.add(point);
            if (name.equals(selectedLabel)) {
                selectedPoint = point;
            }
        }
        setObjects(points);
        selectedObject = selectedPoint;
    }

    @Override
    protected void paint(IGraphicsTarget target, PaintProperties paintProps,
            Point point, SelectionStatus status) throws VizException {

        RGB color = getCapability(ColorableCapability.class).getColor();
        if (status == SelectionStatus.SELECTED) {
            color = GRAY;
        }
        double radius = (MAGIC_CIRCLE_RADIUS * paintProps.getZoomLevel());
        String label = (String) point.getUserData();
        double[] center = descriptor.worldToPixel(new double[] { point.getX(),
                point.getY() });

        // Outer unfilled circle
        DrawableCircle dc = new DrawableCircle();
        dc.basics.x = center[0];
        dc.basics.y = center[1];
        dc.radius = radius;
        dc.basics.color = color;
        dc.lineWidth = 1;

        // Inner filled cicle
        DrawableCircle dfc = new DrawableCircle();
        dfc.basics.x = center[0];
        dfc.basics.y = center[1];
        dfc.radius = radius / 2;
        dfc.basics.color = color;
        dfc.lineWidth = 1;
        dfc.filled = true;

        // draw circles
        target.drawCircle(dc, dfc);

        double labelLoc[] = target.getPointOnCircle(center[0], center[1], 0.0,
                radius, 0);
        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        if (font == null) {
            float defaultSize = target.getDefaultFont().getFontSize();
            font = target.initializeFont(target.getDefaultFont().getFontName(),
                    (float) (defaultSize * magnification), target
                            .getDefaultFont().getStyle());
        }

        DrawableString parameters = new DrawableString(label, color);
        parameters.font = font;
        parameters.setCoordinates(labelLoc[0], labelLoc[1]);
        // parameters.magnification = magnification; // magnification handled by
        // // font
        target.drawStrings(parameters);
    }

    @Override
    public String getDefaultName() {
        return DEFAULT_NAME;
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (isEditable() && selectedObject != null) {
            menuManager.add(deleteElementAction);
            menuManager.add(moveElementAction);
        }
    }

    @Override
    public void pointChanged() {
        resetPoints();
        issueRefresh();
    }

    @Override
    protected boolean isClicked(IDisplayPaneContainer container,
            Coordinate mouseLoc, Point point) {
        endpointClicked = false;
        double[] pixelLoc = container.translateInverseClick(point
                .getCoordinate());
        Point pixelPoint = gf.createPoint(new Coordinate(pixelLoc[0],
                pixelLoc[1]));
        Point clickPoint = gf.createPoint(mouseLoc);
        if (pixelPoint.isWithinDistance(clickPoint, MAGIC_CLICK_DISTANCE)) {
            endpointClicked = true;
            return true;
        }
        return false;

    }

    @Override
    protected Point makeLive(Point object) {
        return (Point) object.clone();
    }

    @Override
    protected Point move(Coordinate lastClickedCoordinate, Coordinate clickLoc,
            Point point) {
        if (clickLoc == null) {
            clickLoc = lastClickedCoordinate;
            // Get The Current Coordinates Of The "Point".
            lastClickedCoordinate = this.dataManager.getPoint(point
                    .getUserData().toString());
        }

        Point newPoint = gf.createPoint(clickLoc);
        newPoint.setUserData(point.getUserData());
        return newPoint;
    }

    @Override
    protected void save(Point oldPoint, Point point) {
        String label = (String) point.getUserData();
        dataManager.setPoint(label, point.getCoordinate());
        resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, null);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                if (font != null) {
                    font.dispose();
                    font = null;
                }
            }
        }
        this.issueRefresh();
    }
}
