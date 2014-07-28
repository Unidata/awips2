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

package com.raytheon.uf.viz.points.ui.layer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.MessageBox;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
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
import com.raytheon.uf.viz.core.rsc.tools.AbstractMovableToolLayer;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.uf.viz.points.data.PointFieldState;
import com.raytheon.uf.viz.points.data.PointNameChangeException;
import com.raytheon.uf.viz.points.ui.dialog.PointEditDialog;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

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
 *  07-31-2012   #875        rferrel     Converted to use points.
 *  07-28-2014   #3430       mapeters    Updated move function to prevent errors
 *                                       when MB3 clicking off the map with
 *                                       tool in editable mode.
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

    private AbstractRightClickAction editElementAction;

    private AbstractRightClickAction hideElementAction;

    private AbstractRightClickAction moveElementAction;

    private AbstractRightClickAction createElementAction;

    private PointsDataManager dataManager;

    private static GeometryFactory gf = new GeometryFactory();

    Map<Integer, IFont> fonts;

    public PointsToolLayer(
            GenericToolsResourceData<PointsToolLayer> resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        this.dataManager = PointsDataManager.getInstance();
        deleteElementAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                deletePoint(selectedObject);
            }
        };
        deleteElementAction.setText("Delete Point");

        editElementAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                editPoint(selectedObject);
            }
        };
        editElementAction.setText("Edit Point...");

        hideElementAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                hidePoint(selectedObject);
            }
        };
        hideElementAction.setText("Hide Point");

        moveElementAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                makeSelectedLive();
            }
        };
        moveElementAction.setText("Move Point");

        createElementAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                createPoint();
            }
        };
        createElementAction.setText("New Point...");

        this.rightClickMovesToCoord = true;
        this.resourceData.addChangeListener(this);
        this.fonts = new HashMap<Integer, IFont>();
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        dataManager.removePointsChangedListener(this);
        for (IFont font : fonts.values()) {
            font.dispose();
        }
        fonts.clear();
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
            selectedLabel = selectedObject.getName();
        }
        Point selectedPoint = null;
        Collection<Point> points = new ArrayList<Point>();
        for (String name : dataManager.getVisiblePointNames()) {
            Point point = dataManager.getPoint(name);
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

        RGB color = point.getColor();
        if (status == SelectionStatus.SELECTED) {
            color = GRAY;
        } else if (color == null || !point.isColorActive()) {
            color = getCapability(ColorableCapability.class).getColor();
        }

        double radius = (MAGIC_CIRCLE_RADIUS * paintProps.getZoomLevel());
        String label = point.getName();
        Coordinate coordinate = point.getCoordinate();
        double[] center = descriptor.worldToPixel(new double[] { coordinate.x,
                coordinate.y });

        // Outer unfilled circle
        DrawableCircle dc = new DrawableCircle();
        dc.basics.x = center[0];
        dc.basics.y = center[1];
        dc.radius = radius;
        dc.basics.color = color;
        dc.lineWidth = 1;

        // Inner filled circle
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

        int fontSize = point.getFontSize().getFontSize();
        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        Integer fontKey = fontSize;
        IFont font = fonts.get(fontKey);

        if (font == null) {
            font = target.getDefaultFont().deriveWithSize(
                    (float) (fontSize * magnification));
            fonts.put(fontKey, font);
        }

        DrawableString parameters = new DrawableString(label, color);
        parameters.font = font;
        parameters.setCoordinates(labelLoc[0], labelLoc[1]);
        target.drawStrings(parameters);
    }

    @Override
    public String getDefaultName() {
        return DEFAULT_NAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IContextMenuContributor#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager, int, int)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (isEditable()) {
            selectedObject = null;
            selectObjectAtMouse(x, y, -1);
            if (selectedObject != null) {
                menuManager.add(editElementAction);
                menuManager.add(hideElementAction);
                menuManager.add(deleteElementAction);
                if (selectedObject.getMovable() == PointFieldState.TRUE) {
                    menuManager.add(moveElementAction);
                }
            } else {
                menuManager.add(createElementAction);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (isEditable()) {
            Coordinate point = null;
            IDisplayPaneContainer container = getResourceContainer();
            try {
                double[] pixelLoc = container.translateInverseClick(coord
                        .asLatLon());
                point = new Coordinate(pixelLoc[0], pixelLoc[1]);
            } catch (TransformException e) {
                throw new VizException(e.getMessage());
            } catch (FactoryException e) {
                throw new VizException(e.getMessage());
            }
            for (Point object : objects) {
                if (isClicked(getResourceContainer(), point, object)) {
                    return object.getName() + " Movable: "
                            + object.getMovable().toString().toLowerCase();
                }
            }
        }
        return null;
    }

    @Override
    public void pointChanged() {
        resetPoints();
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.layer.AbstractMovableToolLayer#isClicked
     * (com.raytheon.uf.viz.core.IDisplayPaneContainer,
     * com.vividsolutions.jts.geom.Coordinate, java.lang.Object)
     */
    @Override
    protected boolean isClicked(IDisplayPaneContainer container,
            Coordinate mouseLoc, Point point) {
        endpointClicked = false;
        double[] pixelLoc = container.translateInverseClick(point
                .getCoordinate());
        com.vividsolutions.jts.geom.Point pixelPoint = gf
                .createPoint(new Coordinate(pixelLoc[0], pixelLoc[1]));
        com.vividsolutions.jts.geom.Point clickPoint = gf.createPoint(mouseLoc);
        if (pixelPoint.isWithinDistance(clickPoint, MAGIC_CLICK_DISTANCE)) {
            endpointClicked = true;
            return true;
        }
        return false;

    }

    @Override
    protected Point makeLive(Point object) {
        return new Point(object);
    }

    @Override
    protected Point move(Coordinate lastClickedCoordinate, Coordinate clickLoc,
            Point point) {
        if (point.getMovable() == PointFieldState.TRUE) {
            if (clickLoc == null) {
                clickLoc = (lastClickedCoordinate == null) ? this.dataManager
                        .getCoordinate(point.getName()) : lastClickedCoordinate;
            }

            Point newPoint = new Point(point);
            newPoint.setCoordinate(clickLoc);
            return newPoint;
        }
        return point;
    }

    @Override
    protected void save(Point oldPoint, Point point) {
        String label = point.getName();
        dataManager.setCoordinate(label, point.getCoordinate());
        resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, null);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                for (IFont font : fonts.values()) {
                    font.dispose();
                }
                fonts.clear();
            }
        }
        this.issueRefresh();
    }

    public void addPoint(Point point) {
        if (point != null) {
            dataManager.addPoint(point);
        }
    }

    public void hidePoint(Point point) {
        if (point != null) {
            point.setHidden(PointFieldState.TRUE);
            dataManager.addPoint(point);
        }
    }

    public boolean deletePoint(Point point) {
        boolean state = false;
        if (point != null) {
            MessageBox d = new MessageBox(getResourceContainer()
                    .getActiveDisplayPane().getDisplay().getActiveShell(),
                    SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
            if (point.isGroup()) {
                d.setMessage("Click OK to delete the group:\n"
                        + point.getGroup()
                        + "\nand all points and sub-groups\nit contains.");
            } else {
                d.setMessage("Click OK to delete point " + point.getName()
                        + "?");
            }
            int status = d.open();
            if (status == SWT.OK) {
                state = true;
                dataManager.deletePoint(point);
            }
        }
        return state;
    }

    private void editPoint(final Point point) {
        ICloseCallback cb = new ICloseCallback() {

            @Override
            public void dialogClosed(Object returnValue) {
                if (returnValue instanceof Point) {
                    Point em = (Point) returnValue;
                    dataManager.updatePoint(point, em);
                }
            }
        };
        PointEditDialog.editPointViaDialog(this, point, cb);
    }

    private void createPoint() {
        Point point = new Point("", lastMouseLoc.y, lastMouseLoc.x,
                PointFieldState.FALSE, PointFieldState.TRUE, false, new RGB(0,
                        0, 0), "");
        ICloseCallback cb = new ICloseCallback() {

            @Override
            public void dialogClosed(Object returnValue) {
                if (returnValue instanceof Point) {
                    Point em = (Point) returnValue;
                    dataManager.addPoint(em);
                }
            }
        };
        PointEditDialog.createNewPointViaDialog(this, point, cb);
    }

    /**
     * This is used to change everything on the point except for its unique
     * name. This throws an exception if the point does not exist.
     * 
     * @param point
     * @throws PointNameChangeException
     */
    public void updatePoint(Point point) throws PointNameChangeException {
        dataManager.updatePoint(point);
    }

    public void updateChildrenHidden(IPointNode node, PointFieldState state) {
        dataManager.updateChildrenHidden(node, state);
    }

    public void updateChildrenMovable(IPointNode node, PointFieldState state) {
        dataManager.updateChildrenMovable(node, state);
    }
}
