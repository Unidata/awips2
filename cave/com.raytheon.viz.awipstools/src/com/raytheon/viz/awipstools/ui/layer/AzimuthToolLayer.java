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

import java.awt.geom.Point2D;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.tools.AbstractMovableToolLayer;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackDisplay;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackUIManager;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a Az/Ran drawing layer. Shows 30,60,90,120 nm rings with
 * corresponding 30degree spokes. Initially draws at HomeLoc, then allows in
 * edit mode, the user to drag the ring around.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Sep192007    #444        ebabin      Initial Creation.
 *  10-21-09    #1711       bsteffen    Modified to use datamanager for home location
 *  05-28-10    #5361        bkowal      Updated the 'handleMouseDown' function so
 *                                       that it would change the "home" location
 *                                       of the tool when the right-mouse
 *                                       button was clicked.
 *  06-03-10    #5631        bkowal      The tool will now be editable by default. 
 *  06-14-10    #6360        bkowal      Ensured that the position of the tool would not
 *                                       be changed twice when the user pressed [1] and
 *                                       released [2] the right-mouse button.
 *  07-28-14    #3430        mapeters    Updated the 'handleMouseUp' function to prevent
 *                                       errors created when MB3 clicking off the map
 *                                       in editable mode.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public class AzimuthToolLayer extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> implements
        IResourceDataChanged, IContextMenuContributor {

    public static final String AZIMUTH_LOCATION = "Az/Ran Overlay";

    private static final int[] RANGES = new int[] { 30, 60, 90, 120 };

    private static final int[] ANGLES = new int[] { 0, 30, 60, 90, 120, 150,
            180, 210, 240, 270, 300, 330 };

    private static final double RANGE_LABEL_ANGLE = 48.0;

    private static final UnitConverter NM_to_M = NonSI.NAUTICAL_MILE
            .getConverterTo(SI.METER);

    private boolean recreate = true;

    private Coordinate currCoordinate = null;

    private double[] centerPixel = null;

    private Coordinate nextCoordinate = null;

    private IFont labelFont;

    private IWireframeShape shape;

    private DrawableString[] labels = new DrawableString[RANGES.length
            + ANGLES.length];

    private IInputHandler mouseHandler = new InputAdapter() {

        private Shell lastShell = null;

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (nextCoordinate != null || (mouseButton == 3 && isEditable())) {
                currCoordinate = getResourceContainer().translateClick(x, y);

                if (currCoordinate == null) {
                    return false;
                }

                centerPixel = descriptor.worldToPixel(new double[] {
                        currCoordinate.x, currCoordinate.y });
                nextCoordinate = null;

                if (lastShell != null) {
                    lastShell.setCursor(lastShell.getDisplay().getSystemCursor(
                            SWT.CURSOR_ARROW));
                    lastShell = null;
                }

                recreate = true;
                issueRefresh();
                return true;
            }
            return false;
        }

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            if (isEditable() && mouseButton == 1 && lastShell != null) {
                nextCoordinate = new Coordinate(x, y);
                return true;
            }
            return false;
        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            if (nextCoordinate != null) {
                nextCoordinate = getResourceContainer().translateClick(x, y);
                issueRefresh();
                return true;
            }
            return false;
        }

        @Override
        public boolean handleMouseMove(int x, int y) {
            if (StormTrackUIManager.getCoordinateIndex(getResourceContainer(),
                    new Coordinate[] { currCoordinate }, new Coordinate(x, y),
                    7.0) >= 0) {
                IWorkbenchWindow window = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow();
                lastShell = window.getShell();
                lastShell.setCursor(lastShell.getDisplay().getSystemCursor(
                        SWT.CURSOR_HAND));
            } else if (lastShell != null) {
                lastShell.setCursor(lastShell.getDisplay().getSystemCursor(
                        SWT.CURSOR_ARROW));
                lastShell = null;
            }
            return false;
        }

    };

    public AzimuthToolLayer(GenericToolsResourceData<AzimuthToolLayer> data,
            LoadProperties props, MapDescriptor descriptor) {
        super(data, props);
        setDescriptor(descriptor);
        data.addChangeListener(this);
        this.currCoordinate = PointsDataManager.getInstance().getHome();
        this.centerPixel = descriptor.worldToPixel(new double[] {
                currCoordinate.x, currCoordinate.y });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    public String getName() {
        return AZIMUTH_LOCATION;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    protected void initInternal(IGraphicsTarget target) throws VizException {
        labelFont = target.initializeFont(
                target.getDefaultFont().getFontName(), 12.0f,
                new Style[] { Style.BOLD });

        for (int i = 0; i < labels.length; ++i) {
            DrawableString str = new DrawableString("", null);
            str.font = labelFont;
            str.horizontalAlignment = HorizontalAlignment.CENTER;
            str.verticallAlignment = VerticalAlignment.MIDDLE;
            labels[i] = str;
        }

        EditableManager.makeEditable(this, true);
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null && mouseHandler != null) {
            container.registerMouseHandler(mouseHandler);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        labelFont.setMagnification(getCapability(MagnificationCapability.class)
                .getMagnification().floatValue());

        if (recreate) {
            if (shape != null) {
                shape.dispose();
            }
            shape = target.createWireframeShape(false, descriptor);
            drawRings(target, paintProps, currCoordinate, centerPixel);
            drawSpokes(target, currCoordinate);
            recreate = false;
        }

        target.drawWireframeShape(shape,
                getCapability(ColorableCapability.class).getColor(),
                getCapability(OutlineCapability.class).getOutlineWidth(),
                getCapability(OutlineCapability.class).getLineStyle(),
                labelFont);

        RGB centerColor = getCapability(ColorableCapability.class).getColor();
        if (nextCoordinate != null) {
            double[] next = descriptor.worldToPixel(new double[] {
                    nextCoordinate.x, nextCoordinate.y });
            target.drawPoint(next[0], next[1], 0.0, centerColor,
                    PointStyle.CIRCLE, 0.5f);
            centerColor = AbstractMovableToolLayer.GRAY;
        }

        target.drawPoint(centerPixel[0], centerPixel[1], 0.0, centerColor,
                PointStyle.CIRCLE, 0.5f);

        target.drawStrings(labels);
    }

    private boolean isEditable() {
        return getCapability(EditableCapability.class).isEditable();
    }

    private void drawRings(IGraphicsTarget target, PaintProperties paintProps,
            Coordinate centerLocation, double[] centerPixel)
            throws VizException {
        GeodeticCalculator calc = new GeodeticCalculator();
        calc.setStartingGeographicPoint(centerLocation.x, centerLocation.y);

        RGB color = getCapability(ColorableCapability.class).getColor();

        for (int i = 0; i < RANGES.length; ++i) {
            int range = RANGES[i];

            double[][] points = new double[361][];

            for (int j = -180; j < 180; ++j) {
                calc.setDirection(j, NM_to_M.convert(range));

                Point2D point = calc.getDestinationGeographicPoint();
                double[] end = descriptor.worldToPixel(new double[] {
                        point.getX(), point.getY() });

                points[j + 180] = end;

                if (j == RANGE_LABEL_ANGLE) {
                    labels[i].setCoordinates(end[0], end[1]);
                    labels[i].setText(range + " nm", color);
                }
            }

            points[points.length - 1] = points[0];

            shape.addLineSegment(points);
        }
    }

    private void drawSpokes(IGraphicsTarget target, Coordinate centerLocation)
            throws VizException {
        GeodeticCalculator calc = new GeodeticCalculator();
        calc.setStartingGeographicPoint(centerLocation.x, centerLocation.y);

        RGB color = getCapability(ColorableCapability.class).getColor();

        for (int i = 0; i < ANGLES.length; ++i) {
            int angle = ANGLES[i];

            double[] start, end;
            calc.setDirection(StormTrackDisplay.adjustAngle(angle),
                    NM_to_M.convert(RANGES[0]));
            Point2D point = calc.getDestinationGeographicPoint();
            start = descriptor.worldToPixel(new double[] { point.getX(),
                    point.getY() });

            calc.setDirection(StormTrackDisplay.adjustAngle(angle),
                    NM_to_M.convert(RANGES[RANGES.length - 1]));
            point = calc.getDestinationGeographicPoint();
            end = descriptor.worldToPixel(new double[] { point.getX(),
                    point.getY() });

            shape.addLineSegment(new double[][] { start, end });

            DrawableString label = labels[RANGES.length + i];
            label.setText("" + angle, color);
            label.setCoordinates(end[0], end[1]);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    protected void disposeInternal() {
        labelFont.dispose();
        if (shape != null) {
            shape.dispose();
        }

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(mouseHandler);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IContextMenuContributor#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, final int x,
            final int y) {
        if (isEditable()) {
            menuManager.add(new Action("Select Location") {
                @Override
                public void run() {
                    currCoordinate = getResourceContainer()
                            .translateClick(x, y);
                    issueRefresh();
                }
            });
        }
    }

}
