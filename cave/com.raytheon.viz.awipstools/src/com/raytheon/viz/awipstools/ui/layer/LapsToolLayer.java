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
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
//import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
//import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
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
 * Nov 2013     #          mccaslin    Draw more graphical boxes: for CWA, previous domain, etc

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

    private IWireframeShape validShapeOrig;
    
    private IWireframeShape validShape;

    private IWireframeShape gridShape;

    private RGB labelColor;

    public static String centerLabel = "Center Point";

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
        
        Envelope shapeArea = data.getValidArea();
        if (validShape == null) {
            validShape = target.createWireframeShape(false, descriptor);
            Coordinate[] coords = new Coordinate[5];

            coords[0] = new Coordinate(shapeArea.getMinX(), shapeArea.getMinY());
            coords[1] = new Coordinate(shapeArea.getMinX(), shapeArea.getMaxY());
            coords[2] = new Coordinate(shapeArea.getMaxX(), shapeArea.getMaxY());
            coords[3] = new Coordinate(shapeArea.getMaxX(), shapeArea.getMinY());
            coords[4] = coords[0];
            validShape.addLineSegment(coords);
        }

        Envelope shapeArea2 = data.getValidAreaOrig();
        if (validShapeOrig == null) {
            validShapeOrig = target.createWireframeShape(false, descriptor);
            Coordinate[] coords = new Coordinate[5];
            coords[0] = new Coordinate(shapeArea2.getMinX(), shapeArea2.getMinY());
            coords[1] = new Coordinate(shapeArea2.getMinX(), shapeArea2.getMaxY());
            coords[2] = new Coordinate(shapeArea2.getMaxX(), shapeArea2.getMaxY());
            coords[3] = new Coordinate(shapeArea2.getMaxX(), shapeArea2.getMinY());
            coords[4] = coords[0];
            validShapeOrig.addLineSegment(coords);
        }
        
        Envelope gridArea = data.getGridArea();
        if (gridShape == null) {
            gridShape = target.createWireframeShape(false, descriptor);
            Coordinate[] coords = new Coordinate[5];

            coords[0] = new Coordinate(gridArea.getMinX(), gridArea.getMinY());
            coords[1] = new Coordinate(gridArea.getMinX(), gridArea.getMaxY());
            coords[2] = new Coordinate(gridArea.getMaxX(), gridArea.getMaxY());
            coords[3] = new Coordinate(gridArea.getMaxX(), gridArea.getMinY());
            coords[4] = coords[0];
            gridShape.addLineSegment(coords);
        }
        
        //Test domain sizes
        data.setLimits(false);
        if (gridArea.getMinX() > shapeArea.getMinX()) {
            data.setLimits(true);
        }  if (gridArea.getMaxX() < shapeArea.getMaxX()) {
            data.setLimits(true);
        }  if (gridArea.getMinY() > shapeArea.getMinY()) {
            data.setLimits(true);
        }  if (gridArea.getMaxY() < shapeArea.getMaxY()) {
            data.setLimits(true);
        }
        
        RGB color = getCapability(ColorableCapability.class).getColor();
        RGB color2 = color;    
        
        // Projected grid domain too small, below the limits of the CWA...
        if (data.getLimits()) { 
        	color2 = new RGB ( 250, 40, 40);
        	labelColor = color2;
        	centerLabel = "[Center point]\nFull CWA is NOT covered by domain";		
        } else {
        	labelColor = color;
        	centerLabel = "Center point";
        }
        
        target.drawWireframeShape(validShape, color, 1, LineStyle.DASHED_LARGE);
        target.drawWireframeShape(gridShape, color2, 1, LineStyle.SOLID);
    	RGB gray = new RGB ( 90, 90, 90);
        target.drawWireframeShape(validShapeOrig, gray, 1, LineStyle.DASH_DOTTED);
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
        //14.1.1 and earlier: target.drawCircle(center[0], center[1], 0, radius, color, 1);
        double labelLoc[] = target.getPointOnCircle(center[0], center[1], 0.0,
                radius, 0);
        //DrawableString string = new DrawableString("center point", color);
        DrawableString string = new DrawableString(centerLabel, labelColor);
        string.setCoordinates(labelLoc[0], labelLoc[1]);
        target.drawStrings(string);
        //14.1.1 and earlier: target.drawString(null, centerLabel, labelLoc[0], labelLoc[1], 0.0, 
        //                    TextStyle.NORMAL, labelColor, HorizontalAlignment.LEFT, null);
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
            if (validShapeOrig != null) {
                validShapeOrig.dispose();
                validShapeOrig = null;
            }
            if (gridShape != null) {
                gridShape.dispose();
                gridShape = null;
            }
            issueRefresh();
        }
    }
    
    /*
    @see
    * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
    * referencing.crs.CoordinateReferenceSystem)
    */
    
    public void project(CoordinateReferenceSystem crs) throws VizException {
        if (validShape != null) {
            validShape.dispose();
            validShape = null;
        }
        if (validShapeOrig != null) {
            validShapeOrig.dispose();
            validShapeOrig = null;
        }
        if (gridShape != null) {
            gridShape.dispose();
            gridShape = null;
        }
        issueRefresh();
    }   

    protected void drawUpperLeftCornerLabel(IGraphicsTarget target,
            PaintProperties paintProps, String label) throws VizException {
        // TODO this screen location code is borrowed from MPELegendResource...
        // should it be put into a shared class, possibly a paint
        // properties method?
        IExtent screenExtent = paintProps.getView().getExtent();
        double scale = (screenExtent.getHeight() / paintProps.getCanvasBounds().height);
        DrawableString tmpDS = new DrawableString("0", new RGB(100, 100, 100));
        tmpDS.font = null;
        double textHeight = target.getStringsBounds(tmpDS).getHeight() * scale;
        double padding = 3 * scale;
        double textSpace = textHeight + padding;
        double cmapHeight = textHeight * 1.25;
        double legendHeight = cmapHeight + 2.0 * textSpace + 2.0 * padding;
        double y1 = screenExtent.getMinY() + legendHeight * 2.5;
        double x1 = screenExtent.getMinX() + padding * 10.0;
        DrawableString string = new DrawableString(label, this.getCapability(
                ColorableCapability.class).getColor());
        string.basics.x = x1; 
        string.basics.y = y1; 
        string.font = null;
        //string.textStyle = IGraphicsTarget.TextStyle.NORMAL;
        //string.horizontalAlignment = HorizontalAlignment.LEFT;
        target.drawStrings(string);
    }   


}
