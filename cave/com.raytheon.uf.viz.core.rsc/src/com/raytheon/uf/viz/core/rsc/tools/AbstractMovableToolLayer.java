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
package com.raytheon.uf.viz.core.rsc.tools;

import java.util.Collection;
import java.util.Random;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Used to render a tool which allows the user to click on and move things on a
 * map. This keeps a Collection of objects of type T and automatically handles
 * things like toggling editing and handling mouse clicks. T should generally be
 * some sort geographic data such as a coordinate or line segment, but all
 * operations are handled by subclasses so use whatever you want. Objects will
 * have one of 3 statuses Normal means to just Draw it on the map, Selected
 * means it is the object the user last clicked on, live is a copy of selected
 * which follows the mouse.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10-21-09     #1711      bsteffen    Initial Creation
 * 05-27-10     #5362      bkowal      Modified 'handleMouseDown' Function
 *                                     So That It Would Be Capable Of 
 *                                     Potentially Moving An Object To The
 *                                     Location Of The Mouse Cursor When The
 *                                     Right-Button Is Clicked - Determined
 *                                     By The New 'rightClickMovesToCoord'
 *                                     Indicator. 
 * 06-08-2010   #5620      bkowal      Added methods that would be capable of
 *                                     changing the mouse cursor whenever
 *                                     the user positioned their mouse over
 *                                     one of the tools. When the user moved
 *                                     their mouse away from the tool, the
 *                                     cursor was set back to the default.
 * 06-14-2010   #6360      bkowal      Ensured that the legend will not change
 *                                     when the user adds a vertex to a baseline
 *                                     via right-clicking. This change will also
 *                                     guarantee that any tool that supports the
 *                                     &quot;right-click-to-move&quot; functionality will
 *                                     not allow the legend to change.
 * 07-21-2010              bkowal      We will now display the standard SWT &quot;hand&quot;
 * Mar 21, 2013       1638 mschenke    Changed to use generic tool data
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @param <T>
 */
public abstract class AbstractMovableToolLayer<T> extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> implements
        IInputHandler, IResourceDataChanged {

    protected enum SelectionStatus {
        NORMAL, SELECTED, LIVE
    };

    // Not really magic, just called magic because I made them up and they have
    // no real meaning, really I am the magic one because I picked such good
    // numbers, they are ordinary numbers though
    protected static final int MAGIC_CLICK_DISTANCE = 15;

    protected static final int MAGIC_CIRCLE_RADIUS = 40;

    protected static final int MAGIC_X_RADIUS = 60;

    public static final RGB GRAY = new RGB(127, 127, 127);

    protected Coordinate lastMouseLoc;

    protected T selectedObject = null;

    protected T liveObject = null;

    protected Collection<T> objects;

    /*
     * Added as part of DR #5620. The requirements state that a different cursor
     * should be displayed when the user positions their mouse over one of the
     * endpoints of the Distance Bearing line segments.
     */
    protected boolean endpointClicked = false;

    // Presently Only Points And Baselines Implement This
    // Behavior Per DR 5360 & DR 5632.But, There Are No
    // Less Than Six (6) Classes That Extend This Abstract
    // Class. So, Any Class That Must Implement The
    // Afore-Mentioned Behavior Will Need To Set This
    // Field To TRUE.
    protected boolean rightClickMovesToCoord;

    protected AbstractMovableToolLayer(
            GenericToolsResourceData<? extends AbstractMovableToolLayer<T>> resourceData,
            LoadProperties loadProperties) {
        this(resourceData, loadProperties, true);
    }

    protected AbstractMovableToolLayer(
            GenericToolsResourceData<? extends AbstractMovableToolLayer<T>> resourceData,
            LoadProperties loadProperties, boolean editable) {
        super(resourceData, loadProperties);
        getCapability(EditableCapability.class).setEditable(editable);
        this.rightClickMovesToCoord = false;
        this.endpointClicked = false;
    }

    /**
     * Set the objects which this resource is displaying
     * 
     * @param objects
     *            of type T
     */
    protected void setObjects(Collection<T> objects) {
        this.objects = objects;
        liveObject = null;
        selectedObject = null;
    }

    /**
     * Delete whichever object the user has currently selected
     */
    protected void deleteSelected() {
        objects.remove(selectedObject);
        issueRefresh();
    }

    /**
     * make the currently selected object live
     */
    protected void makeSelectedLive() {
        liveObject = makeLive(selectedObject);
        issueRefresh();
    }

    /**
     * Replace one object with another
     * 
     * @param oldObject
     * @param newObject
     */
    protected void replace(T oldObject, T newObject) {
        if (selectedObject == oldObject) {
            selectedObject = newObject;
        }
        if (liveObject == oldObject) {
            liveObject = newObject;
        }
        if (objects.remove(oldObject)) {
            objects.add(newObject);
        }
    }

    @Override
    protected void disposeInternal() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(this);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        EditableManager.makeEditable(this,
                getCapability(EditableCapability.class).isEditable());
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(this);
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (liveObject != null) {
            paint(target, paintProps, selectedObject, SelectionStatus.SELECTED);
            paint(target, paintProps, liveObject, SelectionStatus.LIVE);
        }
        for (T object : objects) {
            if ((liveObject != null) && selectedObject.equals(object)) {
                continue;
            }
            paint(target, paintProps, object, SelectionStatus.NORMAL);
        }
    }

    @Override
    public String getName() {
        return getDefaultName();
    }

    /**
     * Paint an object given its status
     * 
     * @param target
     * @param paintProps
     * @param object
     * @param status
     * @throws VizException
     */
    protected abstract void paint(IGraphicsTarget target,
            PaintProperties paintProps, T object, SelectionStatus status)
            throws VizException;

    /**
     * Determine if the object has been clicked, mouseLoc is the canvas
     * location, to compare to world coordinates use mapEditor.translateClick or
     * mapEditor.translateInverseClick, I recommand the inverse one for more
     * accuracy
     * 
     * @param mapEditor
     * @param mouseLoc
     * @param object
     * @return
     */
    protected abstract boolean isClicked(IDisplayPaneContainer container,
            Coordinate mouseLoc, T object);

    /**
     * Copy an object, the newly created object will become the "live object"
     * 
     * @param object
     * @return
     */
    protected abstract T makeLive(T object);

    /**
     * Move an object, both the lastMouseLoc and curMouseLoc are in Lat/Lon
     * 
     * @param lastMouseLoc
     *            Where the mouse started in Lat/Lon
     * @param mouseLoc
     *            Where the mouse is now in Lat/Lon
     * @param object
     *            the object to move, probably the live Object
     * @return
     */
    protected abstract T move(Coordinate lastMouseLoc, Coordinate mouseLoc,
            T object);

    /**
     * Get the default name, editable will be added if this resource is
     * currently editable
     * 
     * @return
     */
    protected abstract String getDefaultName();

    /**
     * Called when user clicks "Button 2" on label.
     * 
     */
    public boolean isEditable() {
        return getCapability(EditableCapability.class).isEditable();
    }

    public boolean handleMouseDown(int x, int y, int mouseButton) {
        if ((liveObject == null) && isEditable()) {
            IDisplayPaneContainer container = getResourceContainer();
            lastMouseLoc = container.translateClick(x, y);

            if (this.rightClickMovesToCoord && (mouseButton == 3)) {
                /*
                 * We do not want to do anything in this case until the user
                 * releases the mouse button to ensure that the user does not
                 * open a context menu.
                 */
                return true;
            }

            if (selectObjectAtMouse(x, y, mouseButton)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 
     * @return true if the selected object was made live
     */
    public boolean selectObjectAtMouse(int x, int y, int mouseButton) {
        for (T object : objects) {
            if (isClicked(getResourceContainer(), new Coordinate(x, y), object)) {
                selectedObject = object;
                if (mouseButton == 1) {
                    makeSelectedLive();
                    return true;
                }
                break;
            }
        }
        return false;
    }

    private int getRandomIndex(int size) {
        Random random = new Random(System.currentTimeMillis());
        return random.nextInt(size);
    }

    public boolean handleMouseDownMove(int x, int y, int button) {
        return handleMouseMove(x, y);
    }

    public boolean handleMouseMove(int x, int y) {
        IDisplayPaneContainer container = getResourceContainer();
        if (liveObject != null) {
            Coordinate newMouseLoc = container.translateClick(x, y);

            if (newMouseLoc != null) {
                liveObject = move(newMouseLoc, lastMouseLoc, liveObject);
                lastMouseLoc = newMouseLoc;
                issueRefresh();
            }
            return true;
        } else if ((liveObject == null) && isEditable()) {
            if (objects == null) {
                return false;
            }

            // if (liveObject != null) {
            // if (isClicked(container, new Coordinate(x, y), liveObject)) {
            // if (!this.endpointClicked) {
            // this.changeCursorCross();
            // } else {
            // this.changeCursorHand();
            // }
            // return true;
            // }
            // }
            // if (selectedObject != null) {
            // if (isClicked(container, new Coordinate(x, y), selectedObject)) {
            // if (!this.endpointClicked) {
            // this.changeCursorCross();
            // } else {
            // this.changeCursorHand();
            // }
            // return true;
            // }
            // }

            for (T object : objects) {
                if (isClicked(container, new Coordinate(x, y), object)) {
                    if (!this.endpointClicked) {
                        this.changeCursorCross();
                    } else {
                        this.changeCursorHand();
                    }
                    return true;
                }
            }
            this.changeCursorNormal();
        }
        return false;
    }

    @SuppressWarnings("unchecked")
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        if (!isEditable()) {
            return false;
        }

        if (this.rightClickMovesToCoord && (mouseButton == 3)) {
            Object[] selectionArray = objects.toArray();
            selectedObject = (T) selectionArray[getRandomIndex(selectionArray.length)];
            liveObject = move(lastMouseLoc, null, selectedObject);
        }

        if (liveObject != null) {
            objects.remove(selectedObject);
            objects.add(liveObject);
            save(selectedObject, liveObject);
            selectedObject = null;
            liveObject = null;
            issueRefresh();
            return true;
        }

        return false;
    }

    protected void save(T oldObject, T newObject) {
        ;
    }

    @Override
    public boolean handleDoubleClick(int x, int y, int button) {
        return false;
    }

    @Override
    public boolean handleKeyDown(int keyCode) {
        return false;
    }

    @Override
    public boolean handleKeyUp(int keyCode) {
        return false;
    }

    protected void changeCursorNormal() {
        this.updateCursorStandard(SWT.CURSOR_ARROW);
    }

    protected void changeCursorCross() {
        this.updateCursorStandard(SWT.CURSOR_SIZEALL);
    }

    protected void changeCursorHand() {
        this.updateCursorStandard(SWT.CURSOR_HAND);
    }

    private void updateCursorStandard(int cursorEnum) {
        IWorkbenchWindow window = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();

        window.getShell().setCursor(
                window.getShell().getDisplay().getSystemCursor(cursorEnum));
    }

    @Override
    public boolean handleMouseHover(int x, int y) {
        return false;
    }

    @Override
    public boolean handleMouseWheel(Event event, int x, int y) {
        return false;
    }

    @Override
    public boolean handleMouseExit(Event event) {
        changeCursorNormal();
        return false;
    }

    @Override
    public boolean handleMouseEnter(Event event) {
        return handleMouseMove(event.x, event.y);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            issueRefresh();
        }
    }

}
