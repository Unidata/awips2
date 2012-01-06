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

package com.raytheon.viz.skewt.mouse;

import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.skewt.SkewTDescriptor;
import com.raytheon.viz.skewt.SkewtDisplay;
import com.raytheon.viz.skewt.rsc.InteractiveSkewTResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Mouse inspect adapter for graphs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2007            njensen     Initial creation
 * 20Nov2007               ebabin      Added support for middle click resource (editable)
 * 30Sept2008              dhladky     Re-worked some.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class SkewtMouseInspectAdapter extends InputAdapter {

    public enum Mode {
        CREATE, MOVE_LINE, MOVE_POINT, PAN, HOVER
    };

    private Display display;

    private Cursor cross;

    private Cursor hand;

    private Cursor arrow;

    private Coordinate coordinateFound = null;

    private Mode mode = Mode.CREATE;

    /** The last mouse position - x */
    private int lastMouseX = -1;

    /** The last mouse position - y */
    private int lastMouseY = -1;

    private int zoomIndex = 0;

    private SkewtDisplay st = null;

    private InteractiveSkewTResource rsc = null;

    public SkewtMouseInspectAdapter() {
        display = Display.getCurrent();
        cross = display.getSystemCursor(SWT.CURSOR_SIZEALL);
        hand = display.getSystemCursor(SWT.CURSOR_HAND);
        arrow = display.getSystemCursor(SWT.CURSOR_ARROW);
    }

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        if (editor == null) {
            return false;
        }
        Coordinate c = editor.translateClick(x, y);
        if (getSkewtDisplay() == null) {
            return false;
        }
        st = getSkewtDisplay();
        ResourceList rl = st.getDescriptor().getResourceList();
        Iterator<?> it = rl.iterator();
        while (it.hasNext()) {
            ResourcePair rp = (ResourcePair) it.next();
            if (rp.getResource() instanceof InteractiveSkewTResource) {
                rsc = (InteractiveSkewTResource) rp.getResource();
                break;
            }
        }

        if (mouseButton == 2) {
            // notifyResourceMiddleClicked(x, y);
            if (rsc != null && rsc.isEditable() == true
                    && rsc.isPoint(c) == true) {
                rsc.removeLayer(rsc.getEp());
            }
        }

        else if (mouseButton == 1) {
            if (rsc != null && rsc.isEditable() == true
                    && rsc.isPoint(c) == true) {
                mode = Mode.MOVE_POINT;
                rsc.getSelectedPoint(c);
            } else {
                mode = Mode.PAN;
            }
            changeMouse(mode);
        } else {
            if (rsc != null && rsc.isEditable() == true
                    && rsc.isPoint(c) == true) {
                mode = Mode.MOVE_POINT;
            } else if (rsc != null && rsc.isEditable() == true
                    && rsc.isPoint(c) == false) {
                rsc.addLayer(c);
                mode = Mode.CREATE;
            }
            changeMouse(mode);
            return false;
        }
        editor.refresh();

        return false;
    }

    public boolean handleMouseDownMove(int x, int y, int button) {
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        if (editor == null) {
            return false;
        }
        boolean mouse = false;

        if (getSkewtDisplay() == null) {
            mouse = false;
        } else {
            st = getSkewtDisplay();
            ResourceList rl = st.getDescriptor().getResourceList();
            Iterator<?> it = rl.iterator();
            while (it.hasNext()) {
                ResourcePair rp = (ResourcePair) it.next();
                if (rp.getResource() instanceof InteractiveSkewTResource) {
                    rsc = (InteractiveSkewTResource) rp.getResource();
                    break;
                } else {
                    break;
                }
            }
        }
        if (button != 1) {
            mouse = false;
        }

        if (mode == Mode.PAN) {
            changeMouse(mode);
            mouse = false;
        }

        if (mode == Mode.MOVE_POINT) {
            Coordinate c = editor.translateClick(x, y);
            changeMouse(mode);
            rsc.moveMousePoint(c);
            lastMouseX = x;
            lastMouseY = y;
            editor.refresh();
            mouse = true;
        }

        return mouse;
    }

    @Override
    public boolean handleMouseHover(int x, int y) {
        // try {
        // if (!mode.name().equals(Mode.MOVE_POINT)) {
        // if (getSkewtDisplay().isInsideEndpoint(lastMouseX, lastMouseY) !=
        // null) {
        // coordinateFound = getSkewtDisplay().isInsideEndpoint(
        // lastMouseX, lastMouseY);
        // this.mode = Mode.HOVER;
        // } else {
        // this.mode = Mode.CREATE;
        //
        // }
        // }
        // } catch (NullPointerException npe) {
        mode = Mode.CREATE;
        // }

        return true;
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        if (editor == null) {
            return false;
        }
        boolean mouse = false;
        if (getSkewtDisplay() == null) {
            mouse = false;
        } else {
            st = getSkewtDisplay();
            ResourceList rl = st.getDescriptor().getResourceList();
            Iterator<?> it = rl.iterator();
            while (it.hasNext()) {
                ResourcePair rp = (ResourcePair) it.next();
                if (rp.getResource() instanceof InteractiveSkewTResource) {
                    rsc = (InteractiveSkewTResource) rp.getResource();
                    break;
                }
            }
        }

        if (mouseButton == 1) {
            // getSkewtDisplay().handleMouseUp();
        } else if (mouseButton == 2) {

            double[] grid = editor.getActiveDisplayPane().screenToGrid(x, y, 0);
            lastMouseX = (int) grid[0];
            lastMouseY = (int) grid[1];

            // if (getSkewtDisplay().isInsideEndpoint(lastMouseX, lastMouseY) !=
            // null) {
            // getSkewtDisplay().deleteLayer();
            // }
        }

        mode = Mode.CREATE;
        changeMouse(mode);
        // editor.refresh();

        return false;
    }

    /**
     * Gets the display you are using
     * 
     * @return SkewTDisplay
     */
    private SkewtDisplay getSkewtDisplay() {
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        if (editor == null) {
            return null;
        }
        if (editor.getActiveDisplayPane().getDescriptor() instanceof SkewTDescriptor) {
            // the ordering in the editor is paramount here!!!!!
            if (editor.getActiveDisplayPane().getRenderableDisplay() instanceof SkewtDisplay) {
                return ((SkewtDisplay) (editor.getActiveDisplayPane()
                        .getRenderableDisplay()));
            }
        }
        return null;
    }

    /**
     * Swap out out mouse pointers
     * 
     * @param mode
     */
    public void changeMouse(Mode mode) {
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        if (editor == null) {
            return;
        }
        if (editor instanceof IWorkbenchPart) {
            IWorkbenchPartSite site = ((IWorkbenchPart) editor).getSite();
            Cursor cursor = site.getShell().getCursor();
            if (mode == Mode.HOVER) {
                if (!hand.equals(cursor)) {
                    site.getShell().setCursor(hand);
                }
            } else if (mode == Mode.MOVE_POINT) {
                if (!cross.equals(cursor)) {
                    site.getShell().setCursor(cross);
                }
            } else {
                if (!arrow.equals(cursor)) {
                    site.getShell().setCursor(arrow);
                }
            }
        }
    }
}