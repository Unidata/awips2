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
package com.raytheon.uf.viz.d2d.ui.map.actions;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IPane.CanvasType;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 *
 * Contains logic for rotating panels
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2013            mschenke    Initial creation
 * Feb 19, 2018 7060       njensen     Don't rotate on inset maps
 * Feb 13, 2020 74164      ksunil      2, 9 and 16 panel support
 * May 10, 2020 78468      ksunil      Map ALT+Ctrl+(0-6) keys for 16 panel setup.
 * May 11, 2023 2029803    mapeters    Support additional panel counts
 *
 * </pre>
 *
 * @author mschenke
 */
public class RotatePanelsHandler extends AbstractTool {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (!(container instanceof IMultiPaneEditor)) {
            return null;
        }

        IMultiPaneEditor editor = (IMultiPaneEditor) container;

        if (editor.getActiveDisplayPane().getType() != CanvasType.MAIN) {
            // Ignore the rotate command for inset canvases
            return null;
        }

        IDisplayPane[] panes = getCanvasesInRotationOrder(editor);
        int numPanes = panes.length;

        // Get direction to rotate
        String dirStr = event.getParameter("direction");
        int direction = Integer.parseInt(dirStr);

        Integer hideIndex = null;
        String hideIndexStr = event.getParameter("hideIndex");
        if (hideIndexStr != null) {
            hideIndex = Integer.parseInt(hideIndexStr);
        }

        // Get pane to start rotation on
        IDisplayPane startPane = null;
        String startStr = event.getParameter("startIndex");
        if (startStr != null) {
            int startIdx = Integer.parseInt(startStr);

            // we can safely access the key pressed. It will map to 0..9
            org.eclipse.swt.widgets.Event ev = (org.eclipse.swt.widgets.Event) event
                    .getTrigger();

            int keyPressed = Character.getNumericValue(ev.character);

            /*
             * sixteenPanelKey is available only in the bindings for 10-16 panel
             * rotation setup.
             */
            String ten2sixteen = event.getParameter("sixteenPanelKey");
            if (ten2sixteen != null) {
                keyPressed = Integer.parseInt(ten2sixteen);
            }

            /*
             * Adjust values for non-4 panel layouts. These panels do not honor
             * left/right as the (already existing code for) 4 panels. Which
             * means direction and hideIndex are always 1.
             */
            if (numPanes != 4) {
                direction = 1;
                if (hideIndex != null) {
                    hideIndex = 1;
                }
            }

            // Now do some startIdx correction
            switch (numPanes) {
            case 2:
                startIdx = keyPressed % 2;
                break;
            case 4:
                if (keyPressed == 9) {
                    startIdx = 3;
                }
                break;
            default:
                if (keyPressed == 1) {
                    /*
                     * Last pane so that rotating forward gets us to the 1st
                     * pane.
                     */
                    startIdx = numPanes - 1;
                } else {
                    /*
                     * -1 to convert to 0-based, and another -1 to get the pane
                     * before so that rotating forward gets us to the desired
                     * pane.
                     */
                    startIdx = keyPressed - 2;
                }
                break;
            }

            if (editor.displayedPaneCount() > 1) {
                // more than one pane so we want to start on resulting pane
                startPane = panes[getNextIndex(panes, startIdx, direction)];
            } else {
                // Get pane specified by startIdx
                startPane = panes[getNextIndex(panes, startIdx, 0)];
            }
        } else {
            // No startStr, get first visible pane
            for (IDisplayPane pane : panes) {
                if (pane.isVisible()) {
                    startPane = pane;
                    break;
                }
            }
        }

        if (startPane != null) {

            rotateToNextPane(editor, startPane, direction, hideIndex);
        }
        return null;
    }

    /**
     * Rotates to next pane in container. If container has > 1 pane displayed,
     * will rotate to pane passed in, otherwise to next in line
     *
     * @param editor
     * @param pane
     */
    public static void rotateToNextPane(IMultiPaneEditor editor,
            IDisplayPane pane) {
        rotateToNextPane(editor, pane, 1, 0);
    }

    /**
     * Rotates to the next panel given the direction
     *
     * @param editor
     * @param pane
     * @param direction
     */
    private static void rotateToNextPane(IMultiPaneEditor editor,
            IDisplayPane pane, int direction, Integer hideIndex) {
        boolean wrapped = false;
        IDisplayPane paneToRotateTo = pane;
        if (editor.displayedPaneCount() == 1) {
            IDisplayPane[] panes = getCanvasesInRotationOrder(editor);
            int paneIdx = -1;
            for (int i = 0; i < panes.length; ++i) {
                if (panes[i] == pane) {
                    paneIdx = i;
                    break;
                }
            }

            if (paneIdx >= 0) {
                int idxToCheck = paneIdx;
                boolean done = false;
                do {
                    int tmpIdx = idxToCheck + direction;
                    idxToCheck = getNextIndex(panes, idxToCheck, direction);
                    if (idxToCheck != tmpIdx) {
                        wrapped = true;
                    }
                    IDisplayPane next = panes[idxToCheck];
                    List<D2DLegendResource> rscs = next.getDescriptor()
                            .getResourceList()
                            .getResourcesByTypeAsType(D2DLegendResource.class);
                    for (D2DLegendResource rsc : rscs) {
                        if (rsc.hasProducts()) {
                            paneToRotateTo = next;
                            done = true;
                            break;
                        }
                    }
                } while (idxToCheck != paneIdx && !done);
            }
        }
        rotateToPane(editor, paneToRotateTo, hideIndex, wrapped);
    }

    /**
     * Sets container so pane passed in is the only visible pane
     *
     * @param container
     * @param pane
     */
    private static void rotateToPane(IMultiPaneEditor editor, IDisplayPane pane,
            Integer hideIndex, boolean wrapped) {
        IDisplayPane[] panes = getCanvasesInRotationOrder(editor);
        boolean found = false;
        for (IDisplayPane editorPane : panes) {
            if (editorPane == pane) {
                found = true;
                break;
            }
        }
        if (found) {
            for (IDisplayPane editorPane : panes) {
                if (editorPane != pane) {
                    editor.hidePane(editorPane);
                }
            }
            editor.showPane(pane);
            editor.setSelectedPane(IMultiPaneEditor.VISIBLE_PANE, pane);
            editor.setSelectedPane(IMultiPaneEditor.IMAGE_ACTION, null);

            if (hideIndex == null) {
                // Search pane for current resource index
                hideIndex = 0;
                for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                    if (rp.getResource() != null && rp.getResource()
                            .hasCapability(BlendableCapability.class)) {
                        hideIndex = rp.getResource()
                                .getCapability(BlendableCapability.class)
                                .getResourceIndex();
                    }
                }
                if (wrapped) {
                    // If we wrapped, switch index
                    if (hideIndex == 0) {
                        hideIndex = 1;
                    } else {
                        hideIndex = 0;
                    }
                }
            }

            // Toggle displayed resource
            for (IDisplayPane p : panes) {
                for (ResourcePair rp : p.getDescriptor().getResourceList()) {
                    if (rp.getResource() != null && rp.getResource()
                            .hasCapability(BlendableCapability.class)) {
                        rp.getResource()
                                .getCapability(BlendableCapability.class)
                                .toggle(hideIndex);
                    }
                }
            }
        }
    }

    /**
     * Gets the editor's main canvases in "snake" order to match A1 rotation
     * ordering. For example, {0,1,2, 3,4,5, 6,7,8} becomes {0,1,2, 5,4,3,
     * 6,7,8}.
     *
     * @param editor
     *            editor to get canvases from
     * @return main editor canvases in order that they should be rotated through
     */
    private static IDisplayPane[] getCanvasesInRotationOrder(
            IMultiPaneEditor editor) {
        IDisplayPane[] canvases = editor.getMainCanvases();

        int[] numRowsColumns = UiUtil.getNumRowsColumns(canvases.length,
                editor.isHorizontalLayout());
        int numRows = numRowsColumns[0];
        int numColumns = numRowsColumns[1];

        List<IDisplayPane> rotateCanvases = new ArrayList<>(canvases.length);
        for (int r = 0; r < numRows; ++r) {
            int rowStartIndex = r * numColumns;
            int rowEndIndex = rowStartIndex + numColumns - 1;
            if (r % 2 == 0) {
                // Add row in normal order
                for (int i = rowStartIndex; i <= rowEndIndex; ++i) {
                    rotateCanvases.add(canvases[i]);
                }
            } else {
                // Add row in reverse order
                for (int i = rowEndIndex; i >= rowStartIndex; --i) {
                    rotateCanvases.add(canvases[i]);
                }
            }
        }

        return rotateCanvases.toArray(new IDisplayPane[0]);
    }

    /**
     * Gets the next index in line for rotation given panes, curIdx, and
     * direction
     *
     * @param panes
     * @param curIdx
     * @param direction
     * @return
     */
    private static int getNextIndex(IDisplayPane[] panes, int curIdx,
            int direction) {
        int idxToCheck = curIdx + direction;
        if (idxToCheck < 0) {
            idxToCheck = panes.length - 1;
        } else if (idxToCheck >= panes.length) {
            idxToCheck = 0;
        }
        return idxToCheck;
    }
}
