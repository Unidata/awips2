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
package com.raytheon.viz.gfe.temporaleditor.mousehandler;

import java.util.Date;

import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.gridmanager.action.UndoAction;
import com.raytheon.viz.gfe.temporaleditor.TEParmDisplayAttributes;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorDiscreteBar;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorUtil;
import com.raytheon.viz.gfe.temporaleditor.actions.SetDiscreteAction;
import com.raytheon.viz.gfe.temporaleditor.actions.SetDiscretePickupAction;
import com.raytheon.viz.gfe.temporaleditor.actions.SetDiscreteWxPickupTEAction;

/**
 * MouseHandler to edit discrete data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * May 28, 2009 #2159      Richard Peter Initial Creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class EditorDiscreteMouseHandler extends MouseHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(EditorDiscreteMouseHandler.class);
    private static final String SET_TO_COMMON_VALUES = "Set to Common Values";

    private static final String SET_TO_RECENT_VALUES = "Set to Recent Values";

    private static final String SET_TO_SESSION_VALUES = "Set to Session Values";

    private MenuManager menuMgr;

    private TemporalEditorDiscreteBar teBar;

    public EditorDiscreteMouseHandler(TemporalEditorDiscreteBar teBar) {
        this.teBar = teBar;
    }

    @Override
    public void mouseClick(MouseEvent e) {
        super.mouseClick(e);

        if (e.button == 1) {
            for (Parm parm : teBar.getParms()) {
                TEParmDisplayAttributes dispAtt = teBar
                        .getParmDisplayAttributes(parm);
                if (dispAtt.isDisplayed()) {
                    TemporalEditorUtil teUtil = teBar.getUtil();
                    Date date = teUtil.pixelToDate(e.x);
                    TimeRange tr = teUtil.dateToHour(date);

                    if (parm.overlappingGrid(date) != null
                            && parm.isOkToEdit(tr)) {
                        Grid2DBit gridArea = DataManager.getCurrentInstance()
                                .getRefManager().getActiveRefSet().getGrid();
                        DataManager.getCurrentInstance().getParmOp()
                                .clearUndoParmList();

                        try {
                            IGridData gridData = parm.startParmEdit(date);

                            if (gridData != null) {
                                gridData.setValue(parm.getParmState()
                                        .getPickUpValue(), gridArea);
                                parm.endParmEdit();
                            }
                        } catch (GFEOperationFailedException exc) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Grid edit failed", exc);
                        }

                        break;
                    }
                }
            }
        } else if (e.button == 2) {
            Parm parm = teBar.getDisplayedParm();

            if (parm != null) {
                DiscreteKey key = teBar.getClickedKey(new Point(e.x, e.y));

                if (key != null) {
                    if (key.isValid()) {
                        DiscreteWxValue wxValue = new DiscreteWxValue(key, parm);
                        parm.getParmState().setPickUpValue(wxValue);
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.gridmanager.MouseHandler#displayContextMenu ()
     */
    @Override
    public void displayContextMenu(MouseEvent e) throws GFEServerException {
        super.displayContextMenu(e);

        if (menuMgr != null) {
            menuMgr.dispose();
        }

        menuMgr = new MenuManager("#PopupMenu");
        menuMgr.add(new UndoAction());

        for (final Parm parm : teBar.getParms()) {
            TEParmDisplayAttributes dispAtt = teBar
                    .getParmDisplayAttributes(parm);
            if (dispAtt.isDisplayed()) {
                TemporalEditorUtil teUtil = teBar.getUtil();
                final Date date = teUtil.pixelToDate(e.x);
                TimeRange tr = teUtil.dateToHour(date);
                IGridData grid = parm.overlappingGrid(date);
                if (grid != null && parm.isOkToEdit(tr)) {
                    menuMgr.add(new Separator());
                    ParmState parmState = parm.getParmState();

                    menuMgr.add(new SetDiscreteAction(parm, date));

                    if (!parmState.getRecentPickupValues().isEmpty()) {
                        menuMgr.add(new SetDiscreteWxPickupTEAction(
                                SET_TO_RECENT_VALUES, parm, parmState
                                        .getRecentPickupValues().toArray(
                                                new WxValue[0]), date));
                    }
                    if (!parmState.getSessionPickupValues().isEmpty()) {
                        menuMgr.add(new SetDiscreteWxPickupTEAction(
                                SET_TO_SESSION_VALUES, parm, parmState
                                        .getSessionPickupValues().toArray(
                                                new WxValue[0]), date));
                    }

                    String compName = parm.getParmID().compositeNameUI();
                    String[] commonItems = GFEPreference
                            .getArrayPreference(compName + "_commonValues");
                    if (commonItems != null && commonItems.length > 0) {
                        menuMgr.add(new SetDiscretePickupAction(
                                SET_TO_COMMON_VALUES, commonItems, parm, date));
                    }
                }
                break;
            }
        }

        Composite composite = teBar.getContainer();
        Menu menu = menuMgr.createContextMenu(composite);
        menu.setVisible(true);
        composite.setMenu(menu);
    }
}
