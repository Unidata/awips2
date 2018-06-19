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
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MessageBox;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.TEEditModeChangedMsg;
import com.raytheon.viz.gfe.core.msgs.TEEditModeChangedMsg.TEMode;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState.VectorMode;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.gridmanager.action.UndoAction;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorNumericBar;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorUtil;
import com.raytheon.viz.gfe.temporaleditor.TimeSeries;

/**
 * MouseHandler to resize temporal editor bars.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 28, 2009  #2159     rjpeter       Initial creation
 * Feb 20, 2015  #4051     dgilling      Allow grids to be edited when there is
 *                                       no active edit area.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class EditorNumericMouseHandler extends MouseHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditorNumericMouseHandler.class);

    private TimeRange lastEditedTr;

    private Parm parmInDrag;

    private MenuManager menuMgr;

    private TemporalEditorNumericBar teBar;

    private boolean adjustingMagnitude;

    public EditorNumericMouseHandler(TemporalEditorNumericBar teBar) {
        this.teBar = teBar;
    }

    @Override
    public void mouseClick(MouseEvent e) {
        super.mouseClick(e);
        TemporalEditorUtil teUtil = teBar.getUtil();
        Date date = teUtil.pixelToDate(e.x);

        float val = teBar.getScale().getValueForHeight(e.y);
        Parm parm = teBar.getClosestParm(date, val);

        if (parm != null) {
            GridType gridType = parm.getGridInfo().getGridType();
            adjustingMagnitude = (e.stateMask & SWT.SHIFT) == 0;

            if (!adjustingMagnitude
                    && (GridType.SCALAR.equals(gridType) || !verifyAbsoluteMode())) {
                // can't adjust direction for scalars or in relative mode
                return;
            }

            try {
                DataManager.getCurrentInstance().getParmOp()
                        .clearUndoParmList();
                IGridData gridToChange = parm.startParmEdit(date);
                if (gridToChange != null) {
                    if (adjustingMagnitude) {
                        processMagnitudeValue(e, parm, date, gridToChange);
                    } else if (GridType.VECTOR.equals(gridType)) {
                        processDirectionValue(e, parm, date, gridToChange);
                    }
                }
                parm.endParmEdit();
            } catch (GFEOperationFailedException exc) {
                statusHandler.handle(Priority.PROBLEM, "Grid edit failed", exc);
            }
        }
    }

    @Override
    public void dragStart(MouseEvent e) {
        TemporalEditorUtil teUtil = teBar.getUtil();
        Date date = teUtil.pixelToDate(e.x);
        TimeRange tr = teUtil.dateToHour(date);
        float val = teBar.getScale().getValueForHeight(e.y);
        Parm parm = teBar.getClosestParm(date, val);

        if (parm != null && parm.isOkToEdit(tr)) {

            GridType gridType = parm.getGridInfo().getGridType();

            if ((e.stateMask & SWT.SHIFT) != 0) {
                if (GridType.VECTOR.equals(gridType) && verifyAbsoluteMode()) {
                    adjustingMagnitude = false;
                } else {
                    return;
                }
            } else {
                adjustingMagnitude = true;
            }

            try {
                IGridData grid = parm.overlappingGrid(date);
                lastEditedTr = grid.getGridTime();

                IGridData gridsToChange = parm.startParmEdit(lastEditedTr
                        .getStart());

                if (gridsToChange != null) {
                    parmInDrag = parm;
                    DataManager.getCurrentInstance().getParmOp()
                            .clearUndoParmList();
                }
            } catch (GFEOperationFailedException exc) {
                statusHandler.handle(Priority.PROBLEM, "Grid edit failed", exc);
            }
        }
    }

    @Override
    public void dragMove(MouseEvent e) {
        super.dragMove(e);
        if (parmInDrag != null) {
            TemporalEditorUtil teUtil = teBar.getUtil();

            Date date = teUtil.pixelToDate(e.x);
            IGridData grid = parmInDrag.overlappingGrid(date);
            if (grid == null) {
                return;
            }

            TimeRange newTR = grid.getGridTime();

            try {
                if (!newTR.equals(lastEditedTr)) {
                    parmInDrag.extendParmEdit(newTR.getStart());
                    lastEditedTr = newTR;
                }

                if (parmInDrag.isOkToEdit(newTR)) {
                    if (adjustingMagnitude) {
                        processMagnitudeValue(e, parmInDrag, date, grid);
                    } else {
                        processDirectionValue(e, parmInDrag, date, grid);
                    }
                    teBar.getTimeSeriesForParm(parmInDrag).generateSamples(
                            newTR);
                }
            } catch (GFEOperationFailedException exc) {
                statusHandler.handle(Priority.PROBLEM, "Grid edit failed", exc);
            }
        }
    }

    @Override
    public void dragEnd(MouseEvent e) {
        if (parmInDrag != null) {
            parmInDrag.endParmEdit();
            parmInDrag = null;
            lastEditedTr = null;
        }
    }

    private boolean verifyAbsoluteMode() {
        TEMode teMode = Message.inquireLastMessage(TEEditModeChangedMsg.class)
                .getMode();

        if (teMode.equals(TEMode.RELATIVE)) {
            MessageBox mb = new MessageBox(teBar.getContainer().getShell(),
                    SWT.ICON_WARNING | SWT.OK);
            mb.setText("Direction Edit not allowed");
            mb.setMessage("Editing Direction is not allowed in TE Relative Edit Mode");
            mb.open();
            return false;
        }

        return true;
    }

    private void processMagnitudeValue(MouseEvent e, Parm parm, Date date,
            IGridData gridToChange) throws GFEOperationFailedException {
        TEMode teMode = Message.inquireLastMessage(TEEditModeChangedMsg.class)
                .getMode();
        Grid2DBit gridArea = TemporalEditorUtil
                .determinePointsToUse(DataManagerUIFactory.getCurrentInstance()
                        .getRefManager().getActiveRefSet());
        float val = teBar.getScale().getValueForHeight(e.y);
        TimeSeries ts = teBar.getTimeSeriesForParm(parm);

        float resolution = ts.getResolution();

        if (teMode.equals(TEMode.RELATIVE)) {
            // relative
            float average = teBar.getAverage(parm, date);

            if (average != -Float.MAX_VALUE) {
                float delta = val - average;

                // limit delta to the resolution of the parm
                delta = Math.round(delta / resolution) * resolution;
                gridToChange.applyDelta(date, delta, false, gridArea);
            }
        } else {
            GridType gridType = parm.getGridInfo().getGridType();
            VectorMode vectorMode = parm.getParmState().getVectorMode();
            WxValue value;

            // limit val to the resolution of the parm
            // val = Math.round(val / resolution) * resolution;

            if (GridType.VECTOR.equals(gridType)) {
                parm.getParmState().setVectorMode(VectorMode.MAGNITUDE);
                value = new VectorWxValue(val, 0, parm);
            } else {
                value = new ScalarWxValue(val, parm);
            }

            gridToChange.setValue(value, gridArea);
            parm.getParmState().setVectorMode(vectorMode);
        }
    }

    /**
     * 
     * @param e
     * @param parm
     * @param date
     * @param gridToChange
     * @throws GFEOperationFailedException
     */
    private void processDirectionValue(MouseEvent e, Parm parm, Date date,
            IGridData gridToChange) throws GFEOperationFailedException {
        TEMode teMode = Message.inquireLastMessage(TEEditModeChangedMsg.class)
                .getMode();

        if (!teMode.equals(TEMode.RELATIVE)) {
            Grid2DBit gridArea = TemporalEditorUtil
                    .determinePointsToUse(DataManagerUIFactory
                            .getCurrentInstance().getRefManager()
                            .getActiveRefSet());
            float dir = teBar.getScale().getDirectionForHeight(e.y);

            VectorMode vectorMode = parm.getParmState().getVectorMode();

            parm.getParmState().setVectorMode(VectorMode.DIRECTION);
            WxValue value = new VectorWxValue(0, dir, parm);

            gridToChange.setValue(value, gridArea);
            parm.getParmState().setVectorMode(vectorMode);
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

        Composite composite = teBar.getContainer();
        Menu menu = menuMgr.createContextMenu(composite);
        menu.setVisible(true);

        composite.setMenu(menu);
    }
}
