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

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.gridmanager.action.UndoAction;
import com.raytheon.viz.gfe.temporaleditor.TEParmDisplayAttributes;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorUtil;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorWeatherBar;
import com.raytheon.viz.gfe.temporaleditor.actions.SetDiscretePickupAction;
import com.raytheon.viz.gfe.temporaleditor.actions.SetDiscreteWxPickupTEAction;
import com.raytheon.viz.gfe.temporaleditor.actions.SetWeatherAction;

/**
 * MouseHandler to edit discrete data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 28, 2009  2159     rjpeter   Initial creation
 * Feb 20, 2015  4051     dgilling  Allow grids to be edited when there is no
 *                                  active edit area.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author rjpeter
 */
public class EditorWeatherMouseHandler extends MouseHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditorWeatherMouseHandler.class);

    private static final String SET_TO_COMMON_VALUES = "Set to Common Values";

    private static final String SET_TO_RECENT_VALUES = "Set to Recent Values";

    private static final String SET_TO_SESSION_VALUES = "Set to Session Values";

    private MenuManager menuMgr;

    private TemporalEditorWeatherBar teBar;

    /**
     * Construct mouse handler for a TemporalEditorWeatherBar
     *
     * @param teBar
     */
    public EditorWeatherMouseHandler(TemporalEditorWeatherBar teBar) {
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

                    if ((parm.overlappingGrid(date) != null)
                            && parm.isOkToEdit(tr)) {
                        Grid2DBit gridArea = TemporalEditorUtil
                                .determinePointsToUse(DataManagerUIFactory
                                        .getCurrentInstance().getRefManager()
                                        .getActiveRefSet());
                        teBar.getDisplayedParm().getDataManager().getParmOp()
                                .clearUndoParmList();

                        try {
                            IGridData gridData = parm.startParmEdit(date);

                            if (gridData != null) {
                                gridData.setValue(
                                        parm.getParmState().getPickUpValue(),
                                        gridArea);
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
                WeatherKey key = teBar.getClickedKey(new Point(e.x, e.y));

                if (key != null) {
                    if (key.isValid()) {
                        WeatherWxValue wxValue = new WeatherWxValue(key, parm);
                        parm.getParmState().setPickUpValue(wxValue);
                    }
                }
            }
        }
    }

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
                if ((grid != null) && parm.isOkToEdit(tr)) {
                    menuMgr.add(new Separator());
                    ParmState parmState = parm.getParmState();

                    menuMgr.add(new SetWeatherAction(parm, date));

                    if (!parmState.getRecentPickuUpValues().isEmpty()) {
                        menuMgr.add(
                                new SetDiscreteWxPickupTEAction(
                                        SET_TO_RECENT_VALUES, parm,
                                        parmState.getRecentPickuUpValues()
                                                .toArray(new WxValue[0]),
                                        date));
                    }
                    if (!parmState.getSessionPickUpValues().isEmpty()) {
                        menuMgr.add(
                                new SetDiscreteWxPickupTEAction(
                                        SET_TO_SESSION_VALUES, parm,
                                        parmState.getSessionPickUpValues()
                                                .toArray(new WxValue[0]),
                                        date));
                    }

                    String compName = parm.getParmID().compositeNameUI();
                    String[] commonItems = GFEPreference
                            .getStringArray(compName + "_commonValues");
                    if ((commonItems != null) && (commonItems.length > 0)) {
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
