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
package com.raytheon.viz.mpe.ui.perspective;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.menus.IMenuService;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.hydrocommon.actions.SetProjection;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.GroupEditPrecipStns;
import com.raytheon.viz.mpe.ui.actions.GroupEditTempStns;
import com.raytheon.viz.mpe.ui.actions.MPESelectPaneAction;
import com.raytheon.viz.mpe.ui.dialogs.EditFreezeStationsDialog;
import com.raytheon.viz.mpe.ui.dialogs.EditPrecipStationsDialog;
import com.raytheon.viz.mpe.ui.dialogs.EditTempStationsDialog;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.perspectives.AbstractCAVEPerspectiveManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2008            randerso     Initial creation
 * Feb 18, 2010  4111      snaples      Updated to support contexts
 * Apr 27, 2010            mschenke     refactor for common perspective switching
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MPEPerspectiveManager extends AbstractCAVEPerspectiveManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MPEPerspectiveManager.class);

    /** The MPE Perspective Class */
    public static final String MPE_PERSPECTIVE = MPEPerspective.ID_PERSPECTIVE;

    private IContextActivation activation;

    private boolean first = false;

    @Override
    public void open() {
        String cval = AppsDefaults.getInstance().getToken(
                "mpe_map_background_color");
        if (cval != null) {
            RGB color = RGBColors.getRGBColor(AppsDefaults.getInstance()
                    .getToken("mpe_map_background_color"));
            BackgroundColor.getInstance(page.getPerspective()).setColor(
                    BGColorMode.EDITOR, color);
        }
        loadDefaultBundle("mpe/default-procedure.xml");
        first = true;
        populate(true);
    }

    @Override
    public void activate() {
        if (perspectiveEditors.size() == 0 && opened) {
            opened = false;
            first = true;
            super.activate();
        } else if (perspectiveEditors.size() != 0 && opened) {
            super.activate();
            first = false;
            populate(false);
        } else {
            super.activate();
            populate(true);
        }
    }

    @Override
    public void close() {
        super.close();
        depopulate();
    }

    @Override
    public void deactivate() {
        super.deactivate();
        depopulate();
    }

    private void populate(boolean pro) {
        IDisplayPaneContainer currentEditor = EditorUtil
                .getActiveVizContainer();
        if (currentEditor != null) {
            if (pro == true) {
                SetProjection.setDefaultProjection(currentEditor, "mpe");
            }
            try {
                MPEDisplayManager.populate(perspectiveWindow, pro, first);
                first = false;
            } catch (VizException e) {
                statusHandler.handle(Priority.CRITICAL, "Error populating", e);
            }
        }
    }

    private void depopulate() {
        try {
            MPEDisplayManager.dePopulate(perspectiveWindow);
        } catch (VizException e) {
            statusHandler.handle(Priority.CRITICAL, "Error depopulating", e);
        }
    }

    @Override
    public IInputHandler[] getPerspectiveInputHandlers(AbstractEditor editor) {
        IInputHandler[] superHandlers = super
                .getPerspectiveInputHandlers(editor);

        IInputHandler handler = new InputAdapter() {

            @Override
            public boolean handleMouseDown(int x, int y, int mouseButton) {
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container == null) {
                    return false;
                }

                if (mouseButton == 1) {
                    Coordinate ll = container.translateClick(x, y);
                    if (MPEDisplayManager.getCurrent().isQpf() == true) {
                        if (MPEDisplayManager.getCurrent().isGroupedt() == true) {
                            GroupEditPrecipStns ges = new GroupEditPrecipStns();
                            ges.group_edit_precip_stations(new ReferencedCoordinate(
                                    ll));
                            return true;
                        }
                    } else if (MPEDisplayManager.getCurrent().isMaxmin() == true) {
                        if (MPEDisplayManager.getCurrent().isGroupedt() == true) {
                            GroupEditTempStns get = new GroupEditTempStns();
                            get.group_edit_temp_stations(new ReferencedCoordinate(
                                    ll));
                            return true;
                        }
                    } else if (MPEDisplayManager.getCurrent().isTimeLapseMode()) {
                        // Stop the loop where it is
                        LoopProperties loopProps = container
                                .getLoopProperties();
                        if (loopProps != null) {
                            loopProps.setLooping(false);
                        }

                        return true;
                    }
                } else if (mouseButton == 3) {
                    Coordinate ll = container.translateClick(x, y);
                    if (MPEDisplayManager.getCurrent().isQpf() == true) {
                        Shell shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();
                        EditPrecipStationsDialog epd = new EditPrecipStationsDialog(
                                shell, new ReferencedCoordinate(ll));
                        epd.open();
                        return true;
                    } else if (MPEDisplayManager.getCurrent().isMaxmin() == true) {
                        Shell shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();
                        EditTempStationsDialog etd = new EditTempStationsDialog(
                                shell, new ReferencedCoordinate(ll));
                        etd.open();
                        return true;
                    } else if (MPEDisplayManager.getCurrent().isZflag() == true) {
                        Shell shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();
                        EditFreezeStationsDialog ezd = new EditFreezeStationsDialog(
                                shell, new ReferencedCoordinate(ll));
                        ezd.open();
                        return true;
                    }
                }

                return false;
            }

            @Override
            public boolean handleMouseMove(int x, int y) {
                return false;
            }
        };

        ArrayList<IInputHandler> handlers = new ArrayList<IInputHandler>();
        handlers.addAll(Arrays.asList(superHandlers));
        handlers.add(handler);
        return handlers.toArray(new IInputHandler[handlers.size()]);
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager,
            IDisplayPaneContainer container, IDisplayPane pane) {
        super.addContextMenuItems(menuManager, container, pane);

        IMenuService menuSrv = (IMenuService) PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getService(IMenuService.class);

        MenuManager mgr = new MenuManager();
        menuSrv.populateContributionManager(mgr,
                "menu:com.raytheon.viz.mpe.ui.contextMenu");

        for (IContributionItem item : mgr.getItems()) {
            menuManager.add(item);
        }
        if (container instanceof IMultiPaneEditor) {
            IMultiPaneEditor editor = (IMultiPaneEditor) container;
            if (editor.getNumberofPanes() > 1
                    && editor.displayedPaneCount() > 1) {
                // Set up load to pane menu
                if (editor.getSelectedPane(IMultiPaneEditor.LOAD_ACTION) == null
                        || editor.isSelectedPane(IMultiPaneEditor.LOAD_ACTION,
                                pane) == false) {
                    MPESelectPaneAction selectPaneAction = new MPESelectPaneAction(
                            pane, IMultiPaneEditor.LOAD_ACTION);
                    selectPaneAction.setContainer(container);
                    selectPaneAction.setSelectedRsc(null);
                    menuManager.add(selectPaneAction);
                }
                if (editor.getSelectedPane(IMultiPaneEditor.LOAD_ACTION) != null) {
                    MPESelectPaneAction selectPaneAction = new MPESelectPaneAction(
                            null, IMultiPaneEditor.LOAD_ACTION);
                    selectPaneAction.setContainer(container);
                    selectPaneAction.setSelectedRsc(null);
                    menuManager.add(selectPaneAction);
                }
            }
        }
    }
}
