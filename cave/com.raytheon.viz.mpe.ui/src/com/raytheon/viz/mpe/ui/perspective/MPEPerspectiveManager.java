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
import java.util.List;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.IMenuService;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.MapManager;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.ProcedureXmlManager;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.hydrocommon.actions.SetProjection;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.GroupEditPrecipStns;
import com.raytheon.viz.mpe.ui.actions.GroupEditTempStns;
import com.raytheon.viz.mpe.ui.actions.MPESelectPaneAction;
import com.raytheon.viz.mpe.ui.dialogs.ChooseDataPeriodDialog;
import com.raytheon.viz.mpe.ui.dialogs.EditFreezeStationsDialog;
import com.raytheon.viz.mpe.ui.dialogs.EditPrecipStationsDialog;
import com.raytheon.viz.mpe.ui.dialogs.EditTempStationsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcFreezeOptionsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcPrecipOptionsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcTempOptionsDialog;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.perspectives.AbstractCAVEPerspectiveManager;
import org.locationtech.jts.geom.Coordinate;

/**
 * Perspective manager for MPE Perspective
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 21, 2008           randerso    Initial creation
 * Feb 18, 2010  4111     snaples     Updated to support contexts
 * Apr 27, 2010           mschenke    refactor for common perspective switching
 * Jan 29, 2013  1550     mpduff      Add ability to preload maps on open.
 * Oct 22, 2013  2491     bsteffen    Switch serialization to
 *                                    ProcedureXmlManager
 * Aug 11, 2017  6148     bkowal      Cleanup.
 * May 10, 2017  7131     mduff       When opening edit station dialogs they are added 
 * Feb 21, 2018  7225     bkowal      Individual edit dialogs cannot be opened in Group Edit mode.
 *                                    Right-click (matches A1) will now change the quality in Group
 *                                    Edit mode.
 * May 10, 2018  7131     mduff       When opening edit station dialogs they are added 
 *                                    as children to the DQC dialog.
 * Aug  8, 2018  6891     tgurney     Change the right-click menu to allow
 *                                    loading to "This" or "Other" pane
 * Aug  9, 2018  7098     tgurney     Close edit station dlg if a second copy
 *                                    is opened, + fix leaked child dlgs
 * </pre>
 *
 * @author randerso
 */

public class MPEPerspectiveManager extends AbstractCAVEPerspectiveManager {

    private static final String MPE = "mpe";

    private EditPrecipStationsDialog editPrecipStnsDlg = null;

    private EditFreezeStationsDialog editFreezeStnsDlg = null;

    private EditTempStationsDialog editTempStnsDlg = null;

    @Override
    public void open() {
        // First time opened, set perspective default background color
        String cval = AppsDefaults.getInstance()
                .getToken("mpe_map_background_color");
        if (cval != null) {
            RGB color = RGBColors.getRGBColor(AppsDefaults.getInstance()
                    .getToken("mpe_map_background_color"));
            BackgroundColor.getInstance(page.getPerspective())
                    .setColor(BGColorMode.EDITOR, color);
        }

        openNewEditor();

        Shell shell = perspectiveWindow.getShell();
        ChooseDataPeriodDialog dialog = new ChooseDataPeriodDialog(shell);
        dialog.open();
    }

    @Override
    public AbstractEditor openNewEditor() {
        try {
            // Unmarshal default bundle xml
            Bundle b = ProcedureXmlManager.getInstance().unmarshal(Bundle.class,
                    PathManagerFactory.getPathManager().getStaticFile(MPE
                            + IPathManager.SEPARATOR + "default-bundle.xml"));
            // Load Bundle to perspective window in new editor
            String editorId = b.getEditor();
            if (editorId != null) {
                IRenderableDisplay[] displays = b.getDisplays();
                if (displays.length > 0) {
                    editorId = DescriptorMap.getEditorId(
                            displays[0].getDescriptor().getClass().getName());
                    AbstractEditor editor = UiUtil.createEditor(
                            perspectiveWindow, editorId, displays);
                    if (editor != null) {
                        initialize(editor);

                        String[] maps;

                        // Get the maps configured for display at startup
                        String displayMaps = AppsDefaults.getInstance()
                                .getToken("mpe_display_maps", "statesCounties");

                        if (displayMaps.contains(",")) {
                            maps = displayMaps.split(",");
                        } else {
                            maps = new String[1];
                            maps[0] = displayMaps;
                        }

                        IDisplayPaneContainer currentEditor = EditorUtil
                                .getActiveVizContainer();
                        MapManager mapMgr = MapManager
                                .getInstance((IMapDescriptor) currentEditor
                                        .getActiveDisplayPane()
                                        .getDescriptor());

                        // Load the configured maps
                        for (String map : maps) {
                            mapMgr.loadMapByBundleName(map.trim());
                        }

                        return editor;
                    } else {
                        throw new VizException(
                                "Failed to open new editor on window");
                    }
                } else {
                    throw new SerializationException(
                            "No displays to load found in MPE default bundle XML");
                }
            }
        } catch (Exception e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    "Error opening new MPE editor: " + e.getLocalizedMessage(),
                    e);
        }
        return null;
    }

    /**
     * Initializes a newly created MPE editor
     *
     * @param editor
     */
    private static void initialize(AbstractEditor editor) {
        // Project editor
        SetProjection.setDefaultProjection(editor, MPE);
    }

    @Override
    public IInputHandler[] getPerspectiveInputHandlers(AbstractEditor editor) {
        IInputHandler[] superHandlers = super.getPerspectiveInputHandlers(
                editor);

        IInputHandler handler = new InputAdapter() {
            @Override
            public boolean handleMouseDown(int x, int y, int mouseButton) {
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container == null) {
                    return false;
                }

                MPEDisplayManager displayManager = MPEDisplayManager
                        .getCurrent();

                Coordinate ll = container.translateClick(x, y);

                if (displayManager.isGroupedt()) {
                    if (mouseButton == 3) {
                        if (displayManager.isQpf()) {
                            GroupEditPrecipStns ges = new GroupEditPrecipStns();
                            ges.group_edit_precip_stations(
                                    new ReferencedCoordinate(ll));
                            return true;
                        } else if (displayManager.isMaxmin()) {
                            GroupEditTempStns get = new GroupEditTempStns();
                            get.group_edit_temp_stations(
                                    new ReferencedCoordinate(ll));
                            return true;
                        }
                    }
                } else if (mouseButton == 3) {
                    if (displayManager.isQpf()) {                         
                        QcPrecipOptionsDialog dlg = displayManager
                                .getQcPrecipDialog();
                        if (editPrecipStnsDlg != null) {
                            editPrecipStnsDlg.close();
                            dlg.removeChildDlg(editPrecipStnsDlg);
                        }
                        editPrecipStnsDlg = new EditPrecipStationsDialog(
                                dlg.getShell(), new ReferencedCoordinate(ll));
                        dlg.addChildDlg(editPrecipStnsDlg);
                        editPrecipStnsDlg.open();
                        return true;
                    } else if (displayManager.isMaxmin()) {
                        QcTempOptionsDialog dlg = displayManager
                                .getQcTempDialog();
                        if (editTempStnsDlg != null) {
                            editTempStnsDlg.close();
                            dlg.removeChildDlg(editTempStnsDlg);
                        }
                        editTempStnsDlg = new EditTempStationsDialog(
                                dlg.getShell(), new ReferencedCoordinate(ll));
                        dlg.addChildDlg(editTempStnsDlg);
                        editTempStnsDlg.open();
                        return true;
                    } else if (displayManager.isZflag()) {
                        QcFreezeOptionsDialog dlg = displayManager
                                .getQcFreezeDialog();
                        if (editFreezeStnsDlg != null) {
                            editFreezeStnsDlg.close();
                            dlg.removeChildDlg(editFreezeStnsDlg);
                        }
                        editFreezeStnsDlg = new EditFreezeStationsDialog(
                                dlg.getShell(), new ReferencedCoordinate(ll));
                        dlg.addChildDlg(editFreezeStnsDlg);
                        editFreezeStnsDlg.open();
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

        List<IInputHandler> handlers = new ArrayList<>();
        handlers.addAll(Arrays.asList(superHandlers));
        handlers.add(handler);
        return handlers.toArray(new IInputHandler[handlers.size()]);
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager,
            IDisplayPaneContainer container, IDisplayPane menuPane) {
        super.addContextMenuItems(menuManager, container, menuPane);

        IMenuService menuSrv = PlatformUI.getWorkbench()
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
                MPESelectPaneAction selectPaneAction = new MPESelectPaneAction(
                        menuPane, "Load to This Panel");
                selectPaneAction.setContainer(container);
                selectPaneAction.setSelectedRsc(null);
                menuManager.add(selectPaneAction);
                IDisplayPane otherPane = Arrays.stream(editor.getDisplayPanes())
                        .filter((pane) -> pane != menuPane).findFirst().get();
                MPESelectPaneAction otherSelectPaneAction = new MPESelectPaneAction(
                        otherPane, "Load to Other Panel");
                otherSelectPaneAction.setContainer(container);
                otherSelectPaneAction.setSelectedRsc(null);
                menuManager.add(otherSelectPaneAction);
            }
        }
    }
}
