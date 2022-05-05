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
package com.raytheon.viz.hydro.perspective;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.MapManager;
import com.raytheon.uf.viz.core.rsc.sampling.actions.LatLonReadoutAction;
import com.raytheon.uf.viz.core.rsc.sampling.actions.SampleAction;
import com.raytheon.uf.viz.pdc.PointDataControlDlg;
import com.raytheon.viz.hydro.gagedisplay.StationDisplay;
import com.raytheon.viz.hydrocommon.actions.SetProjection;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.cmenu.UnloadAllProductsAction;
import com.raytheon.viz.ui.cmenu.ZoomMenuAction;
import com.raytheon.viz.ui.perspectives.AbstractCAVEPerspectiveManager;

/**
 * This class manages the Hydro Perspective in CAVE.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 17, 2008           M. Duff   Initial creation
 * Apr 27, 2010           mschenke  refactor for common perspective switching
 * Mar 01, 2011  5367     jnjanga   Enabled point data displays upon hydroview
 *                                  launch.
 * Mar 14, 2013  1790     rferrel   Changes for non-blocking
 *                                  PointDataControlDlg.
 * Dec 14, 2015  5193     bsteffen  Eclipse 4: Renamed field in super class.
 * Jul 09, 2018  7315     bsteffen  Reopen on activate without accessing super
 *                                  class fields.
 * Sep 21, 2018  7379     mduff     Support PDC Refactor.
 * 
 * </pre>
 * 
 * @author M. Duff
 */
public class HydroPerspectiveManager extends AbstractCAVEPerspectiveManager {
    /** The Hydro Perspective Class */
    public static final String HYDRO_PERSPECTIVE = "com.raytheon.viz.hydro.HydroPerspective";

    private static final SampleAction sampleAction = new SampleAction();

    private static final LatLonReadoutAction readoutAction = new LatLonReadoutAction();

    private static final UnloadAllProductsAction unloadAllAction = new UnloadAllProductsAction();

    /*
     * True if the perspective should be started from scratch next time it is
     * opened.
     */
    private boolean reopen = false;

    @Override
    public void open() {
        loadDefaultBundle(getBundleToLoad());
        String[] maps;

        // Get the maps configured for display at startup
        String displayMaps = AppsDefaults.getInstance().getToken("display_maps",
                "statesCounties");

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
                        .getActiveDisplayPane().getDescriptor());

        // Load the configured maps
        for (String map : maps) {
            mapMgr.loadMapByBundleName(map.trim());
        }

        StationDisplay.getInstance().getMultiPointResource();
        SetProjection.setDefaultProjection(currentEditor, "hv");
        displayGages();
    }

    @Override
    public void activate() {
        if (reopen) {
            opened = false;
            reopen = false;
        }
        super.activate();

        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
                .layout(true, true);
    }

    @Override
    public void deactivate() {
        IEditorReference[] editorReferences = page.getEditorReferences();
        if (editorReferences == null || editorReferences.length == 0) {
            reopen = true;
        }
        super.deactivate();
    }

    /**
     * Hydro bundle to load
     * 
     * @return
     */
    protected String getBundleToLoad() {
        return "hydro/default-procedure.xml";
    }

    /**
     * Hydro context id
     * 
     * @return
     */
    protected String getContextId() {
        return "com.raytheon.viz.hydro.Hydro";
    }

    private void displayGages() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        PointDataControlDlg dlg = PointDataControlDlg.getInstance(shell);
        dlg.openDialog();
        dlg.hide();
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager,
            IDisplayPaneContainer container, IDisplayPane pane) {
        super.addContextMenuItems(menuManager, container, pane);

        sampleAction.setContainer(container);
        unloadAllAction.setContainer(container);

        menuManager.add(new ZoomMenuAction(container));
        menuManager.add(new Separator());
        menuManager.add(sampleAction);

        readoutAction.setContainer(container);
        menuManager.add(readoutAction);
        readoutAction.setSelectedRsc(null);

        menuManager.add(new Separator());
        menuManager.add(unloadAllAction);

        sampleAction.setSelectedRsc(null);
    }
}
