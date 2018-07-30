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
package com.raytheon.uf.viz.damagepath;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.awipstools.ui.layer.InteractiveBaselinesLayer;
import com.raytheon.viz.awipstools.ui.layer.InteractiveBaselinesLayer.Baseline;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Action to create a tornado damage path from an InteractiveBaselineLayer.
 *
 * Adds a menu item to the DamagePath's legend right-click menu which has a
 * submenu to select one of the available baselines.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2015 3977       nabowle     Initial creation
 * Jun 01, 2015 3975       dgilling    Update for DamageLayer changes for
 *                                     multiple polygon support.
 * Jun 18, 2015 4354       dgilling    Update isEnabled to consider editable
 *                                     capability.
 * Jun 19, 2015 3977       nabowle     Specify Tornado Path.
 * Jun 23, 2015 3977       nabowle     Switch to InteractiveBaselineLayer.
 *                                     Renamed from ImportFromDistanceSpeedAction.
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public class NewTornadoDamagePathAction extends AbstractRightClickAction
        implements IMenuCreator {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NewTornadoDamagePathAction.class);

    private Menu menu;

    public NewTornadoDamagePathAction() {
        super("New Tornado Path from Baseline", SWT.DROP_DOWN);
    }

    @Override
    public Menu getMenu(Control parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);
        createMenu(menu);

        return menu;
    }

    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
    }

    @Override
    public Menu getMenu(Menu parent) {
        if (menu != null) {
            menu.dispose();
        }

        createMenu(parent);

        return menu;
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    /**
     * Creates a submenu with every displayed baseline, listed in alphabetical
     * order.
     *
     * @param parent
     */
    private void createMenu(Menu parent) {
        menu = new Menu(parent);
        InteractiveBaselinesLayer ibl = findImportLayer(getSelectedRsc());
        if (ibl == null || ibl.getCurrentBaselines() == null) {
            return;
        }
        List<Baseline> baselines = new ArrayList<>(Arrays.asList(ibl
                .getCurrentBaselines()));
        Collections.sort(baselines, new Comparator<Baseline>() {
            @Override
            public int compare(Baseline b1, Baseline b2) {
                return b1.name.compareTo(b2.name);
            }
        });

        for (Baseline baseline : baselines) {
            ActionContributionItem aci = new ActionContributionItem(
                    new ImportFromBaselineInternalAction(baseline));
            aci.fill(menu, -1);
        }
    }


    @Override
    public boolean isEnabled() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        boolean enabled = rsc.getCapability(EditableCapability.class)
                .isEditable();
        if (enabled) {
            if (rsc != null) {
                enabled = findImportLayer(rsc) != null;
            }
        }
        return enabled;
    }

    /**
     * Finds the import layer.
     *
     * @param rsc
     *            The current resource
     * @return The found import layer, or null if the tool is not loaded.
     */
    private InteractiveBaselinesLayer findImportLayer(
            AbstractVizResource<?, ?> rsc) {
        ResourceList resources = rsc.getDescriptor().getResourceList();
        for (ResourcePair rp : resources) {
            if (rp.getResource() instanceof InteractiveBaselinesLayer) {
                return (InteractiveBaselinesLayer) rp.getResource();
            }
        }
        return null;
    }

    /**
     * Submenu actions for each available baseline.
     *
     * Each action is created with one of the available baselines, which when
     * run will generate a Tornado damage path for that baseline.
     */
    private class ImportFromBaselineInternalAction extends Action {
        final Baseline baseline;

        public ImportFromBaselineInternalAction(Baseline line) {
            super(line.name, Action.AS_PUSH_BUTTON);
            this.baseline = line;
        }

        @Override
        public void run() {
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    DamagePathLayer<?> layer = (DamagePathLayer<?>) getSelectedRsc();

                    Polygon polygon = DamagePathUtils
                            .estimateTornadoDamagePath(baseline);

                    if (polygon != null) {
                        layer.addPolygon(polygon.getExteriorRing()
                                .getCoordinates());
                    }
                }
            });

            getContainer().refresh();
        }

        @Override
        public String getText() {
            return baseline.name;
        }

    }
}
