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
package com.raytheon.viz.grid.actions;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Supplier;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.viz.grid.rsc.GridNameGenerator;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.grid.xml.VerticalInteractionConfigManager;
import com.raytheon.viz.grid.xml.VerticalInteractionLevelGroup;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Action for toggling the Vertical Interaction capability of the selected
 * resource. Vertical Interaction specifies the set of levels that are loaded
 * for the resource.
 *
 * This provides a drop-down menu of radio button options, one for each preset
 * group of levels that's configured for the resource's master level, along with
 * an option to disable Vertical Interaction.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * May 07, 2021  8453     randerso  Initial creation
 * Nov 02, 2022  8963     mapeters  Prevent error when no data is available,
 *                                  redo time matching for other descriptors too,
 *                                  disable/uncheck when not the time match basis
 * Sep 06, 2024  2036517  mapeters  Switch to a drop-down menu that supports
 *                                  multiple groups of levels per master level
 * Sep 17, 2024  2037943  mapeters  Handle duplicates in master level constraint
 *
 * </pre>
 *
 * @author randerso
 */
public class VerticalInteractionAction extends AbstractRightClickAction
        implements IMenuCreator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VerticalInteractionAction.class);

    private static final VerticalInteractionConfigManager config = VerticalInteractionConfigManager
            .getInstance();

    private Menu menu;

    /**
     * Constructor
     */
    public VerticalInteractionAction() {
        super("Vertical Interaction", Action.AS_DROP_DOWN_MENU);
    }

    private GridResourceData getGridResourceData() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        if (rsc != null) {
            AbstractResourceData rscData = rsc.getResourceData();
            if (rscData instanceof GridResourceData) {
                return (GridResourceData) rscData;
            }
        }
        return null;
    }

    /**
     * @return the master level from the selected resource, or null if no level
     *         constraint is specified.
     */
    private String getMasterLevel() {
        GridResourceData rscData = getGridResourceData();
        if (rscData == null) {
            return null;
        }
        String masterLevel = rscData
                .getConstraintValue(GridConstants.MASTER_LEVEL_NAME);
        if (masterLevel != null && masterLevel.contains(",")) {
            /*
             * This is done to make config lookups work when a single master
             * level is duplicated (e.g. "FH,FH"), but a TreeSet is used for
             * consistent ordering in case Vertical Interaction is ever
             * configured for resources that actually have multiple distinct
             * master levels.
             */
            Set<String> masterLevelSet = new TreeSet<>(
                    Arrays.asList(masterLevel.split(",")));
            masterLevel = String.join(",", masterLevelSet);
        }
        return masterLevel;
    }

    @Override
    public boolean isEnabled() {
        /*
         * Return true if vertical interaction levels are configured for the
         * master level associated with the selected resource and we are the
         * time match basis
         */
        List<VerticalInteractionLevelGroup> levelGroups = config
                .getLevelGroups(getMasterLevel());
        return !levelGroups.isEmpty() && isResourceTimeMatchBasis();
    }

    @Override
    public String getToolTipText() {
        /*
         * If the Vertical Interaction menu is disabled, set the tool tip text
         * to indicate why it's disabled
         */
        String masterLevel = getMasterLevel();
        if (masterLevel != null) {
            List<VerticalInteractionLevelGroup> levelGroups = config
                    .getLevelGroups(masterLevel);

            if (levelGroups.isEmpty()) {
                return "No levels configured for master level \"" + masterLevel
                        + "\" in " + VerticalInteractionConfigManager.LOC_PATH;
            } else if (!isResourceTimeMatchBasis()) {
                return "Vertical Interaction can only be enabled for the Time Match Basis";
            }
        }
        return null;
    }

    private boolean isResourceTimeMatchBasis() {
        IDescriptor descriptor = getDescriptor();
        if (descriptor != null) {
            AbstractTimeMatcher tm = descriptor.getTimeMatcher();
            /*
             * Should always be a D2D time matcher anyway since plugin.xml
             * specifies this is for D2DGridResources
             */
            if (tm instanceof D2DTimeMatcher) {
                if (getSelectedRsc() == ((D2DTimeMatcher) tm)
                        .getTimeMatchBasis()) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    @Override
    public Menu getMenu(Control parent) {
        return getMenu(() -> new Menu(parent));
    }

    @Override
    public Menu getMenu(Menu parent) {
        return getMenu(() -> new Menu(parent));
    }

    private Menu getMenu(Supplier<Menu> menuConstructor) {
        if (menu != null) {
            menu.dispose();
        }
        menu = menuConstructor.get();
        fillMenu(menu);
        return menu;

    }

    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
            menu = null;
        }
    }

    private void fillMenu(Menu menu) {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        GridResourceData rscData = getGridResourceData();
        IDescriptor descriptor = getDescriptor();
        if (rsc == null || rscData == null || descriptor == null) {
            return;
        }
        List<VerticalInteractionLevelGroup> levelGroups = config
                .getLevelGroups(getMasterLevel());
        for (VerticalInteractionLevelGroup group : levelGroups) {
            ActionContributionItem enableGroupItem = new ActionContributionItem(
                    new VerticalInteractionInternalAction(group, rsc, rscData,
                            descriptor));
            enableGroupItem.fill(menu, -1);
        }

        ActionContributionItem disableItem = new ActionContributionItem(
                new VerticalInteractionInternalOffAction(rsc, rscData,
                        descriptor));
        disableItem.fill(menu, -1);
    }

    /**
     * Abstract action for sub-menu entries that either enable Vertical
     * Interaction for a level group, or disable Vertical Interaction.
     */
    private abstract static class AbstractVerticalInteractionInternalAction
            extends Action {

        protected final AbstractVizResource<?, ?> rsc;

        protected final GridResourceData rscData;

        protected final IDescriptor descriptor;

        protected final boolean initialChecked;

        protected AbstractVerticalInteractionInternalAction(String label,
                AbstractVizResource<?, ?> rsc, GridResourceData rscData,
                IDescriptor descriptor, boolean initialChecked) {
            super(label, Action.AS_RADIO_BUTTON);
            this.rsc = rsc;
            this.rscData = rscData;
            this.descriptor = descriptor;
            this.initialChecked = initialChecked;
            setChecked(initialChecked);
        }

        protected void setLevelConstraintValue(String levelStr) {
            RequestConstraint levelConstraint = rscData
                    .getConstraint(GridConstants.LEVEL_ONE);
            levelConstraint.setConstraintType(ConstraintType.IN);
            levelConstraint.setConstraintValue(levelStr);
        }

        protected void redoTimeMatching() {
            try {
                // Clear cached times for the resource
                rscData.invalidateAvailableTimesCache();
                descriptor.getTimeMatcher().redoTimeMatching(rsc);
                // Actually re-do the time matching
                descriptor.redoTimeMatching();
                /*
                 * Other panes need updating as well since this action is only
                 * available when we are the time match basis
                 */
                IDisplayPaneContainer container = rsc.getResourceContainer();
                if (container != null) {
                    for (IDisplayPane mainCanvas : container
                            .getMainCanvases()) {
                        IDescriptor canvasDescriptor = mainCanvas
                                .getDescriptor();
                        if (canvasDescriptor != descriptor) {
                            canvasDescriptor.redoTimeMatching();
                        }
                    }
                }
            } catch (VizException e) {
                statusHandler.error(
                        "Error re-doing time matching for Vertical Interaction",
                        e);
            }
        }

        protected boolean shouldRun() {
            /*
             * Only run this action when it's actually toggled on. Don't run it
             * if the user selects it when it was already selected, or if it's
             * actually being toggled off due to another action being selected.
             */
            return isChecked() && !initialChecked;
        }
    }

    /**
     * Internal sub-menu action for enabling Vertical Interaction for one of the
     * preset groups of levels.
     */
    private static class VerticalInteractionInternalAction
            extends AbstractVerticalInteractionInternalAction {

        private final VerticalInteractionLevelGroup levelGroup;

        public VerticalInteractionInternalAction(
                VerticalInteractionLevelGroup levelGroup,
                AbstractVizResource<?, ?> rsc, GridResourceData rscData,
                IDescriptor descriptor) {
            super(levelGroup.getLabel(), rsc, rscData, descriptor,
                    rscData.isSpatial() && levelGroup.matchesLevels(rscData
                            .getConstraintValue(GridConstants.LEVEL_ONE)));
            this.levelGroup = levelGroup;
        }

        @Override
        public void run() {
            if (!shouldRun()) {
                return;
            }
            /*
             * Replace the levels in the constraint with the levels configured
             * for vertical interaction for the master level associated with the
             * selected resource
             */
            setLevelConstraintValue(levelGroup.getLevels());
            rscData.setSpatial(true);
            /*
             * Replace the name generator with a new GridNameGenerator without
             * planeLabelString set to ensure we have one that will properly
             * display the level in the legend.
             */
            rscData.setNameGenerator(new GridNameGenerator());
            // Reload resource with new levels
            redoTimeMatching();
        }
    }

    /**
     * Internal sub-menu action for disabling Vertical Interaction.
     */
    private static class VerticalInteractionInternalOffAction
            extends AbstractVerticalInteractionInternalAction {

        public VerticalInteractionInternalOffAction(
                AbstractVizResource<?, ?> rsc, GridResourceData rscData,
                IDescriptor descriptor) {
            super("Off", rsc, rscData, descriptor, !rscData.isSpatial());
        }

        @Override
        public void run() {
            if (!shouldRun()) {
                return;
            }
            // Set level constraint to just the currently displayed level
            setLevelConstraintValue(getCurrentLevel());
            rscData.setSpatial(false);
            // Reload resource with new levels
            redoTimeMatching();
        }

        private String getCurrentLevel() {
            DataTime rscTime = descriptor.getTimeForResource(rsc);
            String levelValueStr;
            if (rscTime != null && rscTime.isSpatial()) {
                levelValueStr = rscTime.getLevelValue().toString();
            } else {
                /*
                 * No data available for current frame or its time is
                 * non-spatial, default to first configured level
                 */
                String masterLevel = rscData
                        .getConstraintValue(GridConstants.MASTER_LEVEL_NAME);
                String levelsStr = config.getLevelGroups(masterLevel).get(0)
                        .getLevels();
                levelValueStr = levelsStr.split(",")[0];
            }
            return levelValueStr;
        }
    }
}