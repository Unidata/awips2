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
package com.raytheon.viz.pointdata.ui.cmenu;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.localization.perspective.service.ILocalizationService;
import com.raytheon.uf.viz.localization.perspective.service.LocalizationPerspectiveUtils;
import com.raytheon.viz.pointdata.rsc.PlotBlendedResourceData;
import com.raytheon.viz.pointdata.rsc.PlotResourceData;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 */

public class ViewPlotModelAction extends AbstractRightClickAction implements
        IMenuCreator {

    private Menu menu;

    public ViewPlotModelAction() {
        super(SWT.DROP_DOWN);
    }

    @Override
    public boolean isHidden() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        if (rsc != null) {
            AbstractResourceData ard = rsc.getResourceData();
            if (ard != null
                    && (ard instanceof PlotBlendedResourceData || ard instanceof PlotResourceData)) {
                return false;
            }
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "View Plot Model";
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getMenuCreator()
     */
    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Control)
     */
    @Override
    public Menu getMenu(Control parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);
        createMenu(menu);

        return menu;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Menu)
     */
    @Override
    public Menu getMenu(Menu parent) {
        if (menu != null) {
            menu.dispose();
        }

        createMenu(parent);

        return menu;
    }

    private void createMenu(Menu parent) {
        menu = new Menu(parent);

        Set<String> files = new HashSet<>();

        AbstractResourceData ard = getSelectedRsc().getResourceData();
        if (ard instanceof PlotBlendedResourceData) {
            PlotBlendedResourceData pbrd = (PlotBlendedResourceData) ard;
            for (ResourcePair rp : pbrd.getResourceList()) {
                if (rp.getResource() != null
                        && rp.getResource().getResourceData() instanceof PlotResourceData) {
                    files.add(((PlotResourceData) rp.getResource()
                            .getResourceData()).getPlotModelFile());
                }
            }
        } else {
            files.add(((PlotResourceData) ard).getPlotModelFile());
        }

        for (String plotModelFile : files) {
            ActionContributionItem aci = new ActionContributionItem(
                    new EditPlotModelInternalAction(plotModelFile));
            aci.fill(menu, -1);
        }
    }

    private static class EditPlotModelInternalAction extends Action {

        private String plotModelFile;

        public EditPlotModelInternalAction(String plotModelFile) {
            int idx = plotModelFile.indexOf(File.separator);
            if (idx > -1) {
                setText(plotModelFile.substring(idx + 1));
            } else {
                setText(plotModelFile);
            }
            this.plotModelFile = plotModelFile;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            LocalizationFile file = PathManagerFactory.getPathManager()
                    .getStaticLocalizationFile(
                            PlotResourceData.PLOT_DIR + plotModelFile);

            ILocalizationService service = LocalizationPerspectiveUtils
                    .changeToLocalizationPerspective();
            if (service != null) {
                service.openFile(file);
            }
        }
    }

}
