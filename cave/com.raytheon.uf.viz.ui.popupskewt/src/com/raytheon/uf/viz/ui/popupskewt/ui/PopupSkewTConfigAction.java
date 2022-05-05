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
package com.raytheon.uf.viz.ui.popupskewt.ui;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.sounding.providers.VerticalSoundingProviderFactory;
import com.raytheon.uf.viz.ui.popupskewt.config.SoundingSource;
import com.raytheon.uf.viz.ui.popupskewt.config.SoundingSourceConfigFactory;
import com.raytheon.uf.viz.ui.popupskewt.rsc.PopupSkewTResource;

/**
 * {@link Action} for creating menu for configuring a {@link PopupSkewTResource}
 * 
 * TODO: How should this work on four-panel? See A1 D2D
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013       2190 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PopupSkewTConfigAction extends Action implements IMenuCreator {

    private Menu menu;

    private PopupSkewTResource resource;

    public PopupSkewTConfigAction(String menuName, PopupSkewTResource resource) {
        super(menuName, SWT.DROP_DOWN);
        this.resource = resource;
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
            menu = null;
        }
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
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Control)
     */
    @Override
    public Menu getMenu(Control parent) {
        dispose();

        menu = new Menu(parent);
        fillMenu(menu);
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
        dispose();

        menu = new Menu(parent);
        fillMenu(menu);
        return menu;
    }

    /**
     * Fill's the menu with the popup skewt config options
     */
    private void fillMenu(Menu menu) {
        SoundingSource[] sources = SoundingSourceConfigFactory
                .getSoundingSources();
        SoundingSource selected = resource.getSource();

        Action noSampling = new SetSourceInternalAction(null);
        noSampling.setChecked(selected == null);
        new ActionContributionItem(noSampling).fill(menu, -1);

        for (SoundingSource source : sources) {
            if (VerticalSoundingProviderFactory.hasProviderForType(source
                    .getType())) {
                Action action = new SetSourceInternalAction(source);
                action.setChecked(selected == source);
                new ActionContributionItem(action).fill(menu, -1);
            }
        }

        new Separator().fill(menu, -1);

        new ActionContributionItem(new ToggleSkewTInternalAction(
                resource.isPopupSkewTOn())).fill(menu, -1);
    }

    private class SetSourceInternalAction extends Action {

        private static final String NO_SOURCE_STRING = "No Sampling";

        private SoundingSource source;

        public SetSourceInternalAction(SoundingSource source) {
            super(
                    source == null ? NO_SOURCE_STRING : source
                            .getDisplayString(), Action.AS_RADIO_BUTTON);
            this.source = source;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            resource.setSource(source);
        }

    }

    private class ToggleSkewTInternalAction extends Action {

        public ToggleSkewTInternalAction(boolean selected) {
            super("Skew T", Action.AS_CHECK_BOX);
            this.setChecked(selected);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            resource.setPopupSkewTOn(isChecked());
        }

    }

}
