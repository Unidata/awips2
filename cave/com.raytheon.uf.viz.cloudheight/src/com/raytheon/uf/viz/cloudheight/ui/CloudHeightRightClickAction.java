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
package com.raytheon.uf.viz.cloudheight.ui;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.cloudheight.CloudHeightAlgorithm;
import com.raytheon.uf.viz.cloudheight.data.CloudHeightData;
import com.raytheon.uf.viz.cloudheight.data.SoundingSource;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Right click action for cloud height/popup skewt
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 14, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class CloudHeightRightClickAction extends AbstractRightClickAction
        implements IMenuCreator {

    private Menu menu;

    private CloudHeightAlgorithm cloudHeight;

    private IDescriptor descriptor;

    /**
     * 
     */
    public CloudHeightRightClickAction(IDescriptor descriptor,
            CloudHeightAlgorithm cloudHeight) {
        super("Sample Cloud Heights/Radar Skew T", SWT.DROP_DOWN);
        this.cloudHeight = cloudHeight;
        this.descriptor = descriptor;
    }

    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
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

    @Override
    public Menu getMenu(Control parent) {

        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        fillMenu(menu);

        return menu;
    }

    /**
     * 
     */
    private void fillMenu(Menu menu) {
        SoundingSource curSource = cloudHeight.getCurrentSource();
        for (SoundingSource source : CloudHeightData.getCloudHeightData()
                .getSources()) {
            Action internalAction = new SetSourceInternalAction(source);
            internalAction.setChecked(source.equals(curSource));
            ActionContributionItem aci = new ActionContributionItem(
                    internalAction);
            aci.fill(menu, -1);
        }

        new Separator().fill(menu, -1);

        new ActionContributionItem(new ToggleSkewTInternalAction(
                cloudHeight.isSkewT())).fill(menu, -1);
    }

    @Override
    public Menu getMenu(Menu parent) {

        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        fillMenu(menu);

        return menu;
    }

    private class SetSourceInternalAction extends Action {

        private SoundingSource source;

        public SetSourceInternalAction(SoundingSource source) {
            super(source.toString(), Action.AS_RADIO_BUTTON);
            this.source = source;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            cloudHeight.setCurrentSource(source);
            TimeMatchingJob.scheduleTimeMatch(descriptor);
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
            cloudHeight.setSkewT(isChecked());
        }

    }

}
