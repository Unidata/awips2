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
package com.raytheon.viz.gfe.actions;

import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.gridmanager.action.AssignAction;
import com.raytheon.viz.gfe.gridmanager.action.CopyAction;
import com.raytheon.viz.gfe.gridmanager.action.CreateFromScratchAction;
import com.raytheon.viz.gfe.gridmanager.action.DeleteAction;
import com.raytheon.viz.gfe.gridmanager.action.DisplayInfoAction;
import com.raytheon.viz.gfe.gridmanager.action.FragmentAction;
import com.raytheon.viz.gfe.gridmanager.action.PasteAction;
import com.raytheon.viz.gfe.rsc.GFEResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GridOpsAction extends AbstractRightClickAction implements
        IMenuCreator {

    private static final int ONE_SECOND = 1000;

    private Menu menu;

    /**
     * Constructor
     */
    public GridOpsAction() {
        super(SWT.DROP_DOWN);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Grid Ops";
    }

    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
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
        GFEResource rsc = (GFEResource) getSelectedRsc();
        Parm parm = rsc.getParm();
        GridID id = new GridID(parm, DataManager.getCurrentInstance()
                .getSpatialDisplayManager().getSpatialEditorTime());
        TimeRange tr = new TimeRange(id.getDate(), ONE_SECOND);
        TimeConstraints tc = parm.getGridInfo().getTimeConstraints();

        ActionContributionItem aci;
        if (id.grid() != null) {
            if (parm.isOkToEdit(tr)) {
                TimeRange gridValidTime = id.grid().getGridTime();

                // Delete Grid
                aci = new ActionContributionItem(new DeleteAction(parm,
                        tr.getStart()));
                aci.fill(menu, -1);

                // Fragment Grid
                if (tc.anyConstraints()
                        && tc.constraintTime(gridValidTime.getStart()) != gridValidTime) {
                    aci = new ActionContributionItem(new FragmentAction(parm,
                            tr.getStart()));
                    aci.fill(menu, -1);
                }

                // Assign pickup value
                WxValue pickUpValue = parm.getParmState().getPickUpValue();
                aci = new ActionContributionItem(new AssignAction(parm, tr,
                        pickUpValue));
                aci.fill(menu, -1);

                // Assign default value
                WxValue defaultValue = WxValue.defaultValue(parm);
                if (!defaultValue.equals(pickUpValue)) {
                    aci = new ActionContributionItem(new AssignAction(parm, tr,
                            defaultValue));
                }
                aci.fill(menu, -1);
            }

            // Copy Grid
            aci = new ActionContributionItem(
                    new CopyAction(parm, tr.getStart()));
            aci.fill(menu, -1);
        } else if ((tc.constraintTime(id.getDate())).isValid()
                && parm.isOkToEdit(tr)) {
            // Create from Scratch
            aci = new ActionContributionItem(new CreateFromScratchAction(parm,
                    id.getDate()));
            aci.fill(menu, -1);
        }
        if (DataManager.getCurrentInstance().getParmOp()
                .okToPasteGrid(parm, tr.getStart())) {
            // Paste Grid
            aci = new ActionContributionItem(new PasteAction(parm,
                    tr.getStart()));
            aci.fill(menu, -1);
        }

        // Display Info
        aci = new ActionContributionItem(new DisplayInfoAction(parm,
                id.getDate()));
        aci.fill(menu, -1);
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getMenuCreator()
     */
    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }
}
