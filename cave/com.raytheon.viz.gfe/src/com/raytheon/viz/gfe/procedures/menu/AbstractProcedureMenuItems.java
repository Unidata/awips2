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
package com.raytheon.viz.gfe.procedures.menu;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;
import org.eclipse.ui.services.IServiceLocator;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;

/**
 * Abstract compound contribution item for dynamically determining the menu
 * items based on procedures.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 05, 2008            njensen      Initial creation
 * Jul 27, 2015  #4263     dgilling     Support ProcedureMetadataManager.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractProcedureMenuItems extends
        CompoundContributionItem {

    private static final String COMMAND = "com.raytheon.viz.gfe.actions.RunProcedureAction";

    /**
     * Gets the name of the menu
     * 
     * @return
     */
    public abstract String getMenuName();

    /**
     * Gets the id
     */
    @Override
    public abstract String getId();

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        DataManager manager = DataManagerUIFactory.getCurrentInstance();
        if (manager != null) {
            List<String> procs = manager.getProcedureInterface().getMenuItems(
                    getMenuName());
            IServiceLocator locate = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();

            CommandContributionItemParameter[] additional = getAdditionalItems();
            CommandContributionItemParameter[] list = new CommandContributionItemParameter[procs
                    .size() + additional.length];

            int i = 0;
            for (String proc : procs) {
                Map<String, String> parms = new HashMap<String, String>();
                parms.put("name", proc);
                list[i++] = new CommandContributionItemParameter(locate,
                        getId(), COMMAND, parms, null, null, null, proc, null,
                        null, CommandContributionItem.STYLE_PUSH, null, true);
            }

            System.arraycopy(additional, 0, list, procs.size(),
                    additional.length);

            Arrays.sort(list,
                    new Comparator<CommandContributionItemParameter>() {

                        @Override
                        public int compare(
                                CommandContributionItemParameter left,
                                CommandContributionItemParameter right) {
                            return left.label.compareTo(right.label);
                        }
                    });

            IContributionItem[] items = new IContributionItem[list.length];
            i = 0;
            for (CommandContributionItemParameter p : list) {
                items[i++] = new CommandContributionItem(p);
            }

            return items;
        } else {
            return new CommandContributionItem[0];
        }
    }

    protected CommandContributionItemParameter[] getAdditionalItems() {
        return new CommandContributionItemParameter[0];
    }
}
