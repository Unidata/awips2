package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import gov.noaa.nws.ncep.viz.resources.manager.PredefinedAreasMngr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

/**
 * Create the Menu Items for the Predefined Area menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *  4/15/11                  G. Hull      created.
 * 07/28/11       450        G. Hull      Use PredefinedAreasMngr
 * 03/29/12                  B. Hebbard   refactor courtesy of Matt Nash (RTS)
 *                                        to extend CompoundContributionItem
 *                                        (instead of ContributionItem); fixes
 *                                        menu items disappearing (after first
 *                                        menu use) in OB12.4
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class PredefinedAreaMenu extends CompoundContributionItem {
    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        String predefinedAreas[] = PredefinedAreasMngr
                .getAvailPredefinedAreas();

        List<IContributionItem> items = new ArrayList<IContributionItem>();
        for (String areaName : predefinedAreas) {
            Map<String, String> parms = new HashMap<String, String>();
            parms.put("areaName", areaName);
            CommandContributionItem item = new CommandContributionItem(
                    new CommandContributionItemParameter(
                            PlatformUI.getWorkbench(),
                            null,
                            "gov.noaa.nws.ncep.viz.ui.actions.loadPredefinedArea",
                            parms, null, null, null, areaName, null, null,
                            CommandContributionItem.STYLE_PUSH, null, true));

            items.add(item);
        }
        return items.toArray(new IContributionItem[0]);
    }

}

