package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Create the Menu Items for the Overlays menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *  4/15/11                  G. Hull      created.
 * 12/06/11                  B. Hebbard   sort menu entries alphabetically
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

public class OverlayMenu extends CompoundContributionItem {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        List<IContributionItem> items = new ArrayList<IContributionItem>();
        try {
            List<String> overlayRscTypes = ResourceDefnsMngr.getInstance()
                    .getResourceTypesForCategory(
                            ResourceName.OverlayRscCategory, "", false);
            Collections.sort(overlayRscTypes, new Comparator<String>() { // alphabetize
                                                                         // menu...
                        public int compare(String s1, String s2) { // ...case
                                                                   // insensitive
                            return s1.compareToIgnoreCase(s2);
                        }
                    });

            for (String overlayRsc : overlayRscTypes) {
                if (overlayRsc.equals(NmapCommon.getBaseOverlay())) {
                    continue;
                }

                Map<String, String> params = new HashMap<String, String>();
                params.put("overlayName", overlayRsc);
                CommandContributionItemParameter param = new CommandContributionItemParameter(
                        PlatformUI.getWorkbench(), null,
                        "gov.noaa.nws.ncep.viz.ui.actions.loadOverlay", params,
                        null, null, null, overlayRsc, null, null,
                        CommandContributionItem.STYLE_PUSH, null, true);
                CommandContributionItem ovrlyMenuItem = new CommandContributionItem(
                        param);
                items.add(ovrlyMenuItem);
            }
            return items.toArray(new IContributionItem[0]);
        } catch (VizException e) {
            return new IContributionItem[0];
        }
    }
}

