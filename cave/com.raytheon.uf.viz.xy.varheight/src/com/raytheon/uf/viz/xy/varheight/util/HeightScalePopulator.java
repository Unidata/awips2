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
package com.raytheon.uf.viz.xy.varheight.util;

import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.uf.viz.xy.scales.HeightScales;

/**
 * Dynamic height scale contributor
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 07, 2010            mschenke    Initial creation
 * Oct 10, 2022 8946       mapeters    Updated for new combo editor
 *
 * </pre>
 *
 * @author mschenke
 */
public class HeightScalePopulator extends CompoundContributionItem {

    @Override
    protected IContributionItem[] getContributionItems() {
        MenuManager menuMgr = new MenuManager("Scales", "mapControls");

        for (HeightScale scale : HeightScales.getInstance().getScales()) {
            Map<String, String> parms = Map.of(VizConstants.HEIGHT_SCALE_ID,
                    scale.getName());
            CommandContributionItem item = new CommandContributionItem(
                    new CommandContributionItemParameter(
                            PlatformUI.getWorkbench(), null,
                            HeightScaleHandler.SET_SCALE_COMMAND_ID, parms,
                            null, null, null, scale.getName(), null, null,
                            CommandContributionItem.STYLE_PUSH, null, true));
            menuMgr.add(item);
        }

        return menuMgr.getItems();
    }

}
