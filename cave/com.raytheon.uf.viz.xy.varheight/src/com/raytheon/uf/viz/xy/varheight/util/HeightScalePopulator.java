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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.d2d.ui.AbstractHeightDisplay;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.slice.request.HeightScales;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Dynamic height scale contributor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class HeightScalePopulator extends CompoundContributionItem {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        MenuManager menuMgr = new MenuManager("Scales", "mapControls");
        IDisplayPaneContainer cont = EditorUtil.getActiveVizContainer();
        // Load scales if we have d2d map renderable display on editor or, there
        // is no editor opened in the d2d perspective
        if ((cont != null && (cont.getActiveDisplayPane()
                .getRenderableDisplay() instanceof AbstractHeightDisplay))) {
            for (HeightScale scale : HeightScales.getInstance().getScales()) {
                Map<String, String> parms = new HashMap<String, String>();
                parms.put("scale", scale.getName());
                CommandContributionItem item = new CommandContributionItem(
                        new CommandContributionItemParameter(
                                PlatformUI.getWorkbench(), null,
                                "com.raytheon.uf.viz.xy.height.setScale",
                                parms, null, null, null, scale.getName(), null,
                                null, CommandContributionItem.STYLE_PUSH, null,
                                true));
                menuMgr.add(item);
            }
        }
        return menuMgr.getItems();
    }

}
