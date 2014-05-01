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
package com.raytheon.uf.viz.d2d.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.status.StatusConstants;

/**
 * Class for loading the magnification elements to the UI
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MagnificationPopulator extends CompoundContributionItem {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(MagnificationPopulator.class);

    public static final String MAGNIFICATION_PROPERTY = "magnification";

    private static String[] mags;

    static {
        String[] tmp = Activator.getDefault().getPreferenceStore()
                .getStringArray(MAGNIFICATION_PROPERTY);
        List<String> vals = new ArrayList<String>();
        for (int i = 0; i < tmp.length; ++i) {
            try {
                Double.parseDouble(tmp[i]);
                vals.add(tmp[i]);
            } catch (Throwable t) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not parse magnification: " + tmp[i], t);
            }
        }
        mags = vals.toArray(new String[vals.size()]);
    }

    public static String[] getMagnifications() {
        return mags;
    }

    public static String getLabelFor(Double val) {
        if (val == null) {
            return null;
        }

        for (int i = 0; i < mags.length; ++i) {
            try {
                if (Double.parseDouble(mags[i]) == val) {
                    return mags[i];
                }
            } catch (Throwable t) {

            }
        }

        return val.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        MenuManager menuMgr = new MenuManager("Magnification", "mag");
        for (String m : mags) {
            Map<String, String> parms = new HashMap<String, String>();
            parms.put("magnification", m);
            CommandContributionItem item = new CommandContributionItem(
                    new CommandContributionItemParameter(PlatformUI
                            .getWorkbench(), null,
                            "com.raytheon.viz.ui.actions.setmagnification",
                            parms, null, null, null, m, null, null,
                            CommandContributionItem.STYLE_PUSH, null, true));
            menuMgr.add(item);
        }
        return menuMgr.getItems();
    }

}
