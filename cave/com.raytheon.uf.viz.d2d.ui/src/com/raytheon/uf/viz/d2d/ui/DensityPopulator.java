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
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.status.StatusConstants;

/**
 * Class for loading the density items to the UI element
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

public class DensityPopulator extends CompoundContributionItem {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(DensityPopulator.class);

    public static final String DENSITY_PROPERTY = "density";

    public static final String MAX_LABEL = "Max";

    private static String[] densityLabels;

    private static Double[] densityVals;

    static {
        String[] densities = Activator.getDefault().getPreferenceStore()
                .getStringArray(DENSITY_PROPERTY);
        List<String> labels = new ArrayList<String>();
        List<Double> vals = new ArrayList<Double>();

        for (String density : densities) {
            try {
                Double d = Double.valueOf(density);
                labels.add(d < DensityCapability.MAX_THRESHOLD ? density
                        : MAX_LABEL);
                vals.add(d);
            } catch (Throwable t) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not parse density: " + density, t);
                continue;
            }
        }

        densityVals = vals.toArray(new Double[vals.size()]);
        densityLabels = labels.toArray(new String[labels.size()]);
    }

    public static Double getValueFor(String label) {
        if (label == null) {
            return null;
        }

        if (MAX_LABEL.equalsIgnoreCase(label)) {
            return DensityCapability.MAX_THRESHOLD;
        }

        try {
            return Double.parseDouble(label);
        } catch (Throwable t) {

        }

        return null;
    }

    public static String getLabelFor(Double value) {
        if (value == null) {
            return null;
        }
        for (int i = 0; i < densityVals.length; ++i) {
            if (densityVals[i].equals(value)) {
                return densityLabels[i];
            }
        }
        return value < DensityCapability.MAX_THRESHOLD ? value.toString()
                : MAX_LABEL;
    }

    public static Double[] getDensityValues() {
        return densityVals;
    }

    public static String[] getDensityLabels() {
        return densityLabels;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        MenuManager menuMgr = new MenuManager("Density", "density");
        for (int i = 0; i < densityVals.length; ++i) {
            Double val = densityVals[i];
            String lab = densityLabels[i];

            Map<String, String> parms = new HashMap<String, String>();
            parms.put("density", String.valueOf(val));
            CommandContributionItem item = new CommandContributionItem(
                    new CommandContributionItemParameter(
                            PlatformUI.getWorkbench(), null,
                            "com.raytheon.viz.ui.actions.setdensity", parms,
                            null, null, null, lab, null, null,
                            CommandContributionItem.STYLE_PUSH, null, true));
            menuMgr.add(item);
        }

        return menuMgr.getItems();
    }

}
