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
package com.raytheon.viz.grid;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.viz.grid.inv.GridInventory;

/**
 * Grid product formatter. Refactored out of GridProductBrowserDataDefinition.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 19, 2013   2391     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GridProductBrowserDataFormatter {

    /**
     * Format the info for the product browser display
     * 
     * @param param
     *            Parameter
     * @param parameters
     *            List of parameters
     * @return List of ProductBrowserLabel objects
     * @throws CommunicationException
     */
    public static List<ProductBrowserLabel> formatGridData(String param,
            String[] parameters) throws CommunicationException {
        List<ProductBrowserLabel> labels = new ArrayList<ProductBrowserLabel>();
        if (GridInventory.MODEL_NAME_QUERY.equals(param)) {
            DatasetInfoLookup lookup = DatasetInfoLookup.getInstance();
            for (int i = 0; i < parameters.length; i++) {
                DatasetInfo info = lookup.getInfo(parameters[i]);
                if (info == null) {
                    labels.add(new ProductBrowserLabel(parameters[i],
                            parameters[i]));
                } else {
                    labels.add(new ProductBrowserLabel(info.getTitle() + " ("
                            + parameters[i] + ")", parameters[i]));
                }
            }
            Collections.sort(labels);
            return labels;
        } else if (GridInventory.PARAMETER_QUERY.equals(param)) {
            Map<String, DerivParamDesc> library = DerivedParameterGenerator
                    .getDerParLibrary();
            for (int i = 0; i < parameters.length; i++) {
                DerivParamDesc desc = library.get(parameters[i]);
                if (desc == null || desc.getName().isEmpty()) {
                    labels.add(new ProductBrowserLabel(parameters[i],
                            parameters[i]));
                } else {
                    labels.add(new ProductBrowserLabel(desc.getName() + " ("
                            + parameters[i] + ")", parameters[i]));
                }
            }
            Collections.sort(labels);
            return labels;
        } else if (GridInventory.LEVEL_ID_QUERY.equals(param)) {
            Level[] levels = new Level[parameters.length];
            LevelFactory lf = LevelFactory.getInstance();
            for (int i = 0; i < levels.length; i++) {
                levels[i] = lf.getLevel(parameters[i]);
            }
            Arrays.sort(levels, levelComparator);
            for (int i = 0; i < parameters.length; i++) {
                String levelName = levels[i].toString().replace("_", "-");
                levelName = levelName.replace(levels[i].getMasterLevel()
                        .getName(), " " + levels[i].getMasterLevel().getName());
                labels.add(new ProductBrowserLabel(levelName, Long
                        .toString(levels[i].getId())));
            }
            return labels;
        } else if (GridInventory.MASTER_LEVEL_QUERY.equals(param)) {
            LevelFactory lf = LevelFactory.getInstance();
            for (int i = 0; i < parameters.length; i++) {
                MasterLevel masterLevel = lf.getMasterLevel(parameters[i]);
                labels.add(new ProductBrowserLabel(masterLevel.getDescription()
                        + " (" + masterLevel.getName() + ")", masterLevel
                        .getName()));
            }
            Collections.sort(labels);
            return labels;
        }
        return Collections.emptyList();
    }

    private static final Comparator<Level> levelComparator = new Comparator<Level>() {
        @Override
        public int compare(Level o1, Level o2) {
            if (o1.isRangeLevel() == o2.isRangeLevel()) {
                int val = Double.compare(o1.getLevelonevalue(),
                        o2.getLevelonevalue());
                if (val == 0) {
                    val = Double.compare(o1.getLeveltwovalue(),
                            o2.getLeveltwovalue());
                }
                return val;
            }
            if (o1.isRangeLevel()) {
                return 1;
            } else {
                return -1;
            }
        }
    };
}
