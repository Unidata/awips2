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
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.datalisting.DataListing;
import com.raytheon.uf.common.datalisting.impl.DefaultDataListing;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.viz.grid.inv.GridInventory;

/**
 * 
 * {@link DataListing} which can format several different grid elements nicely.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 09, 2015  4153     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridDataListing extends DefaultDataListing {

    public GridDataListing() throws ReflectiveOperationException {
        super(GridRecord.class);
    }

    public GridDataListing(List<String> keySet) {
        super(GridConstants.GRID, keySet);
    }

    public GridDataListing(Set<String> keySet) {
        super(GridConstants.GRID, keySet);
    }

    @Override
    public Map<String, RequestConstraint> getRequestConstraints(Map<String, String> keyVals) {
        Map<String, RequestConstraint> constraints = super.getRequestConstraints(keyVals);
        if (keyVals.containsKey(GridConstants.LEVEL_ID)) {
            constraints.remove(GridConstants.LEVEL_ID);
            /*
             * Convert Level id to level one and level two values so that it is
             * possible to update using a datauri.
             */
            Level level = LevelFactory.getInstance().getLevel(keyVals.get(GridConstants.LEVEL_ID));
            constraints.put(GridInventory.LEVEL_ONE_QUERY, new RequestConstraint(level.getLevelOneValueAsString()));
            constraints.put(GridInventory.LEVEL_TWO_QUERY, new RequestConstraint(level.getLevelTwoValueAsString()));
            constraints.put(GridInventory.MASTER_LEVEL_QUERY, new RequestConstraint(level.getMasterLevel().getName()));
        }
        return constraints;
    }

    @Override
    protected Map<String, String> getFormattedValues(String key, Collection<String> values) {
        if (GridConstants.DATASET_ID.equals(key)) {
            Map<String, String> formatted = new LinkedHashMap<>();
            DatasetInfoLookup lookup = DatasetInfoLookup.getInstance();
            for (String value : values) {
                DatasetInfo info = lookup.getInfo(value);
                if (info == null) {
                    formatted.put(value, value);
                } else {
                    formatted.put(value, info.getTitle() + " (" + value + ")");
                }
            }
            return sortByValue(formatted);
        } else if (GridInventory.PARAMETER_QUERY.equals(key)) {
            Map<String, String> formatted = new LinkedHashMap<>();
            Map<String, DerivParamDesc> library = DerivedParameterGenerator.getDerParLibrary();
            for (String value : values) {
                DerivParamDesc desc = library.get(value);
                if (desc == null || desc.getName().isEmpty()) {
                    formatted.put(value, value);
                } else {
                    formatted.put(value, desc.getName() + " (" + value + ")");
                }
            }
            return sortByValue(formatted);
        } else if (GridInventory.LEVEL_ID_QUERY.equals(key)) {
            List<Level> levels = new ArrayList<>(values.size());
            LevelFactory lf = LevelFactory.getInstance();
            for (String value : values) {
                levels.add(lf.getLevel(value));
            }
            Collections.sort(levels, levelComparator);
            Map<String, String> formatted = new LinkedHashMap<>();
            for (Level level : levels) {
                String levelName = level.toString().replace("_", "-");
                levelName = levelName.replace(level.getMasterLevel().getName(), " " + level.getMasterLevel().getName());
                formatted.put(Long.toString(level.getId()), levelName);
            }
            return formatted;
        } else if (GridInventory.MASTER_LEVEL_QUERY.equals(key)) {
            Map<String, String> formatted = new LinkedHashMap<>();
            LevelFactory lf = LevelFactory.getInstance();
            for (String value : values) {
                MasterLevel masterLevel = lf.getMasterLevel(value);
                formatted.put(masterLevel.getName(), masterLevel.getDescription() + " (" + masterLevel.getName() + ")");
            }
            return sortByValue(formatted);
        }
        return super.getFormattedValues(key, values);
    }

    private static Map<String, String> sortByValue(Map<String, String> formattedMap) {
        List<Entry<String, String>> entryList = new ArrayList<>(formattedMap.entrySet());
        Collections.sort(entryList, sortByValueComparator);
        Map<String, String> sortedMap = new LinkedHashMap<>();
        for (Entry<String, String> entry : entryList) {
            sortedMap.put(entry.getKey(), entry.getValue());
        }
        return sortedMap;
    }

    private static final Comparator<Entry<String, String>> sortByValueComparator = new Comparator<Entry<String, String>>() {

        @Override
        public int compare(Entry<String, String> o1, Entry<String, String> o2) {
            int result = o1.getValue().compareTo(o2.getValue());
            if (result != 0) {
                return result;
            }
            return o1.getKey().compareTo(o2.getKey());
        }

    };

    private static final Comparator<Level> levelComparator = new Comparator<Level>() {
        @Override
        public int compare(Level o1, Level o2) {
            if (o1.isRangeLevel() == o2.isRangeLevel()) {
                int val = Double.compare(o1.getLevelonevalue(), o2.getLevelonevalue());
                if (val == 0) {
                    val = Double.compare(o1.getLeveltwovalue(), o2.getLeveltwovalue());
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
