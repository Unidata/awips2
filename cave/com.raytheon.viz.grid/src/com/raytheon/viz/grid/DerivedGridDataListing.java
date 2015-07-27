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
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.grid.inv.GridInventory;

/**
 * 
 * An extension of the {@link GridDataListing} that queryies the
 * {@link GridInventory} to list all available derived data in addition to data
 * from the database.
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
public class DerivedGridDataListing extends GridDataListing {

    public DerivedGridDataListing() throws ReflectiveOperationException {
        super();
    }

    public DerivedGridDataListing(List<String> keySet) {
        super(keySet);
    }

    public DerivedGridDataListing(Set<String> keySet) {
        super(keySet);
    }

    @Override
    public Collection<String> getValues(String key, Map<String, String> keyVals) throws Exception {
        Collection<String> sources = null;
        Collection<String> params = null;
        Collection<Level> levels = null;
        for (Entry<String, String> queryParam : keyVals.entrySet()) {
            String mapKey = queryParam.getKey();
            String value = queryParam.getValue();
            if (mapKey.equals(GridInventory.MODEL_NAME_QUERY)) {
                sources = Arrays.asList(value);
            } else if (mapKey.equals(GridInventory.PARAMETER_QUERY)) {
                params = Arrays.asList(value);
            } else if (mapKey.equals(GridInventory.MASTER_LEVEL_QUERY)) {
                if (levels == null) {
                    levels = new ArrayList<Level>(LevelFactory.getInstance().getAllLevels());
                }
                Iterator<Level> iter = levels.iterator();
                while (iter.hasNext()) {
                    if (!iter.next().getMasterLevel().getName().equals(value)) {
                        iter.remove();
                    }
                }

            } else if (mapKey.equals(GridInventory.LEVEL_ONE_QUERY)) {
                double doubleValue = Double.parseDouble(value);
                if (levels == null) {
                    levels = new ArrayList<Level>(LevelMappingFactory.getInstance(
                            LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE).getAllLevels());
                }
                Iterator<Level> iter = levels.iterator();
                while (iter.hasNext()) {
                    if (iter.next().getLevelonevalue() != doubleValue) {
                        iter.remove();
                    }
                }
            } else if (mapKey.equals(GridInventory.LEVEL_TWO_QUERY)) {
                double doubleValue = Double.parseDouble(value);
                if (levels == null) {
                    levels = new ArrayList<Level>(LevelMappingFactory.getInstance(
                            LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE).getAllLevels());
                }
                Iterator<Level> iter = levels.iterator();
                while (iter.hasNext()) {
                    if (iter.next().getLeveltwovalue() != doubleValue) {
                        iter.remove();
                    }
                }
            } else if (mapKey.equals(GridInventory.LEVEL_ID_QUERY)) {
                levels = Arrays.asList(LevelFactory.getInstance().getLevel(value));
            }
        }

        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
        GridInventory inventory = (GridInventory) DataCubeContainer.getInventory(GridConstants.GRID);
        if (key.equals(GridInventory.MODEL_NAME_QUERY)) {
            inventory.checkSources(sources, params, levels, returnQueue);
            return returnQueue;
        } else if (key.equals(GridInventory.PARAMETER_QUERY)) {
                inventory.checkParameters(sources, params, levels, false, returnQueue);
            return returnQueue;
        } else if (key.equals(GridInventory.MASTER_LEVEL_QUERY)) {
                inventory.checkLevels(sources, params, levels, returnQueue);
            Set<String> masterlevels = new HashSet<String>();
            LevelFactory lf = LevelFactory.getInstance();
            for (String levelid : returnQueue) {
                Level level = lf.getLevel(levelid);
                masterlevels.add(level.getMasterLevel().getName());
            }
            return masterlevels;
        } else if (key.equals(GridInventory.LEVEL_ID_QUERY)) {
                inventory.checkLevels(sources, params, levels, returnQueue);
            return returnQueue;
        } else {
            return super.getValues(key, keyVals);
        }
    }

}
