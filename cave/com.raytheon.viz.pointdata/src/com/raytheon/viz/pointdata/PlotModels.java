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
package com.raytheon.viz.pointdata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.viz.pointdata.PlotModelFactory2.PlotModelElement;
import com.raytheon.viz.pointdata.util.PointDataInventory;

/**
 * Plot Model Utility class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2013   2391     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PlotModels {
    /** The only instance */
    private static final PlotModels instance = new PlotModels();

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotModels.class);

    /** Constant */
    private final String PLOTLOCATION = "plotModels";

    /** Models */
    private Map<String, List<String>> models;

    /**
     * Private constructor.
     */
    private PlotModels() {
        populateModels();
    }

    /**
     * Get the instance.
     * 
     * @return The instance
     */
    public static PlotModels getInstance() {
        return instance;
    }

    /**
     * Get the parameters for the provided model.
     * 
     * @param model
     *            The model
     * @return List of parameters
     */
    public List<String> getParameters(String model) {
        return models.get(model);
    }

    /**
     * Get the models
     * 
     * @return The models
     */
    public Map<String, List<String>> getModels() {
        return models;
    }

    /**
     * Populate the models.
     */
    private void populateModels() {
        if (models == null) {
            models = new HashMap<String, List<String>>();
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationFile[] files = pm.listFiles(
                    pm.getLocalSearchHierarchy(LocalizationType.CAVE_STATIC),
                    PLOTLOCATION, new String[] { ".svg" }, true, true);

            MapDescriptor fakeDescriptor = null;
            try {
                fakeDescriptor = new MapDescriptor();
            } catch (VizException e) {
                throw new RuntimeException(e);
            }

            for (LocalizationFile file : files) {
                String fileName = file.getName();
                fileName = fileName.substring(PLOTLOCATION.length() + 1);
                try {
                    if (!models.containsKey(fileName)) {
                        List<String> params = new ArrayList<String>();
                        List<PlotModelElement> fields = new PlotModelFactory2(
                                fakeDescriptor, fileName).getPlotFields();
                        for (PlotModelElement p : fields) {
                            if (!p.parameter.equals("")
                                    && !p.parameter.contains(",")) {
                                params.add(p.parameter);
                            } else if (p.parameter.contains(",")) {
                                String[] individualParams = p.parameter
                                        .split(",");
                                for (String param : individualParams) {
                                    params.add(param);
                                }
                            }
                        }
                        models.put(fileName, params);
                    }
                } catch (Throwable t) {
                    // Ignore as some svg files are fonts and not plot models
                    // and the only way to tell is by catching exceptions thrown
                    // from PlotModelFactory2 when constructed with the non-plot
                    // model svg
                }
            }
        }
    }

    /**
     * Get the available levels.
     * 
     * @param source
     *            The source
     * @param model
     *            The model
     * @return Array of levels
     */
    public String[] getLevels(String source, String model) {
        PlotModels models = PlotModels.getInstance();
        Set<String> possibleLevels = null;
        for (String parameter : models.getParameters(model)) {
            BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
            Collection<String> sourcesToProcess = Arrays.asList(source);
            try {
                getInventory().checkLevels(sourcesToProcess,
                        Arrays.asList(parameter), null, returnQueue);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            if (possibleLevels == null) {
                possibleLevels = new HashSet<String>(returnQueue);
            } else {
                possibleLevels.retainAll(returnQueue);
            }
        }

        List<String> validLevels = new ArrayList<String>();
        if (possibleLevels != null) {
            for (String levelid : possibleLevels) {
                try {
                    Level level = LevelFactory.getInstance().getLevel(
                            Long.parseLong(levelid));
                    validLevels
                            .add(LevelMappingFactory
                                    .getInstance(
                                            LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                                    .getLevelMappingForLevel(level)
                                    .getDisplayName());
                } catch (CommunicationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
        return validLevels.toArray(new String[0]);
    }

    /**
     * Get the PointDataInventory.
     * 
     * @return the PointDataInventory
     */
    public PointDataInventory getInventory() {
        return (PointDataInventory) DataCubeContainer.getInventory("obs");
    }
}
