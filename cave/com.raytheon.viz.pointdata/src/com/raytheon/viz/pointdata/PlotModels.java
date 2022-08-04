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

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.pointdata.util.PointDataInventory;

/**
 * Plot Model Utility class.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 18, 2013  2391     mpduff      Initial creation
 * Jun 06, 2014  2061     bsteffen    Remove old PlotResource
 * Sep 09, 2014  3356     njensen     Remove CommunicationException
 * Aug 17, 2015  4717     mapeters    Added null check in getLevels()
 * Feb 12, 2016  5242     dgilling    Remove calls to deprecated Localization APIs.
 * Nov 10, 2019  71272    ksunil      Tweaks to support new plot customization code
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class PlotModels {
    /** The only instance */
    private static final PlotModels instance = new PlotModels();

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
            models = new HashMap<>();
            IPathManager pm = PathManagerFactory.getPathManager();
            ILocalizationFile[] files = pm.listFiles(
                    pm.getLocalSearchHierarchy(LocalizationType.CAVE_STATIC),
                    PLOTLOCATION, new String[] { ".svg" }, true, true);

            MapDescriptor fakeDescriptor = null;
            try {
                fakeDescriptor = new MapDescriptor();
            } catch (VizException e) {
                throw new RuntimeException(e);
            }

            for (ILocalizationFile file : files) {
                String fileName = file.getPath();
                fileName = fileName.substring(PLOTLOCATION.length() + 1);
                try {
                    if (!models.containsKey(fileName)) {
                        List<String> params = new ArrayList<>();
                        IPlotModelFactory factory = null;
                        if (PlotModelFactory.isNewSVGFormat(fileName)) {
                            factory = new PlotModelFactory(fakeDescriptor,
                                    fileName);
                        } else {
                            factory = new PlotModelFactoryDefault(
                                    fakeDescriptor, fileName);
                        }

                        List<IPlotModelElement> fields = factory
                                .getPlotFields();
                        for (IPlotModelElement p : fields) {
                            String param = p.getParamDef().getParamName();
                            if (!param.isEmpty() && !param.contains(",")) {
                                params.add(param);
                            } else if (param.contains(",")) {
                                String[] individualParams = param.split(",");
                                for (String individualParam : individualParams) {
                                    params.add(individualParam);
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
            BlockingQueue<String> returnQueue = new LinkedBlockingQueue<>();
            Collection<String> sourcesToProcess = Arrays.asList(source);
            try {
                getInventory().checkLevels(sourcesToProcess,
                        Arrays.asList(parameter), null, returnQueue);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            if (possibleLevels == null) {
                possibleLevels = new HashSet<>(returnQueue);
            } else {
                possibleLevels.retainAll(returnQueue);
            }
        }

        List<String> validLevels = new ArrayList<>();
        if (possibleLevels != null) {
            for (String levelid : possibleLevels) {
                Level level = LevelFactory.getInstance()
                        .getLevel(Long.parseLong(levelid));
                LevelMapping mapping = LevelMappingFactory
                        .getInstance(
                                LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                        .getLevelMappingForLevel(level);
                if (mapping != null) {
                    validLevels.add(mapping.getDisplayName());
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
