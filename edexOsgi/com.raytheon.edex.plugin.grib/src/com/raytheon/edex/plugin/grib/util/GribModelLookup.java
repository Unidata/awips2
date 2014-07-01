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
package com.raytheon.edex.plugin.grib.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXB;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.uf.common.dataplugin.grid.mapping.DatasetIdMapper;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;

/**
 * 
 * Lookup a GridModel based off the information in the grib file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 29, 2013  15715    dhuffman    Near line 202; Transposed edex site and
 *                                    base precedence per DR: loading was in
 *                                    reverse.
 * Apr 30, 2013  1961     bsteffen    Add ability to disable grib tables.
 * Oct 14, 2013  2473     bsteffen    Remove lookup of deprecated grib files.
 * Apr 25, 2014  2874     bsteffen    Add processType
 * Jul 02, 2014  3230     rferrel     Recursively get model files in initModelList.
 * 
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class GribModelLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribModelLookup.class);

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The singleton instance of GribModelLookup **/
    private static GribModelLookup instance;

    /**
     * A map of the models. The key is a hash of the center, grid, and process
     * id rcg: changed key to a string, too many identical key were generated
     * with the hash.
     */
    private final Map<String, GridModel> models;

    /** A map of the models. The key is the title of the model */
    private final Map<String, GridModel> modelsByName;

    public static synchronized GribModelLookup getInstance() {
        if (instance == null) {
            instance = new GribModelLookup();
        }
        return instance;
    }

    private GribModelLookup() {
        models = new HashMap<String, GridModel>();
        modelsByName = new HashMap<String, GridModel>();
        initModelList();
    }

    /**
     * Get a model based off some fields in the grib file.
     * 
     * @param center
     *            the id of the center
     * @param subcenter
     *            the id of the subcenter
     * @param grid
     *            gridcoverage as defined in the grib file(don't use subgrids)
     * @param process
     *            the process id
     * @param processType
     *            the process type, although this is a byte in the grib file
     *            this method expects the byte to be resolved to a string by
     *            looking up the type id in the grib tables(4.3). grib1 does not
     *            include a value for process type so null is acceptable.
     * @return The grid model if one is find or null if no such model is defined
     *         in the localization files..
     */
    public GridModel getModel(int center, int subcenter, GridCoverage grid,
            int process, String processType) {
        /* Match all fields */
        GridModel model = getModelSimple(center, subcenter, grid, process,
                processType);
        if (model == null && processType != null) {
            /* Match with no processType specified */
            model = getModelSimple(center, subcenter, grid, process, null);
        }
        if (model == null && grid != null) {
            /* Match with no grid specified */
            model = getModelSimple(center, subcenter, null, process,
                    processType);
        }
        if (model == null && grid != null && processType != null) {
            /* Match with no grid or processType */
            model = getModelSimple(center, subcenter, null, process, null);
        }
        return model;
    }

    private GridModel getModelSimple(int center, int subcenter,
            GridCoverage grid, int process, String processType) {
        if (grid != null) {
            for (String gribGridName : GribSpatialCache.getInstance()
                    .getGribCoverageNames(grid)) {
                String key = toKey(center, subcenter, gribGridName, process,
                        processType);
                GridModel model = models.get(key);
                if (model != null) {
                    return model;
                }
            }
        }
        return null;
    }

    public GridModel getModelByName(String name) {
        return modelsByName.get(name);
    }

    public Set<String> getModelNames() {
        return modelsByName.keySet();
    }

    public String getModelName(int center, int subcenter, GridCoverage grid,
            int process, String processType) {
        GridModel model = getModel(center, subcenter, grid, process,
                processType);
        if (model == null || model.getName() == null) {
            String cenSubProc = "GribModel:" + String.valueOf(center) + ":"
                    + String.valueOf(subcenter) + ":" + String.valueOf(process);
            String cenSubProcLoc = null;
            DatasetIdMapper mapper = DatasetIdMapper.getInstance();
            try {
                if (grid.getName() != null) {
                    cenSubProcLoc = cenSubProc + ":" + grid.getName();
                    String name = mapper.lookupBaseName(cenSubProcLoc, "grib");
                    if (!name.equals(cenSubProcLoc)) {
                        return name;
                    }
                }
                for (String gribGridName : GribSpatialCache.getInstance()
                        .getGribCoverageNames(grid)) {
                    cenSubProcLoc = cenSubProc + ":" + gribGridName;
                    String name = mapper.lookupBaseName(cenSubProcLoc, "grib");
                    if (!name.equals(cenSubProcLoc)) {
                        return name;
                    }
                }
                String name = mapper.lookupBaseName(cenSubProc, "grib");
                if (!name.equals(cenSubProcLoc)) {
                    return name;
                }
                return cenSubProcLoc;
            } catch (MultipleMappingException e) {
                statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);

                return e.getArbitraryMapping();
            }
        } else {
            return model.getName();
        }
    }

    private void initModelList() {
        logger.info("Initializing grib models");
        long startTime = System.currentTimeMillis();
        LocalizationContext edexStaticBase = PathManagerFactory
                .getPathManager().getContext(
                        LocalizationContext.LocalizationType.EDEX_STATIC,
                        LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext edexStaticSite = PathManagerFactory
                .getPathManager().getContext(
                        LocalizationContext.LocalizationType.EDEX_STATIC,
                        LocalizationContext.LocalizationLevel.SITE);

        LocalizationFile[] modelFiles = PathManagerFactory.getPathManager()
                .listFiles(
                        new LocalizationContext[] { edexStaticBase,
                                edexStaticSite },
                        "grib" + IPathManager.SEPARATOR + "models", // Win32
                        new String[] { ".xml" }, true, true);

        GridModelSet modelSet = new GridModelSet();

        for (LocalizationFile modelFile : modelFiles) {
            try {
                GridModelSet fileSet = JAXB.unmarshal(modelFile.getFile(),
                        GridModelSet.class);
                modelSet.addModels(fileSet.getModels());
            } catch (Exception e) {
                logger.error("Unable to unmarshal grib models file:"
                        + modelFile, e);
            }
        }

        addModels(modelSet);
        long endTime = System.currentTimeMillis();
        logger.info("Grib models initialized: " + (endTime - startTime) + "ms");
    }

    private void addModels(GridModelSet modelSet) {
        for (GridModel model : modelSet.getModels()) {
            int subCenter = Integer.parseInt(model.getSubCenter());
            modelsByName.put(model.getName(), model);
            for (int process : model.getProcess()) {
                if (model.getAllGrids().isEmpty()) {
                    String key = toKey(model.getCenter(), subCenter, null,
                            process, model.getProcessType());
                    models.put(key, model);
                }
                for (String grid : model.getAllGrids()) {
                    String key = toKey(model.getCenter(), subCenter, grid,
                            process, model.getProcessType());
                    models.put(key, model);
                }
            }
        }
    }

    private String toKey(Integer center, Integer subcenter, String grid,
            Integer process, String processType) {
        StringBuilder builder = new StringBuilder();
        builder.append(center);
        builder.append(subcenter);
        builder.append(grid);
        builder.append(process);
        builder.append(processType);
        return builder.toString();

    }
}
