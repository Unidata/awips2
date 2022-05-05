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
package com.raytheon.uf.common.grib;

import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;

import javax.xml.bind.JAXB;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.grid.mapping.DatasetIdMapper;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
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
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 29, 2013  15715    dhuffman  Near line 202; Transposed edex site and base
 *                                  precedence per DR: loading was in reverse.
 * Apr 30, 2013  1961     bsteffen  Add ability to disable grib tables.
 * Oct 14, 2013  2473     bsteffen  Remove lookup of deprecated grib files.
 * Apr 25, 2014  2874     bsteffen  Add processType
 * Jul 02, 2014  3230     rferrel   Recursively get model files in
 *                                  initModelList.
 * Jul 30, 2014  3455     bsteffen  Allow model matching with no grid defined.
 * Dec 15, 2015  5166     kbisanz   Update logging to use SLF4J
 * Dec 16, 2015  5182     tjensen   Added functionality for file name regex
 *                                  matching and support for meta characters in
 *                                  model names.
 * Feb 16, 2016  5237     bsteffen  Replace deprecated localization API.
 * Apr 11, 2016  5564     bsteffen  Move localization files to common_static
 * Apr 19, 2016  5572     bsteffen  Move to common
 * Mar 14, 2018  7052     bsteffen  Remove unnecessary catch
 * 
 * </pre>
 * 
 * @author unknown
 */
public class GribModelLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribModelLookup.class);

    /** The logger */
    protected transient Logger logger = LoggerFactory.getLogger(getClass());

    /** The singleton instance of GribModelLookup **/
    private static GribModelLookup instance;

    /**
     * A map of the models. The key is a hash of the center, grid, and process
     * id rcg: changed key to a string, too many identical key were generated
     * with the hash.
     */
    private final Map<String, GridModel> models;

    private final List<GridModel> regexModels;

    private GridModelSet allModels;

    private GribCoverageNameLookup coverageNameLookup;

    public static synchronized GribModelLookup getInstance() {
        if (instance == null) {
            instance = new GribModelLookup();
        }
        return instance;
    }

    public GribModelLookup() {
        models = new HashMap<>();
        regexModels = new ArrayList<>();
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
     * @param filePath
     *            the path to the the grib file.
     * @return The grid model if one is find or null if no such model is defined
     *         in the localization files..
     */
    public GridModel getModel(int center, int subcenter, GridCoverage grid,
            int process, String processType, String filePath) {

        /* Check for regex matching */
        GridModel model = getModelRegex(center, subcenter, grid, process,
                processType, filePath);
        if (model == null) {
            /* Match all fields */
            model = getModelSimple(center, subcenter, grid, process,
                    processType);
        }
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
            for (String gribGridName : getGribCoverageNames(grid)) {
                String key = toKey(center, subcenter, gribGridName, process,
                        processType);
                GridModel model = models.get(key);
                if (model != null) {
                    return model;
                }
            }
        } else {
            String key = toKey(center, subcenter, null, process, processType);
            return models.get(key);
        }
        return null;
    }

    private GridModel getModelRegex(int center, int subcenter,
            GridCoverage grid, int process, String processType,
            String filePath) {
        Path path = Paths.get(filePath);
        String fileName = path.getFileName().toString();
        for (GridModel model : regexModels) {
            Matcher m = model.getNamePattern().matcher(fileName);
            if (m.matches()) {
                /* If pattern matches, compare other fields if available */
                if (!(model.getAllGrids().isEmpty()) && grid != null) {
                    boolean gridMatch = !Collections.disjoint(
                            model.getAllGrids(), getGribCoverageNames(grid));
                    if (!gridMatch) {
                        continue;
                    }
                }
                if (model.getProcess() != null
                        && !model.getProcess().isEmpty()) {
                    boolean pidMatch = false;
                    for (int pid : model.getProcess()) {
                        if (pid == process) {
                            pidMatch = true;
                            break;
                        }
                    }
                    if (!pidMatch) {
                        continue;
                    }
                }
                if (model.getCenter() != null && center != model.getCenter()) {
                    continue;
                }
                if (model.getSubCenter() != null && subcenter != Integer
                        .parseInt(model.getSubCenter())) {
                    continue;
                }
                if (model.getProcessType() != null
                        && !model.getProcessType().equals(processType)) {
                    continue;
                }

                return model;

            }
        }
        return null;
    }

    /**
     * @deprecated the resulting names will contain unprocessed replacement
     *             patterns and are not very useful.
     */
    @Deprecated
    public Set<String> getModelNames() {
        Set<String> names = new HashSet<>(allModels.getModels().size(), 1.0f);
        for (GridModel model : allModels.getModels()) {
            names.add(model.getName());
        }
        return names;
    }

    public GridModelSet getAllModels() {
        return allModels;
    }

    public String getModelName(int center, int subcenter, GridCoverage grid,
            int process, String processType, String filePath) {
        GridModel model = getModel(center, subcenter, grid, process,
                processType, filePath);
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
                for (String gribGridName : getGribCoverageNames(grid)) {
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
            String retval = model.getName();
            retval = model.getTransformedName(grid, filePath,
                    getCoverageNameLookup());
            return retval;
        }
    }

    private void initModelList() {
        logger.info("Initializing grib models");
        long startTime = System.currentTimeMillis();
        LocalizationContext edexStaticBase = PathManagerFactory.getPathManager()
                .getContext(LocalizationContext.LocalizationType.COMMON_STATIC,
                        LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext edexStaticSite = PathManagerFactory.getPathManager()
                .getContext(LocalizationContext.LocalizationType.COMMON_STATIC,
                        LocalizationContext.LocalizationLevel.SITE);

        ILocalizationFile[] modelFiles = PathManagerFactory.getPathManager()
                .listFiles(
                        new LocalizationContext[] { edexStaticBase,
                                edexStaticSite },
                        "grib" + IPathManager.SEPARATOR + "models", // Win32
                        new String[] { ".xml" }, true, true);

        GridModelSet modelSet = new GridModelSet();

        for (ILocalizationFile modelFile : modelFiles) {
            try (InputStream is = modelFile.openInputStream()) {
                GridModelSet fileSet = JAXB.unmarshal(is, GridModelSet.class);
                modelSet.addModels(fileSet.getModels());
            } catch (Exception e) {
                logger.error(
                        "Unable to unmarshal grib models file:" + modelFile, e);
            }
        }

        for (GridModel model : modelSet.getModels()) {

            String validNameError = model.checkValidName();
            if (!("".equals(validNameError))) {
                logger.error(validNameError);
            }

            if (model.getFileNameRegex() != null) {
                regexModels.add(model);
            } else {
                int subCenter = Integer.parseInt(model.getSubCenter());
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
        this.allModels = modelSet;
        long endTime = System.currentTimeMillis();
        logger.info("Grib models initialized: " + (endTime - startTime) + "ms");
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

    public void setCoverageNameLookup(
            GribCoverageNameLookup coverageNameLookup) {
        this.coverageNameLookup = coverageNameLookup;
    }

    private synchronized GribCoverageNameLookup getCoverageNameLookup() {
        if (coverageNameLookup == null) {
            try {
                coverageNameLookup = new GribCoverageStore(
                        PathManagerFactory.getPathManager());
            } catch (Exception e) {
                logger.error("Unable to load coverage store", e);
            }
        }
        return coverageNameLookup;
    }

    private Set<String> getGribCoverageNames(GridCoverage coverage) {
        GribCoverageNameLookup coverageNameLookup = getCoverageNameLookup();
        if (coverageNameLookup == null) {
            if (coverage.getName() != null) {
                return Collections.singleton(coverage.getName());
            } else {
                return Collections.emptySet();
            }
        } else {
            return coverageNameLookup.getGribCoverageNames(coverage);
        }
    }
}
