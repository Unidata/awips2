package com.raytheon.viz.pointdata.def;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.IPlotModelFactory;

/**
 *
 * This class reads and writes conditional filters. (It initially reads all the
 * xml files in the ConditionalFilters directory and unmarshals them as
 * ConditionalFilters.)
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2012      #615       S. Gurung   Initial Creation
 * 04/2012      #606       Greg Hull
 * 12/2012      #947       Greg Hull   add pluginName to localization path
 * 12/10/2019   72280      K Sunil     Moved from gov.noaa.nws.ncep.viz.rsc.plotdata here. And
 *                                     changed to extend AbstractConditionalFilterManager
 * Jan 07, 2020 73083      ksunil      SingleTypeJAXBManager is now in the base class.
 *
 * </pre>
 *
 * @author sgurung
 */
public class ConditionalFilterMngr extends AbstractConditionalFilterManager {

    private static final String NCEP_COND_FILTERS_DIR = "ncep"
            + IPathManager.SEPARATOR + "PlotModels" + IPathManager.SEPARATOR
            + IPlotModelFactory.PLOT_FILTERS_DIR;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConditionalFilterMngr.class);

    protected ConditionalFilterMngr() {
    }

    // read in all the xml files in the conditionalFilters directory.
    @Override
    protected synchronized void readConditionalFilters() {
        if (conditionalFilters == null) {
            conditionalFilters = new HashMap<>();

            /*
             * get all of the xml (ConditionalFilter) files in the
             * CONDITIONAL_FILTERS_DIR directory. This will return files from
             * all context levels.
             */
            String[] extensions = new String[] { ".xml" };
            LocalizationContext[] contexts = PathManagerFactory.getPathManager()
                    .getLocalSearchHierarchy(LocalizationType.CAVE_STATIC);
            List<LocalizationFile> condFilterLclFiles = new ArrayList<>();
            condFilterLclFiles.addAll(Arrays.asList(
                    PathManagerFactory.getPathManager().listFiles(contexts,
                            NCEP_COND_FILTERS_DIR, extensions, true, true)));

            /*
             * we are expecting the files to be under a sub-directory that is
             * the name of the plugin. if this is not the case then display a
             * warning msg.
             */
            for (LocalizationFile lFile : condFilterLclFiles) {
                try {

                    ConditionalFilter conditionalFilter = sTypeJAXB
                            .unmarshalFromInputStream(lFile.openInputStream());

                    conditionalFilter.setLocalizationFile(lFile);

                    if (conditionalFilter.getPlugin() == null) {
                        continue;
                    } else if (!lFile.getName().equals(
                            conditionalFilter.createLocalizationFilename())) {
                        /*
                         * This will only cause a problem if the user creates a
                         * USER-level (uses naming convention) and then reverts
                         * back to the base version by deleting the user level
                         * file. The code will look for the base version using
                         * the naming convention and so won't find the file.
                         */
                        statusHandler.warn(
                                "Warning: ConditionalFilter file doesn't follow the naming convention.");
                        statusHandler.warn(lFile.getName() + " should be "
                                + conditionalFilter
                                        .createLocalizationFilename());
                    }

                    conditionalFilters.put(
                            conditionalFilter.getPlugin()
                                    + conditionalFilter.getName(),
                            conditionalFilter);

                } catch (SerializationException | LocalizationException e) {
                    statusHandler.error("Unable to read conditional filters",
                            e);
                }
            }
            if (conditionalFilters.size() == 0) {
                conditionalFilters = null;
            }
        }
    }

    /*
     * Writes a JAXB-based object into the xml file and updates the map.
     */

    @Override
    public void saveConditionalFilter(ConditionalFilter condFilter)
            throws VizException {

        readConditionalFilters();

        if (condFilter == null || condFilter.getSize() == 0
                || condFilter.getName() == null
                || condFilter.getName().isEmpty()) {

            throw new VizException(
                    "saveConditionalFilter: ConditionalFilter is null or doesn't have a name?");
        }
        if (condFilter.getName().equals(NULL_FILTER_NAME)) {
            if (condFilter.getSize() != 0) {
                throw new VizException(
                        "Can't save a non-null filter as " + NULL_FILTER_NAME);
            }
        }

        // create a localization file for the ConditionalFilter
        LocalizationContext userCntxt = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.USER);

        LocalizationFile lFile = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(
                        condFilter.createLocalizationFilename());
        // if the file exists overwrite it.
        if (lFile == null || lFile.getContext()
                .getLocalizationLevel() != LocalizationLevel.USER) {
            lFile = PathManagerFactory.getPathManager().getLocalizationFile(
                    userCntxt, condFilter.createLocalizationFilename());
        }

        condFilter.setLocalizationFile(lFile);

        try (OutputStream os = lFile.openOutputStream()) {
            sTypeJAXB.marshalToStream(condFilter, os);
            lFile.save();

            // update this ConditionalFilter in the map
            conditionalFilters.put(
                    condFilter.getPlugin() + condFilter.getName(), condFilter);
        } catch (LocalizationException | SerializationException
                | IOException e) {
            throw new VizException(e);
        }
    }

}
