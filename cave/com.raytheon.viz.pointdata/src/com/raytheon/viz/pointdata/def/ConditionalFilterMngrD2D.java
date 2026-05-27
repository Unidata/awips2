package com.raytheon.viz.pointdata.def;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.IPlotModelFactory;
import com.raytheon.viz.pointdata.PlotModelFactory;

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
 * 12/10/2019   72281      K Sunil     Initial Creation
 * Jan 07, 2020 73083      ksunil      Minor filter xml parsing changes from code review
 * </pre>
 *
 * @author ksunil
 */
public class ConditionalFilterMngrD2D extends AbstractConditionalFilterManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConditionalFilterMngrD2D.class);

    protected ConditionalFilterMngrD2D() {
    }

    protected synchronized void readConditionalFilters() {
        if (conditionalFilters == null) {
            try {
                Map<String, Set<LocalizationFile>> d2dFilters = PlotModelFactory
                        .getPluginsAndCondFilters();

                conditionalFilters = new HashMap<>();
                for (String pluginNamesInSVG : PlotModelFactory
                        .getPluginsAndModels().keySet()) {

                    if (d2dFilters.containsKey(pluginNamesInSVG)) {
                        for (LocalizationFile lFile : d2dFilters
                                .get(pluginNamesInSVG)) {
                            try (InputStream is = lFile.openInputStream()) {
                                ConditionalFilter conditionalFilter = sTypeJAXB
                                        .unmarshalFromInputStream(is);
                                conditionalFilter.setLocalizationFile(lFile);
                                if (conditionalFilter.getPlugin() == null) {
                                    continue;
                                }
                                conditionalFilters.put(
                                        conditionalFilter.getPlugin()
                                                + conditionalFilter.getName(),
                                        conditionalFilter);
                            } catch (LocalizationException | IOException e) {
                                statusHandler
                                        .error("Unable to read conditional filter: "
                                                + lFile.getPath(), e);
                            }

                        }
                    } else {
                        ConditionalFilter defaultFilter = getDefaultConditionalFilter(
                                pluginNamesInSVG);
                        conditionalFilters.put(
                                defaultFilter.getPlugin()
                                        + defaultFilter.getName(),
                                defaultFilter);
                    }
                    if (conditionalFilters.isEmpty())
                        conditionalFilters = null;

                }
            } catch (SerializationException | VizException e1) {
                statusHandler.error("Unable to read conditional filters", e1);
            }
        }

    }

    /*
     * Writes a JAXB-based object into the xml file and updates the map.
     *
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

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        String path = LocalizationUtil.join(IPlotModelFactory.PLOT_MODEL_DIR,
                IPlotModelFactory.PLOT_FILTERS_DIR,
                condFilter.getName() + ".xml");
        LocalizationFile lFile = PathManagerFactory.getPathManager()
                .getLocalizationFile(context, path);

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
