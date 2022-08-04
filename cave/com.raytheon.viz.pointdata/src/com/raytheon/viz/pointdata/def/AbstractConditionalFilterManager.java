package com.raytheon.viz.pointdata.def;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 *
 * This class was refactored from the original ConditionalFilterMngr. Now
 * contains the common code across the NCP and D2D implementations of
 * Conditional Filters.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/10/2019   72280      K Sunil     Initial Creation
 * Jan 07, 2020 73083      ksunil      Use SingleTypeJAXBManager for reading filters
 * </pre>
 *
 * @author ksunil
 */
public abstract class AbstractConditionalFilterManager
        implements IConditionalFilterMngr {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractConditionalFilterManager.class);

    protected Map<String, ConditionalFilter> conditionalFilters = null;

    protected abstract void readConditionalFilters();

    protected SingleTypeJAXBManager<ConditionalFilter> sTypeJAXB = SingleTypeJAXBManager
            .createWithoutException(ConditionalFilter.class);

    protected AbstractConditionalFilterManager() {
    }

    @Override
    public ArrayList<String> getPlugins() {
        readConditionalFilters();
        ArrayList<String> pluginList = new ArrayList<>();

        for (ConditionalFilter pm : conditionalFilters.values()) {
            if (!pluginList.contains(pm.getPlugin())) {
                pluginList.add(pm.getPlugin());
            }
        }
        return pluginList;
    }

    @Override
    public HashMap<String, ConditionalFilter> getConditionalFiltersByPlugin(
            String plgn) {

        readConditionalFilters();

        HashMap<String, ConditionalFilter> conditionalFiltersByPlugin = new HashMap<>();

        for (ConditionalFilter pm : conditionalFilters.values()) {
            if (plgn == null || plgn.equalsIgnoreCase(pm.getPlugin())) {
                conditionalFiltersByPlugin.put(pm.getName(), pm);
            }
        }

        return conditionalFiltersByPlugin;
    }

    @Override
    public String[] getAllConditionalFiltersByPlugin(String plgn) {

        if (plgn == null || plgn.isEmpty()) {
            return new String[0];
        }

        readConditionalFilters();

        ArrayList<String> cfList = new ArrayList<>();
        for (ConditionalFilter pm : conditionalFilters.values()) {
            if (plgn.equalsIgnoreCase(pm.getPlugin())) {
                cfList.add(pm.getName());
            }
        }

        String[] condFiltersArray = cfList.toArray(new String[0]);

        Arrays.sort(condFiltersArray);

        return condFiltersArray;
    }

    @Override
    public ConditionalFilter getConditionalFilter(String plgn,
            String conditionalFilterName) {
        readConditionalFilters();

        ConditionalFilter conditionalFilter = getConditionalFiltersByPlugin(
                plgn).get(conditionalFilterName);
        return conditionalFilter;
    }

    @Override
    public void deleteConditionalFilter(String pluginName,
            String condFilterName) throws VizException {

        ConditionalFilter delCondFilter = getConditionalFilter(pluginName,
                condFilterName);

        if (delCondFilter == null) {
            throw new VizException("Could not find conditional filter, "
                    + condFilterName + ", for plugin, " + pluginName);
        }

        LocalizationFile lFile = delCondFilter.getLocalizationFile();

        if (lFile == null || !lFile.getFile().exists() || lFile.getContext()
                .getLocalizationLevel() != LocalizationLevel.USER) {
            throw new VizException("File "
                    + delCondFilter.createLocalizationFilename()
                    + " doesn't exist or is not a User Level Conditional Filter.");
        }
        String lFileName = null;
        try {
            lFileName = lFile.getPath();

            lFile.delete();

            conditionalFilters.remove(pluginName + condFilterName);

            lFile = PathManagerFactory.getPathManager()
                    .getStaticLocalizationFile(lFileName);

            /*
             * If there is another file of the same name in the BASE/SITE/DESK
             * then update the conditionalFilters with this version.
             */
            if (lFile != null) {
                if (lFile.getContext()
                        .getLocalizationLevel() == LocalizationLevel.USER) {
                    statusHandler.warn(
                            "Delete Conditional Filter successful. Found another filter with the same name at the same level.");
                    return;

                }
                try (InputStream is = lFile.openInputStream()) {

                    ConditionalFilter conditionalFilter = sTypeJAXB
                            .unmarshalFromInputStream(is);
                    conditionalFilter.setLocalizationFile(lFile);

                    if (conditionalFilter.getPlugin() != null) {
                        conditionalFilters.put(
                                conditionalFilter.getPlugin()
                                        + conditionalFilter.getName(),
                                conditionalFilter);
                    }

                } catch (SerializationException | IOException e) {
                    statusHandler
                            .warn("Delete Conditional Filter successful, but error unmarshalling file: "
                                    + lFileName, e);
                }

            }
        } catch (LocalizationException e) {
            throw new VizException("Error Deleting Conditional Filter: "
                    + condFilterName + ", for plugin: " + pluginName
                    + ", file name: " + lFileName + "\n" + e.getMessage(), e);
        }
    }

    public static ConditionalFilter getDefaultConditionalFilter(String plugin) {
        ConditionalFilter dfltPM = new ConditionalFilter();
        dfltPM.setName(IConditionalFilterMngr.NULL_FILTER_NAME);
        dfltPM.setPlugin(plugin);
        dfltPM.setDescription("");
        dfltPM.getConditionalFilterElements();
        return dfltPM;
    }

}
