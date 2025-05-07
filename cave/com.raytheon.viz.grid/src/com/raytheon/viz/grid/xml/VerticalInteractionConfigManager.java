/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.grid.xml;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Singleton manager of configuration for the Vertical Interaction capability of
 * grid resources.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 06, 2024 2036517    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class VerticalInteractionConfigManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VerticalInteractionConfigManager.class);

    /** Path to configuration file within localization */
    public static final String LOC_PATH = "levels" + IPathManager.SEPARATOR
            + "VerticalInteractionLevels.xml";

    /**
     * Initialization-on-demand holder to prevent instantiation until
     * getInstance() is actually called.
     */
    private static class LazyHolder {
        private static final VerticalInteractionConfigManager instance = new VerticalInteractionConfigManager();
    }

    private final SingleTypeJAXBManager<VerticalInteractionLevels> jaxb = SingleTypeJAXBManager
            .createWithoutException(VerticalInteractionLevels.class);

    private final IPathManager pathMgr = PathManagerFactory.getPathManager();

    /** Master level name -> level groups. Access must be synchronized. */
    protected final Map<String, List<VerticalInteractionLevelGroup>> masterLevelGroups = new HashMap<>();

    /**
     * Private constructor since this is a singleton.
     */
    private VerticalInteractionConfigManager() {
        pathMgr.addLocalizationPathObserver(LOC_PATH, file -> {
            loadConfig();
        });
        loadConfig();
    }

    /**
     * @return config manager instance for the Vertical Interaction capability
     *         of grid resources
     */
    public static VerticalInteractionConfigManager getInstance() {
        return LazyHolder.instance;
    }

    /**
     * @param masterLevel
     * @return the configured Vertical Interaction level groups for the
     *         specified master level, or an empty list if master level is null
     *         or has no level groups configured (guaranteed non-null)
     */
    public List<VerticalInteractionLevelGroup> getLevelGroups(
            String masterLevel) {
        synchronized (masterLevelGroups) {
            return Collections.unmodifiableList(
                    masterLevelGroups.getOrDefault(masterLevel, List.of()));
        }
    }

    /**
     * Re-populate {@link #masterLevelGroups} from the localization file.
     */
    private void loadConfig() {
        Map<LocalizationLevel, LocalizationFile> fileMap = pathMgr
                .getTieredLocalizationFile(LocalizationType.CAVE_STATIC,
                        LOC_PATH);

        List<LocalizationLevel> keyList = new ArrayList<>(fileMap.keySet());
        Collections.sort(keyList);
        VerticalInteractionLevels levels = null;
        for (LocalizationLevel key : keyList) {
            ILocalizationFile lf = fileMap.get(key);
            try (InputStream is = lf.openInputStream()) {
                levels = jaxb.unmarshalFromInputStream(is);
            } catch (IOException | LocalizationException
                    | SerializationException e) {
                statusHandler.error(
                        "Error loading Vertical Interaction level groups from localization file: "
                                + lf,
                        e);
            }
        }

        loadConfig(levels);
    }

    /**
     * Re-populate {@link #masterLevelGroups} from the unmarshalled levels.
     *
     * @param levels
     *            unmarshalled levels
     */
    protected void loadConfig(VerticalInteractionLevels levels) {
        if (levels == null) {
            return;
        }
        synchronized (masterLevelGroups) {
            masterLevelGroups.clear();
            for (VerticalInteractionLevelGroup group : levels.getGroups()) {
                if (group.getLevels().isBlank()) {
                    continue;
                }

                List<VerticalInteractionLevelGroup> groups = masterLevelGroups
                        .computeIfAbsent(group.getMasterLevel(),
                                ml -> new ArrayList<>());
                groups.add(group);
            }
        }
    }
}
