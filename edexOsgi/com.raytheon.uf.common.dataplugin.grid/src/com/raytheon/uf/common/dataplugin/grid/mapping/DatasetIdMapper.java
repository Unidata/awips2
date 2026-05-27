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
package com.raytheon.uf.common.dataplugin.grid.mapping;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.mapping.AliasList;
import com.raytheon.uf.common.util.mapping.AliasNamespace;
import com.raytheon.uf.common.util.mapping.Mapper;

/**
 * Provide mappings of grid datasetId. The "base" namespace is not formally
 * defined and can be considered as whatever the decoders are storing in the
 * database.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2012            bsteffen     Initial creation
 * May 12, 2016 18984      pwang        Load all levels of datasetid alias files
 * Nov 01, 2018 #7536      dgilling     Better handle overrides.
 *
 * </pre>
 *
 * @author bsteffen
 */

public class DatasetIdMapper extends Mapper {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatasetIdMapper.class);

    private static final DatasetIdMapper instance = new DatasetIdMapper();

    private final SingleTypeJAXBManager<AliasList> jaxb = SingleTypeJAXBManager
            .createWithoutException(AliasList.class);

    public static DatasetIdMapper getInstance() {
        return instance;
    }

    private DatasetIdMapper() {
        Map<String, Map<LocalizationLevel, AliasNamespace>> namespaceMap = new HashMap<>();

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        // read in the namespace map
        // Loading all levels of xml files, rather only lowest level
        LocalizationContext[] contexts = pathMgr
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);
        ILocalizationFile[] files = pathMgr.listFiles(contexts, "grid"
                + PathManager.SEPARATOR + "dataset" + IPathManager.SEPARATOR
                + "alias", new String[] { ".xml" }, true, true);

        for (ILocalizationFile file : files) {
            try {
                addAliasList(namespaceMap, file);
            } catch (IOException | LocalizationException e) {
                statusHandler.error(
                        "Unable to read localization file [" + file + "].", e);
            } catch (SerializationException e) {
                statusHandler.error(
                        "Unable to read AliasList from file [" + file + "].",
                        e);
            }
        }

        List<LocalizationLevel> locLevels = Arrays.stream(contexts)
                .map(LocalizationContext::getLocalizationLevel).sorted()
                .collect(Collectors.toList());
        for (Entry<String, Map<LocalizationLevel, AliasNamespace>> entry : namespaceMap
                .entrySet()) {
            AliasNamespace namespace = null;

            Map<LocalizationLevel, AliasNamespace> levelMap = entry.getValue();
            for (LocalizationLevel level : locLevels) {
                AliasNamespace levelNamespace = levelMap.get(level);
                if (levelNamespace != null) {
                    if (namespace == null) {
                        namespace = levelNamespace;
                    } else {
                        namespace.override(levelNamespace);
                    }
                }
            }

            addAliasNamespace(entry.getKey(), namespace);
        }
    }

    private void addAliasList(
            Map<String, Map<LocalizationLevel, AliasNamespace>> namespaceMap,
            ILocalizationFile file) throws IOException, LocalizationException,
            SerializationException {
        try (InputStream inStream = file.openInputStream()) {
            AliasList list = jaxb.unmarshalFromInputStream(inStream);

            Map<LocalizationLevel, AliasNamespace> levelMap = namespaceMap
                    .get(list.getNamespace());
            if (levelMap == null) {
                levelMap = new HashMap<>();
                levelMap.put(file.getContext().getLocalizationLevel(),
                        new AliasNamespace(list));
            } else {
                AliasNamespace namespace = levelMap
                        .get(file.getContext().getLocalizationLevel());
                if (namespace == null) {
                    namespace = new AliasNamespace(list);
                    levelMap.put(file.getContext().getLocalizationLevel(),
                            namespace);
                } else {
                    namespace.mergeAliasList(list);
                }
            }
            namespaceMap.putIfAbsent(list.getNamespace(), levelMap);
        }
    }
}
