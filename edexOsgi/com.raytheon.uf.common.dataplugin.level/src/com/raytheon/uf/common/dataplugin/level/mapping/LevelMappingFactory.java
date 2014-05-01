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
package com.raytheon.uf.common.dataplugin.level.mapping;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Factory for getting level mappings
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/16/2009    #3120     rjpeter     Initial version
 * 11/21/2009    #3576     rjpeter     Added group capability
 * 04/17/2013    #1913     randerso    Moved to common
 * 05/16/2013    #2010     randerso    Added read/write locking to mutable maps
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class LevelMappingFactory {
    // TODO: this should move somewhere else
    public static final String VOLUMEBROWSER_LEVEL_MAPPING_FILE = "volumebrowser/LevelMappingFile.xml";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LevelMappingFactory.class);

    private static Map<String, LevelMappingFactory> instanceMap = new HashMap<String, LevelMappingFactory>();

    private Map<String, LevelMapping> keyToLevelMappings = new HashMap<String, LevelMapping>();

    private volatile boolean levelToLevelMappingsInitialized = false;

    private ReentrantReadWriteLock levelToLevelLock = new ReentrantReadWriteLock();

    private Map<Level, LevelMapping> levelToLevelMappings = new HashMap<Level, LevelMapping>();

    private volatile boolean groupToMasterLevelsInitialized = false;

    private ReentrantReadWriteLock groupToMasterLevelsLock = new ReentrantReadWriteLock();

    private Map<String, Map<MasterLevel, Set<Level>>> groupToMasterLevels = new HashMap<String, Map<MasterLevel, Set<Level>>>();

    public synchronized static LevelMappingFactory getInstance(String filePath) {
        LevelMappingFactory instance = instanceMap.get(filePath);
        if (instance == null) {
            instance = new LevelMappingFactory(filePath);
            instanceMap.put(filePath, instance);
        }
        return instance;
    }

    private LevelMappingFactory(String filePath) {
        File path = PathManagerFactory.getPathManager().getStaticFile(filePath);
        LevelMappingFile levelMapFile = null;
        long start = System.currentTimeMillis();
        try {
            levelMapFile = JAXB.unmarshal(path, LevelMappingFile.class);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "An error was encountered while creating the LevelNameMappingFile from "
                            + path.toString(), e);
        }

        List<LevelMapping> levelMappings = levelMapFile.getLevelMappingFile();

        if (levelMappings != null && levelMappings.size() > 0) {
            for (LevelMapping mapping : levelMappings) {
                if (keyToLevelMappings.containsKey(mapping.getKey())) {
                    // handle multiple entries to same key by appending levels
                    LevelMapping priorEntry = keyToLevelMappings.get(mapping
                            .getKey());
                    priorEntry.getDatabaseLevels().addAll(
                            mapping.getDatabaseLevels());
                } else {
                    keyToLevelMappings.put(mapping.getKey(), mapping);
                }
            }

        }

        try {
            initializeLevelToLevelMappings();
            initializeGroupToMasterLevels();
        } catch (CommunicationException e) {
            statusHandler.error("Error initializing LevelMappingFactory for: "
                    + filePath, e);
        }
        long finish = System.currentTimeMillis();
        System.out.println("LevelMappingFactory initialization took ["
                + (finish - start) + "] ms");
    }

    public LevelMapping getLevelMappingForKey(String key) {
        return keyToLevelMappings.get(key);
    }

    public LevelMapping getLevelMappingForLevel(Level level)
            throws CommunicationException {
        if (!levelToLevelMappingsInitialized) {
            initializeLevelToLevelMappings();
        }

        levelToLevelLock.readLock().lock();
        try {
            LevelMapping retVal = levelToLevelMappings.get(level);
            return retVal;
        } finally {
            levelToLevelLock.readLock().unlock();
        }
    }

    public Collection<LevelMapping> getAllLevelMappings() {
        return keyToLevelMappings.values();
    }

    public Set<Level> getAllLevels() throws CommunicationException {
        if (!levelToLevelMappingsInitialized) {
            initializeLevelToLevelMappings();
        }

        levelToLevelLock.readLock().lock();
        try {
            Set<Level> retVal = levelToLevelMappings.keySet();
            return retVal;
        } finally {
            levelToLevelLock.readLock().unlock();
        }
    }

    public Map<MasterLevel, Set<Level>> getLevelMapForGroup(String group)
            throws CommunicationException {
        if (!groupToMasterLevelsInitialized) {
            initializeGroupToMasterLevels();
        }

        groupToMasterLevelsLock.readLock().lock();
        try {
            Map<MasterLevel, Set<Level>> retVal = groupToMasterLevels
                    .get(group);
            return retVal;
        } finally {
            groupToMasterLevelsLock.readLock().unlock();
        }
    }

    private void initializeLevelToLevelMappings() throws CommunicationException {
        // acquire the write lock
        levelToLevelLock.writeLock().lock();
        try {
            // verify some other thread hasn't already initialized
            if (!levelToLevelMappingsInitialized) {
                for (LevelMapping mapping : keyToLevelMappings.values()) {
                    for (Level l : mapping.getLevels()) {
                        // Only replace the old level mapping if we have
                        // less levels than the old mapping
                        // This should cause the most specific mapping to be
                        // used
                        LevelMapping oldMapping = levelToLevelMappings.get(l);
                        if (oldMapping == null
                                || mapping.getLevels().size() < oldMapping
                                        .getLevels().size()) {
                            levelToLevelMappings.put(l, mapping);
                        }
                    }
                }
                levelToLevelMappingsInitialized = true;
            }
        } finally {
            // release the write lock
            levelToLevelLock.writeLock().unlock();
        }
    }

    private void initializeGroupToMasterLevels() throws CommunicationException {
        // acquire the write lock
        groupToMasterLevelsLock.writeLock().lock();
        try {
            // verify some other thread hasn't already initialized
            if (!groupToMasterLevelsInitialized) {
                for (LevelMapping mapping : keyToLevelMappings.values()) {
                    String group = mapping.getGroup();
                    Map<MasterLevel, Set<Level>> masterLevels = null;

                    if (group != null) {
                        masterLevels = groupToMasterLevels.get(mapping
                                .getGroup());
                        if (masterLevels == null) {
                            masterLevels = new HashMap<MasterLevel, Set<Level>>();
                            groupToMasterLevels.put(group, masterLevels);
                        }
                    }

                    for (Level l : mapping.getLevels()) {

                        // populate grouping map
                        if (masterLevels != null) {
                            MasterLevel ml = l.getMasterLevel();
                            Set<Level> levels = masterLevels.get(ml);

                            if (levels == null) {
                                levels = new HashSet<Level>();
                                masterLevels.put(ml, levels);
                            }

                            levels.add(l);
                        }
                    }
                }
                groupToMasterLevelsInitialized = true;
            }
        } finally {
            // release the write lock
            groupToMasterLevelsLock.writeLock().unlock();
        }
    }
}
