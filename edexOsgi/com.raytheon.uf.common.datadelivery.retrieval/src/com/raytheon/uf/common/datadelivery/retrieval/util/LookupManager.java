package com.raytheon.uf.common.datadelivery.retrieval.util;

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

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.retrieval.xml.LevelLookup;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ParameterConfig;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ParameterLookup;
import com.raytheon.uf.common.datadelivery.retrieval.xml.UnitLookup;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.ServiceLoaderUtil;


/**
 * Lookup table manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2011    357      dhladky     Initial creation
 * Oct 27, 2012   1163     dhladky     Improved, dynamically create files, Added Units
 * Jan 18, 2013   1513     dhladky     Level lookup refit.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class LookupManager {

    /**
     * Implementation of the xml writers that writes to localization files.
     */
    private class LocalizationXmlWriter implements LevelXmlWriter,
            ParameterXmlWriter {
        /**
         * {@inheritDoc}
         */
        @Override
        public void writeLevelXml(LevelLookup ll, String modelName)
                throws Exception {

            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext lc = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            String fileName = getLevelFileName(modelName);
            LocalizationFile lf = pm.getLocalizationFile(lc, fileName);
            File file = lf.getFile();

            SerializationUtil.jaxbMarshalToXmlFile(ll, file.getAbsolutePath());
        }

        /**
         * 
         * {@inheritDoc}
         */
        @Override
        public void writeParameterXml(ParameterLookup pl, String modelName)
                throws Exception {

            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext lc = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            String fileName = getParamFileName(modelName);
            LocalizationFile lf = pm.getLocalizationFile(lc, fileName);
            File file = lf.getFile();

            SerializationUtil.jaxbMarshalToXmlFile(pl, file.getAbsolutePath());
        }
    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LookupManager.class);

    /** Singleton instance of this class */
    private static final LookupManager instance = new LookupManager();

    /** Path to Lookup tables. */
    private static final String CONFIG_FILE_ROOT = "datadelivery"
            + File.separatorChar + "lookups" + File.separatorChar;

    private static final String CONFIG_FILE_PARAM = "ParameterLookup.xml";

    private static final String CONFIG_FILE_LEVEL = "LevelLookup.xml";

    private static final String CONFIG_FILE_UNIT = "UnitLookup.xml";

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     * @throws FileNotFoundException
     */
    public static LookupManager getInstance() {
        return instance;
    }

    private final Map<String, ParameterLookup> parameters = new HashMap<String, ParameterLookup>();

    private final Map<String, LevelLookup> levels = new HashMap<String, LevelLookup>();

    private UnitLookup unitLookup = null;

    private final LevelXmlWriter levelXmlWriter = ServiceLoaderUtil.load(
            LevelXmlWriter.class, new LocalizationXmlWriter());

    private final ParameterXmlWriter parameterXmlWriter = ServiceLoaderUtil
            .load(ParameterXmlWriter.class, new LocalizationXmlWriter());

    /* Private Constructor */
    private LookupManager() {

    }

    /**
     * Level file name
     * 
     * @param modelName
     * @return
     */
    private String getLevelFileName(String modelName) {
        return CONFIG_FILE_ROOT + modelName + CONFIG_FILE_LEVEL;
    }

    /**
     * Gets the Model Levels
     * 
     * @param modelName
     * @return
     */
    public LevelLookup getLevels(String modelName) {

        LevelLookup modelLevels = null;

        if (levels.containsKey(modelName)) {
            modelLevels = levels.get(modelName);
        } else {

            modelLevels = getLevelsFromFile(modelName);
            levels.put(modelName, modelLevels);
        }

        return modelLevels;
    }

    private LevelLookup getLevelsFromFile(String modelName) {

        LevelLookup levelLoookup = null;
        LocalizationFile file = null;
        String fileName = getLevelFileName(modelName);

        try {
            file = getLocalizationFile(fileName);
        } catch (Exception e) {
            statusHandler
                    .error(" Failed to Level Lookup table: " + fileName, e);
        }

        if (file != null) {
            try {
                levelLoookup = readLevelXml(file.getFile());
            } catch (Exception e) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Failed to Read Level Lookup from file: "
                                + file.getName(), e);
            }
        }

        return levelLoookup;
    }

    /**
     * Gets the localized file
     * 
     * @param fileName
     * @return
     */
    private LocalizationFile getLocalizationFile(String fileName) {

        LocalizationFile lf = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> files = pm
                .getTieredLocalizationFile(LocalizationType.COMMON_STATIC,
                        fileName);

        if (files.containsKey(LocalizationLevel.SITE)) {
            lf = files.get(LocalizationLevel.SITE);
        } else {
            lf = files.get(LocalizationLevel.BASE);
        }

        return lf;
    }

    /**
     * Gets the Model parameters
     * 
     * @param modelName
     * @return
     */
    public ParameterLookup getParameters(String modelName) {

        ParameterLookup modelParameters = null;

        if (parameters.containsKey(modelName)) {
            modelParameters = parameters.get(modelName);
        } else {
            modelParameters = getParametersFromFile(modelName);
            if (modelParameters != null) {
                parameters.put(modelName, modelParameters);
            }
        }

        return modelParameters;
    }

    private ParameterLookup getParametersFromFile(String modelName) {

        ParameterLookup paramLoookup = null;
        LocalizationFile file = null;
        String fileName = getParamFileName(modelName);

        try {
            file = getLocalizationFile(fileName);
        } catch (Exception e) {
            statusHandler.error(" Failed to Parameter Lookup table: "
                    + fileName, e);
        }

        if (file != null) {
            try {
                paramLoookup = readParameterXml(file.getFile());
            } catch (Exception e) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Failed to Read Parameter Lookup from file: "
                                + file.getName(), e);
            }
        }

        return paramLoookup;
    }

    /**
     * param file name
     * 
     * @param modelName
     * @return
     */
    private String getParamFileName(String modelName) {
        return CONFIG_FILE_ROOT + modelName + CONFIG_FILE_PARAM;
    }

    /**
     * Unit file name
     * 
     * @return
     */
    private String getUnitFileName() {
        return CONFIG_FILE_ROOT + CONFIG_FILE_UNIT;
    }

    /**
     * Gets the Units
     * 
     * @return
     */
    public UnitLookup getUnits() {

        if (unitLookup == null) {
            unitLookup = getUnitsFromFile();
        }

        return unitLookup;
    }

    /**
     * Load units
     * 
     * @return
     */
    private UnitLookup getUnitsFromFile() {

        LocalizationFile file = null;
        String fileName = getUnitFileName();

        try {
            file = getLocalizationFile(fileName);
        } catch (Exception e) {
            statusHandler.error(" Failed to load Unit Lookup table: "
                    + fileName, e);
        }

        if (file != null) {
            try {
                unitLookup = readUnitsXml(file.getFile());
            } catch (Exception e) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Failed to Read Unit Lookup from file: "
                                + file.getName(), e);
            }
        }

        return unitLookup;
    }

    /**
     * Does a a particular lookup exist?
     * 
     * @param modelName
     * @return
     */
    public boolean levelLookupExists(String modelName) {

        LocalizationFile file = null;
        String fileName = getLevelFileName(modelName);

        try {
            file = getLocalizationFile(fileName);
        } catch (Exception fnfe) {
            statusHandler.error(
                    "Failed to lookup Level Lookup localization file: "
                            + fileName, fnfe);
        }

        if (file != null) {
            return file.exists();
        }

        return false;
    }

    /**
     * Modify level lookups
     * 
     * @param modelName
     *            - name of model collection
     * @param dz
     *            - delta z (change in level height value from one step to next)
     * @param min
     *            - minimum level height value
     * @param max
     *            - maximum level height value
     * @throws Exception
     */
    public void modifyLevelLookups(String modelName, double dz, float min,
            float max, List<Double> levs) throws Exception {

        LevelLookup ll = null;

        if (levelLookupExists(modelName)) {
            ll = getLevelsFromFile(modelName);
            if (ll.getLevelXml() != null) {
                levs = ll.getLevelXml();
            }
        }

        if (ll == null) {
            ll = new LevelLookup();
        }

        boolean gen = false;

        
        if (CollectionUtil.isNullOrEmpty(levs)) {
            ll.setLevelXml(new ArrayList<Double>());
            levs = ll.getLevelXml();
            gen = true;
        } else {
            ll.setLevelXml(levs);
        }

        if (gen) {
            int diff = (int) (max - min);
            int total = (int) Math.abs((diff / dz));
            // These add simple place holder level
            // identifiers. It is up to the admin
            // to add the real values
            if (diff < 0) {
                for (int i = 0; i <= total; i++) {
                    double lev = max + i * dz;
                    levs.add(lev);
                }
            } else {
                for (int i = 0; i <= total; i++) {
                    double lev = min + i * dz;
                    levs.add(lev);
                }
            }
        }

        levelXmlWriter.writeLevelXml(ll, modelName);

        levels.put(modelName, ll);
        statusHandler.info("Updated/Created level lookup! " + modelName);

    }

    /**
     * Modify or create a parameter lookup
     * 
     * @param modelName
     */
    public void modifyParamLookups(String modelName, List<Parameter> newParams) {

        try {
            ParameterLookup pl = null;
            List<ParameterConfig> params = null;

            if (paramLookupExists(modelName)) {
                pl = getParametersFromFile(modelName);
                if (pl.getParameters() != null) {
                    params = pl.getParameters();
                }
            }

            if (pl == null) {
                pl = new ParameterLookup();
            }

            if (params == null) {
                pl.setParameters(new ArrayList<ParameterConfig>());
                params = pl.getParameters();
            }

            for (Parameter param : newParams) {
                ParameterConfig pc = new ParameterConfig();
                pc.setAwips(param.getName());
                pc.setGrads(param.getProviderName());
                params.add(pc);
            }
            // write out file
            parameterXmlWriter.writeParameterXml(pl, modelName);

            parameters.put(modelName, pl);
            statusHandler
                    .info("Updated/Created parameter lookup! " + modelName);

        } catch (Exception e) {
            statusHandler.error("Couldn't update parameter lookup! ", e);
        }
    }

    /**
     * Does a a particular lookup exist?
     * 
     * @param modelName
     * @return
     */
    public boolean paramLookupExists(String modelName) {

        LocalizationFile file = null;
        String fileName = getParamFileName(modelName);
        try {
            file = getLocalizationFile(fileName);
        } catch (Exception fnfe) {
            statusHandler.error(
                    "Failed to lookup Parameter Lookup localization file: "
                            + fileName, fnfe);
        }
        if (file != null) {
            return file.exists();
        }

        return false;
    }

    /**
     * Read levels out of file
     * 
     * @param file
     * @return
     * @throws Exception
     */
    private LevelLookup readLevelXml(File file) throws Exception {

        LevelLookup levelXml = null;

        if (file != null && file.exists()) {
            levelXml = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    LevelLookup.class, file);
        }

        return levelXml;
    }

    /**
     * Read parameter lookup
     * 
     * @param file
     * @return
     * @throws Exception
     */
    private ParameterLookup readParameterXml(File file) throws Exception {

        ParameterLookup paramXml = null;

        if (file != null && file.exists()) {
            paramXml = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    ParameterLookup.class, file);
        }

        return paramXml;
    }

    /**
     * Read unit lookups
     * 
     * @param file
     * @return
     * @throws Exception
     */
    private UnitLookup readUnitsXml(File file) throws Exception {

        UnitLookup unitXml = null;

        if (file != null && file.exists()) {
            unitXml = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    UnitLookup.class, file);
        }

        return unitXml;
    }
    
    
}
