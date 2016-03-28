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
package com.raytheon.uf.viz.alertviz;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.ForcedConfiguration;
import com.raytheon.uf.viz.alertviz.config.Source;

/**
 * Class for managing configurations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 07, 2010            mschenke    Initial creation
 * Mar 16, 2011 6531       rferrel     Start up loads host dependent
 *                                     configuration file.
 * Aug 28, 2012 13528      Xiaochuan   Using setNewConfiguration() to 
 *                                     re-set configuration data and
 *                                     run notifyListeners().
 * Apr 07, 2015 4346       rferrel     Created {@link #retrieveBaseConfiguration}.
 * May 20, 2015 4346       rjpeter     Updated to also load from common_static.
 * Nov 12, 2015 4834       njensen     Changed LocalizationOpFailedException to LocalizationException
 * Jan 11, 2016 5242       kbisanz     Replaced calls to deprecated ILocalizationFile methods
 * Feb 12, 2016 4834       bsteffen    Fix multiple saves of the customConfiguration.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class ConfigurationManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConfigurationManager.class, "GDN_ADMIN", "GDN_ADMIN");

    public static final ConfigContext DEFAULT_BASE_CONFIG = new ConfigContext(
            ConfigContext.DEFAULT_NAME, LocalizationLevel.BASE);

    public static final ConfigContext DEFAULT_SITE_CONFIG = new ConfigContext(
            ConfigContext.DEFAULT_NAME, LocalizationLevel.SITE);

    public static final ConfigContext DEFAULT_WORKSTATION_CONFIG = new ConfigContext(
            ConfigContext.DEFAULT_NAME, LocalizationLevel.WORKSTATION);

    private static final String CONFIG_DIR = ConfigContext.ALERTVIZ_DIR
            + IPathManager.SEPARATOR + ConfigContext.DEFAULT_SUBDIR;

    private static final String[] EXTENSIONS = { ConfigContext.XML_EXT };

    private static ConfigurationManager instance = new ConfigurationManager();

    private JAXBContext context;

    private Marshaller marshaller;

    private Unmarshaller unmarshaller;

    private ConfigContext current;

    /** The config map */
    private final Map<ConfigContext, Configuration> configurationMap;

    /** Listeners get notified when configuration changes */
    private final Set<IConfigurationChangedListener> listeners;

    private LocalizationFile customLocalization;

    private Configuration customConfiguration;

    private Configuration baseConfiguration;

    /*
     * TODO The CustomRepository file for Site Categories and Sources is at the
     * SITE level. All users at a site should be able to add or remove
     * Categories or Sources from this file. If the access level for SITE is
     * changed to remove these permissions, then this file access will need to
     * be changed.
     */
    private final ConfigContext customContext = new ConfigContext(
            "CustomRepository", "customizations", LocalizationLevel.SITE);

    private boolean reloadCustomConfiguration;

    /** Get the configuration manager */
    public static ConfigurationManager getInstance() {
        return instance;
    }

    /** Initialize manager and load preferences */
    private ConfigurationManager() {
        // load all configurations
        configurationMap = new HashMap<ConfigContext, Configuration>();
        listeners = new HashSet<IConfigurationChangedListener>();
        current = null;
        try {
            context = JAXBContext.newInstance(Configuration.class,
                    ForcedConfiguration.class, Category.class, Source.class);

            marshaller = context.createMarshaller();
            unmarshaller = context.createUnmarshaller();
        } catch (Exception e) {
            Container
                    .logInternal(
                            Priority.ERROR,
                            "ConfigurationManager: exception creating "
                                    + "marshalling/unmarshalling objects for Configuration, "
                                    + "ForcedConfiguration, Category, and Source.",
                            e);
            context = null;
            marshaller = null;
            unmarshaller = null;
        }
        loadFromLocalization();
        baseConfiguration = retrieveBaseConfiguration();
    }

    /**
     * Get default WORKSTATION configuration file for host machine. When it does
     * not exist create from Default SITE or BASE.
     * 
     * @return hostContext
     */
    private ConfigContext getDefaultHostContext() {
        ConfigContext workstationContext = DEFAULT_WORKSTATION_CONFIG;

        try {
            ILocalizationFile file = getLocalizationFile(workstationContext);

            if (file == null || !file.exists()) {
                ConfigContext sourceContext = DEFAULT_SITE_CONFIG;
                file = getLocalizationFile(sourceContext);

                if (file == null || !file.exists()) {
                    sourceContext = DEFAULT_BASE_CONFIG;
                }
                Configuration config = retrieveConfiguration(sourceContext);
                saveToFile(workstationContext, config);
            }
        } catch (NullPointerException ex) {
            statusHandler.handle(Priority.CRITICAL,
                    "Unable to load configuration context " + context);
        }
        return workstationContext;
    }

    public ConfigContext[] getConfigurations() {
        loadFromLocalization();
        Set<ConfigContext> keys = configurationMap.keySet();
        return keys.toArray(new ConfigContext[keys.size()]);
    }

    /**
     * Returns a clone of the currently loaded configuration
     * 
     * @return
     */
    public Configuration getCurrentConfiguration() {
        if (current == null) {
            current = getDefaultHostContext();
        }
        Configuration currentConfig = configurationMap.get(current);
        if (currentConfig == null) {
            currentConfig = retrieveConfiguration(current);
        }
        if (currentConfig != null) {
            Configuration custom = getCustomConfiguration();

            if (custom != null) {
                if (baseConfiguration == null) {
                    statusHandler.error("The base configuration "
                            + DEFAULT_BASE_CONFIG.getLocalizationFileName()
                            + " was not loaded.  Check your configuration.");
                } else {
                    /*
                     * merge custom over base then overlay the current config on
                     * that result. preserve locking from the base
                     * configuration.
                     */
                    Configuration baseCustom = baseConfiguration.mergeUnder(
                            custom, true);
                    currentConfig = baseCustom.overlayWith(currentConfig, true);
                }
            }
            configurationMap.put(current, currentConfig);
        } else if (DEFAULT_BASE_CONFIG.equals(current) == false) {
            current = DEFAULT_BASE_CONFIG;
            return getCurrentConfiguration();
        }
        return currentConfig == null ? currentConfig : currentConfig.clone();
    }

    /**
     * Save the Configuration as the current configuration
     * 
     * @param toSave
     */
    public boolean saveCurrentConfiguration(ConfigContext context,
            Configuration toSave) {

        if (saveToFile(context, toSave)) {
            setNewConfiguration(context, toSave);

            return true;
        }
        return false;
    }

    /**
     * @param context
     * @param configData
     */
    public void setNewConfiguration(ConfigContext context,
            Configuration configData) {
        configurationMap.put(context, configData);
        current = context;
        notifyListeners();

    }

    /**
     * Delete the configuration passed in
     * 
     * @param context
     */
    public void deleteConfiguration(ConfigContext context) {
        if (context.isBaseOrConfiguredLevel() || isDefaultConfig(context)) {
            return;
        }
        if (configurationMap.containsKey(context)) {
            if (current.equals(context)) {
                loadAsCurrent(DEFAULT_WORKSTATION_CONFIG);
            }
            ILocalizationFile file = getLocalizationFile(context);
            try {
                file.delete();
            } catch (Exception e) {
                statusHandler.handle(Priority.CRITICAL,
                        "Error deleting configuration", e);
            }
            configurationMap.remove(context);
        }
    }

    /**
     * Set the configuration to be the currently used configuration
     * 
     * @param name
     */
    public void loadAsCurrent(ConfigContext name) {
        current = name;
        notifyListeners();
    }

    /**
     * Add a listener, will get notified when current configuration changes
     * 
     * @param listener
     */
    public void addListener(IConfigurationChangedListener listener) {
        listeners.add(listener);
    }

    public void removeListener(IConfigurationChangedListener listener) {
        listeners.remove(listener);
    }

    /**
     * Get files to display in the dialog.
     */
    private void loadFromLocalization() {
        loadFromLocalizationType(LocalizationType.CAVE_STATIC);
        loadFromLocalizationType(LocalizationType.COMMON_STATIC);
    }

    private void loadFromLocalizationType(LocalizationType type) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext[] contexts = pm.getLocalSearchHierarchy(type);
        ILocalizationFile[] files = pm.listFiles(contexts, CONFIG_DIR,
                EXTENSIONS, true, true); // Win32

        for (ILocalizationFile file : files) {
            LocalizationContext fileContext = file.getContext();

            /*
             * Do not display BASE xml files that get merged into the
             * baseConfiguration.
             */
            if ((fileContext.getLocalizationLevel() != LocalizationLevel.BASE)
                    || ((fileContext.getLocalizationLevel() == LocalizationLevel.BASE) && DEFAULT_BASE_CONFIG
                            .getLocalizationFileName().equals(file.getPath()))) {
                String fileName = file.getPath();
                LocalizationContext locContext = file.getContext();
                String name = fileName.substring(
                        fileName.lastIndexOf(IPathManager.SEPARATOR) + 1, // win32
                        fileName.lastIndexOf("."));
                ConfigContext context = new ConfigContext(name, locContext);
                configurationMap.put(context, null);
            }
        }
    }

    public LocalizationFile getCustomLocalization() {
        if (customLocalization == null) {
            customLocalization = getLocalizationFile(customContext);
            if (!customLocalization.exists()) {
                saveToFile(customContext, new Configuration());
            }
        }
        return customLocalization;
    }

    private Configuration getCustomConfiguration() {
        if (customConfiguration == null || reloadCustomConfiguration) {
            reloadCustomConfiguration = false;
            // this retrieve of the custom localization file needs to be here.
            getCustomLocalization();
            customConfiguration = retrieveConfiguration(customContext);
        }
        return customConfiguration;
    }

    public Configuration retrieveConfiguration(ConfigContext configContext) {
        ILocalizationFile file = getLocalizationFile(configContext);
        if (DEFAULT_BASE_CONFIG.equals(configContext)) {
            if (baseConfiguration == null) {
                baseConfiguration = retrieveBaseConfiguration();
            }
            return baseConfiguration;
        }

        return retrieveConfiguration(file);
    }

    /**
     * Get base Default.xml and merge with any other base xml files.
     * 
     * @return configuration
     */
    private Configuration retrieveBaseConfiguration() {
        ILocalizationFile file = getLocalizationFile(DEFAULT_BASE_CONFIG);
        Configuration configuration = retrieveConfiguration(file);
        configuration = mergeBaseConfigurations(LocalizationType.CAVE_STATIC,
                configuration);
        configuration = mergeBaseConfigurations(LocalizationType.COMMON_STATIC,
                configuration);
        return configuration;
    }

    private Configuration mergeBaseConfigurations(LocalizationType type,
            Configuration configuration) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(type,
                LocalizationLevel.BASE);
        ILocalizationFile[] files = pm.listFiles(context, CONFIG_DIR,
                EXTENSIONS, false, true);
        for (ILocalizationFile f : files) {
            // Merge other base files with the default.
            if (!DEFAULT_BASE_CONFIG.getLocalizationFileName().equals(
                    f.getPath())) {
                Configuration fileConfig = retrieveConfiguration(f);
                Configuration mergeConfig = configuration.mergeUnder(
                        fileConfig, true);
                configuration = mergeConfig.overlayWith(configuration, true);
            }
        }

        return configuration;
    }

    public Configuration retrieveConfiguration(ILocalizationFile file) {
        Configuration config = null;
        if (file != null && file.exists()) {
            try {
                config = (Configuration) deserializeFromFile(file);
            } catch (SerializationException e) {
                statusHandler.handle(Priority.CRITICAL,
                        "Error deserializing configuration xml", e);
            }
        }
        return config;
    }

    private boolean saveToFile(ConfigContext cContext, Configuration config) {
        boolean success = true;
        ILocalizationFile file = getLocalizationFile(cContext);
        try {
            // do not attempt to save to base
            if (file != null
                    && file.getContext().getLocalizationLevel() != LocalizationLevel.BASE) {
                serializeToFile(config, file);
            }
        } catch (SerializationException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error serializing configuration to xml", e);
            success = false;
        } catch (Exception e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error saving configuration", e);
            success = false;
        }
        return success;
    }

    public LocalizationFile getLocalizationFile(ConfigContext config) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.CAVE_STATIC,
                config.getLevel());

        LocalizationFile locFile = pm.getLocalizationFile(ctx,
                config.getLocalizationFileName());
        return locFile;
    }

    private void notifyListeners() {
        for (IConfigurationChangedListener listener : listeners) {
            listener.configurationChanged();
        }
    }

    private void serializeToFile(Object obj, ILocalizationFile file)
            throws SerializationException {
        try (SaveableOutputStream os = file.openOutputStream()) {
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
                    new Boolean(true));
            marshaller.marshal(obj, os);
            os.save();
        } catch (Exception e) {
            throw new SerializationException(e);
        }
    }

    private Object deserializeFromFile(ILocalizationFile file)
            throws SerializationException {
        try (InputStream is = file.openInputStream()) {
            return unmarshaller.unmarshal(is);
        } catch (LocalizationException | IOException | JAXBException e) {
            Container.logInternal(Priority.ERROR,
                    "ConfigurationManager: Exception unmarshalling from file: "
                            + file.getPath(), e);
            throw new SerializationException(e);
        }
    }

    public ForcedConfiguration getForcedConfiguration() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.SITE);
        ILocalizationFile file = pm.getLocalizationFile(ctx,
                ConfigContext.ALERTVIZ_DIR + IPathManager.SEPARATOR
                        + "AlertVizForced.xml");
        if (file != null && file.exists()) {
            try (InputStream is = file.openInputStream()) {
                return (ForcedConfiguration) unmarshaller.unmarshal(is);
            } catch (Exception e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error deserializing forced configuration", e);
            }
        }
        return null;
    }

    public ConfigContext getCurrentContext() {
        return current;
    }

    public void addToCustomization(Category cat) {
        updateCustom(false, cat);
    }

    public void removeFromCustomization(Category cat) {
        updateCustom(true, cat);
    }

    public void addToCustomization(Source source) {
        updateCustom(false, source);
    }

    public void removeFromCustomization(Source source) {
        updateCustom(true, source);
    }

    private void updateCustom(boolean isRemove, Object obj) {
        ILocalizationFile locFile = getCustomLocalization();
        try {
            customConfiguration = (Configuration) deserializeFromFile(locFile);
            if (obj instanceof Category) {
                Map<String, Category> categories = customConfiguration
                        .getCategories();
                Category cat = (Category) obj;
                if (isRemove) {
                    customConfiguration.getCategories().remove(
                            cat.getCategoryName());
                } else {
                    categories.put(cat.getCategoryName(), cat);
                }
                customConfiguration.setCategories(categories);
            } else if (obj instanceof Source) {
                Map<String, Source> sources = customConfiguration.getSources();
                Source source = (Source) obj;
                if (isRemove) {
                    sources.remove(source.getName());
                } else {
                    sources.put(source.getName(), source);
                }
                customConfiguration.setSources(sources);
            }
            serializeToFile(customConfiguration, locFile);
            /* Must reload fresh copy next time it is used. */
            customLocalization = null;
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    public void resetCustomLocalization() {
        reloadCustomConfiguration = true;
    }

    public static boolean isDefaultConfig(ConfigContext context) {
        return DEFAULT_WORKSTATION_CONFIG.equals(context)
                || DEFAULT_SITE_CONFIG.equals(context)
                || DEFAULT_BASE_CONFIG.equals(context);
    }
}
