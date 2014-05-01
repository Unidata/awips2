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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
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
 * Apr 7, 2010            mschenke     Initial creation
 * Mar 16, 2011 6531       rferrel     Start up loads host dependent
 *                                     configuration file.
 * Aug 28 2012  13528	  Xiaochuan	   Using setNewConfiguration() to 
 * 									   re-set configuration data and
 * 									   run notifyListeners().  	 	  	                                  
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
            "Default", LocalizationLevel.BASE);

    public static final ConfigContext DEFAULT_SITE_CONFIG = new ConfigContext(
            "Default", LocalizationLevel.SITE);

    public static final ConfigContext DEFAULT_WORKSTATION_CONFIG = new ConfigContext(
            "Default", LocalizationLevel.WORKSTATION);

    private static ConfigurationManager instance = new ConfigurationManager();

    private JAXBContext context;

    private Marshaller marshaller;

    private Unmarshaller unmarshaller;

    private ConfigContext current;

    /** The config map */
    private Map<ConfigContext, Configuration> configurationMap;

    /** Listeners get notified when configuration changes */
    private Set<IConfigurationChangedListener> listeners;

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
    private ConfigContext customContext = new ConfigContext("CustomRepository",
            "customizations", LocalizationLevel.SITE);

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
        baseConfiguration = retrieveConfiguration(DEFAULT_BASE_CONFIG);
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
            LocalizationFile file = getLocalizationFile(workstationContext);

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
            	}
            	else {
            		/*
            		 * merge custom over base then overlay the current config on
            		 * that result. preserve locking from the base configuration.
            		 */
            		Configuration baseCustom = baseConfiguration.mergeUnder(custom,
            				true);
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
     * @param name
     */
    public void deleteConfiguration(ConfigContext context) {
        if (context.isBaseOrConfiguredLevel() || isDefaultConfig(context)) {
            return;
        }
        if (configurationMap.containsKey(context)) {
            if (current.equals(context)) {
                loadAsCurrent(DEFAULT_WORKSTATION_CONFIG);
            }
            LocalizationFile file = getLocalizationFile(context);
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

    private void loadFromLocalization() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext[] contexts = pm
                .getLocalSearchHierarchy(LocalizationType.CAVE_STATIC);
        String[] extensions = { "xml" };
        LocalizationFile[] files = pm.listFiles(contexts, "alertViz" + "/"
                + "configurations", extensions, true, true); // Win32
        for (LocalizationFile file : files) {
            String fileName = file.getName();
            LocalizationContext locContext = file.getContext();
            String name = fileName.substring(
fileName.lastIndexOf("/") + 1, // win32
                    fileName.lastIndexOf("."));
            ConfigContext context = new ConfigContext(name, locContext);
            configurationMap.put(context, null);
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
        LocalizationFile file = getLocalizationFile(configContext);
        return retrieveConfiguration(file);
    }

    public Configuration retrieveConfiguration(LocalizationFile file) {
        Configuration config = null;
        if (file != null) {
            try {
                File actualFile = file.getFile();
                if (actualFile != null && actualFile.exists()) {
                    config = (Configuration) deserializeFromFile(actualFile);
                }
            } catch (SerializationException e) {
                statusHandler.handle(Priority.CRITICAL,
                        "Error deserializing configuration xml", e);
            }
        }
        return config;
    }

    private boolean saveToFile(ConfigContext cContext, Configuration config) {
        boolean success = true;
        LocalizationFile file = getLocalizationFile(cContext);
        try {
            // do not attempt to save to base
            if (file != null
                    && file.getContext().getLocalizationLevel() != LocalizationLevel.BASE) {
                serializeToFile(config, file.getFile());
                if (!file.save()) {
                    // failed to save delete the local copy and switch to the
                    // default config.
                    file.delete();
                    success = false;
                }
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

    private void serializeToFile(Object obj, File file)
            throws SerializationException {
        FileWriter writer = null;
        Marshaller msh = marshaller;
        try {
            File dir = file.getParentFile();
            if (dir.isDirectory() == false) {
                dir.delete();
                dir.mkdirs();
            }
            msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, new Boolean(true));
            writer = new FileWriter(file);
            msh.marshal(obj, writer);
        } catch (Exception e) {
            throw new SerializationException(e);
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    // Log to internal Log4j log
                    Container
                            .logInternal(
                                    Priority.ERROR,
                                    "ConfigurationManager: exception closing file writer.",
                                    e);
                }
            }
        }
    }

    private Object deserializeFromFile(File file) throws SerializationException {
        FileReader reader = null;
        Unmarshaller msh = unmarshaller;
        try {
            reader = new FileReader(file);
            Object obj = msh.unmarshal(reader);
            return obj;
        } catch (FileNotFoundException e) {
            Container.logInternal(Priority.ERROR,
                    "AlertViz ConfigurationManager unable to find file: "
                            + file.getAbsolutePath(), e);
            throw new SerializationException(e);
        } catch (JAXBException e) {
            Container.logInternal(Priority.ERROR,
                    "ConfigurationManager: JAXB exception unmarshalling from file: "
                            + file.getAbsolutePath(), e);
            throw new SerializationException(e);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }

    public ForcedConfiguration getForcedConfiguration() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile file = pm.getLocalizationFile(ctx, "alertViz"
                + File.separator + "AlertVizForced.xml");
        File actualFile = file.getFile();
        // Win32: JHB put in check for length
        if (actualFile != null && actualFile.exists()
                && actualFile.length() > 0) {
            try {
                return (ForcedConfiguration) unmarshaller.unmarshal(actualFile);
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
        LocalizationFile locFile = getCustomLocalization();
        File customFile = locFile.getFile();
        try {
            customConfiguration = (Configuration) deserializeFromFile(customFile);
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
            serializeToFile(customConfiguration, customFile);
            locFile.save();
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (LocalizationOpFailedException e) {
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
