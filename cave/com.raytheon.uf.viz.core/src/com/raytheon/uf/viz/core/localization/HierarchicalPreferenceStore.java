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

package com.raytheon.uf.viz.core.localization;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.apache.commons.configuration.CombinedConfiguration;
import org.apache.commons.configuration.ConversionException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.OverrideCombiner;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.raytheon.uf.common.localization.FileLocker;
import com.raytheon.uf.common.localization.FileLocker.Type;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Defines a hierarchical preference store, backed by Apache Commons
 * configurator.
 * 
 * <BR>
 * <P>
 * Three scopes are defined, in order of increasing precedence:
 * <UL>
 * <LI><B>base</B> - contains default configurations
 * <LI><B>site</B> - contains configurations specific to a locale
 * <LI><B>user</B> - contains configurations specific to a user
 * </UL>
 * 
 * <BR>
 * <BR>
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2007            chammack    Initial Creation.
 * Feb 5, 2008             chammack    Add API to support set/remove at any tier 
 *                                     and support for listeners
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class HierarchicalPreferenceStore implements IPersistentPreferenceStore {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HierarchicalPreferenceStore.class, "CAVE");

    public static enum Level {
        USER, SITE, BASE, COMBINED
    };

    private CombinedConfiguration combinedConfiguration;

    private CombinedConfiguration localConfiguration;

    private LocalizationFile userFile;

    private XMLConfiguration userConfiguration;

    private XMLConfiguration baseConfiguration;

    private XMLConfiguration siteConfiguration;

    private final List<IPropertyChangeListener> propertyChangeListeners;

    private boolean isDirty;

    public static String EMPTY_CONFIGURATION = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>\n"
            + "<configuration>\n" + "</configuration>\n";

    /**
     * Constructor
     * 
     * @param activator
     * @throws VizException
     */
    public HierarchicalPreferenceStore(AbstractUIPlugin activator)
            throws LocalizationException {
        this.propertyChangeListeners = new ArrayList<IPropertyChangeListener>();
        initialize(activator.getBundle().getSymbolicName(), false);
    }

    private void initialize(final String context, boolean reload)
            throws LocalizationException {
        LocalizationFile baseLocFile = null, siteLocFile = null;
        CombinedConfiguration oldCombConfiguration = this.combinedConfiguration;

        try {
            IPathManager pm = PathManagerFactory.getPathManager();

            LocalizationContext userCtx = pm.getContext(
                    LocalizationType.CAVE_CONFIG, LocalizationLevel.USER);
            LocalizationContext siteCtx = pm.getContext(
                    LocalizationType.CAVE_CONFIG, LocalizationLevel.SITE);
            LocalizationContext baseCtx = pm.getContext(
                    LocalizationType.CAVE_CONFIG, LocalizationLevel.BASE);

            // Get the base file
            baseLocFile = pm.getLocalizationFile(baseCtx, context
                    + File.separator + "config.xml");
            File baseFile = baseLocFile.getFile();

            siteLocFile = pm.getLocalizationFile(siteCtx, context
                    + File.separator + "config.xml");
            if (!reload) {
                siteLocFile
                        .addFileUpdatedObserver(new ILocalizationFileObserver() {
                            @Override
                            public void fileUpdated(FileUpdatedMessage message) {
                                try {
                                    initialize(context, true);
                                } catch (LocalizationException e) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            "Error reloading plugin configuration: "
                                                    + e.getLocalizedMessage(),
                                            e);
                                }
                            }
                        });
            }
            File siteFile = siteLocFile.getFile();

            if (!reload) {
                this.userFile = pm.getLocalizationFile(userCtx, context
                        + File.separator + "config.xml");
                this.userFile
                        .addFileUpdatedObserver(new ILocalizationFileObserver() {
                            @Override
                            public void fileUpdated(FileUpdatedMessage message) {
                                try {
                                    initialize(context, true);
                                } catch (LocalizationException e) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            "Error reloading plugin configuration: "
                                                    + e.getLocalizedMessage(),
                                            e);
                                }
                            }
                        });
            }
            File userFile = this.userFile.getFile();

            FileLocker.lock(this, baseLocFile, Type.READ);
            FileLocker.lock(this, siteLocFile, Type.READ);
            FileLocker.lock(this, this.userFile, Type.READ);

            XMLConfiguration baseConfiguration = new XMLConfiguration(baseFile);
            XMLConfiguration siteConfiguration = null, userConfiguration = null;
            // Create empty files if necessary
            if (siteFile != null) {
                setupEmptyFile(siteFile);
                // Load the individual configuration for site
                siteConfiguration = new XMLConfiguration(siteFile);
            } else {
                siteConfiguration = new XMLConfiguration();
            }

            // Create empty files if necessary
            if (userFile != null) {
                setupEmptyFile(userFile);
                // Load the individual configuration for user
                userConfiguration = new XMLConfiguration(userFile);
            } else {
                userConfiguration = new XMLConfiguration();
            }

            // Create and initialize the node combiner
            OverrideCombiner combiner = new OverrideCombiner();

            // Construct the combined configuration
            CombinedConfiguration combinedConfiguration = new CombinedConfiguration(
                    combiner);
            combinedConfiguration.setForceReloadCheck(true);

            combinedConfiguration.addConfiguration(userConfiguration);
            combinedConfiguration.addConfiguration(siteConfiguration);
            combinedConfiguration.addConfiguration(baseConfiguration);

            this.baseConfiguration = baseConfiguration;
            this.siteConfiguration = siteConfiguration;
            this.userConfiguration = userConfiguration;
            this.combinedConfiguration = combinedConfiguration;

            // Construct the combined configuration (minus the users settings,
            // use this as the default settings)
            localConfiguration = new CombinedConfiguration(combiner);
            localConfiguration.setForceReloadCheck(true);
            localConfiguration.addConfiguration(siteConfiguration, "site");
            localConfiguration.addConfiguration(baseConfiguration, "base");
        } catch (Exception e) {
            throw new LocalizationException(e);
        } finally {
            // Always unlock files
            FileLocker.unlock(this, baseLocFile, siteLocFile, userFile);
        }

        if (reload && oldCombConfiguration != null
                && combinedConfiguration != null) {
            // We are reloading, loop through properties from new
            // configuration and notify of changes. We wait to do this until
            // we've already unlocked the files we read in to avoid any dead
            // locks
            Iterator<?> keyIter = combinedConfiguration.getKeys();
            while (keyIter.hasNext()) {
                String key = String.valueOf(keyIter.next());
                Object newValue = combinedConfiguration.getProperty(key);
                Object oldValue = oldCombConfiguration.getProperty(key);
                firePropertyChangeEvent(key, oldValue, newValue);
            }
        }
    }

    /**
     * Set up an empty configuration file if one does not exist
     * 
     * @param siteStr
     */
    private void setupEmptyFile(File file) {
        if (!file.exists()) {
            File parent = file.getParentFile();
            if (!parent.exists()) {
                parent.mkdirs();
            }

            PrintWriter pw = null;
            try {
                pw = new PrintWriter(file);
                pw.write(EMPTY_CONFIGURATION);

            } catch (IOException e) {
                // ignore
            } finally {
                if (pw != null) {
                    pw.close();
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#addPropertyChangeListener
     * (org.eclipse.jface.util.IPropertyChangeListener)
     */

    public void addPropertyChangeListener(
            org.eclipse.jface.util.IPropertyChangeListener listener) {
        this.propertyChangeListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#contains(java.lang.String)
     */

    public boolean contains(String name) {
        return this.combinedConfiguration.containsKey(name);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#firePropertyChangeEvent
     * (java.lang.String, java.lang.Object, java.lang.Object)
     */

    public void firePropertyChangeEvent(String name, Object oldValue,
            Object newValue) {
        // The following criteria meets the Eclipse contract
        if (oldValue == null || oldValue.equals(newValue)) {
            return;
        }

        final PropertyChangeEvent pe = new PropertyChangeEvent(this, name,
                oldValue, newValue);
        for (final IPropertyChangeListener listener : this.propertyChangeListeners) {
            SafeRunnable.run(new SafeRunnable(JFaceResources
                    .getString("PreferenceStore.changeError")) { //$NON-NLS-1$
                        public void run() {
                            listener.propertyChange(pe);
                        }
                    });

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getBoolean(java.lang.String
     * )
     */
    public boolean getBoolean(String name) {
        return getBoolean(Level.COMBINED, name);
    }

    /**
     * Return a boolean value given a specific level
     * 
     * @param level
     *            the hierarchical level
     * @param name
     *            the parameter name
     * @return the value
     */
    public boolean getBoolean(Level level, String name) {
        try {
            switch (level) {
            case BASE:
                return this.baseConfiguration.getBoolean(name);
            case SITE:
                return this.siteConfiguration.getBoolean(name);
            case USER:
                return this.userConfiguration.getBoolean(name);
            case COMBINED:
                return this.combinedConfiguration.getBoolean(name);
            default:
                throw new IllegalArgumentException("Unsupported level: "
                        + level);
            }
        } catch (NoSuchElementException e) {
            return IPreferenceStore.BOOLEAN_DEFAULT_DEFAULT;
        } catch (ConversionException cfe) {
            return IPreferenceStore.BOOLEAN_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getDefaultBoolean(java.
     * lang.String)
     */

    public boolean getDefaultBoolean(String name) {
        try {
            return localConfiguration.getBoolean(name);
        } catch (NoSuchElementException e) {
            return IPreferenceStore.BOOLEAN_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getDefaultDouble(java.lang
     * .String)
     */

    public double getDefaultDouble(String name) {
        try {
            return localConfiguration.getDouble(name);
        } catch (NoSuchElementException e) {
            return IPreferenceStore.DOUBLE_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getDefaultFloat(java.lang
     * .String)
     */

    public float getDefaultFloat(String name) {
        try {
            return localConfiguration.getFloat(name);
        } catch (NoSuchElementException e) {
            return IPreferenceStore.FLOAT_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getDefaultInt(java.lang
     * .String)
     */

    public int getDefaultInt(String name) {
        try {
            return localConfiguration.getInt(name);
        } catch (NoSuchElementException e) {
            return IPreferenceStore.INT_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getDefaultLong(java.lang
     * .String)
     */

    public long getDefaultLong(String name) {
        try {
            return localConfiguration.getLong(name);
        } catch (NoSuchElementException e) {
            return IPreferenceStore.LONG_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getDefaultString(java.lang
     * .String)
     */

    public String getDefaultString(String name) {
        try {
            String val = localConfiguration.getString(name);
            if (val == null) {
                return IPreferenceStore.STRING_DEFAULT_DEFAULT;
            }

            return val;
        } catch (NoSuchElementException e) {
            return IPreferenceStore.STRING_DEFAULT_DEFAULT;
        }
    }

    /**
     * Return a double value given a specific level
     * 
     * @param level
     *            the hierarchical level
     * @param name
     *            the parameter name
     * @return the value
     */
    public double getDouble(Level level, String name) {
        try {
            switch (level) {
            case BASE:
                return this.baseConfiguration.getDouble(name);
            case SITE:
                return this.siteConfiguration.getDouble(name);
            case USER:
                return this.userConfiguration.getDouble(name);
            case COMBINED:
                return this.combinedConfiguration.getDouble(name);
            default:
                throw new IllegalArgumentException("Unsupported level: "
                        + level);
            }
        } catch (NoSuchElementException e) {
            return IPreferenceStore.DOUBLE_DEFAULT_DEFAULT;
        } catch (ConversionException cfe) {
            return IPreferenceStore.DOUBLE_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getDouble(java.lang.String)
     */
    public double getDouble(String name) {
        return getDouble(Level.COMBINED, name);
    }

    /**
     * Return a float value given a specific level
     * 
     * @param level
     *            the hierarchical level
     * @param name
     *            the parameter name
     * @return the value
     */
    public float getFloat(Level level, String name) {
        try {
            switch (level) {
            case BASE:
                return this.baseConfiguration.getFloat(name);
            case SITE:
                return this.siteConfiguration.getFloat(name);
            case USER:
                return this.userConfiguration.getFloat(name);
            case COMBINED:
                return this.combinedConfiguration.getFloat(name);
            default:
                throw new IllegalArgumentException("Unsupported level: "
                        + level);
            }
        } catch (NoSuchElementException e) {
            return IPreferenceStore.FLOAT_DEFAULT_DEFAULT;
        } catch (ConversionException cfe) {
            return IPreferenceStore.FLOAT_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getFloat(java.lang.String)
     */
    public float getFloat(String name) {
        return getFloat(Level.COMBINED, name);
    }

    public float[] getFloatArray(Level level, String name) {
        String[] s = getStringArray(level, name);
        float[] ret = new float[s.length];

        for (int i = 0; i < s.length; i++) {
            try {
                ret[i] = Float.parseFloat(s[i]);
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid float value in preference " + name, e);
            }
        }
        return ret;
    }

    public float[] getFloatArray(String name) {
        return getFloatArray(Level.COMBINED, name);
    }

    /**
     * Return a integer value given a specific level
     * 
     * @param level
     *            the hierarchical level
     * @param name
     *            the parameter name
     * @return the value
     */
    public int getInt(Level level, String name) {
        try {
            switch (level) {
            case BASE:
                return this.baseConfiguration.getInt(name);
            case SITE:
                return this.siteConfiguration.getInt(name);
            case USER:
                return this.userConfiguration.getInt(name);
            case COMBINED:
                return this.combinedConfiguration.getInt(name);
            default:
                throw new IllegalArgumentException("Unsupported level: "
                        + level);
            }
        } catch (NoSuchElementException e) {
            return IPreferenceStore.INT_DEFAULT_DEFAULT;
        } catch (ConversionException cfe) {
            return IPreferenceStore.INT_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getInt(java.lang.String)
     */
    public int getInt(String name) {
        return getInt(Level.COMBINED, name);
    }

    /**
     * Return a long value given a specific level
     * 
     * @param level
     *            the hierarchical level
     * @param name
     *            the parameter name
     * @return the value
     */
    public long getLong(Level level, String name) {
        try {
            switch (level) {
            case BASE:
                return this.baseConfiguration.getLong(name);
            case SITE:
                return this.siteConfiguration.getLong(name);
            case USER:
                return this.userConfiguration.getLong(name);
            case COMBINED:
                return this.combinedConfiguration.getLong(name);
            default:
                throw new IllegalArgumentException("Unsupported level: "
                        + level);
            }
        } catch (NoSuchElementException e) {
            return IPreferenceStore.LONG_DEFAULT_DEFAULT;
        } catch (ConversionException cfe) {
            return IPreferenceStore.LONG_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getLong(java.lang.String)
     */
    public long getLong(String name) {
        return getLong(Level.COMBINED, name);
    }

    /**
     * Return a String value given a specific level
     * 
     * @param level
     *            the hierarchical level
     * @param name
     *            the parameter name
     * @return the value
     */
    public String getString(Level level, String name) {
        try {
            String v;
            switch (level) {
            case BASE:
                v = this.baseConfiguration.getString(name);
                break;
            case SITE:
                v = this.siteConfiguration.getString(name);
                break;
            case USER:
                v = this.userConfiguration.getString(name);
                break;
            case COMBINED:
                v = this.combinedConfiguration.getString(name);
                break;
            default:
                throw new IllegalArgumentException("Unsupported level: "
                        + level);
            }
            if (v == null) {
                return IPreferenceStore.STRING_DEFAULT_DEFAULT;
            } else {
                return v;
            }
        } catch (NoSuchElementException e) {
            return IPreferenceStore.STRING_DEFAULT_DEFAULT;
        } catch (ConversionException cfe) {
            return IPreferenceStore.STRING_DEFAULT_DEFAULT;
        }
    }

    public String[] getStringArray(String name) {
        return getStringArray(Level.COMBINED, name);
    }

    /**
     * Return a String value given a specific level
     * 
     * @param level
     *            the hierarchical level
     * @param name
     *            the parameter name
     * @return the value
     */
    public String[] getStringArray(Level level, String name) {
        try {
            String[] v;
            switch (level) {
            case BASE:
                v = this.baseConfiguration.getStringArray(name);
                break;
            case SITE:
                v = this.siteConfiguration.getStringArray(name);
                break;
            case USER:
                v = this.userConfiguration.getStringArray(name);
                break;
            case COMBINED:
                v = this.combinedConfiguration.getStringArray(name);
                break;
            default:
                throw new IllegalArgumentException("Unsupported level: "
                        + level);
            }
            if (v == null) {
                return null;
            } else {
                return v;
            }
        } catch (NoSuchElementException e) {
            return null;
        } catch (ConversionException cfe) {
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getString(java.lang.String)
     */
    public String getString(String name) {
        return getString(Level.COMBINED, name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#isDefault(java.lang.String)
     */

    public boolean isDefault(String name) {

        try {
            return (this.baseConfiguration.getProperty(name)
                    .equals(this.combinedConfiguration.getProperty(name)));
        } catch (NoSuchElementException e) {
            return false;
        } catch (NullPointerException e) {
            return false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.IPreferenceStore#needsSaving()
     */

    public boolean needsSaving() {
        return isDirty;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#putValue(java.lang.String,
     * java.lang.String)
     */
    public void putValue(String name, String value) {
        userConfiguration.setProperty(name, value);
        isDirty = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#removePropertyChangeListener
     * (org.eclipse.jface.util.IPropertyChangeListener)
     */
    public void removePropertyChangeListener(
            org.eclipse.jface.util.IPropertyChangeListener listener) {
        this.propertyChangeListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , boolean)
     */
    public void setDefault(String name, boolean value) {
        this.baseConfiguration.setProperty(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , double)
     */
    public void setDefault(String name, double value) {
        this.baseConfiguration.setProperty(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , float)
     */
    public void setDefault(String name, float value) {
        this.baseConfiguration.setProperty(name, value);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , int)
     */
    public void setDefault(String name, int value) {
        this.baseConfiguration.setProperty(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , long)
     */
    public void setDefault(String name, long value) {
        this.baseConfiguration.setProperty(name, value);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , java.lang.String)
     */
    public void setDefault(String name, String defaultObject) {
        this.baseConfiguration.setProperty(name, defaultObject);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setToDefault(java.lang.
     * String)
     */
    public void setToDefault(String name) {
        userConfiguration.clear();
    }

    public void setValue(Level level, String name, boolean value) {
        boolean oldValue = false;
        try {
            oldValue = this.combinedConfiguration.getBoolean(name);
        } catch (RuntimeException e1) {
            // ignore
        }

        switch (level) {
        case USER:
            try {
                if (value == this.combinedConfiguration.getBoolean(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.userConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case SITE:
            try {
                if (value == this.baseConfiguration.getBoolean(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.siteConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case BASE:
            this.baseConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        }
        firePropertyChangeEvent(name, new Boolean(oldValue), new Boolean(value));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * boolean)
     */
    public void setValue(String name, boolean value) {
        setValue(Level.USER, name, value);
    }

    public void setValue(Level level, String name, double value) {

        double oldValue = IPreferenceStore.DOUBLE_DEFAULT_DEFAULT;

        try {
            oldValue = this.combinedConfiguration.getDouble(name);
        } catch (RuntimeException e1) {
            // ignore
        }

        switch (level) {
        case USER:
            try {
                if (value == this.combinedConfiguration.getDouble(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.userConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case SITE:
            try {
                if (value == this.baseConfiguration.getDouble(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.siteConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case BASE:
            this.baseConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        }

        firePropertyChangeEvent(name, new Double(oldValue), new Double(value));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * double)
     */
    public void setValue(String name, double value) {
        setValue(Level.USER, name, value);
    }

    public void setValue(Level level, String name, float value) {

        double oldValue = IPreferenceStore.FLOAT_DEFAULT_DEFAULT;

        try {
            oldValue = this.combinedConfiguration.getDouble(name);
        } catch (RuntimeException e1) {
            // ignore
        }

        switch (level) {
        case USER:
            try {
                if (value == this.combinedConfiguration.getFloat(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.userConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case SITE:
            try {
                if (value == this.baseConfiguration.getFloat(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.siteConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case BASE:
            this.baseConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        }

        firePropertyChangeEvent(name, new Float(oldValue), new Float(value));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * float)
     */
    public void setValue(String name, float value) {
        setValue(Level.USER, name, value);
    }

    public void setValue(Level level, String name, int value) {
        int oldValue = IPreferenceStore.INT_DEFAULT_DEFAULT;

        try {
            oldValue = this.combinedConfiguration.getInt(name);
        } catch (RuntimeException e1) {
            // ignore
        }

        switch (level) {
        case USER:
            try {
                if (value == this.combinedConfiguration.getInt(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.userConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case SITE:
            try {
                if (value == this.baseConfiguration.getInt(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.siteConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case BASE:
            this.baseConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        }

        firePropertyChangeEvent(name, new Integer(oldValue), new Integer(value));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * int)
     */
    public void setValue(String name, int value) {
        setValue(Level.USER, name, value);
    }

    public void setValue(Level level, String name, long value) {

        long oldValue = IPreferenceStore.LONG_DEFAULT_DEFAULT;

        try {
            oldValue = this.combinedConfiguration.getLong(name);
        } catch (RuntimeException e1) {
            // ignore
        }

        switch (level) {
        case USER:
            try {
                if (value == this.combinedConfiguration.getLong(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.userConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case SITE:
            try {
                if (value == this.baseConfiguration.getLong(name)) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.siteConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case BASE:
            this.baseConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        }

        firePropertyChangeEvent(name, new Long(oldValue), new Long(value));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * long)
     */
    public void setValue(String name, long value) {
        setValue(Level.USER, name, value);
    }

    public void setValue(Level level, String name, String value) {

        String oldValue = IPreferenceStore.STRING_DEFAULT_DEFAULT;

        try {
            oldValue = this.combinedConfiguration.getString(name);
        } catch (RuntimeException e1) {
            // ignore
        }

        if (oldValue == null) {
            oldValue = IPreferenceStore.STRING_DEFAULT_DEFAULT;
        }

        switch (level) {
        case USER:
            try {
                if (value.equals(this.combinedConfiguration.getString(name))) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.userConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case SITE:
            try {
                if (value.equals(this.baseConfiguration.getString(name))) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.siteConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case BASE:
            this.baseConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        }
        if (value != null) {
            firePropertyChangeEvent(name, oldValue, value);
        }
    }

    public void setValue(Level level, String name, String[] value) {

        String[] oldValue = null;

        try {
            oldValue = this.combinedConfiguration.getStringArray(name);
        } catch (RuntimeException e1) {
            // ignore
        }

        switch (level) {
        case USER:
            try {
                if (value.equals(this.combinedConfiguration
                        .getStringArray(name))) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.userConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case SITE:
            try {
                if (value.equals(this.baseConfiguration.getStringArray(name))) {
                    return;
                }
            } catch (NoSuchElementException e) {
            }
            this.siteConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        case BASE:
            this.baseConfiguration.setProperty(name, value);
            isDirty = true;
            break;
        }
        if (value != null) {
            firePropertyChangeEvent(name, oldValue, value);
        }
    }

    public String[] getKeys(String prefix) {
        Iterator<?> iterator = this.combinedConfiguration.getKeys(prefix);
        List<String> keys = new ArrayList<String>();
        while (iterator.hasNext()) {
            keys.add((String) iterator.next());
        }

        return keys.toArray(new String[keys.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * java.lang.String)
     */
    public void setValue(String name, String value) {
        setValue(Level.USER, name, value);
    }

    public void setValue(String name, String[] value) {
        setValue(Level.USER, name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.IPersistentPreferenceStore#save()
     */
    public void save() throws IOException {
        try {
            if (this.userFile != null) {
                FileLocker.lock(this, userFile, Type.WRITE);
                userConfiguration.save();
                userFile.save();
            }
        } catch (Exception e) {
            throw new IOException("Error saving configuration file", e);
        } finally {
            FileLocker.unlock(this, userFile);
        }
        isDirty = false;
    }

    /**
     * Checks to see if a particular parameter is available at a certain level
     * 
     * @param level
     *            the level to check
     * @param name
     *            the key to check
     * @return if a value is defined for the key at a particular level
     */
    public boolean isAvailableAtLevel(Level level, String name) {
        try {
            switch (level) {
            case BASE:
                return this.baseConfiguration.containsKey(name);
            case SITE:
                return this.siteConfiguration.containsKey(name);
            case USER:
                return this.userConfiguration.containsKey(name);
            case COMBINED:
                return this.combinedConfiguration.containsKey(name);
            default:
                throw new IllegalArgumentException("Unsupported level: "
                        + level);
            }
        } catch (RuntimeException e) {
            return false;
        }
    }

    /**
     * Removes a value in the preference store at a given level
     * 
     * @param level
     *            the level
     * @param name
     *            the key
     */
    public void removeFromLevel(Level level, String name) {
        try {
            switch (level) {
            case BASE:
                this.baseConfiguration.clearProperty(name);
                break;
            case SITE:
                this.siteConfiguration.clearProperty(name);
                break;
            case USER:
                this.userConfiguration.clearProperty(name);
                break;
            default:
                throw new IllegalArgumentException("Unsupported level: "
                        + level);
            }
        } catch (RuntimeException e) {
            return;
        }
    }

    /**
     * Clears all user overrides starting with or including this key
     * 
     * @param key
     *            the key to clear overrides
     */
    public void clearUserOverrides(String key) {
        this.userConfiguration.clearTree(key);
    }

    /**
     * Clear all values from all levels of the preference store.
     * 
     * USE THIS METHOD WITH CAUTION. THIS IS PRIMARILY INTENDED TO AIDE TESTING.
     */
    public void clear() {
        this.baseConfiguration.clear();
        this.siteConfiguration.clear();
        this.userConfiguration.clear();
        this.combinedConfiguration.clear();
        this.localConfiguration.clear();
    }

}
