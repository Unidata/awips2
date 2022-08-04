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
package com.raytheon.viz.gfe;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.util.SafeRunnable;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;

import jep.JepConfig;
import jep.JepException;

/**
 * Preference store for reading from legacy GFE config files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 11, 2009           njensen   Initial creation
 * Jun 22, 2011  9897     ryu       allow new GFE config and send notification
 * Sep 05, 2013  2307     dgilling  Use better PythonScript constructor.
 * Sep 11, 2013  2033     dgilling  Don't load loadConfig.py from localization
 *                                  store.
 * Feb 20, 2017  5979     njensen   Cast to Number for safety
 * Jan 19, 2018  6594     randerso  Ensure PythonPreferenceStore meets the
 *                                  contract of IPreferenceStore. Updated to
 *                                  reflect Jep 3.6 now returns Python floats as
 *                                  Doubles. Code and Javadoc clean up.
 * Jan 29, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Apr 22, 2019  7770     randerso  Fix sense of boolean preferences
 * Jun 03, 2019  7852     dgilling  Update code for jep 3.8.
 *
 * </pre>
 *
 * @author njensen
 */

public class PythonPreferenceStore
        implements IPreferenceStore, IConfigurationChange {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PythonPreferenceStore.class);

    private static final ExecutorService executor = Executors
            .newCachedThreadPool();

    private final Set<IPropertyChangeListener> propertyChangeListeners = new HashSet<>();

    private final Set<IConfigurationChangeListener> configurationChangeListeners = new HashSet<>();

    private Map<String, Object> baseConfiguration;

    private Map<String, Object> selectedConfiguration;

    private String configName;

    /**
     * Default Constructor
     */
    public PythonPreferenceStore() {
        // creates an uninitialized preference store
    }

    /**
     * Load configuration from Python file
     *
     * @param configName
     *            name of Python config file
     */
    public void loadConfiguration(String configName) {
        clearConfiguration();

        try {
            executor.submit(() -> {
                File scriptFile = null;
                try {
                    scriptFile = new File(FileLocator
                            .resolve(FileLocator.find(
                                    Activator.getDefault().getBundle(),
                                    new Path(FileUtil.join("python", "utility",
                                            "loadConfig.py")),
                                    null))
                            .getPath());
                } catch (IOException e) {
                    statusHandler.fatal(
                            "Unable to find loadConfig.py in internal bundle.",
                            e);
                    return;
                }

                try (PythonScript py = new PythonScript(
                        new JepConfig()
                                .setIncludePath(PyUtil.buildJepIncludePath(
                                        GfePyIncludeUtil.getConfigIncludePath(),
                                        GfePyIncludeUtil.getVtecIncludePath()))
                                .setClassLoader(getClass().getClassLoader()),
                        scriptFile.getPath())) {
                    try {
                        Map<String, Object> args = Collections
                                .singletonMap("configName", "gfeConfig");
                        @SuppressWarnings("unchecked")
                        Map<String, Object> config = (Map<String, Object>) py
                                .execute("loadConfig", args);
                        baseConfiguration = config;
                    } catch (JepException e) {
                        statusHandler
                                .fatal("Unable to load baseline GFE config", e);
                    }

                    try {
                        Map<String, Object> args = Collections
                                .singletonMap("configName", configName);
                        @SuppressWarnings("unchecked")
                        Map<String, Object> config = (Map<String, Object>) py
                                .execute("loadConfig", args);

                        selectedConfiguration = config;
                        this.configName = configName;

                        fireConfigurationChangeEvent(configName);
                    } catch (JepException e) {
                        statusHandler
                                .fatal("Unable to load selected GFE config "
                                        + configName
                                        + ". Using baseline configuration.", e);
                        selectedConfiguration = baseConfiguration;
                    }
                } catch (JepException e) {
                    statusHandler.fatal(
                            "Unable to instantiate PythonScript instance to laod GFE config",
                            e);
                }
            }).get();
        } catch (InterruptedException | RejectedExecutionException e) {
            statusHandler
                    .warn("Failed to execute python job to read configuration ["
                            + configName + "]", e);
        } catch (ExecutionException e) {
            statusHandler.fatal(
                    "Failed to load configuration [" + configName + "].", e);
        }
    }

    @Override
    public void addPropertyChangeListener(IPropertyChangeListener listener) {
        this.propertyChangeListeners.add(listener);
    }

    @Override
    public void addConfigurationChangeListener(
            IConfigurationChangeListener listener) {
        this.configurationChangeListeners.add(listener);
    }

    @Override
    public boolean contains(String name) {
        return selectedConfiguration.containsKey(name);
    }

    @Override
    public void firePropertyChangeEvent(String name, Object oldValue,
            Object newValue) {
        // The following criteria meets the Eclipse contract
        if ((oldValue == null) || oldValue.equals(newValue)) {
            return;
        }

        final PropertyChangeEvent pe = new PropertyChangeEvent(this, name,
                oldValue, newValue);
        for (final IPropertyChangeListener listener : this.propertyChangeListeners) {
            SafeRunnable.run(new SafeRunnable(
                    JFaceResources.getString("PreferenceStore.changeError")) { //$NON-NLS-1$
                @Override
                public void run() {
                    listener.propertyChange(pe);
                }
            });
        }
    }

    @Override
    public void fireConfigurationChangeEvent(final String config) {
        for (final IConfigurationChangeListener listener : this.configurationChangeListeners) {
            SafeRunnable.run(new SafeRunnable(
                    JFaceResources.getString("PreferenceStore.changeError")) { //$NON-NLS-1$
                @Override
                public void run() {
                    listener.configurationChanged(config);
                }
            });
        }
    }

    @Override
    public boolean getDefaultBoolean(String name) {
        return toBoolean(baseConfiguration.get(name));
    }

    @Override
    public double getDefaultDouble(String name) {
        return toDouble(baseConfiguration.get(name));
    }

    @Override
    public float getDefaultFloat(String name) {
        return toFloat(baseConfiguration.get(name));
    }

    @Override
    public int getDefaultInt(String name) {
        return toInt(baseConfiguration.get(name));
    }

    @Override
    public long getDefaultLong(String name) {
        return toLong(baseConfiguration.get(name));
    }

    @Override
    public String getDefaultString(String name) {
        return toString(baseConfiguration.get(name));
    }

    @Override
    public boolean getBoolean(String name) {
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            return toBoolean(obj);
        }
        return getDefaultBoolean(name);
    }

    @Override
    public double getDouble(String name) {
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            return toDouble(obj);
        }
        return getDefaultDouble(name);
    }

    @Override
    public float getFloat(String name) {
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            return toFloat(obj);
        }
        return getDefaultFloat(name);
    }

    @Override
    public int getInt(String name) {
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            return toInt(obj);
        }
        return getDefaultInt(name);
    }

    @Override
    public long getLong(String name) {
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            return toLong(obj);
        }
        return getDefaultLong(name);
    }

    @Override
    public String getString(String name) {
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            return toString(obj);
        }
        return getDefaultString(name);
    }

    private boolean toBoolean(Object obj) {
        boolean result = IPreferenceStore.BOOLEAN_DEFAULT_DEFAULT;
        if (obj instanceof Boolean) {
            result = (Boolean) obj;
        } else if (obj instanceof Number) {
            result = ((Number) obj).doubleValue() != 0.0;
        } else if (obj instanceof String) {
            result = !((String) obj).isEmpty();
        } else if (obj instanceof List) {
            result = !((List<?>) obj).isEmpty();
        }
        return result;
    }

    private double toDouble(Object obj) {
        double result = IPreferenceStore.DOUBLE_DEFAULT_DEFAULT;
        if (obj instanceof Number) {
            result = ((Number) obj).doubleValue();
        }
        return result;
    }

    private float toFloat(Object obj) {
        float result = IPreferenceStore.FLOAT_DEFAULT_DEFAULT;
        if (obj instanceof Number) {
            result = ((Number) obj).floatValue();
        }
        return result;
    }

    private int toInt(Object obj) {
        int result = IPreferenceStore.INT_DEFAULT_DEFAULT;
        if (obj instanceof Number) {
            result = ((Number) obj).intValue();
        }
        return result;
    }

    private long toLong(Object obj) {
        long result = IPreferenceStore.LONG_DEFAULT_DEFAULT;
        if (obj instanceof Number) {
            result = ((Number) obj).longValue();
        }
        return result;
    }

    private String toString(Object obj) {
        String result = IPreferenceStore.STRING_DEFAULT_DEFAULT;
        if (obj != null) {
            result = obj.toString();
        }
        return result;
    }

    @Override
    public boolean isDefault(String name) {
        Object baseValue = baseConfiguration.get(name);
        if (baseValue == null) {
            return false;
        }

        Object selectedValue = selectedConfiguration.get(name);
        return baseValue.equals(selectedValue);
    }

    @Override
    public void putValue(String name, String value) {
        selectedConfiguration.put(name, value);
    }

    @Override
    public void removePropertyChangeListener(IPropertyChangeListener listener) {
        this.propertyChangeListeners.remove(listener);
    }

    @Override
    public void removeConfigurationChangeListener(
            IConfigurationChangeListener listener) {
        this.configurationChangeListeners.remove(listener);
    }

    @Override
    public void setDefault(String name, double value) {
        baseConfiguration.put(name, value);
    }

    @Override
    public void setDefault(String name, float value) {
        baseConfiguration.put(name, value);
    }

    @Override
    public void setDefault(String name, int value) {
        baseConfiguration.put(name, value);
    }

    @Override
    public void setDefault(String name, long value) {
        baseConfiguration.put(name, value);
    }

    @Override
    public void setDefault(String name, String defaultObject) {
        baseConfiguration.put(name, defaultObject);
    }

    @Override
    public void setDefault(String name, boolean value) {
        baseConfiguration.put(name, value);
    }

    @Override
    public void setToDefault(String name) {
        selectedConfiguration.put(name, baseConfiguration.get(name));
    }

    private void setValueInternal(String name, Object obj) {
        Object oldValue = selectedConfiguration.get(name);
        selectedConfiguration.put(name, obj);
        firePropertyChangeEvent(name, oldValue, obj);
    }

    @Override
    public void setValue(String name, double value) {
        setValueInternal(name, value);
    }

    @Override
    public void setValue(String name, float value) {
        setValueInternal(name, value);
    }

    @Override
    public void setValue(String name, int value) {
        setValueInternal(name, value);
    }

    @Override
    public void setValue(String name, long value) {
        setValueInternal(name, value);
    }

    @Override
    public void setValue(String name, String value) {
        setValueInternal(name, value);
    }

    @Override
    public void setValue(String name, boolean value) {
        setValueInternal(name, value);
    }

    @Override
    public boolean needsSaving() {
        // never save through the store, always through manually editing the
        // file through the GUI
        return false;
    }

    /**
     * Returns the current value of the String[]-valued preference with the
     * given name. Returns an empty String[] if there is no preference with the
     * given name.
     *
     * @param name
     *            the name of the preference
     * @return the String[]-valued preference
     */
    public String[] getStringArray(String name) {
        String[] result = null;
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                result = new String[list.size()];
                int i = 0;
                for (Object item : list) {
                    if (item != null) {
                        result[i] = item.toString();
                    } else {
                        result[i] = null;
                    }
                    i++;
                }
            } else if (obj instanceof String[]) {
                result = (String[]) obj;
            } else {
                result = new String[] { obj.toString() };
            }
        } else {
            return new String[0];
        }
        return result;
    }

    /**
     * Returns the current value of the Double[]-valued preference with the
     * given name. Returns an empty Double[] if there is no preference with the
     * given name. If any value in the array cannot be treated as a Double, the
     * default-default value (<code>0.0</code>) will be returned.
     *
     * @param name
     *            the name of the preference
     * @return the Double[]-valued preference
     */
    public Double[] getDoubleArray(String name) {
        Double[] result = null;
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                result = new Double[list.size()];
                int i = 0;
                for (Object item : list) {
                    if (item instanceof Number) {
                        result[i] = ((Number) item).doubleValue();
                    } else {
                        statusHandler.error(String.format(
                                "GFE config file %s contains non-numeric value '%s' in preference %s",
                                configName, obj.toString(), name));
                        result[i] = IPreferenceStore.DOUBLE_DEFAULT_DEFAULT;
                    }
                    i++;
                }
            } else if (obj instanceof Double[]) {
                result = (Double[]) obj;
            }
        } else {
            return new Double[0];
        }
        return result;
    }

    /**
     * Returns the current value of the Float[]-valued preference with the given
     * name. Returns an empty Float[] if there is no preference with the given
     * name. If any value in the array cannot be treated as a Float, the
     * default-default value (<code>0.0f</code>) will be returned.
     *
     * @param name
     *            the name of the preference
     * @return the Float[]-valued preference
     */
    public Float[] getFloatArray(String name) {
        Float[] result = null;
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                result = new Float[list.size()];
                int i = 0;
                for (Object item : list) {
                    if (item instanceof Number) {
                        result[i] = ((Number) item).floatValue();
                    } else {
                        statusHandler.error(String.format(
                                "GFE config file %s contains non-numeric value '%s' in preference %s",
                                configName, obj.toString(), name));
                        result[i] = IPreferenceStore.FLOAT_DEFAULT_DEFAULT;
                    }
                    i++;
                }
            } else if (obj instanceof Float[]) {
                result = (Float[]) obj;
            }
        } else {
            return new Float[0];
        }
        return result;
    }

    /**
     * Returns the current value of the Integer[]-valued preference with the
     * given name. Returns an empty Integer[] if there is no preference with the
     * given name. If any value in the array cannot be treated as an Integer,
     * the default-default value (<code>0</code>) will be returned.
     *
     * @param name
     *            the name of the preference
     * @return the Integer[]-valued preference
     */
    public Integer[] getIntArray(String name) {
        Integer[] result = null;
        Object obj = null;
        obj = selectedConfiguration.get(name);
        if (obj != null) {
            if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                result = new Integer[list.size()];
                int i = 0;
                for (Object item : list) {
                    if (item instanceof Number) {
                        result[i] = ((Number) item).intValue();
                    } else {
                        statusHandler.error(String.format(
                                "GFE config file %s contains non-numeric value '%s' in preference %s",
                                configName, obj.toString(), name));
                        result[i] = IPreferenceStore.INT_DEFAULT_DEFAULT;
                    }
                    i++;
                }
            } else if (obj instanceof Integer[]) {
                result = (Integer[]) obj;
            }
        } else {
            return new Integer[0];
        }
        return result;
    }

    /**
     * Sets the current value of the String[]-valued preference with the given
     * name.
     * <p>
     * A property change event is reported if the current value of the
     * preference actually changes from its previous value. In the event object,
     * the property name is the name of the preference, and the old and new
     * values are wrapped as objects.
     * </p>
     * <p>
     * Note that the preferred way of re-initializing a preference to its
     * default value is to call <code>setToDefault</code>.
     * </p>
     *
     * @param name
     *            the name of the preference
     * @param value
     *            the new current value of the preference
     */
    public void setValue(String name, String[] value) {
        setValueInternal(name, value);
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is a Boolean
     */
    public boolean isBoolean(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Boolean));
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is a String
     */
    public boolean isInt(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Integer));
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is a Float
     */
    public boolean isFloat(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Float));
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is a Double
     */
    public boolean isDouble(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Double));
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is a Long
     */
    public boolean isLong(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Long));
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is a String
     */
    public boolean isString(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof String));
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is a String[]
     */
    public boolean isStringArray(String name) {
        Object obj = selectedConfiguration.get(name);
        boolean result = false;
        if (obj != null) {
            if (obj instanceof String[]) {
                result = true;
            } else if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                if (list.isEmpty() || (list.get(0) instanceof String)) {
                    result = true;
                }
            }
        }

        return result;
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is a Double[]
     */
    public boolean isDoubleArray(String name) {
        Object obj = selectedConfiguration.get(name);
        boolean result = false;
        if (obj != null) {
            if (obj instanceof Double[]) {
                result = true;
            } else if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                if (list.isEmpty() || (list.get(0) instanceof Double)) {
                    result = true;
                }
            }
        }

        return result;
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is a Float[]
     */
    public boolean isFloatArray(String name) {
        Object obj = selectedConfiguration.get(name);
        boolean result = false;
        if (obj != null) {
            if (obj instanceof Float[]) {
                result = true;
            } else if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                if (list.isEmpty() || (list.get(0) instanceof Float)) {
                    result = true;
                }
            }
        }

        return result;
    }

    /**
     * @param name
     *            the name of the preference
     * @return true if the preference is an Integer[]
     */
    public boolean isIntArray(String name) {
        Object obj = selectedConfiguration.get(name);
        boolean result = false;
        if (obj != null) {
            if (obj instanceof Integer[]) {
                result = true;
            } else if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                if (list.isEmpty() || (list.get(0) instanceof Integer)) {
                    result = true;
                }
            }
        }

        return result;
    }

    /**
     * @return the name of the currently loaded Python configuration file
     */
    public String getConfigName() {
        return configName;
    }

    /**
     * Clears the loaded configuration
     */
    public void clearConfiguration() {
        this.baseConfiguration = null;
        this.selectedConfiguration = null;
        this.configName = null;
    }

    /**
     * @return true if a config file is loaded
     */
    public boolean isConfigurationLoaded() {
        return (baseConfiguration != null) && (selectedConfiguration != null)
                && (configName != null);
    }

}
