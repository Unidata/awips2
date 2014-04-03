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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import jep.JepException;

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
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Preference store for reading from legacy GFE config files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2009            njensen     Initial creation
 * Jun 22, 2011  9897      ryu         allow new GFE config and send notification
 * Sep 05, 2013  #2307     dgilling    Use better PythonScript constructor.
 * Sep 11, 2013  #2033     dgilling    Don't load loadConfig.py from 
 *                                     localization store.
 * Apr 02, 2014  #2729     randerso    Added commonPython to path for LogStream
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PythonPreferenceStore implements IPreferenceStore,
        IConfigurationChange {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PythonPreferenceStore.class);

    private final List<IPropertyChangeListener> propertyChangeListeners;

    private final List<IConfigurationChangeListener> configurationChangeListeners;

    private Map<String, Object> baseConfiguration;

    private Map<String, Object> selectedConfiguration;

    private String configName;

    public PythonPreferenceStore(String configName) {
        this.propertyChangeListeners = new ArrayList<IPropertyChangeListener>();
        this.configurationChangeListeners = new ArrayList<IConfigurationChangeListener>();

        this.loadConfiguration(configName);
    }

    @SuppressWarnings("unchecked")
    public void loadConfiguration(String configName) {
        String commonPythonPath = GfePyIncludeUtil.getCommonPythonIncludePath();
        String configPath = GfePyIncludeUtil.getConfigIncludePath();
        String vtecPath = GfePyIncludeUtil.getVtecIncludePath();

        PythonScript py = null;
        try {
            File scriptFile = new File(FileLocator.resolve(
                    FileLocator.find(
                            Activator.getDefault().getBundle(),
                            new Path(FileUtil.join("python", "utility",
                                    "loadConfig.py")), null)).getPath());
            py = new PythonScript(scriptFile.getPath(),
                    PyUtil.buildJepIncludePath(commonPythonPath, configPath,
                            vtecPath), this.getClass().getClassLoader());
        } catch (JepException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Unable to load GFE config", e);
        } catch (IOException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Unable to find loadConfig.py in internal bundle.", e);
        }

        Map<String, Object> args = new HashMap<String, Object>(1, 1f);
        args.put("configName", "gfeConfig");
        try {
            baseConfiguration = (Map<String, Object>) py.execute("loadConfig",
                    args);
        } catch (JepException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Unable to load baseline GFE config", e);
        }

        // Map<String, Object> originalSelectedConfiguration =
        // selectedConfiguration;
        args.put("configName", configName);
        try {
            selectedConfiguration = (Map<String, Object>) py.execute(
                    "loadConfig", args);
        } catch (JepException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Unable to load selected GFE config " + configName
                            + ". Using baseline configuration.", e);
            selectedConfiguration = baseConfiguration;
        }

        if (py != null) {
            py.dispose();
        }

        fireConfigurationChangeEvent(configName);
        this.configName = configName;
    }

    public PythonPreferenceStore(Map<String, Object> config) {
        this.propertyChangeListeners = new ArrayList<IPropertyChangeListener>();
        this.configurationChangeListeners = new ArrayList<IConfigurationChangeListener>();
        this.baseConfiguration = config;
        this.selectedConfiguration = config;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#addPropertyChangeListener
     * (org.eclipse.jface.util.IPropertyChangeListener)
     */
    @Override
    public void addPropertyChangeListener(IPropertyChangeListener listener) {
        this.propertyChangeListeners.add(listener);
    }

    @Override
    public void addConfigurationChangeListener(
            IConfigurationChangeListener listener) {
        this.configurationChangeListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#contains(java.lang.String)
     */
    @Override
    public boolean contains(String name) {
        return selectedConfiguration.containsKey(name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#firePropertyChangeEvent
     * (java.lang.String, java.lang.Object, java.lang.Object)
     */
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
            SafeRunnable.run(new SafeRunnable(JFaceResources
                    .getString("PreferenceStore.changeError")) { //$NON-NLS-1$
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
            SafeRunnable.run(new SafeRunnable(JFaceResources
                    .getString("PreferenceStore.changeError")) { //$NON-NLS-1$
                        @Override
                        public void run() {
                            listener.configurationChanged(config);
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
    @Override
    public boolean getBoolean(String name) {
        Boolean result = (Boolean) selectedConfiguration.get(name);
        if (result == null) {
            result = IPreferenceStore.BOOLEAN_DEFAULT_DEFAULT;
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getDefaultBoolean(java.
     * lang.String)
     */
    @Override
    public boolean getDefaultBoolean(String name) {
        try {
            return (Boolean) baseConfiguration.get(name);
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
    @Override
    public double getDefaultDouble(String name) {
        try {
            return (Double) baseConfiguration.get(name);
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
    @Override
    public float getDefaultFloat(String name) {
        try {
            return (Float) baseConfiguration.get(name);
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
    @Override
    public int getDefaultInt(String name) {
        try {
            return (Integer) baseConfiguration.get(name);
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
    @Override
    public long getDefaultLong(String name) {
        try {
            return (Long) baseConfiguration.get(name);
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
    @Override
    public String getDefaultString(String name) {
        try {
            return (String) baseConfiguration.get(name);
        } catch (NoSuchElementException e) {
            return IPreferenceStore.STRING_DEFAULT_DEFAULT;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getDouble(java.lang.String)
     */
    @Override
    public double getDouble(String name) {
        Number result = (Number) selectedConfiguration.get(name);
        if (result == null) {
            result = IPreferenceStore.DOUBLE_DEFAULT_DEFAULT;
        }
        return result.doubleValue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getFloat(java.lang.String)
     */
    @Override
    public float getFloat(String name) {
        Float result = (Float) selectedConfiguration.get(name);
        if (result == null) {
            result = IPreferenceStore.FLOAT_DEFAULT_DEFAULT;
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getInt(java.lang.String)
     */
    @Override
    public int getInt(String name) {
        Integer result = (Integer) selectedConfiguration.get(name);
        if (result == null) {
            result = IPreferenceStore.INT_DEFAULT_DEFAULT;
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getLong(java.lang.String)
     */
    @Override
    public long getLong(String name) {
        Long result = (Long) selectedConfiguration.get(name);
        if (result == null) {
            result = IPreferenceStore.LONG_DEFAULT_DEFAULT;
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#getString(java.lang.String)
     */
    @Override
    public String getString(String name) {
        String result = (String) selectedConfiguration.get(name);
        if (result == null) {
            result = IPreferenceStore.STRING_DEFAULT_DEFAULT;
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#isDefault(java.lang.String)
     */
    @Override
    public boolean isDefault(String name) {
        return baseConfiguration.containsKey(name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#putValue(java.lang.String,
     * java.lang.String)
     */
    @Override
    public void putValue(String name, String value) {
        selectedConfiguration.put(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#removePropertyChangeListener
     * (org.eclipse.jface.util.IPropertyChangeListener)
     */
    @Override
    public void removePropertyChangeListener(IPropertyChangeListener listener) {
        this.propertyChangeListeners.remove(listener);
    }

    @Override
    public void removeConfigurationChangeListener(
            IConfigurationChangeListener listener) {
        this.configurationChangeListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , double)
     */
    @Override
    public void setDefault(String name, double value) {
        baseConfiguration.put(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , float)
     */
    @Override
    public void setDefault(String name, float value) {
        baseConfiguration.put(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , int)
     */
    @Override
    public void setDefault(String name, int value) {
        baseConfiguration.put(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , long)
     */
    @Override
    public void setDefault(String name, long value) {
        baseConfiguration.put(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , java.lang.String)
     */
    @Override
    public void setDefault(String name, String defaultObject) {
        baseConfiguration.put(name, defaultObject);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setDefault(java.lang.String
     * , boolean)
     */
    @Override
    public void setDefault(String name, boolean value) {
        baseConfiguration.put(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setToDefault(java.lang.
     * String)
     */
    @Override
    public void setToDefault(String name) {
        selectedConfiguration.put(name, baseConfiguration.get(name));
    }

    private void setValueInternal(String name, Object obj) {
        Object oldValue = selectedConfiguration.get(name);
        selectedConfiguration.put(name, obj);
        firePropertyChangeEvent(name, oldValue, obj);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * double)
     */
    @Override
    public void setValue(String name, double value) {
        setValueInternal(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * float)
     */
    @Override
    public void setValue(String name, float value) {
        setValueInternal(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * int)
     */
    @Override
    public void setValue(String name, int value) {
        setValueInternal(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * long)
     */
    @Override
    public void setValue(String name, long value) {
        setValueInternal(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * java.lang.String)
     */
    @Override
    public void setValue(String name, String value) {
        setValueInternal(name, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.IPreferenceStore#setValue(java.lang.String,
     * boolean)
     */
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

    @SuppressWarnings("unchecked")
    public String[] getStringArray(String name) {
        String[] result = null;
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            if (obj instanceof List) {
                List<String> list = (List<String>) obj;
                result = list.toArray(new String[list.size()]);
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

    @SuppressWarnings("unchecked")
    public Float[] getFloatArray(String name) {
        Float[] result = null;
        Object obj = selectedConfiguration.get(name);
        if (obj != null) {
            if (obj instanceof List) {
                List<Float> list = (List<Float>) obj;
                result = list.toArray(new Float[list.size()]);
            } else if (obj instanceof Float[]) {
                result = (Float[]) obj;
            }
        } else {
            return new Float[0];
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    public Integer[] getIntArray(String name) {
        Integer[] result = null;
        Object obj = null;
        obj = selectedConfiguration.get(name);
        if (obj != null) {
            if (obj instanceof List) {
                List<Integer> list = (List<Integer>) obj;
                result = list.toArray(new Integer[list.size()]);
            } else if (obj instanceof Integer[]) {
                result = (Integer[]) obj;
            }
        } else {
            return new Integer[0];
        }
        return result;
    }

    public void setValue(String name, String[] value) {
        setValueInternal(name, value);
    }

    public boolean isBoolean(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Boolean));
    }

    public boolean isInt(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Integer));
    }

    public boolean isFloat(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Float));
    }

    public boolean isDouble(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Double));
    }

    public boolean isLong(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof Long));
    }

    public boolean isString(String name) {
        Object obj = selectedConfiguration.get(name);
        return ((obj != null) && (obj instanceof String));
    }

    public boolean isStringArray(String name) {
        Object obj = selectedConfiguration.get(name);
        boolean result = false;
        if (obj != null) {
            if (obj instanceof String[]) {
                result = true;
            } else if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                if ((list.size() == 0) || (list.get(0) instanceof String)) {
                    result = true;
                }
            }
        }

        return result;
    }

    public boolean isFloatArray(String name) {
        Object obj = selectedConfiguration.get(name);
        boolean result = false;
        if (obj != null) {
            if (obj instanceof Float[]) {
                result = true;
            } else if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                if ((list.size() == 0) || (list.get(0) instanceof Float)) {
                    result = true;
                }
            }
        }

        return result;
    }

    public boolean isIntArray(String name) {
        Object obj = selectedConfiguration.get(name);
        boolean result = false;
        if (obj != null) {
            if (obj instanceof Integer[]) {
                result = true;
            } else if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                if ((list.size() == 0) || (list.get(0) instanceof Integer)) {
                    result = true;
                }
            }
        }

        return result;
    }

    /**
     * @return
     */
    public String getConfigName() {
        return configName;
    }

}
