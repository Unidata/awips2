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

package com.raytheon.uf.common.dataplugin.persist;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.beanutils.PropertyUtils;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURIConfig;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Default implementation of the IHDFFilePathProvider used for locating HDF5
 * data on the file system. This class should be used as the path provider for
 * plugins wishing to store HDF5 and use the rule based purge mechanism in
 * place.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/08/09      1674       bphillip    Initial creation
 * 04/08/13     1293       bkowal      Removed references to hdffileid.
 * 04/30/13     1861       bkowal      Added constant for hdf5 file suffix.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DefaultPathProvider implements IHDFFilePathProvider {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DefaultPathProvider.class);
    
    public static final String HDF5_SUFFIX = ".h5";

    public static final ThreadLocal<SimpleDateFormat> fileNameFormat = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat("-yyyy-MM-dd-HH");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sdf;
        }
    };

    /**
     * The list of keys used to construct the HDF5 directory path. These keys
     * are contained in the plugin pathKeys.xml file.
     */
    protected static ConcurrentHashMap<String, List<String>> keyMap = new ConcurrentHashMap<String, List<String>>();

    private static DefaultPathProvider instance = new DefaultPathProvider();

    public static DefaultPathProvider getInstance() {
        return instance;
    }

    protected DefaultPathProvider() {

    }

    @Override
    public String getHDFPath(String pluginName, IPersistable persistable) {
        StringBuilder pathBuilder = new StringBuilder();

        // Get the key names for this plugin
        List<String> keyNames = getKeyNames(pluginName);

        // If no keys are specified, return the path defaults to the path
        // separator character
        if (keyNames.isEmpty()) {
            pathBuilder.append(File.separator);
        }

        /*
         * If keys are specified, we must iterate through them to construct the
         * path. Reflection is used to get the values from the provided object
         */
        else {
            for (String key : keyNames) {
                try {
                    Object property = persistable;
                    pathBuilder.append(File.separator);

                    // If this keys refers to an embedded object, we must loop
                    // through to get the appropriate field
                    if (key.contains(".")) {
                        String[] subClasses = key.split("\\.");
                        for (String subClass : subClasses) {
                            property = PropertyUtils.getProperty(property,
                                    subClass);
                        }

                    }

                    // This key is not an embedded object meaning it is a field
                    // in the class passed in. We can get the value directly.
                    else {
                        try {
                            property = PropertyUtils.getProperty(persistable,
                                    key);
                        } catch (Throwable t) {
                            // Ignore
                            property = null;
                        }
                    }

                    // For times and dates, we must format them correctly
                    if (property instanceof Calendar) {
                        pathBuilder.append(TimeUtil
                                .formatCalendar((Calendar) property));
                    } else if (property instanceof Date) {
                        pathBuilder
                                .append(TimeUtil.formatDate((Date) property));
                    } else if (property != null) {
                        pathBuilder.append(property.toString());
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        return pathBuilder.toString();
    }

    @Override
    public List<String> getKeyNames(String pluginName) {

        if (pluginName == null) {
            return new ArrayList<String>(0);
        }
        // Retrieve the keys from the xml file if they have not already been
        // loaded
        if (!keyMap.containsKey(pluginName)) {
            try {
                unmarshalPathFile(pluginName);
            } catch (SerializationException e) {
                statusHandler.handle(Priority.ERROR,
                        "Failed to deserialze path key file", e);
                return new ArrayList<String>(0);
            }
        }
        return keyMap.get(pluginName);
    }

    /**
     * Unmarshalls the xml file containing the path keys for this plugin
     * 
     * @param pluginName
     *            The plugin to get the path keys for
     * @throws SerializationException
     *             If errors occur while unmarshalling the file due to syntax
     *             errors or this file does not specify path keys
     */
    private void unmarshalPathFile(String pluginName)
            throws SerializationException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
        LocalizationContext commonStaticSite = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.SITE);

        File basePathFile = pathMgr.getFile(commonStaticBase, "path"
                + File.separator + pluginName + "PathKeys.xml");
        File sitePathFile = pathMgr.getFile(commonStaticSite, "path"
                + File.separator + pluginName + "PathKeys.xml");

        PersistencePathKeySet pathKeySet = null;

        if (sitePathFile.exists()) {
            pathKeySet = (PersistencePathKeySet) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(sitePathFile);
        } else if (basePathFile.exists()) {
            pathKeySet = (PersistencePathKeySet) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(basePathFile);
        }

        List<String> keyNames = null;
        if (pathKeySet != null) {
            List<PersistencePathKey> keyList = pathKeySet.getPathKeys();
            Collections.sort(keyList);
            keyNames = new ArrayList<String>(keyList.size());

            for (int i = 0; i < keyList.size(); i++) {
                keyNames.add(keyList.get(i).getKey());
            }
        } else {
            keyNames = new ArrayList<String>(0);
        }

        keyMap.put(pluginName, keyNames);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.db.dao.IHDFFilePathProvider#getHDFFileName(java
     * .lang.String, com.raytheon.edex.plugin.IPersistable)
     */
    @Override
    public String getHDFFileName(String pluginName, IPersistable persistable) {
        if (pluginName == null) {
            throw new IllegalArgumentException(
                    "Expected argument pluginName not set on object "
                            + persistable.toString());
        }

        if (persistable instanceof PluginDataObject) {
            PluginDataObject pdo = (PluginDataObject) persistable;
            DataURIConfig config = pdo.getClass().getAnnotation(
                    DataURIConfig.class);
            int idx = 0;

            if (config != null) {
                idx = config.persistentIndex();
            }

            String[] dataURIParts = pdo.getDataURI().split("/");
            StringBuffer sb = new StringBuffer();
            sb.append(pluginName);

            for (int i = 0; i < idx; i++) {
                sb.append("-");
                sb.append(dataURIParts[i]);
            }

            Date refTime = ((PluginDataObject) persistable).getDataTime()
                    .getRefTime();
            sb.append(fileNameFormat.get().format(refTime));
            sb.append(".h5");
            return sb.toString();
        }

        return pluginName + ".h5";
    }
}