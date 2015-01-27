package com.raytheon.uf.viz.collaboration.ui.colors;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;
import com.raytheon.uf.viz.collaboration.display.data.ColorInfoMap;
import com.raytheon.uf.viz.collaboration.display.data.UserColorInfo;
import com.raytheon.uf.viz.collaboration.ui.Activator;

/**
 * Abstract class for persisting user color configuration to localization
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2014 3709       mapeters    Initial creation.
 * Dec 09, 2014 3709       mapeters    setColors() sets foreground and background together.
 * Jan 09, 2015 3709       bclement    renamed from AbstractColorConfigManager
 *                                      moved colorInfoMap from subclasses to here
 * Jan 13, 2015 3709       bclement    renamed to PersistentColorConfigStorage, now a utility
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public abstract class PersistentColorConfigStorage<T extends IUser> {

    protected static final String CONFIG_DIR_NAME = "collaboration";

    private static final SingleTypeJAXBManager<ColorInfoMap> jaxb = SingleTypeJAXBManager
            .createWithoutException(ColorInfoMap.class);

    /**
     * Persist color mapping configuration to localization file
     * 
     * @param colorInfoMap
     * @param filePath
     */
    public void persistColors(Map<T, UserColorInfo> map, String filePath) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pathMgr.getLocalizationFile(lContext, filePath);
        try {
            jaxb.marshalToXmlFile(createStorageMap(map), file.getFile()
                    .getPath());
            file.save();
        } catch (Exception e) {
            Activator.statusHandler.error(
                    "Unable to write color information to file: "
                            + file.getName() + " in context " + lContext, e);
        }
    }

    /**
     * Convert runtime map to a map that can be serialized to storage
     * 
     * @param colorInfoMap
     * @return
     */
    protected ColorInfoMap createStorageMap(Map<T, UserColorInfo> colorInfoMap) {
        Map<String, UserColorInfo> rval = new HashMap<>(colorInfoMap.size());
        for (Entry<T, UserColorInfo> entry : colorInfoMap.entrySet()) {
            rval.put(convert(entry.getKey()), entry.getValue());
        }
        return new ColorInfoMap(rval);
    }

    /**
     * Convert map from storage to runtime map
     * 
     * @param persisted
     * @return
     */
    public Map<T, UserColorInfo> unpackStorageMap(
            Map<String, UserColorInfo> persisted) {
        Map<T, UserColorInfo> rval = new HashMap<>(persisted.size());
        for (Entry<String, UserColorInfo> entry : persisted.entrySet()) {
            rval.put(convert(entry.getKey()), entry.getValue());
        }
        return rval;
    }

    /**
     * Convert user object to string key for storage
     * 
     * @param user
     * @return
     */
    protected String convert(T user) {
        return user.getClientIndependentId();
    }

    /**
     * Convert persisted key to user object
     * 
     * @param persisted
     * @return
     */
    protected abstract T convert(String persisted);

    /**
     * Get the color mapping configuration from localization
     * 
     * @param filePath
     * @return empty map if file does not exists in localization
     */
    public Map<T, UserColorInfo> getColors(String filePath) {
        IPathManager pm = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext locContext = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pm.getLocalizationFile(locContext, filePath);
        Map<String, UserColorInfo> rval = null;
        if (file != null && file.exists()) {
            try {
                ColorInfoMap map = jaxb.unmarshalFromXmlFile(file.getFile());
                if (map != null) {
                    rval = map.getColors();
                }
            } catch (SerializationException e) {
                Activator.statusHandler.error(
                        "Unable to read color information from file: "
                                + file.getName() + " in level "
                                + LocalizationLevel.USER, e);
            }
        }
        if (rval == null) {
            rval = new HashMap<>();
        }
        return unpackStorageMap(rval);
    }

}
