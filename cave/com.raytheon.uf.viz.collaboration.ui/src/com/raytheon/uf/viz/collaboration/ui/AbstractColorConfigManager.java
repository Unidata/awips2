package com.raytheon.uf.viz.collaboration.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.viz.collaboration.ui.ColorInfoMap.ColorInfo;

/**
 * Abstract class collaboration chat coloring configuration managers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2014 3709       mapeters    Initial creation.
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
public abstract class AbstractColorConfigManager {

    private static final SingleTypeJAXBManager<ColorInfoMap> jaxb = SingleTypeJAXBManager
            .createWithoutException(ColorInfoMap.class);

    /**
     * Set and store the color type of the given user/site to be the given rgb
     * at the given file location. If creating new {@link ColorInfo} and setting
     * background, set foreground to defaultForeground to prevent it from
     * incorrectly defaulting.
     * 
     * @param key
     * @param type
     * @param rgb
     * @param defaultForeground
     * @param filePath
     */
    protected void setColor(String key, int type, RGB rgb,
            RGB defaultForeground, String filePath) {
        ColorInfoMap colorInfoMap = this.getColorInfoMap();
        if (colorInfoMap == null) {
            colorInfoMap = new ColorInfoMap();
            this.setColorInfoMap(colorInfoMap);
        }
        Map<String, ColorInfo> colors = colorInfoMap.getColors();
        if (colors == null) {
            colorInfoMap.setColors(new HashMap<String, ColorInfo>());
            colors = colorInfoMap.getColors();
        }

        ColorInfo colorInfo = colors.get(key);
        if (colorInfo != null) {
            colorInfo.setColor(type, rgb, defaultForeground);
        } else {
            ColorInfo color = new ColorInfo();
            color.setColor(type, rgb, defaultForeground);
            colors.put(key, color);
        }

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pathMgr.getLocalizationFile(lContext, filePath);
        try {
            jaxb.marshalToXmlFile(colorInfoMap, file.getFile().getPath());
            file.save();
        } catch (Exception e) {
            Activator.statusHandler.error(
                    "Unable to write color information to file: "
                            + file.getName() + " in context " + lContext, e);
        }
    }

    /**
     * Get the {@link ColorInfo} for the given user/site from memory.
     * 
     * @param key
     * @param filePath
     * @return
     */
    protected ColorInfo getColor(String key, String filePath) {
        ColorInfoMap colorInfoMap = this.getColorInfoMap();
        if (colorInfoMap == null) {
            IPathManager pm = (PathManager) PathManagerFactory.getPathManager();
            LocalizationContext locContext = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
            LocalizationFile file = pm
                    .getLocalizationFile(locContext, filePath);

            if (file != null && file.exists()) {
                try {
                    colorInfoMap = jaxb.unmarshalFromXmlFile(file.getFile());
                    this.setColorInfoMap(colorInfoMap);
                } catch (SerializationException e) {
                    Activator.statusHandler.error(
                            "Unable to read color information from file: "
                                    + file.getName() + " in level "
                                    + LocalizationLevel.USER, e);
                }
            }
        }
        if (colorInfoMap != null) {
            Map<String, ColorInfo> colors = colorInfoMap.getColors();
            if (colors != null) {
                return colors.get(key);
            }
        }
        return null;
    }

    public abstract void setColor(String key, int type, RGB rgb,
            RGB defaultForeground);

    public abstract ColorInfo getColor(String key);

    protected abstract ColorInfoMap getColorInfoMap();

    protected abstract void setColorInfoMap(ColorInfoMap colorInfo);
}
