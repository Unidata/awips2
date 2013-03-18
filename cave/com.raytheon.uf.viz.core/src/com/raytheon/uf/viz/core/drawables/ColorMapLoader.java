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

package com.raytheon.uf.viz.core.drawables;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Facilitates loading of colormaps
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 13, 2007             chammack    Initial Creation.
 * Aug 20, 2007             njensen     Added listColorMaps().
 * Aug 20, 2008				dglazesk    JiBX to JaXB
 * Aug 20, 2008				dglazesk    Updated for new ColorMap interface
 * Feb 25, 2013 DR15717     jzeng       Change CAVE_STATIC to COMMON_STATIC
 * </pre>
 * 
 * @author chammack
 * @version 1
 */

public class ColorMapLoader {

    private static final String sharedMutex = "";

    /* This class is used to cache the color maps and update them upon changes */
    private static class IColorMapObserver implements ILocalizationFileObserver {
        public IColorMap colorMap;

        private LocalizationFile file;

        private String name;

        public IColorMapObserver(String name, IColorMap colorMap,
                LocalizationFile file) {
            this.name = name;
            this.colorMap = colorMap;
            this.file = file;
            this.file.addFileUpdatedObserver(this);
        }

        @Override
        public void fileUpdated(FileUpdatedMessage message) {
            synchronized (sharedMutex) {
                switch (message.getChangeType()) {
                case DELETED: {
                    cachedMaps.remove(name);
                    break;
                }
                case ADDED: {
                    try {
                        colorMap = loadColorMap(name, file);
                    } catch (Exception e) {
                        return;
                    }
                    cachedMaps.put(name, this);
                    break;
                }
                case UPDATED: {
                    try {
                        colorMap = loadColorMap(name, file);
                    } catch (Exception e) {
                        cachedMaps.remove(name);
                    }
                    break;
                }
                }
            }
        }
    }

    /** Map of cached color maps **/
    private static Map<String, IColorMapObserver> cachedMaps = new HashMap<String, IColorMapObserver>();

    /**
     * Load a colormap by name
     * 
     * @param name
     *            name of the colormap
     * @return the colormap representation
     * @throws VizException
     */
    public static IColorMap loadColorMap(String name) throws VizException {
        IColorMap cm = null;
        IColorMapObserver cmo = null;
        synchronized (sharedMutex) {
            cmo = cachedMaps.get(name);
            cm = (cmo == null) ? null : cmo.colorMap;
        }
        if (cm == null) {
            try {
                LocalizationFile f = PathManagerFactory.getPathManager()
                        .getStaticLocalizationFile(
                                "colormaps" + IPathManager.SEPARATOR + name
                                        + ".cmap");
                if (f == null || !f.exists()) {
                    // If the file was not found check to see if the
                    // localization context is encoded as part of the path.
                    String[] split = name.split(IPathManager.SEPARATOR, 3);
                    for (LocalizationLevel level : LocalizationLevel.values()) {
                        if (level.name().equals(split[0])) {
                            LocalizationContext context = new LocalizationContext(
                                    LocalizationType.COMMON_STATIC, level,
                                    split[1]);
                            f = PathManagerFactory.getPathManager()
                                    .getLocalizationFile(
                                            context,
                                            "colormaps"
                                                    + IPathManager.SEPARATOR
                                                    + split[2] + ".cmap");
                            if (f == null) {
                                return loadColorMap(split[2]);
                            }
                        }
                    }
                }
                cm = loadColorMap(name, f);
                if (cm != null) {
                    cmo = new IColorMapObserver(name, cm, f);
                    cachedMaps.put(name, cmo);
                } else {
                    throw new VizException("Can't find colormap " + name);
                }
            } catch (SerializationException e) {
                throw new VizException("Exception while loading colormap "
                        + name, e);
            }
        }
        return cm;
    }

    public static LocalizationFile[] listColorMapFiles() {

        IPathManager pm = PathManagerFactory.getPathManager();
        Set<LocalizationContext> searchContexts = new HashSet<LocalizationContext>();

        searchContexts.addAll(Arrays.asList(pm
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC)));

        // Use of LocalizationLevels.values() in this case should be okay since
        // we are requesting all possible context names for the level, doesn't
        // matter if our local context for the level is set
        LocalizationLevel[] levels = pm.getAvailableLevels();
        for (LocalizationLevel level : levels) {
            if (level.isSystemLevel() == false) {
                String[] available = pm.getContextList(level);
                for (String s : available) {
                    LocalizationContext ctx = pm.getContext(
                            LocalizationType.COMMON_STATIC, level);
                    ctx.setContextName(s);
                    searchContexts.add(ctx);
                }
            }
        }

        LocalizationFile[] files = pm.listFiles(searchContexts
                .toArray(new LocalizationContext[searchContexts.size()]),
                "colormaps", new String[] { ".cmap" }, true, true);
        return files;
    }

    public static String shortenName(LocalizationFile file) {
        String name = file.getName()
                .replace("colormaps" + IPathManager.SEPARATOR, "")
                .replace(".cmap", "");
        if (!file.getContext().getLocalizationLevel()
                .equals(LocalizationLevel.BASE)) {
            String level = file.getContext().getLocalizationLevel().name();
            String context = file.getContext().getContextName();
            name = level + PathManager.SEPARATOR + context
                    + PathManager.SEPARATOR + name;
        }
        return name;
    }

    /**
     * Lists all the colormaps available in the colormaps dir
     * 
     * 
     * @return an array of all the colormap names
     */
    public static String[] listColorMaps(
            LocalizationContext.LocalizationLevel aType) {
        LocalizationFile[] files = listColorMapFiles();
        String[] cmaps = new String[files.length];
        for (int i = 0; i < files.length; i++) {
            cmaps[i] = shortenName(files[i]);
        }
        Arrays.sort(cmaps);
        return cmaps;
    }

    private static IColorMap loadColorMap(String name,
            LocalizationFile colorMapFile) throws SerializationException {
        if (colorMapFile != null) {
            ColorMap cm = (ColorMap) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(colorMapFile.getFile()
                            .getAbsolutePath());
            cm.setName(name);
            cm.setChanged(false);
            return cm;
        } else {
            return null;
        }
    }
}
