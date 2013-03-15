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

package com.raytheon.edex.colormap;

import java.awt.image.IndexColorModel;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.edex.exception.ColorTableException;
import com.raytheon.uf.common.colormap.CMapFilenameFilter;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.image.Colormapper;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Manages colormaps.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2007            njensen     Initial creation
 * Aug 20, 2008			   dglazesk    JiBX replaced with JaXB
 * Aug 20, 2008			   dglazesk	   Updated for the new ColorMap interface
 * Feb 15, 2013 1638       mschenke    Moved IndexColorModel creation to common.colormap utility
 * Mar 14, 2013 1794       djohnson    FileUtil.listFiles now returns List.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class ColorMapManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ColorMapManager.class);

    public static final int NUMBER_BITS = Colormapper.COLOR_MODEL_NUMBER_BITS;

    public static final float MAX_VALUE = Colormapper.MAX_VALUE;

    private static ColorMapManager instance;

    private String baseColormapDir;

    private ColorMapManager() {

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        try {
            baseColormapDir = pathMgr.getFile(edexStaticBase, "colormaps")
                    .getCanonicalPath();
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error determining colormap dir", e);
        }

    }

    public static ColorMapManager getInstance() {
        if (instance == null) {
            instance = new ColorMapManager();
        }
        return instance;
    }

    /**
     * Loads a ColorMap with the specified name
     * 
     * @param aColorMap
     *            the name of the color map
     * @return the color map
     * @throws ColorTableException
     */
    public ColorMap getColorMap(String aColorMap) throws ColorTableException {
        ColorMap map = null;

        String baseFilePath = baseColormapDir + File.separator + aColorMap
                + ".cmap";
        File baseFile = new File(baseFilePath);
        if (baseFile.exists()) {

            try {
                map = (ColorMap) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(baseFilePath);
            } catch (SerializationException e) {
                throw new ColorTableException("Exception during serialization "
                        + baseFilePath, e);
            }

        } else {
            throw new ColorTableException("Can't find colormap named "
                    + aColorMap);
        }

        if (map != null) {
            map.setName(aColorMap);
        }

        return map;
    }

    /**
     * Lists the available base and site colormaps
     * 
     * @return
     */
    public String[] listColorMaps() {
        File colormapsDir = new File(baseColormapDir);
        List<File> files = FileUtil.listFiles(colormapsDir,
                new CMapFilenameFilter(), true);
        ArrayList<String> colormaps = new ArrayList<String>();
        for (int i = 0; i < files.size(); i++) {
            File file = files.get(i);
            String name = file.getAbsolutePath();
            int index = name.indexOf("base/colormaps/");
            name = name.substring(index + 15, name.length());
            name = name.substring(0, name.indexOf("."));
            colormaps.add(name);
        }

        String[] cmaps = colormaps.toArray(new String[colormaps.size()]);
        Arrays.sort(cmaps);
        return cmaps;
    }

    /**
     * Call {@link ColorMapUtils#buildColorModel(IColorMap)}
     * 
     * @param aColorMap
     * @return
     */
    @Deprecated
    public static IndexColorModel buildColorModel(IColorMap aColorMap) {
        return Colormapper.buildColorModel(aColorMap);
    }

}
