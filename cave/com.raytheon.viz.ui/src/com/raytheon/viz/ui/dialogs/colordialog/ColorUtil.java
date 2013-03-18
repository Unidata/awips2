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

package com.raytheon.viz.ui.dialogs.colordialog;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Util methods for colormaps.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2007            njensen     Initial creation	
 * Aug 20, 2008			   dglazesk    Updated for the new ColorMap interface
 * 									   and for the JiBX to JaXB transition
 * Mar 07, 2013  15717     jzeng       Change CAVE_STATIC to COMMON_STATIC
 * 
 * </pre>
 * 
 * @author njensen
 */
public class ColorUtil {

    private static final String COLORMAPS_DIR = "colormaps";

    public static final float MAX_VALUE = 255.0f;

    /**
     * Updates a ColorMap with the values in an ArrayList<ColorData>
     * 
     * @param aData
     *            the data to update the colormap to
     * @param aMapToUpdate
     *            the colormap to update
     * @return the updated colormap
     */
    public static ColorMap updateColorMap(List<ColorData> aData,
            ColorMap aMapToUpdate) {
        int size = aData.size();
        float[] r = new float[size];
        float[] g = new float[size];
        float[] b = new float[size];
        float[] a = new float[size];
        for (int i = 0; i < size; i++) {
            ColorData cd = aData.get(i);
            r[i] = cd.rgbColor.red / MAX_VALUE;
            g[i] = cd.rgbColor.green / MAX_VALUE;
            b[i] = cd.rgbColor.blue / MAX_VALUE;
            a[i] = cd.alphaValue / MAX_VALUE;
        }

        aMapToUpdate.setRed(r);
        aMapToUpdate.setGreen(g);
        aMapToUpdate.setBlue(b);
        aMapToUpdate.setAlpha(a);

        return aMapToUpdate;
    }

    /**
     * Builds a ColorMap from an ArrayList<ColorData>
     * 
     * @param aData
     *            the colors to build the colormap with
     * @param aName
     *            the name of the new colormap
     * @return a new colormap
     */
    public static ColorMap buildColorMap(List<ColorData> aData, String aName) {
        int size = aData.size();
        float[] r = new float[size];
        float[] g = new float[size];
        float[] b = new float[size];
        float[] a = new float[size];
        for (int i = 0; i < size; i++) {
            ColorData cd = aData.get(i);
            r[i] = cd.rgbColor.red / MAX_VALUE;
            g[i] = cd.rgbColor.green / MAX_VALUE;
            b[i] = cd.rgbColor.blue / MAX_VALUE;
            a[i] = cd.alphaValue / MAX_VALUE;
        }

        return new ColorMap(aName, r, g, b, a);

    }

    /**
     * Builds an ArrayList<ColorData> from a ColorMap
     * 
     * @param aColorMap
     *            the ColorMap to extract ColorData from
     * @return
     */
    public static ArrayList<ColorData> buildColorData(ColorMap aColorMap) {
        ArrayList<ColorData> colors = new ArrayList<ColorData>();

        if (aColorMap != null) {
            for (Color c : aColorMap.getColors()) {
                RGB rgb = new RGB(Math.round(c.getRed() * MAX_VALUE),
                        Math.round(c.getGreen() * MAX_VALUE), Math.round(c
                                .getBlue() * MAX_VALUE));
                colors.add(new ColorData(rgb, Math.round(c.getAlpha()
                        * MAX_VALUE)));
            }
        }
        return colors;
    }

    /**
     * Saves a ColorMap as an xml file to a local path
     * 
     * @param aColorMap
     *            the ColorMap to save
     * @param aFilePath
     *            the full path name to save to
     * @return the name of the colormap
     * @throws VizException
     */
    public static String saveColorMapLocal(ColorMap aColorMap,
            String aColorMapName, boolean aSiteContext) throws VizException {
        String filename = aColorMapName;
        String newColormapName = new String(aColorMapName);
        if (!filename.endsWith(".cmap")) {
            filename += ".cmap";
        }

        File path = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        if (aSiteContext) {
            path = pm.getFile(pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.SITE), COLORMAPS_DIR + File.separator
                    + filename);

            checkDir(path.getParentFile());

        } else {
            path = pm.getFile(pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.USER), COLORMAPS_DIR + File.separator
                    + filename);
            checkDir(path.getParentFile());
        }

        try {
            SerializationUtil.jaxbMarshalToXmlFile(aColorMap, path.toString());
        } catch (SerializationException e) {
            throw new VizException("Unable to serialize ColorMap "
                    + aColorMap.getName(), e);
        }
        return newColormapName;
    }

    /**
     * check if color map exists for: site if bool is true, user if false
     * 
     * @param aColorMapName
     * @param aSiteContext
     * @return
     */
    public static boolean checkColorMapLocal(String aColorMapName,
            boolean aSiteContext) {
        String filename = aColorMapName;
        if (!filename.endsWith(".cmap")) {
            filename += ".cmap";
        }

        File path = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        if (aSiteContext) {
            path = pm.getFile(pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.SITE), COLORMAPS_DIR + File.separator
                    + filename);

            return path.exists();
        } else {
            path = pm.getFile(pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.USER), COLORMAPS_DIR + File.separator
                    + filename);
            return path.exists();
        }

    }

    /**
     * Checks if a path exists and if not, creates the necessary directories
     * 
     * @param aDirPath
     */
    private static void checkDir(File file) {
        if (!file.exists()) {
            file.mkdirs();
        }
    }

    /**
     * Saves a ColorMap to EDEX through the Localization Manager
     * 
     * @param aColorMap
     *            the ColorMap to save
     * @param aFilename
     *            the name of the file
     * @throws VizException
     */
    public static void saveColorMapServer(ColorMap aColorMap, String aFilename,
            boolean aSiteContext) throws LocalizationOpFailedException {
        String filename = aFilename;
        if (!filename.endsWith(".cmap")) {
            filename += ".cmap";
        }

        String xml;
        try {
            // JAXB marshaling
            xml = SerializationUtil.marshalToXml(aColorMap);
        } catch (JAXBException e1) {
            throw new LocalizationOpFailedException(
                    "Unable to Marshal colormap " + aColorMap.getName(), e1);
        }

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = null;
        if (aSiteContext) {
            context = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.SITE);
        } else {
            context = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.USER);
        }
        // use / for standard localization of File.separator
        filename = "colormaps/" + filename;

        LocalizationFile localizationFile = pathMgr.getLocalizationFile(
                context, filename);
        try {
            FileUtil.bytes2File(xml.getBytes(), localizationFile.getFile());
        } catch (IOException e) {
            throw new LocalizationOpFailedException("Unable to save file", e);
        }

        localizationFile.save();
    }

    /**
     * Deletes a color map from the local colormaps user context dir and
     * synchronizes with server
     * 
     * @param aColorMapName
     *            the name of the colormap to delete
     */
    public static void deleteColorMap(String aColorMapName)
            throws LocalizationOpFailedException {
        String filename = aColorMapName;
        if (!filename.endsWith(".cmap")) {
            filename += ".cmap";
        }
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile lfile = pm.getLocalizationFile(pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER),
                COLORMAPS_DIR + File.separator + filename);
        File file = lfile.getFile();

        if (lfile.exists()) {
            lfile.delete();
        }

        if (file.exists()) {
            file.delete();
        }
    }
}
