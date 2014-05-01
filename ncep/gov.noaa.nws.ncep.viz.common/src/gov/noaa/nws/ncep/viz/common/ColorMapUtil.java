package gov.noaa.nws.ncep.viz.common;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Facilitates loading of colormaps
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer     Description
 * ------------ ----------  -----------  --------------------------
 * 11/17/2009   187          Q.Zhou      Initial created.
 * 12/17/2009                G. Hull     placeholder for category
 * 01/02/2010   204			 M. Li	     check for Radar or Sat resource
 * 03/14/2010                B. Hebbard	 add path separator btw tblDir and rsc (2 places);
 *                                       fix circular build dependency on MosaicResource	
 * 03/21/2010   259          G. Hull     load by category        
 * 07/15/2011   450          G. Hull     use new NcPathManager 
 * 03/15/2012   621          S. Gurung   Added methods to read lockedColorMaps.tbl;
 * 										 load/check for locked colormaps.
 * 04/10/2013   #958         qzhou       Added SolarImage in getColorMapCategories.
 * 08/06/2013   2210         njensen     Moved colormaps to common_static
 * Nov 11, 2013 2361         njensen     Use ColorMap.JAXB for XML processing
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class ColorMapUtil {
    ColorMapUtil() {
    }

    /**
     * Load a colormap by name
     * 
     * @param name
     *            name of the colormap
     * @return the colormap representation
     * @throws VizException
     */
    public static IColorMap loadColorMap(String cat, String name)
            throws VizException {
        String cmapCat = cat.substring(0, 1) + cat.substring(1).toLowerCase();

        // Read lockedColorMaps.tbl to get the list of locked color maps
        LockedColorMaps lockedCmaps = readLockedColorMapFile();
        if (lockedCmaps != null && lockedCmaps.isLocked(name)) {
            return loadLockedColorMap(cat, name);
        } else {

            try {
                File f = NcPathManager.getInstance().getStaticFile(
                        NcPathConstants.COLORMAPS_DIR + File.separator
                                + cmapCat + File.separator + name + ".cmap");

                if (f != null) {
                    ColorMap cm = ColorMap.JAXB.unmarshalFromXmlFile(f
                            .getAbsolutePath());

                    cm.setName(name);
                    return cm;
                } else {
                    throw new VizException("Can't find colormap dude " + name);
                }
            } catch (SerializationException e) {
                throw new VizException("Unable to parse colormap " + name, e);
            }
        }
    }

    public static IColorMap loadColorMap(String cat, String name, boolean locked)
            throws VizException {
        if (locked)
            return loadLockedColorMap(cat, name);
        else
            return loadColorMap(cat, name);
    }

    // TODO Add logic for if the colormap is in the base/site level and provide
    // appropriate confirmation msg
    public static boolean colorMapExists(String cat, String name) {
        String fname = name;
        if (!name.endsWith(".cmap")) {
            fname = fname + ".cmap";
        }
        File f = NcPathManager.getInstance().getStaticFile(
                NcPathConstants.COLORMAPS_DIR + File.separator + cat
                        + File.separator + fname);

        return (f != null && f.exists());
    }

    // TODO : read from Localization
    public static String[] getColorMapCategories() {
        return new String[] { "Satellite", "Solarimage", "Radar", "Other" };
    }

    /**
     * Lists all the colormaps available in the colormaps dir
     * 
     * 
     * @return an array of all the colormap names
     */
    public static String[] listColorMaps(String cat) {

        NcPathManager pathMngr = NcPathManager.getInstance();

        String cmapCat = cat.substring(0, 1) + cat.substring(1).toLowerCase();

        Set<LocalizationContext> searchContexts = new HashSet<LocalizationContext>();
        searchContexts.addAll(Arrays.asList(pathMngr
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC)));

        Map<String, LocalizationFile> lFiles = pathMngr.listFiles(
                NcPathConstants.COLORMAPS_DIR + File.separator + cmapCat,
                new String[] { "cmap" }, false, true);

        ArrayList<String> cmaps = new ArrayList<String>(lFiles.size());

        for (LocalizationFile lFile : lFiles.values()) {
            if (!lFile.getFile().exists()) {
                System.out.println("cmap file " + lFile.getName()
                        + " doesn't exist???");
            }

            // lFile.getName() is /luts/Satellite/<fname> in case we want to
            // save this
            String fname = lFile.getFile().getName();
            cmaps.add(fname.substring(0, fname.lastIndexOf(".")));
        }

        String[] cmapArr = cmaps.toArray(new String[0]);
        Arrays.sort(cmapArr);
        return cmapArr;
    }

    // assume that check/prompt for overwriting has already been done.
    //
    public static void saveColorMap(ColorMap colorMap, String cmapCat,
            String cmapName) throws VizException {

        String cmapFileName = NcPathConstants.COLORMAPS_DIR + File.separator
                + cmapCat + File.separator + cmapName;
        if (!cmapFileName.endsWith(".cmap")) {
            cmapFileName += ".cmap";
        }

        // Prompt to also save to DESK? or determine if we are logged in 'as' a
        // DESK and only
        // save to desk?
        //
        LocalizationContext context = NcPathManager.getInstance().getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
        LocalizationFile lclFile = NcPathManager.getInstance()
                .getLocalizationFile(context, cmapFileName);
        File cmapFile = lclFile.getFile();

        try {
            ColorMap.JAXB
                    .marshalToXmlFile(colorMap, cmapFile.getAbsolutePath());

            lclFile.save();

        } catch (SerializationException e) {
            throw new VizException("Unable to Marshal ColorMap "
                    + colorMap.getName());
        } catch (LocalizationOpFailedException lofe) {
            throw new VizException("Unable to Localize ColorMap "
                    + colorMap.getName());
        }

    }

    // TODO : add code to check for BASE/system context
    public static void deleteColorMap(String cmapCat, String cmapName)
            throws VizException {
        String cmapFilename = cmapName;

        if (!cmapFilename.endsWith(".cmap")) {
            cmapFilename += ".cmap";
        }

        LocalizationFile cmapFile = NcPathManager.getInstance()
                .getStaticLocalizationFile(
                        NcPathConstants.COLORMAPS_DIR + File.separator
                                + cmapCat + File.separator + cmapFilename);

        if (cmapFile.getContext().getLocalizationLevel() != LocalizationLevel.USER) {
            throw new VizException("Can't delete a Colormap localized at the "
                    + cmapFile.getContext().getLocalizationLevel().toString()
                    + " level");
        }

        // TODO : check if there is a BASE/SITE/DESK level colormap by the same
        // name and
        // inform the user that they will be reverting to another version of the
        // file.

        try {
            cmapFile.delete();
        } catch (LocalizationOpFailedException e) {
            throw new VizException(e);
        }
    }

    /**
     * 
     * Return a pointer to the lockedColorMaps.tbl localization file
     * 
     * @return an absolute pointer on the filesystem to the file
     */
    public static File getLockedColorMapFile() {

        Map<LocalizationLevel, LocalizationFile> files = PathManagerFactory
                .getPathManager().getTieredLocalizationFile(
                        LocalizationType.CAVE_STATIC,
                        NcPathConstants.LOCKED_CMAP_TBL);

        File locCmapFile = null;

        if (files != null) {
            locCmapFile = files.get(LocalizationLevel.BASE).getFile();
            if (files.containsKey(LocalizationLevel.SITE)) {
                locCmapFile = files.get(LocalizationLevel.SITE).getFile();
            }
        }

        return locCmapFile;
    }

    // Read lockedColorMaps.tbl localization file
    public static LockedColorMaps readLockedColorMapFile() {
        File locCmapFile = ColorMapUtil.getLockedColorMapFile();

        if (locCmapFile == null || !locCmapFile.exists()) {
            return null;
        }

        LockedColorMaps lockedCmaps = null;
        try {
            lockedCmaps = new LockedColorMaps(locCmapFile.getAbsolutePath());

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return lockedCmaps;
    }

    /**
     * Load a locked colormap by name
     * 
     * @param name
     *            name of the colormap
     * @return the colormap representation
     * @throws VizException
     */
    public static IColorMap loadLockedColorMap(String cat, String name)
            throws VizException {
        String cmapCat = cat.substring(0, 1) + cat.substring(1).toLowerCase();

        try {

            Map<LocalizationLevel, LocalizationFile> files = PathManagerFactory
                    .getPathManager()
                    .getTieredLocalizationFile(
                            LocalizationType.COMMON_STATIC,
                            NcPathConstants.COLORMAPS_DIR + File.separator
                                    + cmapCat + File.separator + name + ".cmap");

            File f = null;

            if (files != null) {
                f = files.get(LocalizationLevel.BASE).getFile();
                if (files.containsKey(LocalizationLevel.SITE)) {
                    f = files.get(LocalizationLevel.SITE).getFile();
                }
                ColorMap cm = ColorMap.JAXB.unmarshalFromXmlFile(f
                        .getAbsolutePath());
                cm.setName(name);
                return cm;
            } else {
                throw new VizException("Can't find colormap " + name);
            }
        } catch (SerializationException e) {
            throw new VizException("Unable to parse colormap " + name, e);
        }
    }

}