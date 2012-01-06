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

package gov.noaa.nws.ncep.edex.util.ncgrib;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.time.DataTime;

/**
 * Performs the optional translation of the grib parameter names
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/15/10      4553        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class NcgribParamTranslator {

    /** The singleton instance */
    private static NcgribParamTranslator instance;

    /** The map of parameter translations for grib 1 parameters */
    private Map<String, String> ncgrib1Map;

    /** The map of parameter translations for grib 2 parameters */
    private Map<String, String> ncgrib2Map;

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public static synchronized NcgribParamTranslator getInstance() {
        if (instance == null) {
            instance = new NcgribParamTranslator();
        }
        return instance;
    }

    /**
     * Creates the singleton instance
     */
    private NcgribParamTranslator() {
        try {
            initNcgrib1Lookup();
        } catch (GribException e) {
            e.printStackTrace();
        }

        try {
            initNcgrib2Lookup();
        } catch (GribException e) {
            e.printStackTrace();
        }
    }

    /**
     * Translates a grib parameter if necessary
     * 
     * @param gribVersion
     *            The version of the grib. Necessary so the correct table is
     *            used for translations
     * @param model
     *            The grib model object from the record
     * @param dataTime
     *            The datatime of the grib data
     * @return The translated grib parameter
     */
    public String translateParameter(int gribVersion, NcgribModel model,
            DataTime dataTime) {

        String parName = model.getParameterAbbreviation();
        String pcs = parName + "_" + getGenProcess(model);
        String centerName = getCenterName(model);
        String subcenterName = getSubcenterName(model);
        if (centerName != null && !centerName.equals("NONE")) {
            pcs += "-" + centerName;
        }

        if (subcenterName != null && !subcenterName.equals("NONE")) {
            pcs += "-" + subcenterName;
        }

        String dimstr = "_" + model.getLocation().getNx() + "x"
                + model.getLocation().getNy();

        long duration = dataTime.getValidPeriod().getDuration() / 1000;

        String durPert = "_" + String.valueOf(duration) + "-"
                + dataTime.getFcstTime() % 60;
        String fcststr = "_" + dataTime.getMatchFcst();

        String trnStr = getMap(gribVersion).get(
                pcs + dimstr + durPert + fcststr);
        if (trnStr == null) {
            trnStr = getMap(gribVersion).get(pcs + dimstr + durPert);
        }

        if (trnStr == null) {
            trnStr = getMap(gribVersion).get(pcs + durPert);
        }

        if (trnStr == null) {
            trnStr = getMap(gribVersion).get(parName + durPert);
        }

        if (trnStr == null) {
            trnStr = getMap(gribVersion).get(pcs);
        }

        if (trnStr == null) {
            trnStr = getMap(gribVersion).get(parName);
        }

        return trnStr;
    }

    /**
     * Gets the correct parameter map based on the grib version
     * 
     * @param version
     *            The grib version
     * @return The parameter translation map for the specified grib version
     */
    private Map<String, String> getMap(int version) {
        if (version == 1) {
            return ncgrib1Map;
        } else {
            return ncgrib2Map;
        }
    }

    /**
     * Gets the name of the center for the parameter contained in the provided
     * grib model object
     * 
     * @param model
     *            The grib model object
     * @return The center name
     */
    private String getCenterName(NcgribModel model) {
        int center = model.getCenterid();
        int subcenter = model.getSubcenterid();
        return (String) NcgribTableLookup.getInstance().getTableValue(center,
                subcenter, "0", center);
    }

    /**
     * Gets the name of the sub-center for the parameter contained in the
     * provided grib model object
     * 
     * @param model
     *            The grib model object
     * @return The sub-center name
     */
    private String getSubcenterName(NcgribModel model) {
        int center = model.getCenterid();
        int subcenter = model.getSubcenterid();

        return (String) NcgribTableLookup.getInstance().getTableValue(center,
                subcenter, "C-center" + center, subcenter);
    }

    /**
     * Gets the name of the generating process contained in the provided grib
     * model object
     * 
     * @param model
     *            The grib model object
     * @return The generating process name
     */
    private String getGenProcess(NcgribModel model) {
        int center = model.getCenterid();
        int subcenter = model.getSubcenterid();

        return (String) NcgribTableLookup.getInstance().getTableValue(center,
                subcenter, "A-center" + center, model.getGenprocess());
    }

    /**
     * Initializes the grib 2 parameter translation map
     * 
     * @throws GribException
     *             If errors occur
     */
    private void initNcgrib2Lookup() throws GribException {
        ncgrib2Map = new HashMap<String, String>();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        String path = "";
        try {
            path = pathMgr.getFile(commonStaticBase,
                    "ncgrid" + File.separator + "master_ncgrib2_lookup.txt")
                    .getCanonicalPath();

        } catch (IOException e) {
            throw new GribException("Error getting ncgrib model definitions", e);
        }

        BufferedReader in = null;
        String[] tokens = null;
        try {
            in = new BufferedReader(new FileReader(path));
            String str;

            /*
             * Reading in the file
             */
            while ((str = in.readLine()) != null) {
                str = str.trim();
                if (str.isEmpty() || str.startsWith("//")) {
                    continue;
                }

                tokens = str.split(" ");
                if (tokens.length < 2) {
                    continue;
                }
                ncgrib2Map.put(tokens[0], tokens[tokens.length - 1]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        try {
            in.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Initializes the grib 1 parameter translation map
     * 
     * @throws GribException
     *             If errors occur
     */
    private void initNcgrib1Lookup() throws GribException {
        ncgrib1Map = new HashMap<String, String>();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        String path = "";
        try {
            path = pathMgr.getFile(commonStaticBase,
                    "ncgrid" + File.separator + "master_ncgrib1_lookup.txt")
                    .getCanonicalPath();

        } catch (IOException e) {
            throw new GribException("Error getting ncgrib model definitions", e);
        }

        BufferedReader in = null;
        String[] tokens = null;
        try {
            in = new BufferedReader(new FileReader(path));
            String str;

            /*
             * Reading in the file
             */
            while ((str = in.readLine()) != null) {
                str = str.trim();
                if (str.isEmpty() || str.startsWith("//")) {
                    continue;
                }

                tokens = str.split(" ");
                if (tokens.length < 2) {
                    continue;
                }
                ncgrib1Map.put(tokens[0], tokens[tokens.length - 1]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        try {
            in.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
