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

package com.raytheon.edex.util.grib;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.util.DataFieldTableLookup;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.parameter.Parameter;
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
public class GribParamTranslator {

    /** The singleton instance */
    private static GribParamTranslator instance;

    /** The map of parameter translations for grib 1 parameters */
    private Map<String, String> grib1Map;

    /** The map of parameter translations for grib 2 parameters */
    private Map<String, String> grib2Map;

    /** The map of parameter aliases */
    private Map<String, Map<String, String>> parameterNameMap;

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public static synchronized GribParamTranslator getInstance() {
        if (instance == null) {
            instance = new GribParamTranslator();
        }
        return instance;
    }

    /**
     * Creates the singleton instance
     */
    private GribParamTranslator() {
        try {
            initGrib1Lookup();
        } catch (GribException e) {
            e.printStackTrace();
        }

        try {
            initGrib2Lookup();
        } catch (GribException e) {
            e.printStackTrace();
        }

        try {
            initParameterAliases();
        } catch (GribException e) {
            e.printStackTrace();
        }
    }

    public void getParameterNameAlias(String modelName, Parameter parameter) {
        Map<String, String> modelMap = parameterNameMap.get(modelName);
        if (modelMap != null) {
            String newName = modelMap.get(parameter.getAbbreviation());
            if (newName != null) {
                parameter.setName(newName);
                return;
            }
        }
        String newName = DataFieldTableLookup.getInstance().lookupName(
                parameter.getAbbreviation());
        if (newName != null) {
            parameter.setName(newName);
        }
    }

    /**
     * Translates a grib parameter if necessary
     * 
     * @param gribVersion
     *            The version of the grib. Necessary so the correct table is
     *            used for translations
     * @param parName
     *            the original parameter abbreviation from the grib file
     * @param center
     *            the center from the grib data
     * @param subcenter
     *            the subcenter from the grib data
     * @param genProcess
     *            the genProcess of the grib data.
     * @param dataTime
     *            The datatime of the grib data the geospatial location of the
     *            grib data, if this model is being subgridded then this expects
     *            the full coverage, not the subgridded coverage.
     * @return The translated grib parameter
     */
    public String translateParameter(int gribVersion, String parName,
            int center, int subcenter, int genProcess, DataTime dataTime,
            GridCoverage location) {

        String pcs = parName + "_"
                + getGenProcess(center, subcenter, genProcess);
        String centerName = getCenterName(center, subcenter);
        String subcenterName = getSubcenterName(center, subcenter);
        if (centerName != null && !centerName.equals("NONE")) {
            pcs += "-" + centerName;
        }

        if (subcenterName != null && !subcenterName.equals("NONE")) {
            pcs += "-" + subcenterName;
        }

        String dimstr = "_" + location.getNx() + "x" + location.getNy();

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
            return grib1Map;
        } else {
            return grib2Map;
        }
    }

    /**
     * Gets the name of the center for the parameter contained in the provided
     * center and subcenter
     * 
     * @param center
     * @param subcenter
     * @return The center name
     */
    private String getCenterName(int center, int subcenter) {
        return (String) GribTableLookup.getInstance().getTableValue(center,
                subcenter, "0", center);
    }

    /**
     * Gets the name of the sub-center for the parameter contained in the
     * provided center and subcenter
     * 
     * @param center
     * @param subcenter
     * @return The sub-center name
     */
    private String getSubcenterName(int center, int subcenter) {
        return (String) GribTableLookup.getInstance().getTableValue(center,
                subcenter, "C-center" + center, subcenter);
    }

    /**
     * Gets the name of the generating process contained in the provided grib
     * center and subcenter
     * 
     * @param center
     * @param subcenter
     * @return The generating process name
     */
    private String getGenProcess(int center, int subcenter, int genProcess) {

        return (String) GribTableLookup.getInstance().getTableValue(center,
                subcenter, "A-center" + center, genProcess);
    }

    /**
     * Initializes the grib 2 parameter translation map
     * 
     * @throws GribException
     *             If errors occur
     */
    private void initGrib2Lookup() throws GribException {
        grib2Map = new HashMap<String, String>();

        Map<LocalizationLevel, LocalizationFile> files = PathManagerFactory
                .getPathManager().getTieredLocalizationFile(
                        LocalizationType.COMMON_STATIC,
                        "grid" + File.separator + "master_grib2_lookup.txt");
        loadDefs(files.get(LocalizationLevel.BASE).getFile(), 2);
        if (files.containsKey(LocalizationLevel.SITE)) {
            loadDefs(files.get(LocalizationLevel.SITE).getFile(), 2);
        }
    }

    /**
     * Initializes the grib 1 parameter translation map
     * 
     * @throws GribException
     *             If errors occur
     */
    private void initGrib1Lookup() throws GribException {
        grib1Map = new HashMap<String, String>();
        Map<LocalizationLevel, LocalizationFile> files = PathManagerFactory
                .getPathManager().getTieredLocalizationFile(
                        LocalizationType.COMMON_STATIC,
                        "grid" + File.separator + "master_grib1_lookup.txt");
        loadDefs(files.get(LocalizationLevel.BASE).getFile(), 1);
        if (files.containsKey(LocalizationLevel.SITE)) {
            loadDefs(files.get(LocalizationLevel.SITE).getFile(), 1);
        }
    }

    private void initParameterAliases() throws GribException {
        parameterNameMap = new HashMap<String, Map<String, String>>();
        Map<LocalizationLevel, LocalizationFile> files = PathManagerFactory
                .getPathManager().getTieredLocalizationFile(
                        LocalizationType.COMMON_STATIC,
                        "grid" + File.separator + "parameterNameAlias.txt");
        loadParameterNameAliases(files.get(LocalizationLevel.BASE).getFile());
        if (files.containsKey(LocalizationLevel.SITE)) {
            loadParameterNameAliases(files.get(LocalizationLevel.SITE)
                    .getFile());
        }
    }

    /**
     * Loads paramter aliases from the grib master lookup file
     * 
     * @param lookupFile
     *            The lookup file to parse
     * @param gribVersion
     *            The grib version
     * @throws GribException
     *             If errors occur while processing the file
     */
    private void loadDefs(File lookupFile, int gribVersion)
            throws GribException {
        BufferedReader in = null;
        String[] tokens = null;
        try {
            in = new BufferedReader(new FileReader(lookupFile));
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
                if (gribVersion == 1) {
                    grib1Map.put(tokens[0], tokens[tokens.length - 1]);
                } else {
                    grib2Map.put(tokens[0], tokens[tokens.length - 1]);
                }
            }
        } catch (IOException e) {
            throw new GribException(
                    "Error processing master grib parameters file", e);
        }

        try {
            in.close();
        } catch (IOException e) {
            throw new GribException(
                    "Error processing master grib parameters file", e);
        }
    }

    private void loadParameterNameAliases(File lookupFile) throws GribException {
        BufferedReader in = null;
        String[] tokens = null;
        try {
            in = new BufferedReader(new FileReader(lookupFile));
            String str;

            /*
             * Reading in the file
             */
            while ((str = in.readLine()) != null) {
                str = str.trim();
                if (str.isEmpty() || str.startsWith("//")) {
                    continue;
                }

                tokens = str.split("::");
                if (tokens.length < 3) {
                    continue;
                }
                String modelName = tokens[0].trim();
                String parameterAbbreviation = tokens[1].trim();
                String parameterName = tokens[2].trim();
                if (!parameterNameMap.containsKey(modelName)) {
                    parameterNameMap.put(modelName,
                            new HashMap<String, String>());
                }
                parameterNameMap.get(modelName).put(parameterAbbreviation,
                        parameterName);

            }
        } catch (IOException e) {
            throw new GribException(
                    "Error processing master grib parameters file", e);
        }

        try {
            in.close();
        } catch (IOException e) {
            throw new GribException(
                    "Error processing master grib parameters file", e);
        }
    }
}
