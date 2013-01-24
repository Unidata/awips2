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
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.util.GenProcess;
import com.raytheon.edex.plugin.grib.util.Grib1Parameter;
import com.raytheon.edex.plugin.grib.util.Grib1ParameterLookup;
import com.raytheon.edex.plugin.grib.util.GribLevel;
import com.raytheon.edex.plugin.grib.util.GribParameter;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Class used to access and manage data from any defined tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GribTableLookup {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** No center number */
    private static final int NO_CENTER = -1;

    private static final int NO_SUBCENTER = -1;

    /** The map of defined tables */
    private Map<Integer, Map<String, GribTable>> tableMap;

    /** The singleton instance */
    private static GribTableLookup instance;

    /**
     * Gets the singleton instance of GribTableLookup
     * 
     * @return The singleton instance of GribTableLookup
     */
    public static synchronized GribTableLookup getInstance() {
        if (instance == null) {
            Grib1ParameterLookup.getInstance();
            instance = new GribTableLookup();
        }
        return instance;
    }

    /**
     * Creates a new GribTableLookup object
     */
    private GribTableLookup() {
        tableMap = new HashMap<Integer, Map<String, GribTable>>();
        initTables();
    }

    /**
     * Gets the equivalent grib 2 parameter from the information extracted from
     * a grib 1 record
     * 
     * @param centerid
     *            The center ID
     * @param subcenterid
     *            The subcenter ID
     * @param grib1TableVersion
     *            The grib1 parameter table version
     * @param grib1Value
     *            The grib1 parameter table value
     * @return The grib 2 parameter
     */
    public GribParameter getGrib2Parameter(int centerid, int subcenterid,
            int grib1TableVersion, int grib1Value) {
        Grib1Parameter grib1Param = Grib1ParameterLookup.getInstance()
                .getParameter(centerid, grib1TableVersion, grib1Value);
        if (grib1Param != null) {
            String tableName = "4.2." + grib1Param.getGrib2discipline() + "."
                    + grib1Param.getGrib2category();
            return (GribParameter) getTableValue(centerid, subcenterid,
                    tableName, grib1Param.getGrib2Value());
        }
        return null;
    }

    /**
     * Gets a GribTable from the cache
     * 
     * @param centerid
     *            The center id defining the table
     * @param tableName
     *            The table name
     * @return The GribTable if present, else null
     */
    public GribTable getTable(int centerid, int subcenterid, String tableName) {
        if (!tableMap.containsKey(getTableMapKey(centerid, subcenterid))) {
            return null;
        }
        return tableMap.get(getTableMapKey(centerid, subcenterid)).get(
                tableName);
    }

    /**
     * Gets a value from a Grib Table.<br>
     * This method first checks any tables defined by a specific center. If not
     * found there, the common tables (i.e.tables not defined by a specific
     * center) are checked. If the value is still not found null is returned
     * 
     * @param centerid
     *            The center id defining the table
     * @param tableName
     *            The table name
     * @param value
     *            The key to look for
     * @return The value associated with the key in the specified table. Null if
     *         not found
     */
    public Object getTableValue(int centerid, int subcenterid,
            String tableName, int value) {

        GribTable table = getTable(centerid, subcenterid, tableName);
        if (table == null) {
            return getCommonValue(tableName, value);
        } else {
            if (table.contains(value)) {
                return table.get(value);
            } else {
                return getCommonValue(tableName, value);
            }
        }
    }

    /**
     * Gets a value from a non center-specific table
     * 
     * @param tableName
     *            The name of the table
     * @param value
     *            The key to look for
     * @return The value in the table associated with the provided key. Null if
     *         not present
     */
    public Object getCommonValue(String tableName, int value) {
        GribTable table = getTable(NO_CENTER, NO_SUBCENTER, tableName);
        if (table == null) {
            return null;
        } else {
            return table.get(value);
        }
    }

    /**
     * Initializes predefined tables. The predefined tables are stored as flat
     * files in the utility directy to be access by the localization service
     * <p>
     * The localization service reads in all files and populates the cached
     * tables accordingly
     */
    private void initTables() {

        /*
         * Gets all predefined found in the utility directory
         */
        IPathManager pm = PathManagerFactory.getPathManager();
        String commonPath = pm.getFile(
                pm.getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.BASE), "/grib/tables").getPath();

        String sitePath = pm.getFile(
                pm.getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.SITE), "/grib/tables").getPath();

        initTablesFromPath(commonPath);
        initTablesFromPath(sitePath);

    }

    private void initTablesFromPath(String commonPath) {
        FilenameFilter filter = new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return new File(dir.getPath() + File.separator + name)
                        .isDirectory();
            }
        };

        FilenameFilter tableFilter = new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".table");
            }
        };

        ArrayList<File> files = FileUtil.listFiles(new File(commonPath),
                filter, false);
        for (File f : files) {
            int center = 0;
            try {
                center = Integer.parseInt(f.getPath().substring(
                        f.getPath().lastIndexOf("/") + 1));

                ArrayList<File> files2 = FileUtil.listFiles(new File(commonPath
                        + File.separator + String.valueOf(center)), filter,
                        true);

                int subcenter = 0;
                for (File f2 : files2) {
                    if (!f2.getPath().contains(".svn")) {
                        subcenter = Integer.parseInt(f2.getPath().substring(
                                f2.getPath().lastIndexOf("/") + 1));
                        ArrayList<File> tableFiles = FileUtil.listFiles(f2,
                                tableFilter, false);
                        String tableName = null;
                        for (File table : tableFiles) {
                            tableName = table.getName().replace(".table", "")
                                    .trim();
                            try {
                                createTable(table, center, subcenter, tableName);
                            } catch (GribException e) {
                                logger.error("Unable to create table: "
                                        + tableName + " for Center: " + center
                                        + " Subcenter: " + subcenter);
                                continue;
                            }
                        }

                    }
                }
            } catch (NumberFormatException e) {
                continue;
            }
        }
    }

    /**
     * Creates and stores a GribTable from a flat file
     * 
     * @param file
     *            The flat file to read from
     * @return The GribTable generated from the flat file
     * @throws GribException
     *             If problems occur reading the file or creating the GribTable
     */
    private GribTable createTable(File file, int center, int subcenter,
            String tableName) throws GribException {
        GribTable table = new GribTable(center, subcenter, tableName);
        BufferedReader in = null;
        String[] tokens = null;
        try {
            in = new BufferedReader(new FileReader(file));
            String str;

            /*
             * Reading in the file
             */
            while ((str = in.readLine()) != null) {
                str = str.trim();
                if (str.isEmpty() || str.startsWith("#")) {
                    continue;
                }

                tokens = str.split(":");

                /*
                 * Special handling of parameter tables
                 */
                if (tableName.startsWith("4.2.")) {
                    String name = null, abbreviation = null, d2dAbbrev = null, unit = null;
                    if (tokens.length > 2) {
                        name = tokens[2].trim();
                        if (tokens.length > 3) {
                            unit = tokens[3].trim();
                            if (tokens.length > 4) {
                                abbreviation = tokens[4].trim();
                                if (tokens.length > 5) {
                                    d2dAbbrev = tokens[5].trim();
                                }
                            }
                        }
                    }
                    String[] tableTokens = tableName.split("\\.");

                    GribParameter param = null;
                    int number = Integer.parseInt(tokens[0]);
                    param = new GribParameter(center, subcenter,
                            Integer.parseInt(tableTokens[2]),
                            Integer.parseInt(tableTokens[3]), number, name,
                            abbreviation, unit, d2dAbbrev);

                    table.addEntry(Integer.valueOf(tokens[0]), param);
                }
                /*
                 * Special handling of surface table
                 */
                else if (tableName.startsWith("4.5")) {
                    int number = Integer.parseInt(tokens[0]);
                    String name = null, unit = null, abbreviation = null, d2dAbbrev = null;
                    name = tokens[2].trim();
                    if (tokens.length > 3) {
                        unit = tokens[3].trim();
                        if (tokens.length > 4) {
                            abbreviation = tokens[4].trim();
                            if (tokens.length > 5) {
                                d2dAbbrev = tokens[5].trim();
                            }
                        }
                    }
                    GribLevel surface = null;
                    surface = new GribLevel(center, subcenter, number, name,
                            unit, abbreviation, d2dAbbrev);

                    // check to make sure we have a level defined for this
                    // surface, if its not mapped this will insert a bare bones
                    // level
                    if (abbreviation != null && abbreviation.length() > 0) {
                        MasterLevel level = new MasterLevel(abbreviation);
                        level.setDescription(name);
                        level.setUnitString(unit);
                        LevelFactory.getInstance().checkMasterLevel(level);
                    }

                    table.addEntry(number, surface);
                }
                /*
                 * Special handling of generating process table
                 */
                else if (tableName.equals("A")) {
                    int number = Integer.parseInt(tokens[0]);
                    String description = tokens[2];
                    String name = null;
                    if (tokens.length == 4) {
                        name = tokens[3];
                    }
                    GenProcess process = new GenProcess(center, subcenter,
                            number, name, description);
                    table.addEntry(Integer.valueOf(tokens[0]), process);
                } else {
                    table.addEntry(Integer.valueOf(tokens[0]), tokens[2]);
                }

            }
        } catch (Exception e) {
            throw new GribException(
                    "Unable to load table information from file: " + file, e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    logger.error("Unable to close file: " + file, e);
                }
            }
        }

        // Insert the new table into the map if necessary
        insertTable(center, subcenter, tableName, table);
        return table;
    }

    private void insertTable(int centerid, int subcenterid, String tableName,
            GribTable table) {
        if (!tableMap.containsKey(getTableMapKey(centerid, subcenterid))) {
            tableMap.put(getTableMapKey(centerid, subcenterid),
                    new HashMap<String, GribTable>());
        }
        tableMap.get(getTableMapKey(centerid, subcenterid)).put(tableName,
                table);
    }

    private int getTableMapKey(int centerid, int subcenterid) {
        return centerid * 10000 + subcenterid;
    }
}
