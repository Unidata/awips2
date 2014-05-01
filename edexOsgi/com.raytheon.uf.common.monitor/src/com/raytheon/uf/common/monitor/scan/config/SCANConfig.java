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
package com.raytheon.uf.common.monitor.scan.config;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.monitor.data.CommonTableConfig.SortDirection;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.CELLTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.DMDTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.MESOTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanColors;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanThresholdColor;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.TVSTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.WARN_TYPE;
import com.raytheon.uf.common.monitor.scan.xml.SCANAttributesXML;

/**
 * Configuration class used for SCAN. This is a singleton class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2009 3039       lvenable     Initial creation
 * Apr 25, 2013   1926     njensen      synchronized instance creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANConfig {
    /**
     * Class instance
     */
    private static volatile SCANConfig classInstance;

    /**
     * Unwarned configuration data class.
     */
    private UnwarnedConfig unwarnedConfig;

    /**
     * CELL trend configuration manager.
     */
    private TrendSetConfigMgr cellTrendConfigMgr;

    /**
     * DMD trend configuration manager.
     */
    private TrendSetConfigMgr dmdTrendConfigMgr;

    /**
     * CELL configuration manager.
     */
    private CellConfigMgr cellConfigMgr;

    /**
     * DMD configuration manager.
     */
    private DmdConfigMgr dmdConfigMgr;

    /**
     * MESO configuration manager.
     */
    private MesoConfigMgr mesoConfigMgr;

    /**
     * TVS configuration manager.
     */
    private TvsConfigMgr tvsConfigMgr;

    /**
     * Default string text.
     */
    private final String defaultName = "default";

    /*
     * Threshold colors for CELL, MESO, and TVS
     */
    private final Color upperColor = new Color(Display.getDefault(), 187, 34,
            34);

    private final Color midColor = new Color(Display.getDefault(), 221, 221, 34);

    private final Color lowerColor = new Color(Display.getDefault(), 222, 222,
            222);

    private final Color defaultColor = new Color(Display.getDefault(), 153,
            153, 153);

    /*
     * Threshold colors for DMD
     */
    private final Color dmdUpperColor = new Color(Display.getDefault(), 255, 0,
            0);

    private final Color dmdMidColor = new Color(Display.getDefault(), 255, 255,
            0);

    private final Color dmdLowerColor = new Color(Display.getDefault(), 0, 255,
            0);

    private final Color dmdDefaultColor = new Color(Display.getDefault(), 255,
            255, 255);

    /*
     * IDENT table item color.
     */
    private final Color identColor = new Color(Display.getDefault(), 153, 136,
            102);

    /*
     * Sort color used in the table column image
     */
    private final Color sortColor = new Color(Display.getDefault(), 133, 104,
            190);

    /*
     * Common SCAN colors
     */
    private final Color attributesColor = new Color(Display.getDefault(), 40,
            76, 114);

    private final Color rankColor = new Color(Display.getDefault(), 154, 80, 76);

    private final Color configColor = new Color(Display.getDefault(), 102, 102,
            52);

    private final Color cwaFilterColor = new Color(Display.getDefault(), 64,
            112, 108);

    private final Color vertColor = new Color(Display.getDefault(), 78, 106,
            140);

    private final Color tipsColor = new Color(Display.getDefault(), 86, 68, 0);

    private final Color linkToFrameColor = new Color(Display.getDefault(), 86,
            68, 0);

    private final Color tvsColor = new Color(Display.getDefault(), 253, 130,
            245);

    private final Color unwarnedSvrColor = new Color(Display.getDefault(), 255,
            255, 0);

    private final Color unwarnedTorColor = new Color(Display.getDefault(), 240,
            0, 255);

    private final Color cellForegroundColor = new Color(Display.getDefault(),
            0, 0, 0);

    /**
     * Clutter control color used for the table column image.
     */
    private final Color clutterCntlColor = new Color(Display.getDefault(), 0,
            255, 0);

    /**
     * Radius interpolation color used for the table column image.
     */
    private final Color radVarColor = new Color(Display.getDefault(), 64, 112,
            108);

    /**
     * Map indication which CELL data items are formatted as an integer.
     */
    private HashMap<String, Boolean> cellIntFormatMap;

    /**
     * Map indication which DMD data items are formatted as an integer.
     */
    private HashMap<String, Boolean> dmdIntFormatMap;

    /**
     * Map indication which MESO data items are formatted as an integer.
     */
    private HashMap<String, Boolean> mesoIntFormatMap;

    /**
     * Map indication which TVS data items are formatted as an integer.
     */
    private HashMap<String, Boolean> tvsIntFormatMap;

    /**
     * Map indication which CELL trend graphs are formatted as an integer.
     */
    private HashMap<String, Boolean> cellTrendGraphIntFormatMap;

    /**
     * Map indication which DMD trend graphs are formatted as an integer.
     */
    private HashMap<String, Boolean> dmdTrendGraphIntFormatMap;

    /**
     * Map that indicates the classification relative to the STRANK value.
     */
    private Map<String, String> dmdClassificationFormatMap;

    /**
     * Map that indicates the classification relative to the mdaSR value.
     */
    private Map<String, String> mesoClassificationFormatMap;

    /**
     * Private constructor.
     */
    private SCANConfig() {
        init();
    }

    /**
     * Get an instance of this class.
     * 
     * @return Class instance.
     */
    public static SCANConfig getInstance() {
        SCANConfig retVal = classInstance;
        if (retVal == null) {
            synchronized (SCANConfig.class) {
                retVal = classInstance;
                if (retVal == null) {
                    classInstance = new SCANConfig();
                    retVal = classInstance;
                }
            }
        }

        return retVal;
    }

    /**
     * Initialize method.
     */
    private void init() {
        unwarnedConfig = new UnwarnedConfig();
        cellConfigMgr = new CellConfigMgr();
        dmdConfigMgr = new DmdConfigMgr();
        mesoConfigMgr = new MesoConfigMgr();
        tvsConfigMgr = new TvsConfigMgr();

        cellTrendConfigMgr = new TrendSetConfigMgr("CellTrendSets.xml");
        dmdTrendConfigMgr = new TrendSetConfigMgr("DmdTrendSets.xml");

        setupTableIntFormatMap();
        setupTrendIntFormatMaps();
        setupDmdClassificationMap(dmdConfigMgr.getArrtibuteXML(
                DMDTable.STRANK.getColName()).getUpper());
        setupMesoClassificationMap(mesoConfigMgr.getArrtibuteXML(
                MESOTable.MDASR.getColName()).getUpper());
    }

    /**
     * Setup the table integer format maps. These maps are used to determine
     * which items in the tables are to be formatted as integers.
     */
    private void setupTableIntFormatMap() {
        /*
         * CELL table
         */
        cellIntFormatMap = new HashMap<String, Boolean>();

        cellIntFormatMap.put(CELLTable.HSIZE.getColName(), false);
        cellIntFormatMap.put(CELLTable.DBZHT.getColName(), false);
        cellIntFormatMap.put(CELLTable.TOP.getColName(), false);
        cellIntFormatMap.put(CELLTable.LAT.getColName(), false);
        cellIntFormatMap.put(CELLTable.LON.getColName(), false);
        cellIntFormatMap.put(CELLTable.CGRATE.getColName(), false);
        cellIntFormatMap.put(CELLTable.CAPE.getColName(), false);
        cellIntFormatMap.put(CELLTable.SREH.getColName(), false);

        /*
         * DMD table
         */
        dmdIntFormatMap = new HashMap<String, Boolean>();

        dmdIntFormatMap.put(DMDTable.BASE.getColName(), false);
        dmdIntFormatMap.put(DMDTable.DEPTH.getColName(), false);
        dmdIntFormatMap.put(DMDTable.LLDIAM.getColName(), false);
        dmdIntFormatMap.put(DMDTable.HTMXVR.getColName(), false);
        dmdIntFormatMap.put(DMDTable.LAT.getColName(), false);
        dmdIntFormatMap.put(DMDTable.LON.getColName(), false);

        /*
         * MESO table
         */
        mesoIntFormatMap = new HashMap<String, Boolean>();

        mesoIntFormatMap.put(MESOTable.LAT.getColName(), false);
        mesoIntFormatMap.put(MESOTable.LON.getColName(), false);

        /*
         * TVS table
         */
        tvsIntFormatMap = new HashMap<String, Boolean>();

        tvsIntFormatMap.put(TVSTable.MXDVHT.getColName(), false);
        tvsIntFormatMap.put(TVSTable.BASE.getColName(), false);
        tvsIntFormatMap.put(TVSTable.DEPTH.getColName(), false);
        tvsIntFormatMap.put(TVSTable.TOP.getColName(), false);
        tvsIntFormatMap.put(TVSTable.SHRHT.getColName(), false);
        tvsIntFormatMap.put(TVSTable.LAT.getColName(), false);
        tvsIntFormatMap.put(TVSTable.LON.getColName(), false);

    }

    /**
     * Setup the trend integer format maps. These maps are used to determine
     * which labels on the trend graphs are to be formatted as integers.
     */
    private void setupTrendIntFormatMaps() {
        /*
         * CELL
         */
        cellTrendGraphIntFormatMap = new HashMap<String, Boolean>();

        cellTrendGraphIntFormatMap.put(CELLTable.HSIZE.getColName(), false);

        /*
         * DMD
         */
        dmdTrendGraphIntFormatMap = new HashMap<String, Boolean>();

        dmdTrendGraphIntFormatMap.put(DMDTable.BASE.getColName(), false);
        dmdTrendGraphIntFormatMap.put(DMDTable.DEPTH.getColName(), false);
        dmdTrendGraphIntFormatMap.put(DMDTable.LLDIAM.getColName(), false);
    }

    public void setupDmdClassificationMap(double upperVal) {

        /*
         * Classification Map
         */
        dmdClassificationFormatMap = new HashMap<String, String>();

        final int MAX_CLASS_ENTRIES = 30;

        dmdClassificationFormatMap.put("N/A", "NA");

        for (int i = 1; i < (int) upperVal; ++i) {
            dmdClassificationFormatMap.put(Integer.toString(i), "circ");
        }

        for (int i = (int) upperVal; i < MAX_CLASS_ENTRIES; ++i) {
            dmdClassificationFormatMap.put(Integer.toString(i), "MESO");
        }
    }

    /**
     * Setup MESO classification map so that "circ" or "MESO" values will
     * appear.
     * 
     * @param upperVal
     */
    public void setupMesoClassificationMap(double upperVal) {

        /*
         * Classification Map
         */
        mesoClassificationFormatMap = new HashMap<String, String>();

        final int MAX_CLASS_ENTRIES = 30;

        mesoClassificationFormatMap.put("N/A", "NA");

        for (int i = 1; i < (int) upperVal; ++i) {
            mesoClassificationFormatMap.put(Integer.toString(i), "circ");
        }

        for (int i = (int) upperVal; i < MAX_CLASS_ENTRIES; ++i) {
            mesoClassificationFormatMap.put(Integer.toString(i), "MESO");
        }
    }

    /*
     * TODO : should dispose when the SCAN resource is disposed
     */
    /**
     * Dispose of all the color resources.
     */
    public void disposeResources() {
        upperColor.dispose();
        midColor.dispose();
        lowerColor.dispose();
        defaultColor.dispose();

        dmdUpperColor.dispose();
        dmdMidColor.dispose();
        dmdLowerColor.dispose();
        dmdDefaultColor.dispose();

        identColor.dispose();
        sortColor.dispose();

        attributesColor.dispose();
        rankColor.dispose();
        configColor.dispose();
        cwaFilterColor.dispose();
        vertColor.dispose();
        tipsColor.dispose();
        linkToFrameColor.dispose();
        tvsColor.dispose();
        cellForegroundColor.dispose();

        clutterCntlColor.dispose();
        radVarColor.dispose();

        unwarnedSvrColor.dispose();
        unwarnedTorColor.dispose();
    }

    /**
     * Create the unwarned configuration.
     */
    public void createUnwarnedConfig() {
        unwarnedConfig = new UnwarnedConfig();
    }

    /**
     * Get the unwarned configuration.
     * 
     * @return The unwarned configuration.
     */
    public UnwarnedConfig getUnwarnedConfig() {
        return unwarnedConfig;
    }

    /**
     * Determine if the table item data needs to be formatted as an integer.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @return True if the format is an integer, false for decimal.
     */
    public boolean displayAsIntTable(ScanTables scanTable, String attrName) {
        if (scanTable == ScanTables.CELL) {
            if (cellIntFormatMap.containsKey(attrName) == true) {
                return false;
            }
        } else if (scanTable == ScanTables.DMD) {
            if (dmdIntFormatMap.containsKey(attrName) == true) {
                return false;
            }
        } else if (scanTable == ScanTables.MESO) {
            if (mesoIntFormatMap.containsKey(attrName) == true) {
                return false;
            }
        } else if (scanTable == ScanTables.TVS) {
            if (tvsIntFormatMap.containsKey(attrName) == true) {
                return false;
            }
        }

        return true;
    }

    /**
     * Determine if the trend label needs to be formatted as an integer.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @return True if the format is an integer, false for decimal.
     */
    public boolean displayAsIntTrend(ScanTables scanTable, String attrName) {
        if (scanTable == ScanTables.CELL) {
            if (cellTrendGraphIntFormatMap.containsKey(attrName) == true) {
                return false;
            }
        } else if (scanTable == ScanTables.DMD) {
            if (dmdTrendGraphIntFormatMap.containsKey(attrName) == true) {
                return false;
            }
        }

        return true;
    }

    /**
     * Unload the SCAN configuration.
     */
    public void unloadConfig() {
        disposeResources();
        classInstance = null;
    }

    /**
     * Get an array of column/attribute names.
     * 
     * @param scanTable
     *            Scan table.
     * @return Array of column/attribute names.
     */
    public String[] getTableColumnNames(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getColumnNames();
    }

    /**
     * Get the default column width for the specified table.
     * 
     * @param table
     *            Scan table.
     * @return Default column width.
     */
    public int getDefaultColumnWidth(ScanTables table) {
        if (table == ScanTables.CELL) {
            return 60;
        } else if (table == ScanTables.DMD) {
            return 60;
        } else if (table == ScanTables.MESO) {
            return 60;
        } else if (table == ScanTables.TVS) {
            return 60;
        }

        return 60;
    }

    public int getDefaultTableWidth(ScanTables table) {
        if (table == ScanTables.CELL) {
            return 1075;
        } else if (table == ScanTables.DMD) {
            return 1230;
        } else if (table == ScanTables.MESO) {
            return 900;
        } else if (table == ScanTables.TVS) {
            return 835;
        }

        return 1175;
    }

    public int getCountyColumnIndex(ScanTables table) {
        if (table == ScanTables.CELL) {
            return CELLTable.COUNTY.ordinal();
        } else if (table == ScanTables.DMD) {
            return DMDTable.COUNTY.ordinal();
        } else if (table == ScanTables.MESO) {
            return MESOTable.COUNTY.ordinal();
        } else if (table == ScanTables.TVS) {
            return TVSTable.COUNTY.ordinal();
        }

        return 1;
    }

    /**
     * Get a common SCAN color.
     * 
     * @param scanColor
     *            Specified scan color.
     * @return Scan color.
     */
    public Color getScanColor(ScanColors scanColor) {
        switch (scanColor) {
        case Attributes:
        case Unwarned:
            return attributesColor;
        case Rank:
            return rankColor;
        case Configurations:
            return configColor;
        case CWAFilter:
            return cwaFilterColor;
        case Vert:
            return vertColor;
        case Tips:
            return tipsColor;
        case LinkToFrame:
            return linkToFrameColor;
        case Sort:
            return sortColor;
        case ClutterControl:
            return clutterCntlColor;
        case RadVar:
            return radVarColor;
        case TVS:
            return tvsColor;
        }

        return defaultColor;
    }

    /**
     * Get a table item color.
     * 
     * @param scanTable
     *            Specified SCAN table.
     * @param colName
     *            Column/Attribute name.
     * @param thresholdColor
     *            Threshold color.
     * @return The table item color.
     */
    public Color getColorThresholdDialogColor(ScanTables scanTable,
            String colName, ScanThresholdColor thresholdColor) {
        /*
         * Check the sort order to determine if the upper and lower threshold
         * colors need to be swapped. Sort by ascending will have the smaller
         * numbers on top and those are the critical values.
         */
        boolean reverseColors = false;

        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        int sortDir = cfgMgr.getSortDirection(colName);

        /*
         * Check if the data can be colored with the threshold colors. If so,
         * then check to see if the threshold colors should be reversed.
         */
        if (cfgMgr.getArrtibuteXML(colName).getColored() == true) {
            if (sortDir == SortDirection.Ascending.getSortDir()) {
                reverseColors = true;
            }
        }

        /*
         * Check for the DMD table. The CELL, MESO, and TVS tables are in the
         * else statement.
         */
        if (scanTable == ScanTables.DMD) {
            switch (thresholdColor) {
            case Upper:
                if (reverseColors == false) {
                    return dmdUpperColor;
                }
                // Return the reverse color
                return dmdLowerColor;

            case Mid:
                return dmdMidColor;

            case Lower:
                if (reverseColors == false) {
                    return dmdLowerColor;
                }
                // Return the reverse color
                return dmdUpperColor;

            case Default:
                return dmdDefaultColor;
            }
        } else {
            // CELL, MESO, TVS
            switch (thresholdColor) {
            case Upper:
                if (reverseColors == false) {
                    return upperColor;
                }
                return lowerColor;
            case Mid:
                return midColor;
            case Lower:
                if (reverseColors == false) {
                    return lowerColor;
                }
                return upperColor;
            case Default:
                return defaultColor;
            }
        }

        return defaultColor;
    }

    /**
     * Get a table item color.
     * 
     * @param scanTable
     *            Specified SCAN table.
     * @param colName
     *            Column/Attribute name.
     * @param thresholdColor
     *            Threshold color.
     * @return The table item color.
     */
    public Color getTableItemColor(ScanTables scanTable, String colName,
            ScanThresholdColor thresholdColor) {
        if (thresholdColor == ScanThresholdColor.Ident) {
            return identColor;
        }

        /*
         * Check the sort order to determine if the upper and lower threshold
         * colors need to be swapped. Sort by ascending will have the smaller
         * numbers on top and those are the critical values.
         */
        boolean reverseColors = false;

        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        int sortDir = cfgMgr.getSortDirection(colName);

        /*
         * Check if the data can be colored with the threshold colors. If so,
         * then check to see if the threshold colors should be reversed.
         */
        if (cfgMgr.getArrtibuteXML(colName).getColored() == true) {
            if (sortDir == SortDirection.Ascending.getSortDir()) {
                reverseColors = true;
            }
        }

        /*
         * Check for the DMD table. The CELL, MESO, and TVS tables are in the
         * else statement.
         */
        if (scanTable == ScanTables.DMD) {
            switch (thresholdColor) {
            case Upper:
                if (reverseColors == false) {
                    return dmdUpperColor;
                }
                // Return the reverse color
                return dmdDefaultColor;

            case Mid:
                if (reverseColors == false) {
                    return dmdMidColor;
                }
                // Return the reverse color
                return dmdLowerColor;

            case Lower:
                if (reverseColors == false) {
                    return dmdLowerColor;
                }
                // Return the reverse color
                return dmdMidColor;

            case Default:

                if (reverseColors == false) {
                    return dmdDefaultColor;
                }
                // Return the reverse color
                return dmdUpperColor;
            }
        } else {
            // CELL, MESO, TVS
            switch (thresholdColor) {
            case Upper:
                if (reverseColors == false) {
                    return upperColor;
                }
                return defaultColor;
            case Mid:
                if (reverseColors == false) {
                    return midColor;
                }
                return lowerColor;
            case Lower:
                if (reverseColors == false) {
                    return lowerColor;
                }
                return midColor;
            case Default:
                if (reverseColors == false) {
                    return defaultColor;
                }
                return upperColor;
            }
        }

        return defaultColor;
    }

    public Color getUnwarnedColor(WARN_TYPE wrnType) {
        if (wrnType == null) {
            return identColor;
        }

        if (wrnType == WARN_TYPE.TVS) {
            return unwarnedTorColor;
        } else if (wrnType == WARN_TYPE.SEVERE) {
            return unwarnedSvrColor;
        }

        return identColor;
    }

    public Color getCellForegroundColor() {
        return cellForegroundColor;
    }

    /**
     * Get the threshold color.
     * 
     * @param scanTable
     *            Specified scan table
     * @param colName
     *            Column/Attribute name.
     * @param value
     *            Value to determine the threshold color.
     * @return The threshold color.
     */
    public Color getThresholdColor(ScanTables scanTable, String colName,
            double value) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return getTableItemColor(scanTable, colName,
                cfgMgr.getThresholdColor(colName, value));
    }

    /**
     * Load the default configuration for the specified scan table.
     * 
     * @param scanTable
     *            the specified scan table.
     */
    public void reload(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        cfgMgr.loadDefaultConfig();
    }

    /**
     * Get an array of visible columns for the specified scan table.
     * 
     * @param scanTable
     *            Scan table.
     * @return Boolean array indicating which columns are visible.
     */
    public boolean[] getVisibleColumns(ScanTables scanTable) {
        SCANAttributesXML attrXML;
        ArrayList<SCANAttributesXML> columnData = getAttributes(scanTable);
        boolean[] visCols = new boolean[columnData.size()];

        Arrays.fill(visCols, false);

        for (int i = 0; i < columnData.size(); i++) {
            attrXML = columnData.get(i);
            visCols[i] = attrXML.getInTable();
        }

        return visCols;
    }

    /**
     * Get an array of attribute data for the specified scan table.
     * 
     * @param scanTable
     *            Scan table.
     * @return Array of Attribute data.
     */
    public ArrayList<SCANAttributesXML> getAttributes(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getAttributes();
    }

    /**
     * Get the sort direction for the specified scan table and attribute/column
     * name.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute/column name.
     * @return SWT.UP for ascending, SWT.DOWN for descending, or SWT.NONE for no
     *         sort.
     */
    public int getSortDirection(ScanTables scanTable, String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getSortDirection(attrName);
    }

    /**
     * Get the column index for the specified scan table and attribute/column
     * name.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute/Column name.
     * @return Column index.
     */
    public int getColumnIndex(ScanTables scanTable, String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getColumnIndex(attrName);
    }

    /**
     * Get the default name.
     * 
     * @return
     */
    public String getDefaultName() {
        return defaultName;
    }

    /**
     * Get the columns names that can be ranked (sorted).
     * 
     * @param scanTable
     *            Scan table.
     * @return String array of column names.
     */
    public String[] getRankColumns(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getRankColumns();
    }

    /**
     * Get the available column names that can be ranked (sorted).
     * 
     * @param scanTable
     *            Scan table.
     * @return HashMap of column names that are available (visible).
     */
    public HashMap<String, Object> getAvailRankColumns(ScanTables scanTable) {
        HashMap<String, Object> availRankMap = new HashMap<String, Object>();

        String[] colNames = getTableColumnNames(scanTable);
        boolean[] visibleCols = getVisibleColumns(scanTable);

        for (int i = 0; i < colNames.length; i++) {
            if (visibleCols[i] == true) {
                availRankMap.put(colNames[i], null);
            }
        }
        return availRankMap;
    }

    /**
     * Get the column index of the default rank.
     * 
     * @param scanTable
     *            Scan table.
     * @return Column index.
     */
    public int getDefaultRankColumnIndex(ScanTables scanTable) {

        if (scanTable == ScanTables.CELL) {
            if ((getDefaultRank(scanTable) != null)
                    && (getDefaultRank(scanTable).length() > 0)
                    && !getDefaultRank(scanTable).equalsIgnoreCase("Default")) {
                CELLTable ct = CELLTable.valueOf(getDefaultRank(scanTable)
                        .toUpperCase());
                return ct.ordinal();
            }
        } else if (scanTable == ScanTables.DMD) {
            if ((getDefaultRank(scanTable) != null)
                    && (getDefaultRank(scanTable).length() > 0)
                    && !getDefaultRank(scanTable).equalsIgnoreCase("Default")) {
                DMDTable dt = DMDTable.valueOf(getDefaultRank(scanTable)
                        .toUpperCase());
                return dt.ordinal();
            }
        } else if (scanTable == ScanTables.MESO) {
            if ((getDefaultRank(scanTable) != null)
                    && (getDefaultRank(scanTable).length() > 0)
                    && !getDefaultRank(scanTable).equalsIgnoreCase("Default")) {
                MESOTable mt = MESOTable.valueOf(getDefaultRank(scanTable)
                        .toUpperCase());
                return mt.ordinal();
            }
        } else if (scanTable == ScanTables.TVS) {
            if ((getDefaultRank(scanTable) != null)
                    && (getDefaultRank(scanTable).length() > 0)
                    && !getDefaultRank(scanTable).equalsIgnoreCase("Default")) {
                TVSTable mt = TVSTable.valueOf(getDefaultRank(scanTable)
                        .toUpperCase());
                return mt.ordinal();
            }
        }

        return -1;
    }

    /**
     * Get the default rank name for the specified scan table.
     * 
     * @param scanTable
     *            Scan table.
     * @return The default rank name.
     */
    public String getDefaultRank(ScanTables scanTable) {
        switch (scanTable) {
        case CELL:
            return cellConfigMgr.getScanCellCfgXML().getDefaultRank();
        case DMD:
            return dmdConfigMgr.getScanDmdCfgXML().getDefaultRank();
        case MESO:
            return mesoConfigMgr.getScanMesoCfgXML().getDefaultRank();
        case TVS:
            return tvsConfigMgr.getScanTvsCfgXML().getDefaultRank();
        }

        return null;
    }

    /**
     * Set the visible columns for the specified scan table.
     * 
     * @param scanTable
     *            Scan table.
     * @param visCols
     *            Visible columns.
     */
    public void setVisibleColumns(ScanTables scanTable, boolean[] visCols) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        cfgMgr.setVisibleAttributes(visCols);
    }

    /**
     * Check if the table/attribute can view a trend graph.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute/column name.
     * @return True if a trend graph should be displayed, false otherwise.
     */
    public boolean canViewTrend(ScanTables scanTable, String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getTrend(attrName);
    }

    public Point getMinMaxValues(ScanTables scanTable, String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getMinMaxValues(attrName);
    }

    /**
     * Check if the table/attribute can view a time-height graph.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute/column name.
     * @return True if a time-height graph should be displayed, false otherwise.
     */
    public boolean canViewTimeHeight(ScanTables scanTable, String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getTimeHeight(attrName);
    }

    /**
     * Check if the CWA filter should be applied for the specified table.
     * 
     * @param scanTable
     *            Scan table.
     * @return True if the CWA filter should be applied, false otherwise.
     */
    public boolean getCWAFilter(ScanTables scanTable) {
        switch (scanTable) {
        case CELL:
            return cellConfigMgr.getScanCellCfgXML().getFilterOption();
        case DMD:
            return dmdConfigMgr.getScanDmdCfgXML().getFilterOption();
        }

        return false;
    }

    /**
     * Set the CWA filter state for the specified scan table.
     * 
     * @param scanTable
     *            Scan table.
     * @param flag
     *            True to turn on CWA filtering, false to turn off CWA
     *            filtering.
     */
    public void setCWAFilter(ScanTables scanTable, boolean flag) {
        switch (scanTable) {
        case CELL:
            cellConfigMgr.getScanCellCfgXML().setFilterOption(flag);
            break;
        case DMD:
            dmdConfigMgr.getScanDmdCfgXML().setFilterOption(flag);
            break;
        }
    }

    /**
     * Check if the attribute is a clutter control attribute.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @return True if the attribute is a clutter control, false otherwise.
     */
    public boolean isClutterControl(ScanTables scanTable, String attrName) {
        String ccStr = null;
        switch (scanTable) {
        case CELL:
            ccStr = cellConfigMgr.getClutterControl();
            break;
        case DMD:
            ccStr = dmdConfigMgr.getClutterControl();
            break;
        }

        if (ccStr == null) {
            return false;
        }

        if (ccStr.compareTo(attrName) == 0) {
            return true;
        }

        return false;
    }

    /**
     * Check if the attribute is a radius interpolation attribute.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @return True if the radius interpolation is a clutter control, false
     *         otherwise.
     */
    public boolean isRadVar(ScanTables scanTable, String attrName) {
        String radVarStr = null;
        switch (scanTable) {
        case CELL:
            radVarStr = cellConfigMgr.getRadVar();
            break;
        case DMD:
            radVarStr = dmdConfigMgr.getRadVar();
            break;
        }

        if (radVarStr.compareTo(attrName) == 0) {
            return true;
        }

        return false;
    }

    public boolean getAlarmsDisabled(ScanTables scanTable) {
        switch (scanTable) {
        case CELL:
            return cellConfigMgr.getAlarmsDisabled();

        case DMD:
            return dmdConfigMgr.getAlarmsDisabled();

        }

        return false;
    }

    public void setAlarmsDisabled(ScanTables scanTable, boolean flag) {
        switch (scanTable) {
        case CELL:
            cellConfigMgr.setAlarmsDisabled(flag);
            break;
        case DMD:
            dmdConfigMgr.setAlarmsDisabled(flag);
            break;
        }
    }

    public boolean getAlarmBell(ScanTables scanTable) {
        switch (scanTable) {
        case CELL:
            return cellConfigMgr.getAlarmBell();

        case DMD:
            return dmdConfigMgr.getAlarmBell();

        }

        return false;
    }

    public void setAlarmBell(ScanTables scanTable, boolean flag) {
        switch (scanTable) {
        case CELL:
            cellConfigMgr.setAlarmBell(flag);
            break;
        case DMD:
            dmdConfigMgr.setAlarmBell(flag);
            break;
        }
    }

    /**
     * Check if the tips should be shown on the specified table.
     * 
     * @param scanTable
     *            Scan table.
     * @return True if tool tips should be shown.
     */
    public boolean showTips(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.showTips();
    }

    /**
     * Set the show tips for the specified table.
     * 
     * @param scanTable
     *            Scan table.
     * @param showFlag
     *            Show tips flag. True to show tips, false to not show tips.
     */
    public void setShowTips(ScanTables scanTable, boolean showFlag) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        cfgMgr.setShowTips(showFlag);
    }

    /**
     * Get the upper threshold value for the specified table and attribute name.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @return The upper value.
     */
    public double getUpperThreshold(ScanTables scanTable, String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getUpper(attrName);
    }

    /**
     * Get the mid threshold value for the specified table and attribute name.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @return The mid value.
     */
    public double getMidThreshold(ScanTables scanTable, String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getMid(attrName);
    }

    /**
     * Get the lower threshold value for the specified table and attribute name.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @return The lower value.
     */
    public double getLowerThreshold(ScanTables scanTable, String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getLower(attrName);
    }

    /**
     * Set the thresholds for the specified scan table and attribute name.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @param upper
     *            Upper value.
     * @param mid
     *            Mid value.
     * @param lower
     *            Lower value.
     */
    public void setThresholds(ScanTables scanTable, String attrName,
            double upper, double mid, double lower) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        cfgMgr.setThresholds(attrName, upper, mid, lower);
    }

    /**
     * Get a LinkedHapMap of available threshold attributes & units for the
     * specified scan table.
     * 
     * @param scanTable
     *            Scan table.
     * @return LinkedHapMap of threshold attributes & units.
     */
    public LinkedHashMap<String, String> getThreshAttributes(
            ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getThreshAttributes();
    }

    /**
     * Get a LinkedHapMap of available threshold attributes & units for the
     * specified scan table.
     * 
     * @param scanTable
     *            Scan table.
     * @return LinkedHapMap of threshold attributes & units.
     */
    public LinkedHashMap<String, String> getTimeHeightAttributeUnits(
            ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getTimeHeightAttributes();
    }

    /**
     * Get a LinkedHapMap of available clutter control attributes & units for
     * the specified scan table.
     * 
     * @param scanTable
     *            Scan table.
     * @return LinkedHapMap of clutter control attributes & units.
     */
    public LinkedHashMap<String, String> getClutterAttributes(
            ScanTables scanTable) {
        switch (scanTable) {
        case CELL:
            return cellConfigMgr.getClutterAttributes();
        case DMD:
            return dmdConfigMgr.getClutterAttributes();
        }

        return null;
    }

    /**
     * Check the scan table should be linked to the display so both will update
     * at the same time.
     * 
     * @param type
     *            Scan table.
     * @return True to link the table to the display, false otherwise.
     */
    public boolean getLinkToFrame(
            com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables type) {
        switch (type) {
        case CELL:
        case MESO:
        case TVS:
            return cellConfigMgr.getScanCellCfgXML().getLinkToFrame();
        case DMD:
            return dmdConfigMgr.getScanDmdCfgXML().getLinkToFrame();
        }

        return false;
    }

    /**
     * Set the link to frame configuration.
     * 
     * @param scanTable
     *            Scan table.
     * @param flag
     *            True to link the table to the display, false otherwise.
     */
    public void setLinkToFrame(ScanTables scanTable, boolean flag) {
        switch (scanTable) {
        case CELL:
            cellConfigMgr.getScanCellCfgXML().setLinkToFrame(flag);
            break;
        case DMD:
            dmdConfigMgr.getScanDmdCfgXML().setLinkToFrame(flag);
            break;
        }
    }

    /**
     * Get the attribute configuration data for the specified scan table and
     * attribute name.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @return SCAN attribute data.
     */
    public SCANAttributesXML getAttributeXML(ScanTables scanTable,
            String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getArrtibuteXML(attrName);
    }

    /**
     * Get an array of names that can display a trend graph.
     * 
     * @param scanTable
     *            Scan table.
     * @return An array of attribute names that can display a trend graph.
     */
    public String[] getTrendAttributes(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getTrendAttributeNames();
    }

    /**
     * Get the attribute data for the attribute name in the specified scan
     * table.
     * 
     * @param scanTable
     *            Scan table.
     * @param attrName
     *            Attribute name.
     * @return SCAN attribute data. Can be null if the attribute name does not
     *         display a trend graph.
     */
    public SCANAttributesXML getTrendAttrData(ScanTables scanTable,
            String attrName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getTrendAttrData(attrName);
    }

    public String getCurrentConfigFileName(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getCurrentConfigFileName();
    }

    public String getDefaultConfigName(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getDefaultConfigName();
    }

    public String getConfigurationPath(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.getConfigPath();
    }

    public void loadNewConfigFileName(ScanTables scanTable, String newCfgName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        cfgMgr.loadNewConfig(newCfgName);
    }

    public void loadDefaultConfigFileName(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        cfgMgr.loadDefaultConfig();
    }

    public void saveCurrentConfigurationFile(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        cfgMgr.saveConfig();
    }

    public void saveConfigurationFileAs(ScanTables scanTable, String newName) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        cfgMgr.saveConfigAs(newName);
    }

    public boolean currentConfigIsDefault(ScanTables scanTable) {
        AbsConfigMgr cfgMgr = getAbsConfigMgr(scanTable);
        return cfgMgr.currentConfigIsDefault();
    }

    /**
     * Get the storm cell configuration data.
     * 
     * @return Storm cell configuration data.
     */
    public StormCellConfig getStormCellConfig() {
        SCANAttributesXML attrXML = getAttributeXML(ScanTables.CELL,
                cellConfigMgr.getScanCellCfgXML().getClutterControl());

        boolean symsCircleHigh = cellConfigMgr.getScanCellCfgXML()
                .getSymsCircleHigh();
        boolean symsCircleMid = cellConfigMgr.getScanCellCfgXML()
                .getSymsCircleMid();
        boolean symsCircleLow = cellConfigMgr.getScanCellCfgXML()
                .getSymsCircleLow();
        boolean futureTracks = cellConfigMgr.getScanCellCfgXML()
                .getFutureTracks();
        boolean pastTracks = cellConfigMgr.getScanCellCfgXML().getPastTracks();
        boolean symsArrowHigh = cellConfigMgr.getScanCellCfgXML()
                .getSymsArrowHigh();
        boolean symsArrowMid = cellConfigMgr.getScanCellCfgXML()
                .getSymsArrowMid();
        boolean symsArrowLow = cellConfigMgr.getScanCellCfgXML()
                .getSymsArrowLow();
        boolean symsIdsHigh = cellConfigMgr.getScanCellCfgXML().getSymsIdHigh();
        boolean symsIdsMid = cellConfigMgr.getScanCellCfgXML().getSymsIdMid();
        boolean symsIdsLow = cellConfigMgr.getScanCellCfgXML().getSymsIdLow();
        int minRadius = cellConfigMgr.getScanCellCfgXML().getMinRadius();
        int maxRadius = cellConfigMgr.getScanCellCfgXML().getMaxRadius();
        String radVar = cellConfigMgr.getScanCellCfgXML().getRadVar();
        double radLow = cellConfigMgr.getScanCellCfgXML().getRadLow();
        double radHigh = cellConfigMgr.getScanCellCfgXML().getRadHigh();
        boolean arrowMode = cellConfigMgr.getScanCellCfgXML().getArrowMode();
        int arrowConversion = cellConfigMgr.getScanCellCfgXML()
                .getArrowConversion();
        String attrName = cellConfigMgr.getScanCellCfgXML().getClutterControl();
        double upperVal = attrXML.getUpper();
        double midVal = attrXML.getMid();
        double lowerVal = attrXML.getLow();
        boolean linkToFrame = cellConfigMgr.getScanCellCfgXML()
                .getLinkToFrame();

        StormCellConfig scanDisplayCfg = new StormCellConfig(symsCircleHigh,
                symsCircleMid, symsCircleLow, pastTracks, futureTracks,
                symsArrowHigh, symsArrowMid, symsArrowLow, symsIdsHigh,
                symsIdsMid, symsIdsLow, minRadius, maxRadius, radVar, radLow,
                radHigh, arrowMode, arrowConversion, attrName, upperVal,
                midVal, lowerVal, linkToFrame);

        return scanDisplayCfg;
    }

    /**
     * Get the DMD display filter configuration data.
     * 
     * @return The DMD display filter configuration data.
     */
    public DmdDisplayFilterConfig getDmdDisplayFilterConfig() {
        SCANAttributesXML attrXML = dmdConfigMgr
                .getArrtibuteXML(DMDTable.STRANK.getColName());

        boolean track = dmdConfigMgr.getScanDmdCfgXML().getTrackOpt();
        boolean overlap = dmdConfigMgr.getScanDmdCfgXML().getOverlapOpt();

        double upperVal = attrXML.getUpper();
        double midVal = attrXML.getMid();
        double lowerVal = attrXML.getLow();

        DmdDisplayFilterConfig displayCfg = new DmdDisplayFilterConfig(track,
                overlap, upperVal, midVal, lowerVal);

        return displayCfg;
    }

    /**
     * Get the abstract configuration manager that is the common base for the
     * CELL, DMD, MESO, and TVS configuration managers.
     * 
     * @param scanTable
     *            Scan table.
     * @return Abstract configuration manager for the specified scan table.
     */
    public AbsConfigMgr getAbsConfigMgr(ScanTables scanTable) {
        switch (scanTable) {
        case CELL:
            return cellConfigMgr;
        case DMD:
            return dmdConfigMgr;
        case MESO:
            return mesoConfigMgr;
        case TVS:
            return tvsConfigMgr;
        }

        return cellConfigMgr;
    }

    /**
     * Get the trend set configuration manager for the specified scan table.
     * 
     * @param scanTable
     *            Scan table.
     * @return Trend set configuration manager.
     */
    public TrendSetConfigMgr getTrendConfigMgr(ScanTables scanTable) {
        if (scanTable == ScanTables.CELL) {
            return cellTrendConfigMgr;
        } else if (scanTable == ScanTables.DMD) {
            return dmdTrendConfigMgr;
        }

        return cellTrendConfigMgr;
    }

    /**
     * Get the classification based on the STRANK value
     * 
     * @param rank
     * @return
     */
    public String getDmdClassification(String rank) {
        return dmdClassificationFormatMap.get(rank);
    }

    /**
     * Get the classification based on the MDASR value
     * 
     * @param rank
     * @return
     */
    public String getMesoClassification(String rank) {
        return mesoClassificationFormatMap.get(rank);
    }

    /**
     * Convert strength rank from string to double
     * 
     * @param stRank
     * @return
     */
    public double convertStrankValue(String stRank) {
        double tmpValue = Double.NaN;

        if (stRank.matches("[0-9.]+") == true) {
            tmpValue = Double.valueOf(stRank);
        } else if (stRank.endsWith("L") || stRank.endsWith("M")) {
            try {
                tmpValue = Double.valueOf(stRank.substring(0,
                        stRank.length() - 1));
            } catch (Exception ex) {
                tmpValue = -999.0;
            }
        }

        return tmpValue;
    }

    /**
     * Determine the Base data color for DMD
     * 
     * @param strVal
     *            elev0 value
     * @param value
     *            base value
     * @return Color
     */
    public Color getDMDBaseBGColor(String strVal, Double value) {
        if (strVal.startsWith("0")) {
            return Display.getCurrent().getSystemColor(SWT.COLOR_RED);
        } else if (strVal.equalsIgnoreCase("Y")) {
            return Display.getCurrent().getSystemColor(SWT.COLOR_RED);
        } else if (value < 1) {
            return Display.getCurrent().getSystemColor(SWT.COLOR_RED);
        } else {
            return Display.getCurrent().getSystemColor(SWT.COLOR_WHITE);
        }
    }
}
