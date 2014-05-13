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
package com.raytheon.uf.viz.monitor.scan.tables;

import java.util.HashMap;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.CELLTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.DMDTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.MESOTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.TVSTable;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.SortDirection;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;

public class SCANToolTipTextMgr {
    private static SCANToolTipTextMgr classInstance;

    private HashMap<String, String> cellTableCellTips;

    private HashMap<String, String> cellTableColumnTips;

    private HashMap<String, String> dmdTableCellTips;

    private HashMap<String, String> dmdTableColumnTips;

    private HashMap<String, String> mesoTableCellTips;

    private HashMap<String, String> mesoTableColumnTips;

    private HashMap<String, String> tvsTableCellTips;

    private HashMap<String, String> tvsTableColumnTips;

    private SCANToolTipTextMgr() {
        init();
    }

    public static SCANToolTipTextMgr getInstance() {
        if (classInstance == null) {
            classInstance = new SCANToolTipTextMgr();
        }

        return classInstance;
    }

    private void init() {
        cellTableCellTips = new HashMap<String, String>();
        cellTableColumnTips = new HashMap<String, String>();

        dmdTableCellTips = new HashMap<String, String>();
        dmdTableColumnTips = new HashMap<String, String>();

        mesoTableCellTips = new HashMap<String, String>();
        mesoTableColumnTips = new HashMap<String, String>();

        tvsTableCellTips = new HashMap<String, String>();
        tvsTableColumnTips = new HashMap<String, String>();

        /*
         * Create tips for the CELL table
         */
        generateCellTableCellTips();
        generateCellTableColumnTips();

        /*
         * Create tips for the DMD table
         */
        generateDmdTableCellTips();
        generateDmdTableColumnTips();

        /*
         * Create tips for the MESO table
         */
        // generateMesoTableCellTips(); -- Not needed at this time
        generateMesoTableColumnTips();

        /*
         * Create tips for the TVS table
         */
        // generateTvsTableCellTips(); -- Not needed at this time
        generateTvsTableColumnTips();
    }

    private void generateCellTableCellTips() {
        SCANConfig scanCfg = SCANConfig.getInstance();

        for (CELLTable tableCol : CELLTable.values()) {
            if (tableCol == CELLTable.IDENT) {
                cellTableCellTips
                        .put(tableCol.getColName(),
                                "Left click to zoom and recenter,\nright click to display trend set.");
                // TODO : check for TOR/SVR (unwarned)
            } else if (scanCfg.canViewTrend(ScanTables.CELL, tableCol
                    .getColName()) == true) {
                cellTableCellTips.put(tableCol.getColName(),
                        "Left click to view a trend\nfor this attribute.");
            }
        }
    }

    private void generateCellTableColumnTips() {
        SCANConfig scanCfg = SCANConfig.getInstance();
        StringBuffer mainStr = new StringBuffer();
        mainStr.append("- Left click to rank by this attribute,\n");
        mainStr.append("- left click again to reverse rank direction");

        cellTableColumnTips.clear();
        for (CELLTable tableCol : CELLTable.values()) {
            int sortDir = scanCfg.getSortDirection(ScanTables.CELL, tableCol
                    .getColName());

            // If the sort direction is none then there is no tip text.
            if (sortDir == SortDirection.None.getSortDir()) {
                continue;
            }

            // Create the tip text
            if (scanCfg
                    .isClutterControl(ScanTables.CELL, tableCol.getColName()) == true
                    && scanCfg.isRadVar(ScanTables.CELL, tableCol.getColName()) == true) {
                cellTableColumnTips.put(tableCol.getColName(), mainStr
                        + getClutterAndRadiusString());
            } else if (scanCfg.isClutterControl(ScanTables.CELL, tableCol
                    .getColName()) == true) {
                cellTableColumnTips.put(tableCol.getColName(), mainStr
                        + getClutterControlString());
            } else if (scanCfg.isRadVar(ScanTables.CELL, tableCol.getColName()) == true) {
                cellTableColumnTips.put(tableCol.getColName(), mainStr
                        + getRadiusInterpoationString());
            } else {
                cellTableColumnTips.put(tableCol.getColName(), mainStr
                        .toString());
            }
        }
    }

    private void generateDmdTableCellTips() {
        SCANConfig scanCfg = SCANConfig.getInstance();

        // TODO : fix for DMD

        for (DMDTable tableCol : DMDTable.values()) {
            if (tableCol == DMDTable.IDENT) {
                dmdTableCellTips
                        .put(tableCol.getColName(),
                                "Left click to zoom and recenter,\nright click to display trend set.");
                // TODO : check for TOR/SVR (unwarned)
            } else if (scanCfg.canViewTrend(ScanTables.DMD, tableCol
                    .getColName()) == true) {
                StringBuilder sb = new StringBuilder();
                if (scanCfg.canViewTrend(ScanTables.DMD, tableCol.getColName()) == true) {
                    sb
                            .append("Left click to view a trend\nfor this attribute.");
                }

                if (scanCfg.canViewTimeHeight(ScanTables.DMD, tableCol
                        .getColName())) {
                    sb
                            .append(" Right\nclick to view a time-height\ntrend for this attribute.");
                }

                dmdTableCellTips.put(tableCol.getColName(), sb.toString());
            }
        }
    }

    private void generateDmdTableColumnTips() {
        SCANConfig scanCfg = SCANConfig.getInstance();
        StringBuffer mainStr = new StringBuffer();
        mainStr.append("- Left click to rank by this attribute,\n");
        mainStr.append("- left click again to reverse rank direction");

        dmdTableColumnTips.clear();
        for (DMDTable tableCol : DMDTable.values()) {
            int sortDir = scanCfg.getSortDirection(ScanTables.DMD, tableCol
                    .getColName());

            // If the sort direction is none then there is no tip text.
            if (sortDir == SortDirection.None.getSortDir()) {
                continue;
            }

            // Create the tip text
            if (scanCfg.isClutterControl(ScanTables.DMD, tableCol.getColName()) == true) {
                dmdTableColumnTips.put(tableCol.getColName(), mainStr
                        + getClutterControlString());
            } else {
                dmdTableColumnTips.put(tableCol.getColName(), mainStr
                        .toString());
            }
        }
    }

    private void generateMesoTableColumnTips() {
        SCANConfig scanCfg = SCANConfig.getInstance();
        StringBuffer mainStr = new StringBuffer();
        mainStr.append("- Left click to rank by this attribute,\n");
        mainStr.append("- left click again to reverse rank direction");

        mesoTableColumnTips.clear();
        for (MESOTable tableCol : MESOTable.values()) {
            int sortDir = scanCfg.getSortDirection(ScanTables.MESO, tableCol
                    .getColName());

            // If the sort direction is none then there is no tip text.
            if (sortDir == SortDirection.None.getSortDir()) {
                continue;
            }

            // Create the tip text
            if (scanCfg
                    .isClutterControl(ScanTables.MESO, tableCol.getColName()) == true) {
                mesoTableColumnTips.put(tableCol.getColName(), mainStr
                        + getClutterControlString());
            } else {
                mesoTableColumnTips.put(tableCol.getColName(), mainStr
                        .toString());
            }
        }
    }

    private void generateTvsTableColumnTips() {
        SCANConfig scanCfg = SCANConfig.getInstance();
        StringBuffer mainStr = new StringBuffer();
        mainStr.append("- Left click to rank by this attribute,\n");
        mainStr.append("- left click again to reverse rank direction");

        tvsTableColumnTips.clear();
        for (TVSTable tableCol : TVSTable.values()) {
            int sortDir = scanCfg.getSortDirection(ScanTables.TVS, tableCol
                    .getColName());

            // If the sort direction is none then there is no tip text.
            if (sortDir == SortDirection.None.getSortDir()) {
                continue;
            }

            // Create the tip text
            if (scanCfg.isClutterControl(ScanTables.TVS, tableCol.getColName()) == true) {
                tvsTableColumnTips.put(tableCol.getColName(), mainStr
                        + getClutterControlString());
            } else {
                tvsTableColumnTips.put(tableCol.getColName(), mainStr
                        .toString());
            }
        }
    }

    private String getClutterControlString() {
        StringBuilder ccSb = new StringBuilder();

        ccSb.append("\nThe light green title indicates that this\n");
        ccSb.append("variable is the current clutter control\n");
        ccSb.append("variable (see Configurations->D2D Display).");

        return ccSb.toString();
    }

    private String getRadiusInterpoationString() {
        StringBuilder radVarSb = new StringBuilder();

        radVarSb.append("\nThe dark green background indicates this\n");
        radVarSb.append("is the current radius interpolation variable\n");
        radVarSb.append("(see Configurations->D2D Display).");

        return radVarSb.toString();
    }

    private String getClutterAndRadiusString() {
        StringBuilder ccRadVar = new StringBuilder();

        ccRadVar.append("\nThe dark green background and light green\n");
        ccRadVar.append("title indicate that this is both the current\n");
        ccRadVar.append("radius interpolation variable and the\n");
        ccRadVar.append("current clutter control variable\n");
        ccRadVar.append("(see Configurations->D2D Display).");

        return ccRadVar.toString();
    }

    public String getTableCellTip(ScanTables scanTable, String columnName) {
        switch (scanTable) {
        case CELL:
            return cellTableCellTips.get(columnName);
        case DMD:
            return dmdTableCellTips.get(columnName);
            // case MESO :
            // return mesoTableCellTips.get(columnName);
            // case TVS :
            // return tvsTableCellTips.get(columnName);
        }

        return null;
    }

    public String getTableColumnTip(ScanTables scanTable, String columnName) {
        switch (scanTable) {
        case CELL:
            return cellTableColumnTips.get(columnName);
        case DMD:
            return dmdTableColumnTips.get(columnName);
        case MESO:
            return mesoTableColumnTips.get(columnName);
        case TVS:
            return tvsTableColumnTips.get(columnName);
        }

        return null;
    }

    public void regenerateTableColumnTips(ScanTables scanTable) {
        switch (scanTable) {
        case CELL:
            generateCellTableColumnTips();
            break;
        case DMD:
            generateDmdTableColumnTips();
            break;
        case MESO:
            generateMesoTableColumnTips();
            break;
        case TVS:
            generateTvsTableColumnTips();
            break;
        }
    }
}
