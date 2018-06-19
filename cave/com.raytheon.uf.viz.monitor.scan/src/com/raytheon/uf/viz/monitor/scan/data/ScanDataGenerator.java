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
package com.raytheon.uf.viz.monitor.scan.data;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.TVSTableDataRow;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.DmdDisplayFilterConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.uf.viz.monitor.scan.tables.SCANTableCellData;
import com.raytheon.uf.viz.monitor.scan.tables.SCANTableData;
import com.raytheon.uf.viz.monitor.scan.tables.SCANTableRowData;

public class ScanDataGenerator {
    private final SCANConfig scanCfg;

    private final ScanMonitor scanMonitor;

    private Map<String, UnwarnedCell> unwarnedCells = null;

    private boolean isUnwarned = false;

    private String site = null;

    public ScanDataGenerator(String site) {
        scanCfg = SCANConfig.getInstance();
        scanMonitor = ScanMonitor.getInstance();
        this.site = site;
    }

    public Map<String, UnwarnedCell> getUnwarnedCells() {
        return unwarnedCells;
    }

    public void setUnwarnedCells(Map<String, UnwarnedCell> unwarnedCells) {
        this.unwarnedCells = unwarnedCells;
        isUnwarned = true;
    }

    /**
     * Get the unwarned cell I want
     * 
     * @param ident
     * @return
     */
    private UnwarnedCell getUnwarnedCell(String ident) {

        if (this.unwarnedCells.containsKey(ident)) {
            return unwarnedCells.get(ident);
        }
        return null;
    }

    public SCANTableData generateCellData(
            ScanTableData<? extends ScanTableDataRow> tableData) {
        SCANTableData tData = new SCANTableData(ScanTables.CELL);

        if (tableData != null) {

            for (String key : tableData.getTableData().keySet()) {
                CellTableDataRow ctdr = (CellTableDataRow) tableData
                        .getTableData().get(key);

                if (scanCfg.getCWAFilter(ScanTables.CELL) == true) {
                    if (ctdr.getCwa().equals(scanMonitor.getCwa(site))) {
                        addCellRow(ctdr, tData);
                    }
                } else {
                    addCellRow(ctdr, tData);
                }
            }

            tData.setFeatureIds(tableData.getFeatureIds());
        }

        return tData;
    }

    public SCANTableData generateDMDData(
            ScanTableData<? extends ScanTableDataRow> tableData) {
        SCANTableData tData = new SCANTableData(ScanTables.DMD);

        if (tableData != null) {

            for (String key : tableData.getTableData().keySet()) {
                DMDTableDataRow dtdr = (DMDTableDataRow) tableData
                        .getTableData().get(key);

                if (scanCfg.getCWAFilter(ScanTables.DMD) == true) {
                    if (dtdr.getCwa().equals(scanMonitor.getCwa(site))) {
                        addDMDRow(dtdr, tData);
                    }
                } else {
                    addDMDRow(dtdr, tData);
                }
            }

            tData.setFeatureIds(tableData.getFeatureIds());
        }

        return tData;
    }

    public SCANTableData generateTVSData(
            ScanTableData<? extends ScanTableDataRow> tableData) {
        SCANTableData tData = new SCANTableData(ScanTables.TVS);

        if (tableData != null) {

            for (String key : tableData.getTableData().keySet()) {
                TVSTableDataRow ttdr = (TVSTableDataRow) tableData
                        .getTableData().get(key);

                if (scanCfg.getCWAFilter(ScanTables.CELL) == true) {
                    if (ttdr.getCwa().equals(scanMonitor.getCwa(site))) {
                        addTVSRow(ttdr, tData);
                    }
                } else {
                    addTVSRow(ttdr, tData);
                }
            }

            tData.setFeatureIds(tableData.getFeatureIds());
        }

        return tData;
    }

    public SCANTableData generateMesoData(
            ScanTableData<? extends ScanTableDataRow> tableData) {
        SCANTableData tData = new SCANTableData(ScanTables.MESO);

        if (tableData != null) {

            for (String key : tableData.getTableData().keySet()) {
                DMDTableDataRow mesoData = (DMDTableDataRow) tableData
                        .getTableData().get(key);

                if (scanCfg.getCWAFilter(ScanTables.CELL) == true) {
                    if (mesoData.getCwa().equals(scanMonitor.getCwa(site))) {
                        addMesoRow(mesoData, tData);
                    }
                } else {
                    addMesoRow(mesoData, tData);
                }
            }

            tData.setFeatureIds(tableData.getFeatureIds());
        }

        return tData;
    }

    public SCANTableData generateFilteredMesoData(
            ScanTableData<? extends ScanTableDataRow> tableData) {
        final DmdDisplayFilterConfig config = SCANConfig.getInstance()
                .getDmdDisplayFilterConfig();
        SCANTableData tData = new SCANTableData(ScanTables.MESO);
        Map<String, DMDTableDataRow> sparseTable = new HashMap<String, DMDTableDataRow>();
        DMDTableDataRow tempRow;

        if (tableData != null) {
            for (String key : tableData.getTableData().keySet()) {
                DMDTableDataRow mesoData = (DMDTableDataRow) tableData
                        .getTableData().get(key);
                if (convertStrankValue(mesoData.getRank()) >= config
                        .getUpperVal()) {
                    if ((!sparseTable.isEmpty())
                            && (sparseTable.containsKey(mesoData.getStrmID()))) {
                        tempRow = sparseTable.get(mesoData.getStrmID());
                        if ((convertStrankValue(tempRow.getRank())) < (convertStrankValue(mesoData
                                .getRank()))) {
                            sparseTable.put(mesoData.getStrmID(), mesoData);
                        }
                    } else {
                        sparseTable.put(mesoData.getStrmID(), mesoData);
                    }
                }
            }
        }

        for (String s : sparseTable.keySet()) {
            if (scanCfg.getCWAFilter(ScanTables.CELL) == true) {
                if (sparseTable.get(s).getCwa()
                        .equals(scanMonitor.getCwa(site))) {
                    addMesoRow(sparseTable.get(s), tData);
                }
            } else {
                addMesoRow(sparseTable.get(s), tData);
            }
        }

        return tData;
    }

    /**
     * Set the stRank background cell color.
     */
    private double convertStrankValue(String stRank) {
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

    private void addCellRow(CellTableDataRow crow, SCANTableData tData) {
        int numCols = SCANConfigEnums.CELLTable.values().length;

        SCANTableRowData trd = new SCANTableRowData(numCols);

        ScanTables scanTable = ScanTables.CELL;

        if (isUnwarned && getUnwarnedCells().containsKey(crow.getIdent())) {

            UnwarnedCell uc = getUnwarnedCell(crow.getIdent());
            // System.out.println("Drawing unwarned "+uc.getWarnType().name()+" CELL ID: "+crow.getIdent());

            trd.setTableCellData(
                    SCANConfigEnums.CELLTable.IDENT.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.CELLTable.IDENT.getColName(), crow
                                    .getIdent(), uc.getWarnType()));
        } else {
            trd.setTableCellData(
                    SCANConfigEnums.CELLTable.IDENT.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.CELLTable.IDENT.getColName(), crow
                                    .getIdent()));
            trd.setIdent(crow.getIdent());
        }
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.AZM.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.AZM
                        .getColName(), crow.getAzm()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.RNG.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.RNG
                        .getColName(), crow.getRng()));

        trd.setTableCellData(
                SCANConfigEnums.CELLTable.TVS.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.TVS
                        .getColName(), crow.getTvs()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.MDASR.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.MDASR.getColName(), crow
                                .getMdaSR()));

        trd.setTableCellData(
                SCANConfigEnums.CELLTable.POSH.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.POSH
                        .getColName(), crow.getPosh()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.POH.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.POH
                        .getColName(), crow.getPoh()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.HSIZE.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.HSIZE.getColName(), crow
                                .getHsize()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.VIL.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.VIL
                        .getColName(), crow.getVil()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.DBZ.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.DBZ
                        .getColName(), crow.getDbz()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.DBZHT.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.DBZHT.getColName(), crow
                                .getDbzHt()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.TOP.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.TOP
                        .getColName(), crow.getTop()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.DIR.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.DIR
                        .getColName(), crow.getDir()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.SPD.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.SPD
                        .getColName(), crow.getSpd()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.AZM15.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.AZM15.getColName(), crow
                                .getAzm15()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.RNG15.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.RNG15.getColName(), crow
                                .getRng15()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.AZM30.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.AZM30.getColName(), crow
                                .getAzm30()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.RNG30.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.RNG30.getColName(), crow
                                .getRng30()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.AZM45.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.AZM45.getColName(), crow
                                .getAzm45()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.RNG45.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.RNG45.getColName(), crow
                                .getRng45()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.AZM60.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.AZM60.getColName(), crow
                                .getAzm60()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.RNG60.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.RNG60.getColName(), crow
                                .getRng60()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.MVTERR.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.MVTERR.getColName(), crow
                                .getMvtErr()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.MVTMN.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.MVTMN.getColName(), crow
                                .getMvtMn()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.LAT.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.LAT
                        .getColName(), crow.getLat()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.LON.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.LON
                        .getColName(), crow.getLon()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.POLH.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.POLH
                        .getColName(), crow.getPolh()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.SVRWX.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.SVRWX.getColName(), crow
                                .getSvrwx()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.HVYPR.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.HVYPR.getColName(), crow
                                .getHvyPr()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.PPOS.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.CELLTable.PPOS
                        .getColName(), crow.getPos()));
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.CGRATE.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.CGRATE.getColName(), crow
                                .getCgRate()));
        if (crow.getCape() != -99999.0) {
            trd.setTableCellData(
                    SCANConfigEnums.CELLTable.CAPE.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.CELLTable.CAPE.getColName(), crow
                                    .getCape()));
        } else {
            trd.setTableCellData(SCANConfigEnums.CELLTable.CAPE.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.CELLTable.CAPE.getColName(), "N/A"));
        }
        if (crow.getSreh() != -99999.0) {
            trd.setTableCellData(
                    SCANConfigEnums.CELLTable.SREH.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.CELLTable.SREH.getColName(), crow
                                    .getSreh()));
        } else {
            trd.setTableCellData(SCANConfigEnums.CELLTable.SREH.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.CELLTable.SREH.getColName(), "N/A"));
        }
        trd.setTableCellData(
                SCANConfigEnums.CELLTable.COUNTY.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.CELLTable.COUNTY.getColName(), crow
                                .getCounty()));

        trd.setVcp(crow.getVcp());

        tData.addDataRow(trd);
    }

    private void addDMDRow(DMDTableDataRow mrow, SCANTableData tData) {

        if (!mrow.getRank().equals("N/A")) {
            // && (new Double(mrow.getRank()) >= scanCfg
            // .getDmdDisplayFilterConfig().getLowerVal())) {
            int numCols = SCANConfigEnums.DMDTable.values().length;

            SCANTableRowData trd = new SCANTableRowData(numCols);

            ScanTables scanTable = ScanTables.DMD;

            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.IDENT.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.IDENT.getColName(), mrow
                                    .getIdent()));
            trd.setIdent(mrow.getIdent());
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.AZM.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.AZM.getColName(), mrow
                                    .getAzm()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.RNG.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.RNG.getColName(), mrow
                                    .getRng() * ScanUtils.KM_TO_NMI));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.STRANK.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.STRANK.getColName(), mrow
                                    .getRank() + mrow.getRankType()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.CLASS.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.CLASS.getColName(), mrow
                                    .getRank()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.STATUS.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.STATUS.getColName(), mrow
                                    .getStatus()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.MSI.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.MSI.getColName(), mrow
                                    .getMsi()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.TVS.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.TVS.getColName(), mrow
                                    .getTvs()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.ELEV0.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.ELEV0.getColName(), mrow
                                    .getLowestElev(mrow.getElev0())));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.BASE.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.BASE.getColName(), mrow
                                    .getBase(), mrow.getLowestElev(mrow
                                    .getElev0())));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.DEPTH.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.DEPTH.getColName(), mrow
                                    .getDepth()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.RELDEP.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.RELDEP.getColName(), mrow
                                    .getRelDepth()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.LLDIAM.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.LLDIAM.getColName(), mrow
                                    .getLlDiam()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.LLVR.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.LLVR.getColName(), mrow
                                    .getLlVr()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.MAXVR.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.MAXVR.getColName(), mrow
                                    .getMaxVr()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.HTMXVR.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.HTMXVR.getColName(), mrow
                                    .getHtMxVr()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.LLSHR.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.LLSHR.getColName(), mrow
                                    .getLlShear()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.LLGTG.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.LLGTG.getColName(), mrow
                                    .getLlgtg()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.LLCONV.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.LLCONV.getColName(), mrow
                                    .getLlConv()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.MLCONV.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.MLCONV.getColName(), mrow
                                    .getMlConv()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.DIR.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.DIR.getColName(), mrow
                                    .getDir()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.SPD.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.SPD.getColName(), mrow
                                    .getSpd()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.AGE.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.AGE.getColName(), mrow
                                    .getAge()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.STRMID.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.STRMID.getColName(), mrow
                                    .getStrmID()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.LAT.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.LAT.getColName(), mrow
                                    .getLat()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.LON.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.LON.getColName(), mrow
                                    .getLon()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.COUNTY.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.COUNTY.getColName(), mrow
                                    .getCounty()));
            trd.setTableCellData(
                    SCANConfigEnums.DMDTable.CWA.ordinal(),
                    new SCANTableCellData(scanTable,
                            SCANConfigEnums.DMDTable.CWA.getColName(), mrow
                                    .getCwa()));
            tData.addDataRow(trd);
        }
    }

    private void addTVSRow(TVSTableDataRow trow, SCANTableData tData) {
        int numCols = SCANConfigEnums.TVSTable.values().length;

        SCANTableRowData trd = new SCANTableRowData(numCols);

        ScanTables scanTable = ScanTables.TVS;

        trd.setTableCellData(
                SCANConfigEnums.TVSTable.STRMID.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.TVSTable.STRMID.getColName(), trow
                                .getStrmID()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.IDENT.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.IDENT
                        .getColName(), trow.getIdent()));
        trd.setIdent(trow.getIdent());
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.TYPE.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.TYPE
                        .getColName(), trow.getType()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.AZM.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.AZM
                        .getColName(), trow.getAzm()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.RNG.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.RNG
                        .getColName(), trow.getRng()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.AVGDV.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.AVGDV
                        .getColName(), trow.getAvgDv()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.LLDV.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.LLDV
                        .getColName(), trow.getLlDV()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.MAXDV.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.MAXDV
                        .getColName(), trow.getMaxDV()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.MXDVHT.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.TVSTable.MXDVHT.getColName(), trow
                                .getMaxDvHt()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.BASE.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.BASE
                        .getColName(), trow.getBase()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.DEPTH.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.DEPTH
                        .getColName(), trow.getDepth()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.TOP.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.TOP
                        .getColName(), trow.getTop()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.SHEAR.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.SHEAR
                        .getColName(), trow.getShear()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.SHRHT.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.SHRHT
                        .getColName(), trow.getShrHt()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.LAT.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.LAT
                        .getColName(), trow.getLat()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.LON.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.TVSTable.LON
                        .getColName(), trow.getLon()));
        trd.setTableCellData(
                SCANConfigEnums.TVSTable.COUNTY.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.TVSTable.COUNTY.getColName(), trow
                                .getCounty()));
        tData.addDataRow(trd);
    }

    private void addMesoRow(DMDTableDataRow mrow, SCANTableData tData) {
        int numCols = SCANConfigEnums.MESOTable.values().length;

        SCANTableRowData trd = new SCANTableRowData(numCols);

        ScanTables scanTable = ScanTables.MESO;

        // Since tvs can have letters that differ between dmd and meso tables
        // we must appropriately populate tvs string field in meso table.
        String tvsStr = mrow.getTvs().trim();
        if ((tvsStr.equals("TVS")) || (tvsStr.equals("Y"))) {
            tvsStr = "Y";
        } else {
            tvsStr = "N";
        }

        // Since stRank can have letters we must strip out the potential
        // characters when populating class string field in meso table.
        String classStr = mrow.getRank().trim();
        StringBuilder sb = new StringBuilder();
        char[] rankChars = classStr.toCharArray();

        for (int i = 0; i < rankChars.length; i++) {
            if (String.valueOf(rankChars[i]).matches("[0-9.]") == true) {
                sb.append(rankChars[i]);
            }
        }
        classStr = sb.toString();

        trd.setTableCellData(
                SCANConfigEnums.MESOTable.STRMID.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.STRMID.getColName(), mrow
                                .getStrmID()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.IDENT.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.IDENT.getColName(), mrow
                                .getIdent()));
        trd.setIdent(mrow.getIdent());
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.AZM.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.AZM
                        .getColName(), mrow.getAzm()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.RNG.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.RNG
                        .getColName(), mrow.getRng()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.MDASR.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.MDASR.getColName(), mrow
                                .getRank()));
        trd.setTableCellData(SCANConfigEnums.MESOTable.CLASS.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.CLASS.getColName(), classStr));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.LLVR.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.LLVR
                        .getColName(), mrow.getLlVr()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.LLGTG.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.LLGTG.getColName(), mrow
                                .getLlgtg()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.BASE.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.BASE
                        .getColName(), mrow.getBase()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.DEPTH.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.DEPTH.getColName(), mrow
                                .getDepth()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.RELDEP.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.RELDEP.getColName(), mrow
                                .getRelDepth()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.MAXVR.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.MAXVR.getColName(), mrow
                                .getMaxVr()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.HTMXVR.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.HTMXVR.getColName(), mrow
                                .getHtMxVr()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.TVS.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.TVS
                        .getColName(), tvsStr));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.DIR.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.DIR
                        .getColName(), mrow.getDir()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.SPD.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.SPD
                        .getColName(), mrow.getSpd()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.MSI.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.MSI
                        .getColName(), mrow.getMsi()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.LAT.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.LAT
                        .getColName(), mrow.getLat()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.LON.ordinal(),
                new SCANTableCellData(scanTable, SCANConfigEnums.MESOTable.LON
                        .getColName(), mrow.getLon()));
        trd.setTableCellData(
                SCANConfigEnums.MESOTable.COUNTY.ordinal(),
                new SCANTableCellData(scanTable,
                        SCANConfigEnums.MESOTable.COUNTY.getColName(), mrow
                                .getCounty()));
        tData.addDataRow(trd);
    }

    /**
     * get the site
     * 
     * @return
     */
    public String getSite() {
        return site;
    }

}
