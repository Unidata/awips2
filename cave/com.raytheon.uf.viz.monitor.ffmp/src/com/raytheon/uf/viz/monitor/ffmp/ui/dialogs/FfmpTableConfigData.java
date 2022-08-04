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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.SortDirection;
import com.raytheon.uf.viz.monitor.data.ColumnAttribData;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.xml.FFMPTableColumnXML;

/**
 * Table column/row configuration container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2011            lvenable     Initial creation
 * Mar 8, 2012  DR 14406  gzhang       Fixing QPF Column Title Missing 
 * Feb 19, 2013  1635     dhladky      Fixed multiple guidance displays
 * Sep 21, 2015  4756     dhladky      Multiple guidance source upgrades.
 * Jul 30, 2018  6720     njensen      Update for changed method names
 * Aug 07, 2018  6720     njensen      Added toString()
 * 
 * </pre>
 * 
 * 
 * @author lvenable
 */

public class FfmpTableConfigData {

    private String siteKey;

    /**
     * Column attribute map.
     */
    private Map<String, ColumnAttribData> tableColumnAttrMap = new HashMap<>();

    /**
     * FFMP table column names.
     */
    public enum COLUMN_NAME {

        NAME("NAME"),
        RATE("RATE"),
        QPE("QPE"),
        QPF("QPF"),
        GUID("GUID"),
        RATIO("RATIO"),
        DIFF("DIFF");

        private final String columnName;

        private COLUMN_NAME(String name) {
            columnName = name;
        }

        public String getColumnName() {
            return columnName;
        }
    }

    /**
     * QPF Display Type index.
     */
    private String qpfDisplayName = null;

    /**
     * FFG displayed
     */
    private List<String> ffgNames = null;

    private String ffgGraphType = null;

    private String qpfGraphType = null;

    private LinkedHashMap<String, String> ffmpTableColMap = new LinkedHashMap<>();

    public FfmpTableConfigData(String siteKey) {
        this.siteKey = siteKey;

        init();
    }

    private void init() {
        createTableColumns();

        createFFMPTableColumnAttributes();
    }

    /**
     * Create the FFMP Basin Table column attributes.
     * 
     * @param qpfName
     *            The QPF data type
     */
    private void createFFMPTableColumnAttributes() {
        FFMPConfig config = FFMPConfig.getInstance();
        AttributesDlgData attrData = new AttributesDlgData();

        List<FFMPTableColumnXML> columnList = config.getFFMPConfigData()
                .getTableColumnData();
        SortDirection sortDir = SortDirection.Ascending;
        boolean reverseFilter = false;
        for (String key : ffmpTableColMap.keySet()) {
            String columnName = key;
            String displayName = null;
            if (columnName.contains("_")) {
                String[] parts = columnName.split("_");
                columnName = parts[1];
                displayName = parts[0];
            }
            for (FFMPTableColumnXML col : columnList) {
                if (col.getColumnName().equalsIgnoreCase(columnName)) {
                    String colName = col.getColumnName();
                    reverseFilter = col.getReverseFilter();
                    String sort = col.getSort();
                    if (sort != null) {
                        if ("ascending".equalsIgnoreCase(sort)) {
                            sortDir = SortDirection.Ascending;
                        } else if ("descending".equalsIgnoreCase(sort)) {
                            sortDir = SortDirection.Decending;
                        } else {
                            sortDir = SortDirection.None;
                        }
                    }
                    if (colName.equalsIgnoreCase(
                            COLUMN_NAME.NAME.getColumnName())) {
                        tableColumnAttrMap.put(key,
                                new ColumnAttribData(
                                        COLUMN_NAME.NAME.getColumnName(),
                                        sortDir, reverseFilter, "::"));
                        attrData.setColumnVisible(key,
                                col.getDisplayedInTable());
                    } else if (colName.equalsIgnoreCase(
                            COLUMN_NAME.RATE.getColumnName())) {
                        tableColumnAttrMap.put(key,
                                new ColumnAttribData(
                                        COLUMN_NAME.RATE.getColumnName(),
                                        sortDir, reverseFilter, "::"));
                        attrData.setColumnVisible(key,
                                col.getDisplayedInTable());
                    } else if (colName.equalsIgnoreCase(
                            COLUMN_NAME.QPE.getColumnName())) {
                        tableColumnAttrMap.put(key,
                                new ColumnAttribData(
                                        COLUMN_NAME.QPE.getColumnName(),
                                        sortDir, reverseFilter, "::"));
                        attrData.setColumnVisible(key,
                                col.getDisplayedInTable());
                    } else if (colName.equalsIgnoreCase(
                            COLUMN_NAME.QPF.getColumnName())) {
                        tableColumnAttrMap.put(key,
                                new ColumnAttribData(
                                        qpfDisplayName + "::"
                                                + COLUMN_NAME.QPF
                                                        .getColumnName(),
                                        sortDir, reverseFilter, "::"));
                        attrData.setColumnVisible(key,
                                col.getDisplayedInTable());
                    } else if (colName.equalsIgnoreCase(
                            COLUMN_NAME.GUID.getColumnName())) {
                        for (String ffgName : ffgNames) {
                            tableColumnAttrMap.put(
                                    ffgName + "_"
                                            + COLUMN_NAME.GUID.getColumnName(),
                                    new ColumnAttribData(
                                            ffgName + "::"
                                                    + COLUMN_NAME.GUID
                                                            .getColumnName(),
                                            sortDir, reverseFilter, "::"));
                        }
                        attrData.setColumnVisible(
                                COLUMN_NAME.GUID.getColumnName(),
                                col.getDisplayedInTable());
                    } else if ("RATIO".equalsIgnoreCase(colName)) {
                        for (String ffgName : ffgNames) {
                            tableColumnAttrMap.put(
                                    ffgName + "_"
                                            + COLUMN_NAME.RATIO.getColumnName(),
                                    new ColumnAttribData(
                                            ffgName + "::"
                                                    + COLUMN_NAME.RATIO
                                                            .getColumnName(),
                                            sortDir, reverseFilter, "::"));
                        }
                        attrData.setColumnVisible(
                                COLUMN_NAME.RATIO.getColumnName(),
                                col.getDisplayedInTable());
                    } else if (colName.equalsIgnoreCase(
                            COLUMN_NAME.DIFF.getColumnName())) {
                        for (String ffgName : ffgNames) {
                            tableColumnAttrMap.put(
                                    ffgName + "_"
                                            + COLUMN_NAME.DIFF.getColumnName(),
                                    new ColumnAttribData(
                                            ffgName + "::"
                                                    + COLUMN_NAME.DIFF
                                                            .getColumnName(),
                                            sortDir, reverseFilter, "::"));
                        }
                        attrData.setColumnVisible(
                                COLUMN_NAME.DIFF.getColumnName(),
                                col.getDisplayedInTable());
                    }

                    boolean includedInTable = false;
                    if (colName
                            .equalsIgnoreCase(COLUMN_NAME.GUID.getColumnName())
                            || colName.equalsIgnoreCase(
                                    COLUMN_NAME.RATIO.getColumnName())
                            || colName.equalsIgnoreCase(
                                    COLUMN_NAME.DIFF.getColumnName())) {
                        if (config.getIncludedGuids().contains(displayName)) {
                            includedInTable = true;
                            attrData.setGuidColumnIncluded(displayName,
                                    includedInTable);
                        }
                    }

                    break;
                }
            }
        }

        config.setAttrData(attrData);
    }

    private void createTableColumns() {
        FFMPMonitor monitor = FFMPMonitor.getInstance();
        FFMPRunConfigurationManager configManager = FFMPRunConfigurationManager
                .getInstance();
        FFMPRunXML runner = configManager.getRunner(monitor.getWfo());
        ProductRunXML prodRunXml = runner.getProduct(siteKey);
        String name = prodRunXml.getProductName();

        FFMPSourceConfigurationManager sourceConfigManager = FFMPSourceConfigurationManager
                .getInstance();
        ProductXML productXml = sourceConfigManager
                .getProductByPrimarySourceName(name);

        ffgNames = prodRunXml.getGuidanceDisplayNames(productXml);

        String includedQpf = FFMPConfig.getInstance().getFFMPConfigData()
                .getIncludedQPF();
        if ((includedQpf != null) && (!"xxxxx".equalsIgnoreCase(includedQpf))) {
            qpfDisplayName = includedQpf;
            if (!prodRunXml.getQpfDisplayNames(productXml)
                    .contains(qpfDisplayName)) {
                qpfDisplayName = prodRunXml.getQpfDisplayNames(productXml)
                        .get(0);
            }
        } else {
            qpfDisplayName = prodRunXml.getQpfDisplayNames(productXml).get(0);
        }

        List<String> guidDisplayNames = productXml
                .getAvailableGuidanceDisplayNames();

        ffmpTableColMap.put(COLUMN_NAME.NAME.getColumnName(),
                COLUMN_NAME.NAME.getColumnName());
        ffmpTableColMap.put(COLUMN_NAME.RATE.getColumnName(),
                COLUMN_NAME.RATE.getColumnName());
        ffmpTableColMap.put(COLUMN_NAME.QPE.getColumnName(),
                COLUMN_NAME.QPE.getColumnName());
        ffmpTableColMap.put(COLUMN_NAME.QPF.getColumnName(),
                COLUMN_NAME.QPF.getColumnName());
        for (String s : guidDisplayNames) {
            ffmpTableColMap.put(s + "_" + COLUMN_NAME.GUID.getColumnName(),
                    COLUMN_NAME.GUID.getColumnName());
            ffmpTableColMap.put(s + "_" + COLUMN_NAME.RATIO.getColumnName(),
                    COLUMN_NAME.RATIO.getColumnName());
            ffmpTableColMap.put(s + "_" + COLUMN_NAME.DIFF.getColumnName(),
                    COLUMN_NAME.DIFF.getColumnName());
        }
    }

    /**
     * Get the table column keys.
     * 
     * @return Array of table column keys
     */
    public String[] getTableColumnKeys() {
        Set<String> keySet = ffmpTableColMap.keySet();
        return keySet.toArray(new String[keySet.size()]);
    }

    /**
     * Get the column index.
     * 
     * @param columnName
     *            Column name/key.
     * @return The column index.
     */
    public int getTableColumnIndex(String columnName) {
        Set<String> keySet = ffmpTableColMap.keySet();
        int idx = 0;
        for (String key : keySet) {
            if (columnName.equalsIgnoreCase(key)) {
                return idx;
            }
            idx++;
        }

        return 0;
    }

    /**
     * Get the column attribute data for the table.
     * 
     * @param columnNameKey
     *            Column name/key.
     * @return Column attribute data.
     */
    public ColumnAttribData getTableColumnAttr(String columnNameKey) {
        return tableColumnAttrMap.get(columnNameKey);
    }

    /**
     * @return the qpfTypeIdx
     */
    public String getQpfDisplayName() {
        return qpfDisplayName;
    }

    /**
     * Set the QPF Type. At the same time set the column header text.
     * 
     * @param qpfDisplayName
     *            the qpfType to set
     */
    public void setQpfDisplayName(String qpfDisplayName, String siteKey) {
        FFMPMonitor monitor = FFMPMonitor.getInstance();
        FFMPRunConfigurationManager configManager = FFMPRunConfigurationManager
                .getInstance();
        FFMPRunXML runner = configManager.getRunner(monitor.getWfo());
        ProductRunXML prodRunXml = runner.getProduct(siteKey);
        String name = prodRunXml.getProductName();

        FFMPSourceConfigurationManager sourceConfigManager = FFMPSourceConfigurationManager
                .getInstance();
        ProductXML prodXml = sourceConfigManager
                .getProductByPrimarySourceName(name);
        String qpfName = prodRunXml
                .getQpfSourcesByDisplayName(prodXml, qpfDisplayName).get(0)
                .getDisplayName();

        int sDir = tableColumnAttrMap.get(COLUMN_NAME.QPE.getColumnName())
                .getSortDir();
        SortDirection sortDir = SortDirection.Ascending;
        if (sDir == SortDirection.Decending.getSortDir()) {
            sortDir = SortDirection.Decending;
        } else if (sDir == SortDirection.Both.getSortDir()) {
            sortDir = SortDirection.Both;
        } else if (sDir == SortDirection.None.getSortDir()) {
            sortDir = SortDirection.None;
        }

        tableColumnAttrMap.put(COLUMN_NAME.QPF.getColumnName(),
                new ColumnAttribData(
                        qpfName + "::" + COLUMN_NAME.QPF.getColumnName(),
                        sortDir, false, "::"));

        this.qpfDisplayName = qpfDisplayName;
    }

    /**
     * @return the ffgGraphType
     */
    public String getFfgGraphType() {
        if (ffgGraphType == null) {
            ffgGraphType = ffgNames.get(0);
        }

        return ffgGraphType;
    }

    /**
     * @return the ffgGraphType
     */
    public String getQpfGraphDisplayName() {
        if (qpfGraphType == null) {
            FFMPMonitor monitor = FFMPMonitor.getInstance();
            FFMPRunConfigurationManager configManager = FFMPRunConfigurationManager
                    .getInstance();
            FFMPRunXML runner = configManager.getRunner(monitor.getWfo());
            ProductRunXML prodRunXml = runner.getProduct(siteKey);
            String name = prodRunXml.getProductName();

            FFMPSourceConfigurationManager sourceConfigManager = FFMPSourceConfigurationManager
                    .getInstance();
            ProductXML prodXml = sourceConfigManager
                    .getProductByPrimarySourceName(name);
            String qpfName = prodRunXml
                    .getQpfSourcesByDisplayName(prodXml, qpfDisplayName).get(0)
                    .getDisplayName();

            qpfGraphType = qpfName;
        }

        return qpfGraphType;
    }

    /**
     * @param qpfGraphType
     *            the qpfgraphType to set
     */
    public void setQpfGraphType(String qpfGraphType) {
        this.qpfGraphType = qpfGraphType;
    }

    /**
     * @param ffgGraphType
     *            the ffgGraphType to set
     */
    public void setFfgGraphType(String ffgGraphType) {
        this.ffgGraphType = ffgGraphType;
    }

    @Override
    public String toString() {
        return "FfmpTableConfigData [siteKey=" + siteKey
                + ", tableColumnAttrMap=" + tableColumnAttrMap
                + ", qpfDisplayName=" + qpfDisplayName + ", ffgNames="
                + ffgNames + ", ffgGraphType=" + ffgGraphType
                + ", qpfGraphType=" + qpfGraphType + ", ffmpTableColMap="
                + ffmpTableColMap + "]";
    }

}
