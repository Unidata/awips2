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
package com.raytheon.uf.viz.monitor.safeseas.ui.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.MonitorConfigConstants;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.SafeSeasDisplay;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDispMonSwellData;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;

/**
 * SAFESEAS Display Swell Table.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * ????                   ????      Initial creation
 * Dec 26, 2015  5115     skorolev  Corrected imports.
 * Jun 02, 2016  5673     randerso  Fixed header alignment in threshold dialogs
 * Jun 26, 2018  7004     tgurney   Center-align table data
 * May 07, 2019  7689     randerso  Code cleanup
 * May 21, 2019  7689     randerso  Refactor handling of FSSObs thresholds
 *
 * </pre>
 *
 */
public class SSDisplaySwellTab extends TabItemComp
        implements IUpdateDisplayMonitorSwell {
    private SSDispMonSwellEditDlg displaySwellEditDlg;

    private List<SSDispMonSwellData> ssDataArray;

    public SSDisplaySwellTab(TabFolder parent, DataUsageKey duKey) {
        super(parent, duKey, true);
    }

    @Override
    protected void createListHeader() {
        /*
         * Create filler label.
         */
        createGroupHeader("", 0, 0, false);
        createHeader("", 0, 0, false);

        /*
         * Primary Swell
         */
        createGroupHeader("Primary Swell", 1, 8, true);
        createHeader("Height(ft)", 1, 2, true);
        createHeader("Periods(s)", 3, 4, true);
        createHeader("Dir(deg)\n(from)", 5, 6, true);
        createHeader("Dir(deg)\n(to)", 7, 8, true);

        /*
         * Secondary Swell
         */
        createGroupHeader("Secondary Swell", 9, 16, true);
        createHeader("Height(ft)", 9, 10, true);
        createHeader("Periods(s)", 11, 12, true);
        createHeader("Dir(deg)\n(from)", 13, 14, true);
        createHeader("Dir(deg)\n(to)", 15, 16, true);
    }

    @Override
    protected void populateTable() {
        if (ssDataArray == null) {
            createDataArray();
        }

        boolean update = false;
        if (dataTable.getItemCount() > 0) {
            update = true;
        }

        List<String> areaIDArray = new ArrayList<>();

        String currentAreaID;

        SSDispMonSwellData sssd = null;

        int numColumns = 17;
        for (int c = 0; c < numColumns; c++) {
            new TableColumn(dataTable, SWT.CENTER);
        }

        for (int i = 0; i < ssDataArray.size(); i++) {

            TableItem item;
            if (update) {
                item = dataTable.getItem(i);
            } else {
                item = new TableItem(dataTable, SWT.NONE);
            }

            sssd = ssDataArray.get(i);

            currentAreaID = sssd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /*
             * Primary Swell
             */
            appendIntData(item, 1, sssd.getPriSwellHeightR(),
                    sssd.getPriSwellHeightY());

            double higherThreshold = Math.max(sssd.getPriSwellPeriodR(),
                    sssd.getPriSwellPeriodY());
            double lowerThreshold = Math.min(sssd.getPriSwellPeriodR(),
                    sssd.getPriSwellPeriodY());
            if (rankSwellPeriodHigh) {
                sssd.setRankSwellPeriodHigh(true);
                sssd.setPriSwellPeriodR(higherThreshold);
                sssd.setPriSwellPeriodY(lowerThreshold);
            } else {
                sssd.setRankSwellPeriodHigh(false);
                sssd.setPriSwellPeriodR(lowerThreshold);
                sssd.setPriSwellPeriodY(higherThreshold);
            }
            appendIntData(item, 3, sssd.getPriSwellPeriodR(),
                    sssd.getPriSwellPeriodY());

            appendIntData(item, 5, sssd.getPriSwellDirFromY(),
                    sssd.getPriSwellDirFromR());
            appendIntData(item, 7, sssd.getPriSwellDirToR(),
                    sssd.getPriSwellDirToY());

            /*
             * Secondary Swell
             */
            appendIntData(item, 9, sssd.getSecSwellHeightR(),
                    sssd.getSecSwellHeightY());

            higherThreshold = Math.max(sssd.getSecSwellPeriodR(),
                    sssd.getSecSwellPeriodY());
            lowerThreshold = Math.min(sssd.getSecSwellPeriodR(),
                    sssd.getSecSwellPeriodY());
            if (rankSwellPeriodHigh) {
                // sssd.setRankSwellPeriodHigh(true);
                sssd.setSecSwellPeriodR(higherThreshold);
                sssd.setSecSwellPeriodY(lowerThreshold);
            } else {
                // sssd.setRankSwellPeriodHigh(false);
                sssd.setSecSwellPeriodR(lowerThreshold);
                sssd.setSecSwellPeriodY(higherThreshold);
            }
            appendIntData(item, 11, sssd.getSecSwellPeriodR(),
                    sssd.getSecSwellPeriodY());
            appendIntData(item, 13, sssd.getSecSwellDirFromY(),
                    sssd.getSecSwellDirFromR());
            appendIntData(item, 15, sssd.getSecSwellDirToR(),
                    sssd.getSecSwellDirToY());

        }

        packListControls();
    }

    private void createDataArray() {
        ssDataArray = new ArrayList<>();

        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        String xmlKey;

        List<String> areaList = sstm.getCfgMgr().getAreaList();
        for (String areaID : areaList) {
            SSDispMonSwellData sssd = new SSDispMonSwellData();

            sssd.setAreaID(areaID);

            /*
             * Primary Swell
             */
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT.getXmlKey();
            sssd.setPriSwellHeightR(
                    sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setPriSwellHeightY(
                    sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD.getXmlKey();
            sssd.setPriSwellPeriodR(
                    sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setPriSwellPeriodY(
                    sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM.getXmlKey();
            sssd.setPriSwellDirFromR(
                    sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setPriSwellDirFromY(
                    sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO.getXmlKey();
            sssd.setPriSwellDirToR(
                    sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setPriSwellDirToY(
                    sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            /*
             * Secondary Swell
             */
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_HT.getXmlKey();
            sssd.setSecSwellHeightR(
                    sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setSecSwellHeightY(
                    sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_PD.getXmlKey();
            sssd.setSecSwellPeriodR(
                    sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setSecSwellPeriodY(
                    sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM.getXmlKey();
            sssd.setSecSwellDirFromR(
                    sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setSecSwellDirFromY(
                    sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO.getXmlKey();
            sssd.setSecSwellDirToR(
                    sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setSecSwellDirToY(
                    sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            /*
             * Add data to array.
             */
            ssDataArray.add(sssd);
        }
    }

    private SSDispMonSwellData getDataAtFirstSelection() {
        int index = dataTable.getSelectionIndex();

        return ssDataArray.get(index);
    }

    private void updateDataArray(SSDispMonSwellData sssd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int dataListIndexe : dataListIndexes) {
            currentIndex = dataListIndexe;

            ssDataArray.get(currentIndex).updateData(sssd);
        }
    }

    @Override
    public void commitDataToXML() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SSDispMonSwellData sssd : ssDataArray) {

            MonitorConfigConstants.setRankSwellPeriodHigh(rankSwellPeriodHigh);

            areaID = sssd.getAreaID();

            /*
             * Primary Swell
             */
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sssd.getPriSwellHeightR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sssd.getPriSwellHeightY());

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sssd.getPriSwellPeriodR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sssd.getPriSwellPeriodY());

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sssd.getPriSwellDirFromR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sssd.getPriSwellDirFromY());

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sssd.getPriSwellDirToR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sssd.getPriSwellDirToY());

            /*
             * Secondary Swell
             */
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_HT.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sssd.getSecSwellHeightR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sssd.getSecSwellHeightY());

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_PD.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sssd.getSecSwellPeriodR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sssd.getSecSwellPeriodY());

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sssd.getSecSwellDirFromR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sssd.getSecSwellDirFromY());

            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sssd.getSecSwellDirToR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sssd.getSecSwellDirToY());
        }
    }

    @Override
    public void reloadData() {
        dataTable.removeAll();
        ssDataArray.clear();
        ssDataArray = null;

        populateTable();
    }

    @Override
    protected void editDataAction() {
        SSDispMonSwellData sssd = getDataAtFirstSelection();

        if (displaySwellEditDlg == null) {
            displaySwellEditDlg = new SSDispMonSwellEditDlg(
                    getParent().getShell(), sssd, this, true);
            displaySwellEditDlg.open();
            displaySwellEditDlg = null;
        }

    }

    @Override
    public void updateThresholdData(SSDispMonSwellData sssd) {
        updateDataArray(sssd);
        populateTable();

    }
}
