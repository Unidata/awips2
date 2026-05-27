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
package com.raytheon.viz.hydro.questionabledata;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.DataLimitData;
import com.raytheon.viz.hydrocommon.data.RejectedData;
import com.raytheon.viz.hydrocommon.datamanager.DataLimitDataManager;
import com.raytheon.viz.hydrocommon.datamanager.PhysicalElementDataManager;
import com.raytheon.viz.hydrocommon.datamanager.RejectedDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.hydrocommon.util.HydroQC;

/**
 * Class for managing database query calls. QuestionableDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2008 1636       askripsky   Initial Creation
 * Feb 07, 2013 1578       rferrel     Code cleanup for non-blocing dialogs.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class QuestionableDataManager extends PhysicalElementDataManager {

    /** Singleton instance of this class. */
    private static final QuestionableDataManager manager = new QuestionableDataManager();

    /** Format to use for time strings. */
    private SimpleDateFormat timeFormat;

    /**
     * Private constructor.
     */
    private QuestionableDataManager() {
        super();
        timeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SS");
        timeFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static QuestionableDataManager getInstance() {
        return manager;
    }

    /**
     * Get tabularDisplayData from the DB
     * 
     * @param sortCriteria
     * @param daysBack
     * 
     * @return list
     */
    public List<QuestionableData> getQuestionableData(String selectedTable,
            int daysBack, String sortCriteria) {
        List<QuestionableData> rval = new ArrayList<QuestionableData>();

        PhysicalElementTable tableName = null;
        String orderByCriteria = "";

        // Calculate the time stamp for the time constraint
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date date = SimulatedTime.getSystemTime().getTime();
        cal.setTime(date);
        cal.add(Calendar.DAY_OF_YEAR, (-1 * daysBack));
        String obsTimeConstraint = timeFormat.format(cal.getTime());

        // Find which table to select data from
        tableName = getSelectedPE(selectedTable);

        // Get Ordering
        if (sortCriteria.compareTo("Location") == 0) {
            orderByCriteria = "phs.lid, phs.obstime DESC";
        } else if (sortCriteria.compareTo("Time") == 0) {
            orderByCriteria = "phs.obstime DESC, phs.lid";
        } else if (sortCriteria.compareTo("Shef Quality") == 0) {
            orderByCriteria = "phs.shef_qual_code DESC, phs.lid, phs.obstime DESC";
        } else if (sortCriteria.compareTo("Quality Code") == 0) {
            orderByCriteria = "quality_code DESC, lid, obstime DESC";
        }

        List<Object[]> data = getPhysicalElementData(tableName,
                obsTimeConstraint, orderByCriteria);

        for (Object[] currData : data) {
            rval.add(new QuestionableData(currData));
        }

        return rval;
    }

    /**
     * Get the table for the selected PE.
     * 
     * @param selectedPE
     * @return
     */
    private PhysicalElementTable getSelectedPE(String selectedPE) {
        PhysicalElementTable tableName = PhysicalElementTable.agricultural;

        if (selectedPE.compareTo("Agriculture") == 0) {
            tableName = PhysicalElementTable.agricultural;
        } else if (selectedPE.compareTo("Discharge") == 0) {
            tableName = PhysicalElementTable.discharge;
        } else if (selectedPE.compareTo("Evaporation") == 0) {
            tableName = PhysicalElementTable.evaporation;
        } else if (selectedPE.compareTo("FishCount") == 0) {
            tableName = PhysicalElementTable.fishcount;
        } else if (selectedPE.compareTo("Gate Dam") == 0) {
            tableName = PhysicalElementTable.gatedam;
        } else if (selectedPE.compareTo("Ground") == 0) {
            tableName = PhysicalElementTable.ground;
        } else if (selectedPE.compareTo("Height (Stage)") == 0) {
            tableName = PhysicalElementTable.height;
        } else if (selectedPE.compareTo("Ice") == 0) {
            tableName = PhysicalElementTable.ice;
        } else if (selectedPE.compareTo("Lake") == 0) {
            tableName = PhysicalElementTable.lake;
        } else if (selectedPE.compareTo("Moisture") == 0) {
            tableName = PhysicalElementTable.moisture;
        } else if (selectedPE.compareTo("Power") == 0) {
            tableName = PhysicalElementTable.power;
        } else if (selectedPE.compareTo("Precipitation (PC)") == 0) {
            tableName = PhysicalElementTable.rawpc;
        } else if (selectedPE.compareTo("Precipitation (PP)") == 0) {
            tableName = PhysicalElementTable.rawpp;
        } else if (selectedPE.compareTo("Precipitation (Other)") == 0) {
            tableName = PhysicalElementTable.rawpother;
        } else if (selectedPE.compareTo("Pressure") == 0) {
            tableName = PhysicalElementTable.pressure;
        } else if (selectedPE.compareTo("Radiation") == 0) {
            tableName = PhysicalElementTable.radiation;
        } else if (selectedPE.compareTo("Snow") == 0) {
            tableName = PhysicalElementTable.snow;
        } else if (selectedPE.compareTo("Temperature") == 0) {
            tableName = PhysicalElementTable.temperature;
        } else if (selectedPE.compareTo("WaterQuality") == 0) {
            tableName = PhysicalElementTable.waterquality;
        } else if (selectedPE.compareTo("Weather") == 0) {
            tableName = PhysicalElementTable.weather;
        } else if (selectedPE.compareTo("Wind") == 0) {
            tableName = PhysicalElementTable.wind;
        } else if (selectedPE.compareTo("YUnique") == 0) {
            tableName = PhysicalElementTable.yunique;
        }

        return tableName;
    }

    /**
     * Create description for the selected data.
     * 
     * @param selectedData
     * @return description
     */
    public String getDescription(QuestionableData selectedData) {
        int qualityCode = selectedData.getQualityCode();

        StringBuffer rval = new StringBuffer(HydroQC.buildQcDescr(qualityCode));

        if (HydroQC.checkQcBit(HydroQC.REASONRANGE_QC, qualityCode) == false) {
            // Get data for the specific pe at the given obs time
            List<DataLimitData> dlData = DataLimitDataManager.getInstance()
                    .getLimits(
                            selectedData.getLid(),
                            selectedData.getPe(),
                            Integer.toString(selectedData.getDur()),
                            selectedData.getObstime().toString()
                                    .substring(5, 10));

            if (dlData.size() > 0) {
                double reasonRangeMax = dlData.get(0).getReasonRangeMax();
                double reasonRangeMin = dlData.get(0).getReasonRangeMin();

                rval.append(" <");

                if (reasonRangeMax != HydroConstants.MISSING_VALUE) {
                    rval.append(String.format(" Max : %f ", reasonRangeMax));
                } else {
                    rval.append(" Max : MISSING VALUE ");
                }

                if (reasonRangeMin != HydroConstants.MISSING_VALUE) {
                    rval.append(String.format("Min : %f ", reasonRangeMin));
                } else {
                    rval.append(" Min : MISSING VALUE ");
                }

                rval.append("> ");
            } else {
                rval.append(" WARNING: NO DATA LIMITS FOUND!!!");
            }

        }

        return rval.toString();
    }

    /**
     * Deletes PE records from the correct tables and inserts the records into
     * the RejectedData table. Also handles the manual cascade delete for RawP*
     * to CurP* and running 'load_obs_river()' db function for height or
     * discharge
     * 
     * @param recordsToDelete
     * @param selectedTable
     * @throws VizException
     */
    public void deleteRecords(List<QuestionableData> recordsToDelete,
            String selectedTable) throws VizException {
        for (QuestionableData currData : recordsToDelete) {
            removePhysicalElementData(currData);

            if (currData.getPe().compareToIgnoreCase("PC") == 0) {
                // Data is from rawPC, cascade delete to CurPC
                removePhysicalElementData(currData, PhysicalElementTable.curpc);
            } else if (currData.getPe().compareToIgnoreCase("PP") == 0) {
                // Data is from rawPP
                removePhysicalElementData(currData, PhysicalElementTable.curpp);
            }
            /* if height or discharge then calculate new RiverStatus as well */
            else if ((currData.getPe().toUpperCase().charAt(0) == 'H')
                    || (currData.getPe().toUpperCase().charAt(0) == 'Q')) {
                String command = String.format(
                        "load_obs_river('%s', '%s', '%s')", currData.getLid(),
                        currData.getPe(), currData.getTs());

                execFunction(command);
            }

            // Add the rejected data to the rejecteddata table
            RejectedDataManager.getInstance().putRejectedData(currData);

        }
    }

    /**
     * Change the record's quality code to missing and update the data base.
     * 
     * @param missingData
     * @throws VizException
     */
    public void setMissing(List<QuestionableData> missingData)
            throws VizException {
        for (QuestionableData currData : missingData) {
            /* set postingtime to current time */
            Date now = SimulatedTime.getSystemTime().getTime();
            currData.setPostingTime(now);

            /* set the shef_qual_code with a "M" for Manual edit */
            currData.setShefQualCode("M");

            // set the QC and revision
            currData.setQualityCode(HydroConstants.QC_MANUAL_PASSED);
            currData.setRevision(1);

            // copy with original value
            RejectedData newData = HydroDataUtils
                    .convertPhysicalElementToRejected(currData);

            /* set value to MISSING, read time widgets and replace in structure */
            currData.setValue(((Integer) HydroConstants.MISSING_VALUE)
                    .doubleValue());

            // Update the physical element table with a missing value
            putPhysicalElementData(currData);

            // Put the original data into the rejecteddata table
            RejectedDataManager.getInstance().putRejectedData(newData);
        }
    }
}
