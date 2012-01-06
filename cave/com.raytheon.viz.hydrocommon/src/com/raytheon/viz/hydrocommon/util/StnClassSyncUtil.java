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
package com.raytheon.viz.hydrocommon.util;

import java.util.ArrayList;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.DataIngestFilterData;
import com.raytheon.viz.hydrocommon.data.DcpData;
import com.raytheon.viz.hydrocommon.data.ObserverData;
import com.raytheon.viz.hydrocommon.data.RPFFcstPointData;
import com.raytheon.viz.hydrocommon.data.ReservoirData;
import com.raytheon.viz.hydrocommon.data.StationClassData;
import com.raytheon.viz.hydrocommon.data.TelemData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;

/**
 * This class is used to Synchronize the cache table stnclass
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2009 1802       askripsk     Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class StnClassSyncUtil {

    /**
     * Synchronizes the stnclass table with the ingestfilter, dcp, telem and
     * observer tables. The Modify Location dialog uses the stnclass to display
     * information about the location, so it has one place to look for the data.
     * 
     * @param lid
     * @throws VizException
     */
    public static void setStnClass(String lid) throws VizException {
        boolean telemFound = false;
        boolean dcpFound = false;
        boolean obsFound = false;
        boolean fcstPtFound = false;
        boolean reservoirFound = false;

        /* determine the station's data sources */
        // Telem
        TelemData telemSeedData = new TelemData();
        telemSeedData.setLid(lid);

        ArrayList<TelemData> telemData = HydroDBDataManager.getInstance()
                .getData(telemSeedData);

        int telemCount = telemData.size();
        if (telemCount != 0) {
            telemFound = true;
        }

        // DCP
        DcpData dcpSeedData = new DcpData();
        dcpSeedData.setLid(lid);

        int dcpCount = HydroDBDataManager.getInstance().checkData(dcpSeedData);
        if (dcpCount != 0) {
            dcpFound = true;
        }

        // Observer
        ObserverData obsSeedData = new ObserverData();
        obsSeedData.setLid(lid);

        int obsCount = HydroDBDataManager.getInstance().checkData(obsSeedData);
        if (obsCount != 0) {
            obsFound = true;
        }

        // RiverPro Forecast Point
        RPFFcstPointData fcstPtSeedData = new RPFFcstPointData();
        fcstPtSeedData.setLid(lid);

        int fcstPtCount = HydroDBDataManager.getInstance().checkData(
                fcstPtSeedData);
        if (fcstPtCount != 0) {
            fcstPtFound = true;
        }

        // Reservoir
        ReservoirData reservoirSeedData = new ReservoirData();
        fcstPtSeedData.setLid(lid);

        int reservoirCount = HydroDBDataManager.getInstance().checkData(
                reservoirSeedData);
        if (reservoirCount != 0) {
            reservoirFound = true;
        }

        /* determine the station class */

        /* initialize */
        int Hnum = 0;
        int Pnum = 0;
        int Snum = 0;
        int Tnum = 0;
        int Qnum = 0;
        int PAnum = 0;
        int numpe = 0;

        boolean is_offriv = false;
        boolean is_res = false;
        boolean is_riv = false;
        boolean is_precip = false;
        boolean is_snow = false;
        boolean is_temp = false;
        boolean is_other = false;
        boolean is_undef = false;

        /*
         * an official forecast point is a station that is defined in the
         * RpfFcstPoint table. a reservoir include points that have an entry in
         * the reservoir table
         */

        if (fcstPtFound) {
            is_offriv = true;
        }

        if (reservoirFound) {
            is_res = true;
        }

        /* get data elements defined for station */
        DataIngestFilterData ingestSeedData = new DataIngestFilterData();
        ingestSeedData.setWhereClause(" WHERE lid= '" + lid
                + "' AND ingest= 'T'");

        ArrayList<DataIngestFilterData> ingestFilterData = HydroDBDataManager
                .getInstance().getData(ingestSeedData);

        if (ingestFilterData.size() > 0) {

            /* get the count of params for later use */
            Hnum = getPECount(ingestFilterData, "H");
            Qnum = getPECount(ingestFilterData, "Q");

            Snum = getPECount(ingestFilterData, "S");
            Tnum = getPECount(ingestFilterData, "T");

            Pnum = getPECount(ingestFilterData, "P");
            PAnum = getPECount(ingestFilterData, "PA");
            Pnum = Pnum - PAnum;

            numpe = ingestFilterData.size();

            /*
             * also, a station is a reservoir if it has a param type of HP or HT
             * or LS
             */
            if (getPECount(ingestFilterData, "HP") > 0
                    || getPECount(ingestFilterData, "HT") > 0
                    || getPECount(ingestFilterData, "LS") > 0) {
                is_res = true;
            }

            /*
             * a station is a river data point if it has an H or Q parameter and
             * is not considered an official forecast point or reservoir station
             */

            if (!is_offriv && !is_res) {
                if (Hnum > 0 || Qnum > 0) {
                    is_riv = true;
                }
            }

            /*
             * check if the station is a precipitation station, snow, or
             * temperature station
             */

            if (Pnum > 0)
                is_precip = true;

            if (Snum > 0)
                is_snow = true;

            if (Tnum > 0)
                is_temp = true;
        }
        /* no ingest filter entries found for this station */
        else {
            numpe = 0;
        }

        /* now check the special station classes */

        if ((numpe - (Hnum + Qnum + Pnum + Snum + Tnum)) > 0) {
            is_other = true;
        }

        if (!is_offriv && !is_riv && !is_res && !is_precip && !is_snow
                && !is_temp && !is_other) {
            is_undef = true;
        }

        /*
         * now with all the information in hand, load the information into the
         * StnClass table.
         */
        StringBuffer dispClass = new StringBuffer();
        if (is_offriv) {
            dispClass.append("F");
        }
        if (is_res) {
            dispClass.append("D");
        }
        if (is_riv) {
            dispClass.append("R");
        }
        if (is_precip) {
            dispClass.append("P");
        }
        if (is_snow) {
            dispClass.append("S");
        }
        if (is_temp) {
            dispClass.append("T");
        }
        if (is_other) {
            dispClass.append("O");
        }
        if (is_undef) {
            dispClass.append("U");
        }

        String dcp;
        if (dcpFound) {
            dcp = "T";
        } else {
            dcp = "F";
        }

        String telemType = "";
        if (telemFound) {
            telemType = telemData.get(0).getType();
        }

        String obs;
        if (obsFound) {
            obs = "T";
        } else {
            obs = "F";
        }

        /*
         * load the data by deleting the existing entry and loading the new
         * entry.
         */
        StationClassData stnClassOldData = new StationClassData();
        stnClassOldData.setLid(lid);

        HydroDBDataManager.getInstance().deleteRecord(stnClassOldData);

        StationClassData stnClassNewData = new StationClassData();

        // LID
        stnClassNewData.setLid(lid);

        // Station Type
        stnClassNewData.setStationType(dispClass.toString());

        // DCP
        stnClassNewData.setDcp(dcp);

        // Observer
        stnClassNewData.setObserver(obs);

        // Telem Type
        stnClassNewData.setTelemetry(telemType);

        // Save new record to the database
        HydroDBDataManager.getInstance().putData(stnClassNewData);
    }

    /**
     * Updates the stnclass entry for all locations.
     * 
     * @throws VizException
     */
    public static void setStnClassAll() throws VizException {
        // Get the list of lids from the Location Table'
        String lidQuery = "SELECT lid FROM location ORDER BY lid";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                lidQuery);

        // Delete all from the stnClass since the number of records from locview
        // should match the number of records in the stnclass.
        String stnDelete = "DELETE FROM stnclass WHERE lid like '%'";

        HydroDBDataManager.getInstance().runStatement(stnDelete);

        // Throw LIDs to: set_stnclass(String currlid)
        for (QueryResultRow currLid : data.getRows()) {
            setStnClass((String) currLid.getColumn(data.getColumnNames().get(
                    "lid")));
        }
    }

    /**
     * @param ingestFilterData
     *            The list of ingest filters for a particular location.
     * @param currPE
     *            The PE prefix to search for.
     * @return The number of PEs in the ingest filter list that starts with the
     *         specified PE prefix.
     */
    private static int getPECount(
            ArrayList<DataIngestFilterData> ingestFilterData, String currPE) {
        int peCount = 0;

        for (DataIngestFilterData currFilter : ingestFilterData) {
            if (currFilter.getPe().startsWith(currPE)) {
                peCount++;
            }
        }

        return peCount;
    }
}
