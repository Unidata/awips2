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
package com.raytheon.viz.hydro.pointdatacontrol.engine;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.GageData.ThreatIndex;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2008            mpduff     Initial creation
 * Feb 03, 2014  16843     lbousaidi  add a check for index OutOfBoundsException
 * Mar 08, 2017  19647     snaples     Fixed Class Cast exception when returning List to Arraylist.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PDCFindBasis {

    public static List<GageData> determineRsMofo(
            List<GageData> obsReportList,
            List<GageData> fcstReportList) {
        String currentLid = null;
        double obsVal = PDCConstants.MISSING_VALUE;
        double fcstVal = PDCConstants.MISSING_VALUE;
        List<GageData> reportHead = null;

        /*
         * loop on the two lists and where there are cases where there is an
         * entry in both obs and fcst lists for the same station, then use the
         * higher of the two values. if only one list has a value, use it.
         */

        /* if neither list has data, return now */

        if ((obsReportList == null) && (fcstReportList == null)) {
            return null;
        } else if (obsReportList == null) {
            return fcstReportList;
        } else if (fcstReportList == null) {
            return obsReportList;
        }

        /*
         * loop until we are at the end of both lists. this is an interesting
         * block of logic because it requires simultaneously marching thru two
         * independent sets of data, while tracking the common lid info in them
         * at the same time.
         */

        int obsIndex = 0;
        int fcstIndex = 0;

        /*
         * loop until we are at the end of both lists. this is an interesting
         * block of logic because it requires simultaneously marching thru two
         * independent sets of data, while tracking the common lid info in them
         * at the same time.
         */

        while ((obsIndex < obsReportList.size())
                || (fcstIndex < fcstReportList.size())) {

            /*
             * determine which is the current id to work on. this is the id that
             * is first alphanumerically when looking at both of the lists
             */
            if (((obsIndex < obsReportList.size()) && (obsReportList
                    .get(obsIndex) != null))
                            && (fcstIndex < fcstReportList.size()) && (fcstReportList
                            .get(fcstIndex) != null)) {
                if (obsReportList.get(obsIndex).getLid().compareTo(
                        fcstReportList.get(fcstIndex).getLid()) <= 0) {
                        currentLid = obsReportList.get(obsIndex).getLid();
                } else {
                    currentLid = fcstReportList.get(fcstIndex).getLid();
                }
            } else if ((obsIndex< obsReportList.size()) && (obsReportList
       				.get(obsIndex) != null))  {
            	currentLid = obsReportList.get(obsIndex).getLid();
            } else {
                currentLid = fcstReportList.get(fcstIndex).getLid();
            }
            
            /* initialize for this lid */
            obsVal = PDCConstants.MISSING_VALUE;
            fcstVal = PDCConstants.MISSING_VALUE;

            /* load the temporary value if the id matches the current id */
            if ((obsIndex < obsReportList.size())
                    && obsReportList.get(obsIndex).getLid().equals(currentLid)) {
                obsVal = obsReportList.get(obsIndex).getValue();
            }

            /* ditto for fcst */

            if ((fcstIndex < fcstReportList.size())
                    && (fcstReportList.get(fcstIndex).getLid()
                            .equals(currentLid))) {
                fcstVal = fcstReportList.get(fcstIndex).getValue();
            }

            /* load in the record if a valid value was found */
            if ((obsVal != PDCConstants.MISSING_VALUE)
                    || (fcstVal != PDCConstants.MISSING_VALUE)) {
                if (obsVal >= fcstVal) {
                    reportHead = loadObsFcstReport(obsReportList.get(obsIndex),
                            reportHead);
                } else {
                    reportHead = loadObsFcstReport(fcstReportList
                            .get(fcstIndex), reportHead);
                }
            }

            /* get the next record from the respective lists */

            if ((obsIndex < obsReportList.size())
                    && (obsReportList.get(obsIndex).getLid().equals(currentLid))) {
                obsIndex++;
            }

            if ((fcstIndex < fcstReportList.size())
                    && (fcstReportList.get(fcstIndex).getLid().equals(currentLid))) {
                fcstIndex++;
            }
        } /* end of while loop of ptrs != NULL */

        return reportHead;
    }

    /**
     * This function appends the given report to the input list. The input list
     * is passed back as an argument to handle the initial case where the list
     * is empty. This function appends to a lists, which it creates if it is
     * initially empty.
     * 
     * @param reportList
     * @param reportHead
     * @return
     */
    private static List<GageData> loadObsFcstReport(GageData reportList,
            List<GageData> reportHead) {
        GageData onereport = new GageData();
        List<GageData> outputList = new ArrayList<>();

        if (reportList == null) {
            // fprintf ( stderr , "In routine \"load_obsfcst_report\":\n"
            // "the \"repPtr\" input argument is NULL.\n"
            // "Exiting the routine with a NULL return value.\n" ) ;
            return null;
        }

        /* copy fields */
        onereport.setLid(reportList.getLid());
        onereport.setPe(reportList.getPe());
        onereport.setDur(reportList.getDur());
        onereport.setTs(reportList.getTs());
        onereport.setExtremum(reportList.getExtremum());
        onereport.setProbability(reportList.getProbability());
        onereport.setShefQualCode(reportList.getShefQualCode());
        onereport.setQuality_code(reportList.getQuality_code());
        onereport.setValue(reportList.getValue());
        onereport.setValue2(PDCConstants.MISSING_VALUE);
        onereport.setValidtime(reportList.getValidtime());
        onereport.setBasistime(reportList.getBasistime());
        onereport.setThreatIndex(ThreatIndex.THREAT_MISSING_DATA);

        /*
         * initialize the list; else set to Head pointer since the outputPtr
         * gets set to null each time this function is accessed.
         */

        if (reportHead == null) {
            outputList.add(onereport);
        } else {
            outputList = reportHead;
        }

        /* add the data to the list */
        outputList.add(onereport);

        return outputList;
    }

    /**
     * Simply copy the contents of a list to another list. This creates a new
     * list.
     */
    public static List<GageData> copyReportList(
            List<GageData> inputHead) {
        List<GageData> output = new ArrayList<>();
        GageData onereport = null;

        /* have special check in the event of an empty input list */
        if (inputHead == null) {
            return null;
        }

        /* copy the report */
        for (int i = 0; i < inputHead.size(); i++) {
            onereport = new GageData();
            /* copy fields */
            onereport.setLid(inputHead.get(i).getLid());
            onereport.setPe(inputHead.get(i).getPe());
            onereport.setDur(inputHead.get(i).getDur());
            onereport.setTs(inputHead.get(i).getTs());
            onereport.setExtremum(inputHead.get(i).getExtremum());
            onereport.setProbability(inputHead.get(i).getProbability());
            onereport.setShefQualCode(inputHead.get(i).getShefQualCode());
            onereport.setQuality_code(inputHead.get(i).getQuality_code());
            onereport.setValue(inputHead.get(i).getValue());
            onereport.setValue2(PDCConstants.MISSING_VALUE);
            onereport.setValidtime(inputHead.get(i).getValidtime());
            onereport.setBasistime(inputHead.get(i).getBasistime());
            onereport.setThreatIndex(ThreatIndex.THREAT_MISSING_DATA);

            output.add(onereport);
        }

        return output;
    }
}