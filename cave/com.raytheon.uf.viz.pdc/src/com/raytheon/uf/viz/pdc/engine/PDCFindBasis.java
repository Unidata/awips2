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
package com.raytheon.uf.viz.pdc.engine;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.GageData.ThreatIndex;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants;

/**
 * Point Data Control Find Basis
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Nov 21, 2008           mpduff     Initial creation
 * Feb 03, 2014  16843    lbousaidi  add a check for index OutOfBoundsException
 * Mar 08, 2017  19647    snaples    Fixed Class Cast exception when returning
 *                                   List to Arraylist.
 * Apr 16, 2018  6630     randerso   Fix IndexOutOfBounds in determineRsMofo
 * Sep 21, 2018 7379      mduff       Moved for PDC Refactor.
 * Aug 14, 2019  21519    dhaines    Fixed major bug in loadObsFcstReport - this
 *                                   method would return null if the object to
 *                                   add is null, which would set the list this
 *                                   method adds to null also, essentially dropping
 *                                   all previously added objects and starting over
 *                                   in the middle of the list of objects to add,
 *                                   causing gages with present values to not
 *                                   render on the map unless the user manually checks
 *                                   the "check missing" box in the point data control
 *                                   window
 * Oct 11, 2019  21621    dhaines    Reworked the looping in determineRsMofo - in its
 *                                   previous state, much/all of the objects from the
 *                                   forecast gagedata list would be dropped. Looks like
 *                                   the bug in 21519 made this bigger problem
 *                                   unnoticable.
 * </pre>
 *
 * @author mpduff
 */

public class PDCFindBasis {

    public static List<GageData> determineRsMofo(List<GageData> obsReportList,
            List<GageData> fcstReportList) {
        List<GageData> reportHead = null;

        /*
         * If either list is null/empty return the other or null if both are
         */
        if ((obsReportList == null || obsReportList.isEmpty())
                && (fcstReportList == null || fcstReportList.isEmpty())) {
             return null;
        } else if (obsReportList == null || obsReportList.isEmpty()) {
            return fcstReportList;
        } else if (fcstReportList == null || fcstReportList.isEmpty()) {
            return obsReportList;
        }

        /*
         * Loop through both lists, with the observed list on the outside, and
         * where there are cases where there is an entry in both obs and fcst
         * lists for the same station, then use the higher of the two values.
         * Use the observed list when a value is missing from the forecast list.
         */
        for (GageData obgd : obsReportList) {
            boolean found = false;
            for (GageData fcgd : fcstReportList) {
                if (obgd.getLid().equals(fcgd.getLid())) {
                    found = true;
                    double obVal = Optional.ofNullable(obgd.getValue())
                            .orElse(new Double(PDCConstants.MISSING_VALUE));
                    double fcVal = Optional.ofNullable(fcgd.getValue())
                            .orElse(new Double(PDCConstants.MISSING_VALUE));

                    if (fcVal > obVal) {
                        reportHead = loadObsFcstReport(fcgd, reportHead);
                    } else {
                        reportHead = loadObsFcstReport(obgd, reportHead);
                    }
                }
            }

            if (!found) {
                reportHead = loadObsFcstReport(obgd, reportHead);
            }
        }

        /*
         * Loop through both again with the forecast loop being the outer one
         * this time, only adding the objects we missed in the last loop.
         */
        for (GageData fcgd : fcstReportList) {
            boolean found = false;
            for (GageData obgd : obsReportList) {
                if (obgd.getLid().equals(fcgd.getLid())) {
                    found = true;
                }
            }
            if (!found) {
                reportHead = loadObsFcstReport(fcgd, reportHead);
            }
        }

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
            return reportHead;
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

        if (reportHead != null) {
            outputList = reportHead;
        }

        /* add the data to the list */
        outputList.add(onereport);

        return outputList;
    }

    /**
     * Simply copy the contents of a list to another list. This creates a new
     * list.
     *
     * @param input
     *            input list
     * @return the copied list
     */
    public static List<GageData> copyReportList(List<GageData> input) {
        List<GageData> output = new ArrayList<>();
        GageData onereport = null;

        /* have special check in the event of an empty input list */
        if (input == null) {
            return null;
        }

        /* copy the report */
        for (int i = 0; i < input.size(); i++) {
            onereport = new GageData();
            /* copy fields */
            onereport.setLid(input.get(i).getLid());
            onereport.setPe(input.get(i).getPe());
            onereport.setDur(input.get(i).getDur());
            onereport.setTs(input.get(i).getTs());
            onereport.setExtremum(input.get(i).getExtremum());
            onereport.setProbability(input.get(i).getProbability());
            onereport.setShefQualCode(input.get(i).getShefQualCode());
            onereport.setQuality_code(input.get(i).getQuality_code());
            onereport.setValue(input.get(i).getValue());
            onereport.setValue2(PDCConstants.MISSING_VALUE);
            onereport.setValidtime(input.get(i).getValidtime());
            onereport.setBasistime(input.get(i).getBasistime());
            onereport.setThreatIndex(ThreatIndex.THREAT_MISSING_DATA);

            output.add(onereport);
        }

        return output;
    }
}