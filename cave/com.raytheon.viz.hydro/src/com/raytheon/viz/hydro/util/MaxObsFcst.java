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
package com.raytheon.viz.hydro.util;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydro.stationprofile.HydroDataReport;

/**
 * Returns the greater of the max observation or forecast and the time
 * corresponding to that value.
 * 
 * If the forecast for highest ranked type-source forecast
 * value is in the past, then this function recomputes the
 * max forecast.
 *  <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2010            mpduff     Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class MaxObsFcst {
    private String lid;

    private String primaryPe;

    private int hoursBack;

    private int fcstBasisHrsAgo;

    public MaxObsFcst(String lid, String primaryPe, int hoursBack,
            int fcstBasisHrsAgo) {
        this.lid = lid;
        this.primaryPe = primaryPe;
        this.hoursBack = hoursBack;
        this.fcstBasisHrsAgo = fcstBasisHrsAgo;
    }

    public HydroDataReport[] calcMaxObsFcst() {
        /*
         * define the beginning of the window for looking for the observed data,
         * and the beginning of the window for the basis time of the forecast
         * data.
         */
        PDCDataManager pdcManager = PDCDataManager.getInstance();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        int useTsRank = 99;
        int prevTsRank = 99;
        String useTs = null;
        boolean obsFound = false;
        boolean fcstFound = false;
        HydroDataReport obsReport = null;
        HydroDataReport fcstReport = null;

        Calendar now = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Calendar beginTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        beginTime.add(Calendar.HOUR, hoursBack * -1);

        Calendar beginBasisTime = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        beginBasisTime.add(Calendar.HOUR, fcstBasisHrsAgo * -1);

        Calendar bestObsValidTime = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        bestObsValidTime.setTimeInMillis(0);

        Calendar bestFcstValidTime = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        bestFcstValidTime.setTimeInMillis(0);

        /*
         * in an effort to minimize reads of the database, get the RiverStatus
         * info all at once, for all ts's and for both observed and forecast.
         * There is validtime limit for observed data
         */
        String where = String.format(" WHERE lid='%s' AND pe='%s' AND "
                + " (validtime >= '%s' OR ts LIKE 'F%%') AND "
                + " (basistime IS NULL OR basistime >= '%s' )", lid, primaryPe,
                sdf.format(beginTime.getTime()), sdf.format(beginBasisTime
                        .getTime()));

        List<Riverstatus> rsList = null;
        ArrayList<String[]> uList = null;
        try {
            rsList = pdcManager.getRiverStatusList(where);

            /*
             * get a unique list of entries for ts's that match the given lid
             * and pe. the ingestfilter entries are needed because they contain
             * the type-source rank information. insert a comma in between the
             * two fields to make them easier to parse. note that the ts rank
             * sort method will not handle numbers greater than 9 (i.e. 12 is
             * ranked higher than 3)! also note that the query does not filter
             * out extremums that are not "Z". only bother getting info if
             * riverstat entries exist. we try and read riverstatus first since
             * that table is smaller
             */
            if (rsList != null) {
                String where2 = String.format(
                        " WHERE lid= '%s' AND pe= '%s' AND ingest= 'T' "
                                + " ORDER BY 1 ", lid, primaryPe);

                uList = HydroData.loadUnique("ts_rank||ts", "ingestFilter",
                        where2);
            }

        } catch (VizException e) {
            e.printStackTrace();
        }

        /* process the data if there are possible entries to check */
        if ((uList != null) && (rsList != null)) {

            /*
             * loop on the observed entries and try and get the observed data.
             * note that processed data is grouped in with observed data
             * searches.
             */
            for (String[] sa : uList) {
                /* extract the type source and rank currently being considered */
                useTsRank = Integer.parseInt(sa[0]);
                useTs = sa[1];

                /*
                 * only process the observed type-sources entries, and only if
                 * the ts rank is the same as the previously checked ts or no
                 * obs data has been found at all yet
                 */
                if ((useTs.startsWith("R") || useTs.startsWith("P"))
                        && ((useTsRank == prevTsRank) || !obsFound)) {
                    /*
                     * loop on the riverstatus entries for the observed value
                     * that matches the type-source being considered
                     */
                    for (Riverstatus riverStatus : rsList) {
                        /*
                         * only use this riverstatus entry if it is for the
                         * current ts and the validtime for the current ts
                         * is more recent than the previous validtime for ts
                         * with a possible matching rank
                         */
                        if (riverStatus.getId().getTs().equals(useTs)
                                && (riverStatus.getValidtime().getTime() > bestObsValidTime
                                        .getTimeInMillis())) {
                            
                            
                            obsReport = new HydroDataReport();
                            obsReport.setPe(riverStatus.getId().getPe());
                            obsReport.setDur(riverStatus.getDur());
                            obsReport.setTs(riverStatus.getId().getTs());
                            obsReport.setExtremum(riverStatus.getExtremum());
                            obsReport.setValue(riverStatus.getValue());
                            obsReport.setValidTime(riverStatus.getValidtime());
                            
                            bestObsValidTime.setTimeInMillis(riverStatus
                                    .getValidtime().getTime());
                            obsFound = true;
                            break;
                        }
                    }

                    prevTsRank = useTsRank;
                }
            }

            /*
             * now loop on the ingestfilter entries again, in a similar fashion
             * but this time for the best forecast
             */
            boolean recomputeMaxFcst = false;

            /* loop on candidate type-sources */
            for (String[] sa : uList) {
                useTsRank = Integer.parseInt(sa[0]);
                useTs = sa[1];

                if (useTs.startsWith("F")) {
                    for (Riverstatus riverStatus : rsList) {
                        if (riverStatus.getId().getTs().equals(useTs)) {
                            /*
                             * if the time of returned max fcst value is in the
                             * past, then we need to recompute the max fcst
                             * information and reload the recomputed value into
                             * the RiverStatus table. the rule is that the
                             * RiverStatus max fcst value should always be the
                             * max forecast that has a valid time after NOW, and
                             * with a basis time that is not too old.
                             * 
                             * by allowing the earlier RiverStatus retrieval
                             * above to get old data, then possibly rejecting it
                             * because it is old or too far, this method ensures
                             * that a lesser ts-ranked value, that may otherwise
                             * be used because its max is not in the past, is
                             * not necesarily used instead of a higher ranked
                             * type-source which has a forecast in the future
                             */

                            if (riverStatus.getValidtime().getTime() < now
                                    .getTimeInMillis()) {
                                recomputeMaxFcst = true;
                            } else {
                                // update to use fcstReport object as in get_curobs_maxfcst.c line 248
                                fcstReport = new HydroDataReport();
                                fcstReport.setPe(riverStatus.getId().getPe());
                                fcstReport.setDur(riverStatus.getDur());
                                fcstReport.setTs(riverStatus.getId().getTs());
                                fcstReport.setExtremum(riverStatus.getExtremum());
                                fcstReport.setValue(riverStatus.getValue());
                                fcstReport.setValidTime(riverStatus.getValidtime());
                                fcstReport.setBasisTime(riverStatus.getBasistime());
                                                                 
                                fcstFound = true;
                            }
                            break;
                        }
                    }
                }

                /*
                 * don't continue with the next type-source if value is found or
                 * if have to recompute maxfcst info
                 */
                if (fcstFound || recomputeMaxFcst) {
                    break;
                }
            }

            /*
             * if the max forecast data needs to be recomputed because the max
             * forecast value is in the past, then recompute them. then, loop on
             * all the forecast ts entries again. we can't just loop on the one
             * ts that had the old max fcst because it is possible that a better
             * choice forecast ts (i.e. higher ts_rank) also had an old max
             * fcst, and we should pick that value up if it is updated. also
             * note that the way the recompute flag is defined, that if no
             * forecast data exists for a location, it won't bother trying to
             * recompute it, since there should be nothing to recompute; thereby
             * this algorithm doesn't waste energy looking for data that isn't
             * there.
             */

            if (recomputeMaxFcst) {
                /* recompute them accordingly */
                System.out.println(String.format(
                        "recomputing maxfcst since time < NOW; lid, pe=%s %s",
                        lid, primaryPe));
                if (primaryPe.startsWith("H")) {
                    LoadMaxFcst.loadMaxFcstDataLidPe("FcstHeight", lid,
                            primaryPe);
                } else if (primaryPe.startsWith("Q")) {
                    LoadMaxFcst.loadMaxFcstDataLidPe("FcstDischarge", lid,
                            primaryPe);
                }

                /*
                 * now that the data are recomputed, get the value in a similar
                 * fashion as before
                 */
                where = String.format(
                        " WHERE lid='%s' AND pe='%s' AND ts='%s' "
                                + " AND validtime >= '%s'  "
                                + " AND basistime >= '%s'  ", lid, primaryPe,
                        useTs, sdf.format(now.getTime()), sdf
                                .format(beginBasisTime.getTime()));
                try {
                    rsList = pdcManager.getRiverStatusList(where);
                    boolean keepGoing = true;
                    int index = 0;
                    
                    /* find the best value, considering the different type-sources */
                    while (keepGoing && !fcstFound) {
                        String[] sa = uList.get(index);
                        useTs = sa[1];

                        if (useTs.startsWith("F")) {
                            for (Riverstatus riverStatus: rsList) {
                                if (riverStatus.getId().getTs().equals(useTs)) {
                                    fcstReport = new HydroDataReport();
                                    fcstReport.setPe(riverStatus.getId().getPe());
                                    fcstReport.setDur(riverStatus.getDur());
                                    fcstReport.setTs(riverStatus.getId().getTs());
                                    fcstReport.setExtremum(riverStatus.getExtremum());
                                    fcstReport.setValue(riverStatus.getValue());
                                    fcstReport.setValidTime(riverStatus.getValidtime());
                                    fcstReport.setBasisTime(riverStatus.getBasistime());
                                    
                                    fcstFound = true;
                                }
                            }                            
                        }
                        index++;
                        
                        if (uList.size() <= index) {
                            break;
                        }
                    }
                    
                    
                } catch (VizException e) {
                    e.printStackTrace();
                }                
            }
        }
        
        HydroDataReport[] data = new HydroDataReport[2];
        data[0] = obsReport;
        data[1] = fcstReport;
        
        return data;
    }
}
