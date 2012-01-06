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
package com.raytheon.viz.hydrocommon.ratingcurve;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.datamanager.HydroDataManager;

/**
 * This class displays the Rating Curve dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sep 8, 2008              lvenable    Initial creation.
 * 24 Nov 2008  1682        dhladky     Made interactive.
 * 15 Dec 2009  2422        mpduff      Added query for rating date and 
 *                                      USGS rating number.
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class RatingCurveDataManager extends HydroDataManager {

    private SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy");

    public RatingCurveDataManager() {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Rating Curve
     * 
     * @param lid
     * @return ArrayList<RatingCurveData>
     */
    public ArrayList<RatingCurveData> getRatingCurve(String lid) {

        ArrayList<RatingCurveData> ratingCurve = null;

        if (lid != null) {
            String query = "SELECT stage, discharge FROM rating WHERE lid='"
                    + lid + "' ORDER BY stage asc";
            ArrayList<Object[]> data = null;
            ratingCurve = new ArrayList<RatingCurveData>();

            try {
                data = (ArrayList<Object[]>) DirectDbQuery.executeQuery(query,
                        HydroConstants.IHFS, QueryLanguage.SQL);

                if (data != null) {
                    for (Object[] objects : data) {
                        RatingCurveData rcd = new RatingCurveData(objects);
                        ratingCurve.add(rcd);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return ratingCurve;
    }

    /**
     * Insert Rating Curve
     * 
     * @param RatingCurveImport
     */
    public void insertRatingCurve(RatingCurveImport rci) {

        if (rci.lid != null) {
            String query = null;
            for (RatingCurveData rcd : rci) {
                query = "INSERT INTO rating (lid, stage, discharge) values ('"
                        + rci.lid + "', " + rcd.getStage() + ", "
                        + rcd.getDischarge() + ")";
                try {
                    DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                            QueryLanguage.SQL);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Delete Rating Curve
     * 
     * @param lid
     * @return ArrayList<RatingCurveData>
     */
    public void deleteRatingCurve(String lid) {

        if (lid != null) {
            try {
                String query = "DELETE FROM rating WHERE lid='" + lid + "'";
                DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                        QueryLanguage.SQL);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Rating Curve Shift Data
     * 
     * @param lid
     * @return ArrayList<RatingCurveShifData>
     */
    public ArrayList<RatingCurveShiftData> getRatingCurveShift(String lid) {

        ArrayList<RatingCurveShiftData> ratingCurveShift = new ArrayList<RatingCurveShiftData>();

        if (lid != null) {
            String query = "SELECT lid, date, shift_amount, active FROM ratingshift WHERE lid='"
                    + lid + "' " + "ORDER BY date desc";
            ArrayList<Object[]> data = null;

            try {
                data = (ArrayList<Object[]>) DirectDbQuery.executeQuery(query,
                        HydroConstants.IHFS, QueryLanguage.SQL);

                if (data != null) {
                    for (Object[] objects : data) {
                        RatingCurveShiftData rcsd = new RatingCurveShiftData(
                                objects);
                        ratingCurveShift.add(rcsd);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return ratingCurveShift;
    }

    /**
     * delete Rating Curve Shift Data
     * 
     * @param lid
     */
    public void deleteRatingCurveShift(RatingCurveShiftData rcsd) {

        if (rcsd.getLid() != null) {
            String query = "DELETE FROM ratingshift WHERE lid='"
                    + rcsd.getLid() + "' and date = '" + rcsd.getDateString()
                    + "'";
            try {
                DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                        QueryLanguage.SQL);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * delete Rating Curve Shift Data
     * 
     * @param lid
     */
    public void insertRatingCurveShift(RatingCurveShiftData rcsd) {
        if (rcsd.getLid() != null) {
            // Check to see if we need to update or insert
            ArrayList<RatingCurveShiftData> dataList = getRatingCurveShift(rcsd.getLid());
            boolean doUpdate = false;
            if (dataList.size() > 0) {
                for (RatingCurveShiftData data: dataList) {
                    // if a pk match, then need to update
                    if (data.getLid().equals(rcsd.getLid()) && data.getDate().getTime().equals(rcsd.getDate().getTime())) {
                        doUpdate = true;
                        break;
                    }
                }
            }
            
            String lid = rcsd.getLid();
            double value = rcsd.getValue();
            String date = rcsd.getDateString();
            String active = "";
            if (rcsd.isActive()) {
                active = "T";
            } else {
                active = "F";
            }

            String query = null;
            if (doUpdate) {
                // do an update
                query = "update ratingShift set shift_amount = " + value + ", active = '" + active + "' " 
                    + " where lid = '" + lid + "' and date = '" + date + "'";
            } else {
                query = "INSERT INTO ratingshift (lid, date, shift_amount, active) VALUES ('"
                    + lid + "', '" + date + "', " + value + ", '" + active + "')";
            }
            
            try {
                DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                        QueryLanguage.SQL);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Rating Curve
     * 
     * @param lid
     * @return ArrayList<RatingCurveData>
     */
    public void deleteRatingCurveData(RatingCurveData rcd, String lid) {
        if (lid != null) {
            String query = "DELETE FROM rating WHERE lid='" + lid
                    + "' and stage = " + rcd.getStage() + " and discharge = "
                    + rcd.getDischarge();
            try {
                DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                        QueryLanguage.SQL);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Rating Curve
     * 
     * @param lid
     * @return ArrayList<RatingCurveData>
     */
    public void insertRatingCurveData(RatingCurveData rcd, String lid) {
        if (lid != null) {
            // Check for update or insert
            String countQuery = "select count(*) from rating where lid = '" + lid + "'" 
                    + " and stage = " + rcd.getStage();
            try {
                List<Object[]> rs = DirectDbQuery.executeQuery(countQuery, HydroConstants.IHFS, QueryLanguage.SQL);
                if ((rs != null) && (rs.size() > 0)) {
                    Object[] oa = rs.get(0);
                    if ((oa != null) && (oa.length > 0)) {
                        long count = (Long) oa[0];
                        if (count > 0) {
                            // need to delete, then insert
                            String query = "delete from rating where lid = '" + lid + "' and stage = " + rcd.getStage();
                            DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                                    QueryLanguage.SQL);
                        }
                        // need to do an insert
                        String query = "INSERT INTO rating (lid, stage, discharge) VALUES ('"
                            + lid
                            + "', "
                            + rcd.getStage()
                            + ", "
                            + rcd.getDischarge()
                            + ")";
                        DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                                QueryLanguage.SQL);
                    } else {
                        throw new Exception("Error accessing Hydor Database");
                    }
                } else {
                    throw new Exception("Error accessing Hydor Database");
                }
                
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public String getRatingInfo(String lid) {
        String label = null;
        if (lid != null) {
            String query = "select ratedat, usgs_ratenum from riverstat where lid = '"
                    + lid + "'";
            ArrayList<Object[]> rs = runQuery(query);

            if ((rs != null) && (rs.size() == 1)) {
                Date d = (Date) rs.get(0)[0];
                String date = "";
                if (d != null) {
                    date = sdf.format(d);
                }
                String rateNum = (String) rs.get(0)[1];
                if (rateNum == null) {
                    rateNum = "";
                }
                label = "Date of Rating: " + date + "\nUSGS Rating No.: "
                        + rateNum;
            }
        }

        return label;
    }
}
