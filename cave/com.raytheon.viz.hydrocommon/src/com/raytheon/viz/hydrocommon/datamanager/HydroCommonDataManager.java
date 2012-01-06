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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;
import java.util.Date;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.Rating;
import com.raytheon.viz.hydrocommon.data.RatingShift;

/**
 * A generic concrete data manager for HydroCommon
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep  4, 2009            mpduff     Initial creation
 * Sep 09, 2009 2259       mpduff     Refactored to HydroCommon
 * Apr 21, 2010 4564       mpduff     Added ts rank code
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class HydroCommonDataManager extends HydroDataManager {
    private static HydroCommonDataManager instance = null;
    
    /**
     * Private constructor.
     */
    private HydroCommonDataManager() {
        
    }
    
    public static synchronized HydroCommonDataManager getInstance() {
        if (instance == null) {
            instance = new HydroCommonDataManager();
        }
        
        return instance;
    }
    
    
    /**
     * Get the rating values for the specified lid.
     * 
     * @param lid
     *            The lid to search on
     * @return The PDC rating object
     */
    public Rating getRating(String lid) {
        if (ratingMap.containsKey(lid)) {
            return ratingMap.get(lid);
        }

        String sql = "select lid, stage, discharge from rating where lid = '"
                + lid + "' order by stage asc";
        ArrayList<Object[]> result = runQuery(sql);
        Rating rating = null;
        if ((result != null) && (result.size() > 0)) {
            rating = new Rating();
            rating.setLid((String) result.get(0)[0]);

            for (int i = 0; i < result.size(); i++) {
                if (result.get(i)[1] != null) {
                    rating.addStage((Double) result.get(i)[1]);
                } else {
                    rating.addStage(HydroConstants.MISSING_VALUE);
                }

                if (result.get(i)[2] != null) {
                    rating.addDischarge((Double) result.get(i)[2]);
                } else {
                    rating.addDischarge(HydroConstants.MISSING_VALUE);
                }
            }
        }
        ratingMap.put(lid, rating);

        return rating;
    }

    /**
     * Get the rating shift for the specified lid.
     * 
     * @param lid
     *            The lid to search on
     * @return The rating shift object
     */
    public RatingShift getRatingShift(String lid) {
        if (ratingShiftMap.containsKey(lid)) {
            return ratingShiftMap.get(lid);
        }

        String sql = "select lid, date, shift_amount, active from ratingshift where "
                + " lid = '" + lid + "' and active = 'T' order by date desc";
        RatingShift rs = null;
        ArrayList<Object[]> results = runQuery(sql);

        if ((results != null) && (results.size() > 0)) {
            /* Take lastest shift value */
            Object[] row = results.get(0);
            rs = new RatingShift();
            rs.setLid((String) row[0]);
            rs.setDate((Date) row[1]);
            rs.setShiftAmount((Double) row[2]);
            rs.setActive((String) row[3]);
        }

        ratingShiftMap.put(lid, rs);

        return rs;
    }

    public ArrayList<String> getTsRank(String lid, String pe) {
        ArrayList<String> tsRankList = new ArrayList<String>();
        
        String query = "select ts from ingestfilter where lid = '" + lid + "' and pe = '" + pe + 
            "' order by ts_rank";
        
        ArrayList<Object[]> rs = runQuery(query);
        
        if ((rs != null) && (rs.size() > 0)) {
            for (Object[] oa: rs) {
                tsRankList.add((String) oa[0]);
            }
        }
        
        return tsRankList;
        
    }
}
