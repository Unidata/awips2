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
package com.raytheon.viz.mpe.util;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Tdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Zdata;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Define attributes common to netCDF and grib write routines.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2011            snaples     Initial creation
 * Mar 5, 2013  15884      wkwock      gridPointLL and gridPointUR should be integer
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GridAttributes {

    static boolean first = true;

    static int dqcEndingObsTime;

    static CommonGridAttributes commonGridAttributes;

    static Hrap_Grid hrap_grid;

    static int XOR;

    static int YOR;

    static int num_period_qc;

    static AppsDefaults apps_defaults = AppsDefaults.getInstance();

    /**
     * @param data
     *            type code
     * @param j
     *            - Hydro Day number
     * @param num_period_qc
     */
    public int define_grid_attributes(int data_type_code, int j,
            int numperiod_qc) {
        Coordinate latlonhrap;
        int num;
        commonGridAttributes = new CommonGridAttributes();
        num_period_qc = numperiod_qc;
        Pdata[] pdata = DailyQcUtils.pdata;
        Tdata[] tdata = DailyQcUtils.tdata;
        Zdata[] zdata = DailyQcUtils.zdata;
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(new Date());
        final int SIXHRSEC = 21600;
        final int DAYINSEC = 86400;
        if (commonGridAttributes.siteID == null) {
            first = true;
        }

        if (first == true) {
            String rfcid = "";
            String gridType = "";
            String projectionType = "";
            hrap_grid = DailyQcUtils.getHrap_grid();
            dqcEndingObsTime = DailyQcUtils.getEnding6HourObsTime();
            XOR = hrap_grid.hrap_minx;
            YOR = hrap_grid.hrap_miny;
            commonGridAttributes.gridSize[0] = hrap_grid.maxi;
            commonGridAttributes.gridSize[1] = hrap_grid.maxj;

            commonGridAttributes.domainOrigin = new Coordinate(XOR, YOR);

            commonGridAttributes.domainExtent = new Coordinate(hrap_grid.maxi,
                    hrap_grid.maxj);
            Coordinate irap = new Coordinate(XOR, YOR);
            latlonhrap = DailyQcUtils.getHraptoLatLon(irap);
            commonGridAttributes.latLonLL = new Coordinate((latlonhrap.x),
                    latlonhrap.y);

            irap.x += hrap_grid.maxi;
            irap.y += hrap_grid.maxj;
            latlonhrap = DailyQcUtils.getHraptoLatLon(irap);
            commonGridAttributes.latLonUR = new Coordinate((latlonhrap.x),
                    latlonhrap.y);

            rfcid = apps_defaults.getToken("awips_rfc_id");
            if (rfcid.length() != 0) {
                commonGridAttributes.siteID = rfcid;
            }
            commonGridAttributes.gridPointLL[0] = XOR;
            commonGridAttributes.gridPointLL[1] = YOR;
            commonGridAttributes.gridPointUR[0] = XOR + hrap_grid.maxi;
            commonGridAttributes.gridPointUR[1] = YOR + hrap_grid.maxj;

            gridType = apps_defaults.getToken("mpe_dqc_gridtype");
            if (gridType.length() != 0) {
                commonGridAttributes.gridType = gridType;
            }
            commonGridAttributes.len_grty = gridType.length();

            projectionType = apps_defaults.getToken("mpe_dqc_projectiontype");
            if (projectionType.length() != 0) {
                commonGridAttributes.projectionType = projectionType;
            }
            commonGridAttributes.len_prty = projectionType.length();

            for (int i = 0; i < 6; i++) {
                commonGridAttributes.validTimes[i][0] = 0;
                commonGridAttributes.validTimes[i][1] = 0;
            }

            first = false;
        }

        /*
         * define parameters depending on data type
         */

        if (data_type_code == 1) {
            /* QPE (precip data) */
            commonGridAttributes.descriptiveName = "QPE";
            commonGridAttributes.units = "in";
            commonGridAttributes.len_units = 2;

            num = 0;
            for (int l = 0; l < 5; l++) {
                if (pdata[j].used[l] == 0) {
                    continue;
                }
                if (l < 4) {
                    cal.setTime(pdata[j].data_time);
                    cal.add(Calendar.SECOND, -(4 - l) * SIXHRSEC);
                    commonGridAttributes.validTimes[l][0] = cal
                            .getTimeInMillis() / 1000;
                    cal.add(Calendar.SECOND, SIXHRSEC);
                    commonGridAttributes.validTimes[l][1] = commonGridAttributes.validTimes[l][0]
                            + SIXHRSEC;
                } else {
                    cal.setTime(pdata[j].data_time);
                    commonGridAttributes.validTimes[l][0] = (cal
                            .getTimeInMillis() / 1000) - DAYINSEC;
                    cal.setTime(pdata[j].data_time);
                    commonGridAttributes.validTimes[l][1] = cal
                            .getTimeInMillis() / 1000;
                }
                num++;
            }
            return num_period_qc = num;
        } else if (data_type_code == 2) {

            /* QTE (temperature data) */
            commonGridAttributes.descriptiveName = "QTE";
            commonGridAttributes.units = "F";
            commonGridAttributes.len_units = 1;

            num = 0;

            for (int l = 5; l >= 0; l--) {
                if (tdata[j].used[l] == 0) {
                    continue;
                }
                if (l >= 4) {
                    cal.setTime(pdata[j].data_time);
                    commonGridAttributes.validTimes[l][1] = cal
                            .getTimeInMillis() / 1000;
                    cal.add(Calendar.SECOND, -DAYINSEC);
                    commonGridAttributes.validTimes[l][0] = cal
                            .getTimeInMillis() / 1000;
                } else {
                    if (dqcEndingObsTime == 12) {
                        cal.setTime(pdata[j].data_time);
                        cal.add(Calendar.SECOND, -(4 - l) * SIXHRSEC);
                        cal.add(Calendar.SECOND, SIXHRSEC);
                        commonGridAttributes.validTimes[l][0] = cal
                                .getTimeInMillis() / 1000;
                    } else {
                        cal.setTime(pdata[j].data_time);
                        cal.add(Calendar.SECOND, -(4 - l) * SIXHRSEC);
                        commonGridAttributes.validTimes[l][0] = cal
                                .getTimeInMillis() / 1000;
                    }
                    cal.add(Calendar.SECOND, SIXHRSEC);
                    commonGridAttributes.validTimes[l][1] = cal
                            .getTimeInMillis() / 1000;
                }
                num++;
            }
            return num_period_qc = num;
        } else {
            /* QFE (Freezing level data) */
            commonGridAttributes.descriptiveName = "QFE";
            commonGridAttributes.units = "ft";
            commonGridAttributes.len_units = 2;

            num = 0;

            for (int l = 0; l < 4; l++) {
                if (zdata[j].used[l] == 0) {
                    continue;
                }

                if (dqcEndingObsTime == 12) {
                    cal.setTime(pdata[j].data_time);
                    cal.add(Calendar.SECOND, -(4 - l) * SIXHRSEC);
                    cal.add(Calendar.SECOND, SIXHRSEC);
                    commonGridAttributes.validTimes[l][0] = cal
                            .getTimeInMillis() / 1000;
                } else {
                    cal.setTime(pdata[j].data_time);
                    cal.add(Calendar.SECOND, -(4 - l) * SIXHRSEC);
                    commonGridAttributes.validTimes[l][0] = cal
                            .getTimeInMillis() / 1000;
                }
                cal.add(Calendar.SECOND, SIXHRSEC);
                commonGridAttributes.validTimes[l][1] = cal.getTimeInMillis() / 1000;
            }
            num++;
        }
        return num_period_qc = num;
    }

    /**
     * @return the commonGridAttributes
     */
    public CommonGridAttributes getCommonGridAttributes() {
        return commonGridAttributes;
    }

    /**
     * @param commonGridAttributes
     *            the commonGridAttributes to set
     */
    public static void setCommonGridAttributes(
            CommonGridAttributes commonAttributes) {
        commonGridAttributes = commonAttributes;
    }
}
