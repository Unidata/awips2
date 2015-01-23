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


/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class MakeRsel {
    
    DailyQcUtils dqc = DailyQcUtils.getInstance();

//    Pcp pcp = DailyQcUtils.pcp;
//
//    Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();

    CreateMap cm = new CreateMap();

    public void make_rsel(int num, int mnum) {

        boolean render_all = dqc.render_all;
        int wfo_orig = dqc.wfo_orig;
//        int pcp_in_use[] = DailyQcUtils.pcp_in_use;
//        Maps[] mean_areal_precip_global = DailyQcUtils.mean_areal_precip_global;
        final String fbuf = "pcp";
        int x, y, ix, iy, l;
        int minx, miny, totx, toty, uzpts, lzpts, mzpts, gzpts;
        float lz, uz, mz, gz;
        int ib;

        if (dqc.pcp_in_use[num] == -1) {
            return;
        }
        cm.read_file(fbuf, num, dqc.pcp);

        minx = dqc.getHrap_grid().hrap_minx;
        miny = dqc.getHrap_grid().hrap_miny;
        totx = dqc.getHrap_grid().maxi;
        toty = dqc.getHrap_grid().maxj;

        for (ib = 0; ib < dqc.getMax_basins(); ib++) {

            if (render_all == false) {

                /* skip if you are not the owner and there is no value */
                /* should not affect daily_qc */
                /* may affect verify */
                /* should fix specify */
                /* auto_specify ??? */

                if (dqc.mean_areal_precip_global[ib].owner != wfo_orig
                        && dqc.mean_areal_precip_global[ib].zuz[num] < 0
                        && dqc.mean_areal_precip_global[ib].zlz[num] < 0
                        && dqc.mean_areal_precip_global[ib].zmz[num] < 0
                        && dqc.mean_areal_precip_global[ib].zgz[num] < 0) {
                    continue;
                }

            }

            mz = 0;
            uz = 0;
            lz = 0;
            gz = 0;
            uzpts = 0;
            lzpts = 0;
            mzpts = 0;
            gzpts = 0;

            for (l = 0; l < dqc.mean_areal_precip_global[ib].hrap_points; l++) {

                x = dqc.mean_areal_precip_global[ib].hrap_data[l].x;
                y = dqc.mean_areal_precip_global[ib].hrap_data[l].y;

                ix = x - minx;
                iy = y - miny;

                if (ix < 0 || iy < 0 || ix >= totx || iy >= toty
                        || dqc.pcp.value[ix][iy] < 0) {

                    continue;

                }

                if (dqc.mean_areal_precip_global[ib].hrap_data[l].zone[3] == 1) {

                    gz = gz + dqc.pcp.value[ix][iy] / 100.0f;
                    gzpts++;

                }

                if (dqc.mean_areal_precip_global[ib].hrap_data[l].zone[2] == 1) {

                    uz = uz + dqc.pcp.value[ix][iy] / 100.0f;
                    uzpts++;

                }

                if (dqc.mean_areal_precip_global[ib].hrap_data[l].zone[1] == 1) {

                    mz = mz + dqc.pcp.value[ix][iy] / 100.0f;
                    mzpts++;

                }

                if (dqc.mean_areal_precip_global[ib].hrap_data[l].zone[0] == 1) {

                    lz = lz + dqc.pcp.value[ix][iy] / 100.0f;
                    lzpts++;

                }

            }

            if (gzpts == 0) {
                gz = -1;
            } else {
                gz = gz / gzpts;
            }

            if (uzpts == 0) {
                uz = -1;
            } else {
                uz = uz / uzpts;
            }

            if (mzpts == 0) {
                mz = -1;
            } else {
                mz = mz / mzpts;
            }

            if (lzpts == 0) {
                lz = -1;
            } else {
                lz = lz / lzpts;
            }

            dqc.mean_areal_precip_global[ib].zuz[mnum] = uz;
            dqc.mean_areal_precip_global[ib].zlz[mnum] = lz;
            dqc.mean_areal_precip_global[ib].zmz[mnum] = mz;
            dqc.mean_areal_precip_global[ib].zgz[mnum] = gz;

            dqc.mean_areal_precip_global[ib].zmaps_done[mnum] = 1;

        }
        return;
    }

}
