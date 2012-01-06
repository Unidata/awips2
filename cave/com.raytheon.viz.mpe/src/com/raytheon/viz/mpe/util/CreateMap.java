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

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Maps;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;

/**
 * Contains the routines which create MAPs from gridded precipitation data.
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

public class CreateMap {

    Pcp pcp = DailyQcUtils.pcp;

    Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();

    /**
     * Creates a MAP from a gridded precipitation field.
     * 
     * @param num
     */
    public void create_map(int num) {

        boolean render_all = DailyQcUtils.render_all;
        int wfo_orig = DailyQcUtils.wfo_orig;
        int pcp_in_use[] = DailyQcUtils.pcp_in_use;
        Maps[] mean_areal_precip_global = DailyQcUtils.mean_areal_precip_global;
        String fbuf = "pcp";
        int x, y, ix, iy, l;
        int minx, miny, totx, toty, uzpts, lzpts, mzpts, gzpts;
        float lz, uz, mz, gz;
        int ib;

        if (pcp_in_use[num] == -1) {
            return;
        }

        read_file(fbuf, num, pcp);

        minx = hrap_grid.hrap_minx;
        miny = hrap_grid.hrap_miny;
        totx = hrap_grid.maxi;
        toty = hrap_grid.maxj;
        // for (ib = 0; (!mean_areal_precip_global[ib].hb5.equals("")); ib++) {
        for (ib = 0; ib < DailyQcUtils.getMax_basins(); ib++) {

            if (render_all == false) {

                /* skip if you are not the owner and there is no value */
                /* should not affect daily_qc */
                /* may affect verify */
                /* should fix specify */
                /* auto_specify ??? */

                if (mean_areal_precip_global[ib].owner != wfo_orig
                        && mean_areal_precip_global[ib].uz[num] < 0
                        && mean_areal_precip_global[ib].lz[num] < 0
                        && mean_areal_precip_global[ib].mz[num] < 0
                        && mean_areal_precip_global[ib].gz[num] < 0) {
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

            for (l = 0; l < mean_areal_precip_global[ib].hrap_points; l++) {

                x = mean_areal_precip_global[ib].hrap_data[l].x;
                y = mean_areal_precip_global[ib].hrap_data[l].y;

                ix = x - minx;
                iy = y - miny;

                if (ix < 0 || iy < 0 || ix >= totx || iy >= toty
                        || pcp.value[ix][iy] == -9999) {

                    continue;

                }

                if (mean_areal_precip_global[ib].hrap_data[l].zone[3] == 1) {

                    gz = gz + (pcp.value[ix][iy] / 100.0f);
                    gzpts++;

                }

                if (mean_areal_precip_global[ib].hrap_data[l].zone[2] == 1) {

                    uz = uz + (pcp.value[ix][iy] / 100.0f);
                    uzpts++;

                }

                if (mean_areal_precip_global[ib].hrap_data[l].zone[1] == 1) {

                    mz = mz + (pcp.value[ix][iy] / 100.0f);
                    mzpts++;

                }

                if (mean_areal_precip_global[ib].hrap_data[l].zone[0] == 1) {

                    lz = lz + (pcp.value[ix][iy] / 100.0f);
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

            DailyQcUtils.mean_areal_precip_global[ib].uz[num] = uz;
            DailyQcUtils.mean_areal_precip_global[ib].lz[num] = lz;
            DailyQcUtils.mean_areal_precip_global[ib].mz[num] = mz;
            DailyQcUtils.mean_areal_precip_global[ib].gz[num] = gz;

            DailyQcUtils.mean_areal_precip_global[ib].maps_done[num] = 1;

        }
        return;
    }

    public void read_file(String prefix, int num, Pcp pcp) {
        BufferedReader in = null;
        String scratchdir = "";
        int i, j;
        String fname = "";
        int pid = DailyQcUtils.pid;

        scratchdir = AppsDefaults.getInstance().getToken("mpe_scratch_dir");
        if (scratchdir.length() > 0) {
            fname = String.format("%s/%s.%d.%s", scratchdir, prefix, num, pid);
        } else {
            fname = String.format("%s.%d.%s", prefix, num, pid);
        }
        try {

            in = new BufferedReader(new FileReader(fname));
            Scanner s = null;

            for (i = 0; i < hrap_grid.maxi; i++) {
                String kbuf = in.readLine();
                s = new Scanner(kbuf);
                for (j = 0; j < hrap_grid.maxj; j++) {
                    if (s.hasNextInt()) {
                        pcp.value[i][j] = s.nextInt();
                    } else {
                        break;
                    }
                }
            }
            s.close();
            in.close();
        } catch (FileNotFoundException e) {

            for (i = 0; i < hrap_grid.maxi; i++) {
                for (j = 0; j < hrap_grid.maxj; j++) {
                    pcp.value[i][j] = 0;
                }

            }
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            return;
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
