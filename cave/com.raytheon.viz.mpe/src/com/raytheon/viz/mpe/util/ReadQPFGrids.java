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
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ReadQPFGrids {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadQPFGrids.class);

    Hrap_Grid hrap_grid;

    Pcp pcp = DailyQcUtils.pcp;

    BufferedReader in = null;

    public int read_qpf_grids(int num, String dbuf) {

    	System.out.println("ReadQPFGrids.read_qpf_grids(): num = " + num);
    	
        int i, j;
        hrap_grid = DailyQcUtils.getHrap_grid();
        String kbuf = "";
        // int minhrapi, minhrapj, maxhrapi, maxhrapj;
        // int ghrapi, ghrapj;
        // int ii, jj;
        int gmini, gminj, gmaxi, gmaxj;
        int iflag;
        int fe = 0;
        File tb = new File(dbuf);

        pcp.value = new int[hrap_grid.maxi][hrap_grid.maxj];

        // for (j = (hrap_grid.maxj - hrap_grid.hrap_miny) - 1; j >= 0; j--) {
        for (i = 0; i < (hrap_grid.maxi); i++) {
            for (j = 0; j < (hrap_grid.maxj); j++) {
                pcp.value[i][j] = 0;
            }
        }

        if (tb.lastModified() == 0 || tb.length() == 0) {
            fe = 0;
        }
        if (fe == -1) {
            return -1;
        }

        try {

            in = new BufferedReader(new FileReader(dbuf));
            if (in == null) {
                return -1;
            }
            // minhrapi = hrap_grid.hrap_minx;
            // minhrapj = hrap_grid.hrap_miny;
            // maxhrapi = hrap_grid.maxi;
            // maxhrapj = hrap_grid.maxj;

            kbuf = in.readLine().trim();
            Scanner s = new Scanner(kbuf);
            gmini = (int) s.nextDouble();
            gminj = (int) s.nextDouble();
            gmaxi = (int) s.nextDouble();
            gmaxj = (int) s.nextDouble();
            if (s.hasNextDouble()) {
                iflag = (int) s.nextDouble();
            } else {
                iflag = 0;
            }
            for (i = 0; i < gmaxi; i++) {

                if (iflag == 0) {
                    kbuf = in.readLine().trim();
                } else {
                    kbuf = in.readLine().trim();
                }

                s = new Scanner(kbuf);

                /* get hrap coord of gridded data */

                // ghrapi = gmini + i;
                //
                // ii = ghrapi - minhrapi;

                // if (ghrapi > minhrapi && ghrapi <= maxhrapi) {

                for (j = 0; j < gmaxj; j++) {

                    // ghrapj = gminj + j;
                    //
                    // jj = ghrapj - minhrapj;

                    // if (ghrapj >= minhrapj && ghrapj < maxhrapj) {
                    int lo = -1;
                    if (s.hasNextInt() == true) {
                        lo = s.nextInt();
                    }
                    pcp.value[i][j] = lo;

                    // }
                }
                // }
            }
            in.close();

            /* copy to internal file */

            write_file("pcp", num, pcp);

            DailyQcUtils.pcp_in_use[num] = 1;

            return 1;

        } catch (FileNotFoundException e) {
            System.out.println("File not found : " + dbuf);
            return -1;
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return -1;
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    public void write_file(String prefix, int num, Pcp pcp) {
        BufferedWriter out = null;
        String scratchdir = "";
        int i, j;
        String fname = "";
        int pid = DailyQcUtils.pid;
        hrap_grid = DailyQcUtils.getHrap_grid();
        File wf;

        scratchdir = AppsDefaults.getInstance().getToken("mpe_scratch_dir");
        if (scratchdir.length() > 0) {
            fname = String.format("%s/%s.%d.%s", scratchdir, prefix, num, pid);
        } else {
            fname = String.format("%s.%d.%s", prefix, num, pid);
        }
        wf = new File(fname);
        try {

            out = new BufferedWriter(new FileWriter(fname));
            StringBuffer sb = new StringBuffer();
            // for (j = hrap_grid.maxj - hrap_grid.hrap_miny - 1; j >= 0; j--) {
            for (i = 0; i < hrap_grid.maxi; i++) {
                sb.setLength(0);
                for (j = 0; j < hrap_grid.maxj; j++) {
                    sb.append(" " + pcp.value[i][j]);
                }
                out.write(sb.toString());
                out.newLine();
            }
            out.close();
            wf.setReadable(true, false);
            wf.setWritable(true, false);
            return;

        } catch (IOException e) {
            // TODO Auto-generated catch block
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return;
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

    }
}
