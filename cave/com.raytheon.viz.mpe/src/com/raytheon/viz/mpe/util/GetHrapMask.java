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
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Scanner;

import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 4, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GetHrapMask {

    private int minhrapi;

    private int minhrapj;

    private int maxhrapi;

    private int maxhrapj;

    int ghrapi, ghrapj;

    int gmini;

    int gminj;

    int gmaxi;

    int gmaxj;

    int i, j, ii, jj;

    BufferedReader in = null;

    public int get_hsa_to_grid_mask(Hrap_Grid hrap_grid, String[] tag,
            boolean wfo_all, String hrap_grid_mask_file) {

        File mask_file = new File(hrap_grid_mask_file);
        long iir = mask_file.lastModified();

        if (iir == 0 || mask_file.length() == 0) {
            if (mask_file.length() == 0) {
                mask_file.delete();
            }
            hrap_grid.owner = new int[hrap_grid.maxi][hrap_grid.maxj];
            for (i = 0; i < hrap_grid.maxi; i++) {
                Arrays.fill(hrap_grid.owner[i], 1);
            }
            wfo_all = true;
            return 1;
        }
        wfo_all = false;
        /* Initialize each grid bin in the HRAP grid to missing. */
        // for (j = hrap_grid.maxj - hrap_grid.hrap_miny - 1; j >= 0; j--) {
        for (i = 0; i < hrap_grid.maxi; i++) {
            Arrays.fill(hrap_grid.owner[i], -1);
        }

        minhrapi = hrap_grid.hrap_minx;
        minhrapj = hrap_grid.hrap_miny;
        maxhrapi = hrap_grid.maxi;
        maxhrapj = hrap_grid.maxj;
        try {

            /* Initialize the wfo names to empty string. */
            for (i = 0; i < 20; i++) {
                tag[i] = "";
            }

            in = new BufferedReader(new FileReader(hrap_grid_mask_file));
            String cr = in.readLine();
            Scanner s = new Scanner(cr);
            gmini = s.nextInt();
            gminj = s.nextInt();
            gmaxi = s.nextInt();
            gmaxj = s.nextInt();

            for (i = 0; i < 20; i++) {
                tag[i] = s.next();
            }

            for (i = 0; i < gmaxi; i++) {
                cr = in.readLine();
                s = new Scanner(cr);

                /* get hrap coord of gridded data */
                // ghrapi = gmini + i;
                // ii = ghrapi - minhrapi;
                //
                // if (ghrapi >= minhrapi && ghrapi < maxhrapi) {
                for (j = 0; j < gmaxj; j++) {
                    // ghrapj = gminj + j;
                    // jj = ghrapj - minhrapj;

                    // if (ghrapj >= minhrapj && ghrapj < maxhrapj) {
                    hrap_grid.owner[i][j] = s.nextInt();
                    // }
                    // }

                }

            }
            s.close();
            in.close();
            in = null;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            return 0;
        } catch (IOException e) {
            e.printStackTrace();
            return 0;
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return 1;
    }
}
