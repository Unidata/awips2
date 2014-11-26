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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

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

public class WriteQPFGrids {

    BufferedWriter out = null;
    
    DailyQcUtils dqc = DailyQcUtils.getInstance();

//    Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();
//
//    Pcp pcp = DailyQcUtils.pcp;

    public void write_qpf_grids(String dbuf) {

        int i, j;
        int iflag;
        File qg = new File(dbuf);

        try {
            out = new BufferedWriter(new FileWriter(qg));
            iflag = 1;

            // fprintf(fp,"%d %d %d %d %d\n",hrap_grid->hrap_minx,hrap_grid->hrap_miny,hrap_grid->maxi
            // ,hrap_grid->maxj,iflag);
            StringBuffer sb = new StringBuffer();
            sb.append(String.format("%d %d %d %d %d", dqc.getHrap_grid().hrap_minx,
                    dqc.getHrap_grid().hrap_miny, dqc.getHrap_grid().maxi, dqc.getHrap_grid().maxj, iflag));
            out.write(sb.toString());
            out.newLine();

            // for (j = hrap_grid.maxj - hrap_grid.hrap_miny - 1; j >= 0; j--) {
            for (i = 0; i < dqc.getHrap_grid().maxi; i++) {
                sb.setLength(0);
                for (j = 0; j < dqc.getHrap_grid().maxj; j++) {
                    sb.append(String.format(" %5d", dqc.pcp.value[i][j]));
                }
                out.write(sb.toString());
                out.newLine();
            }
            out.close();
            qg.setReadable(true, false);
            qg.setWritable(true, false);
            return;
        } catch (IOException e) {
            e.printStackTrace();
            return;
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
