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

import com.raytheon.viz.mpe.util.DailyQcUtils.Zdata;

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

public class CalculateZLevel {

    public void calculate_zlevel(int max_zstations, Zdata[] zdata) {
        int j, h, k;
        int j1;
        int j2 = 0;
        int k1 = 0;
        int k2 = 0;
        int MAX_GAGEQC_DAYS = 10;

        for (j = 0; j < MAX_GAGEQC_DAYS; j++) {

            for (k = 0; k < 4; k++) {

                if (zdata[j].used[k] == 0) {

                    j1 = -1;

                    if ((j == 0 && k == 3)
                            || (j == MAX_GAGEQC_DAYS - 1 && k == 0)) {
                        continue;
                    }

                    if ((k == 1 || k == 2) && zdata[j].used[k - 1] != 0
                            && zdata[j].used[k + 1] != 0) {

                        j1 = j;
                        j2 = j;
                        k1 = k - 1;
                        k2 = k + 1;

                    }

                    if ((k == 0) && zdata[j].used[1] != 0
                            && zdata[j + 1].used[3] != 0) {

                        j1 = j;
                        j2 = j + 1;
                        k1 = 1;
                        k2 = 3;

                    }

                    if ((k == 3) && zdata[j].used[2] != 0
                            && zdata[j - 1].used[0] != 0) {

                        j1 = j;
                        j2 = j - 1;
                        k1 = 2;
                        k2 = 0;

                    }

                    if (j1 == -1) {
                        continue;
                    }

                    zdata[j].used[k] = 6;

                    for (h = 0; h < max_zstations; h++) {

                        if (zdata[j1].zstn[h].zlevel1[k1].data >= 0
                                && zdata[j2].zstn[h].zlevel1[k2].data >= 0) {

                            zdata[j].zstn[h].zlevel1[k].data = (zdata[j1].zstn[h].zlevel1[k1].data + zdata[j2].zstn[h].zlevel1[k2].data) / 2;

                            zdata[j].zstn[h].zlevel1[k].qual = 5;

                        }

                        if (zdata[j1].zstn[h].zlevel2[k1].data >= 0
                                && zdata[j2].zstn[h].zlevel2[k2].data >= 0) {

                            zdata[j].zstn[h].zlevel2[k].data = (zdata[j1].zstn[h].zlevel2[k1].data + zdata[j2].zstn[h].zlevel2[k2].data) / 2;

                            zdata[j].zstn[h].zlevel2[k].qual = 5;

                        }

                    }

                }

            }

        }

        // TODO Auto-generated method stub

    }

}
