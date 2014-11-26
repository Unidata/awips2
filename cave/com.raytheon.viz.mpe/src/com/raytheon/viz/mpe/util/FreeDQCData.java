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

import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Maps;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Tdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Zdata;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class FreeDQCData {

    DailyQcUtils dqc = DailyQcUtils.getInstance();
    
    public void free_dqc_data() {

        // Do nothing right now

        dqc.pdata = new Pdata[0];
        dqc.tdata = new Tdata[0];
        dqc.zdata = new Zdata[0];

        MeanMonthlyPrecip mmp = new MeanMonthlyPrecip();
        mmp.setIsoh(null);
        MeanMonthlyTemp mmt = new MeanMonthlyTemp();
        mmt.setMaxmin(null);
        dqc.setHrap_grid(new Hrap_Grid());
        dqc.pcp = new Pcp();
        dqc.spf = new Pcp();
        dqc.tpf = new Pcp();
        dqc.mean_areal_precip_global = new Maps[0];

    }

}
