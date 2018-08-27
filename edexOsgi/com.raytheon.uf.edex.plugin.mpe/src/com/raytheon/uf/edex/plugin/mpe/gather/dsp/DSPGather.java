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
package com.raytheon.uf.edex.plugin.mpe.gather.dsp;

import com.raytheon.uf.edex.plugin.mpe.gather.MpeRadarGather;
import com.raytheon.uf.edex.plugin.mpe.gather.MpeRadarGatherConstants;

/**
 * Replacement for the DSPgather script.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2018 5588       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class DSPGather extends MpeRadarGather {

    protected DSPGather() {
        super(MpeRadarGatherConstants.DSP_PRODUCT_TYPE,
                MpeRadarGatherConstants.AppsDefaults.DSP_PROD_DIR);
    }
}
