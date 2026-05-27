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
package com.raytheon.uf.edex.plugin.mpe.gather.dhr;

import com.raytheon.uf.edex.plugin.mpe.gather.MpeRadarGather;
import com.raytheon.uf.edex.plugin.mpe.gather.MpeRadarGatherConstants;

/**
 * Replacement for the DHRgather script.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2016 4625       bkowal      Initial creation
 * Nov 22, 2016 5588       nabowle     Check PSM param for precipitation.
 * Jan 10, 2016 6058       bkowal      Silently ignore invalid DHR input files. Do not fully
 *                                     read an entire file just to check for precip.
 * Jul 24, 2018 5588       mapeters    Fix determination of whether DHR product has precip,
 *                                     account for legacy/java decode paths using separate
 *                                     gather dirs, abstracted out to {@link MpeRadarGather}
 *
 * </pre>
 *
 * @author bkowal
 */
public class DHRGather extends MpeRadarGather {

    public DHRGather() {
        super(MpeRadarGatherConstants.DHR_PRODUCT_TYPE,
                MpeRadarGatherConstants.AppsDefaults.DHR_PROD_DIR);
    }
}