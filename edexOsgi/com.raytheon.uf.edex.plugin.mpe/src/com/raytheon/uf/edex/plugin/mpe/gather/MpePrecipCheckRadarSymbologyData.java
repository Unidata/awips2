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
package com.raytheon.uf.edex.plugin.mpe.gather;

import java.nio.ByteBuffer;

import com.raytheon.uf.edex.plugin.mpe.gather.radar.InvalidMpeRadarException;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarSymbologyData;

/**
 * Extension of {@link MpeRadarSymbologyData} designed to only read the Radar
 * Symbology data from an input file. Also handles the case when the Radar
 * Symbology data cannot be found in the specified file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2017 6058       bkowal      Initial creation
 * Jul 24, 2018 5588       mapeters    Moved from com.raytheon.uf.edex.plugin.mpe.gather.dhr
 *
 * </pre>
 *
 * @author bkowal
 */

public final class MpePrecipCheckRadarSymbologyData
        extends MpeRadarSymbologyData {

    public boolean readPrecipIndicationParams(final ByteBuffer buf) {
        try {
            readPsmParams(buf);
        } catch (InvalidMpeRadarException e) {
            /*
             * Apparently, there is a mixture of files with the proper structure
             * and files without the proper structure in the DHR/DSP Gather file
             * directories. So, if a file with the wrong structure is
             * encountered, it will just be ignored to match the legacy
             * implementation.
             */
            return false;
        }
        return (getPsmParams() != null
                && getPsmParams().length > MpeRadarGatherConstants.DHR_PARAM_PRECIP);
    }
}