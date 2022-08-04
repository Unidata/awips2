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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.mosaic;

import com.raytheon.uf.edex.plugin.mpe.MpeException;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPERadarMosaic;

/**
 * Indicates that a radar mosaic could not be successfully generated.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class MosaicGenerationFailedException extends MpeException {

    private static final long serialVersionUID = 3254957665301411283L;

    private static final String MESSAGE_FMT = "Failed to generate Radar Mosaic: %s. %s";

    public MosaicGenerationFailedException(final HPERadarMosaic mosaic,
            String message) {
        super(String.format(MESSAGE_FMT, mosaic.name(), message));
    }

    public MosaicGenerationFailedException(final HPERadarMosaic mosaic,
            String message, Throwable cause) {
        super(String.format(MESSAGE_FMT, mosaic.name(), message), cause);
    }
}