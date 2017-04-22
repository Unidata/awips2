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

import java.nio.file.Path;

/**
 * Signals that reading a DHR Radar file has failed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2016 4625       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class DHRInputException extends Exception {

    private static final long serialVersionUID = -1909092988941293954L;

    public DHRInputException(final Path radarDHRFilePath, Throwable cause) {
        super(generateExMsg(radarDHRFilePath), cause);
    }

    private static String generateExMsg(Path radarDHRFilePath) {
        return String.format("Failed to read Radar DHR file: %s.",
                radarDHRFilePath.toString());
    }
}