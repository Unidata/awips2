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
package com.raytheon.uf.edex.plugin.mpe.gather.radar;

import com.raytheon.uf.edex.plugin.mpe.MpeException;

/**
 * Exception to indicate decodinga radar file has failed.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */

public class MpeRadarDecodeException extends MpeException {

    private static final long serialVersionUID = 2565071267928349747L;

    public MpeRadarDecodeException(String message) {
        super(message);
    }

    public MpeRadarDecodeException(String message, Exception cause) {
        super(message, cause);
    }
}
