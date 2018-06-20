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
package com.raytheon.uf.edex.ohd.pproc;

/**
 * Indicates that an issue was encountered during preparation for or active
 * execution of MPE precipitation data creation.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2017 6407       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class PrecipCreationException extends Exception {

    private static final long serialVersionUID = -2113291261213494742L;

    public PrecipCreationException(String message) {
        super(message);
    }

    public PrecipCreationException(String message, Throwable cause) {
        super(message, cause);
    }
}