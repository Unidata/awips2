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
package com.raytheon.uf.edex.ohd.satpre;

/**
 * Indicates that an issue was encountered during precipitation retrieval or
 * persistence.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2017 6407       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class PrecipDataException extends Exception {

    private static final long serialVersionUID = -1698762774140724659L;

    public PrecipDataException(String message) {
        super(message);
    }

    public PrecipDataException(String message, Throwable cause) {
        super(message, cause);
    }
}