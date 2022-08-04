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
package com.raytheon.uf.edex.plugin.mpe.dpa;

/**
 * Signals decoding of DPA radar record aborted, with message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2016 4622       jschmid     Initial creation
 * 
 * </pre>
 * 
 * @author jschmid
 */

public class DecodeDPAException extends Exception {

    private static final long serialVersionUID = 5210154108038204476L;

    public DecodeDPAException(String message) {
        super(message);
    }

    public DecodeDPAException(String message, Throwable cause) {
        super(message, cause);
    }
}
