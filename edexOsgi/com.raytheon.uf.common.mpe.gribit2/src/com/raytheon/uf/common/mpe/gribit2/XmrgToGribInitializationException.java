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
package com.raytheon.uf.common.mpe.gribit2;

/**
 * Indicates that xmrg to grib initialization has failed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class XmrgToGribInitializationException extends Exception {

    private static final long serialVersionUID = -7789247578724815856L;

    private static final String INIT_ERR_MSG = "Failed to intialize the xmrg to grib converter. %s";

    public XmrgToGribInitializationException(String message) {
        super(String.format(INIT_ERR_MSG, message));
    }

    public XmrgToGribInitializationException(String message, Throwable cause) {
        super(String.format(INIT_ERR_MSG, message), cause);
    }
}