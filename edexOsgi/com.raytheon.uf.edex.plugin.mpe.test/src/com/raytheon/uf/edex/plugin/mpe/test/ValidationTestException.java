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
package com.raytheon.uf.edex.plugin.mpe.test;

/**
 * Indicates that a {@link IValidationTest} has failed to finish. This exception
 * should only be used when a condition occurs that will prevent the ongoing
 * execution of a test, not when a validation step within the test fails as a
 * result of non-matching data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2016  5576       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class ValidationTestException extends Exception {

    private static final long serialVersionUID = -2547528720502054970L;

    public ValidationTestException(String message) {
        super(message);
    }

    public ValidationTestException(String message, Throwable cause) {
        super(message, cause);
    }
}