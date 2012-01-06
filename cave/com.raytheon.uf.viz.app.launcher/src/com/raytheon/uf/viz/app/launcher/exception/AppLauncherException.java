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
package com.raytheon.uf.viz.app.launcher.exception;

/**
 * Exception thrown when an error occurs launching a external application.
 * Constructors corresponding to the constructors for Exception are provided
 * <P>
 * Preferred usage is {@code AppLauncherException(String)} or
 * {@code AppLauncherException(String,Throwable)}.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2009 2081       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0
 */
public class AppLauncherException extends Exception {

    private static final long serialVersionUID = 1L;

    /**
     * Constructor. Creates an empty exception.
     */
    public AppLauncherException() {
        // intentionally empty
    }
    /**
     * Constructor. Creates an exception with the specified message.
     * @param message the message for the exception
     */
    public AppLauncherException(String message) {
        super(message);
        // intentionally empty
    }
    /**
     * Constructor. Creates an exception with the specified cause.
     * @param cause the cause of the exception
     */
    public AppLauncherException(Throwable cause) {
        super(cause);
        // intentionally empty
    }
    /**
     * Constructor. Creates an exception with the specified message
     * and cause.
     * @param message the message for the exception
     * @param cause the cause of the exception
     */
    public AppLauncherException(String message, Throwable cause) {
        super(message, cause);
        // intentionally empty
    }

}
