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
package com.raytheon.uf.edex.plugin.mpe.apps;

/**
 * Indicates that an Apps_defaults-mapped property could not be initialized on a
 * target object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class AppsDefaultsLoadException extends Exception {

    private static final long serialVersionUID = 5337908185922283968L;

    public AppsDefaultsLoadException(String message) {
        super(message);
    }

    public AppsDefaultsLoadException(String message, Throwable cause) {
        super(message, cause);
    }
}