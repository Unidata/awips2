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
package com.raytheon.uf.edex.registry.ebxml.constants;

/**
 * This class holds the canonical ClassificationNodes are defined for the
 * ErrorSeverityType ClassificationScheme. These constants define the severity
 * of the error returned from the registry services
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ErrorSeverity {

    /**
     * The problem reported is only a warning and will not cause a fault
     */
    public static final String WARNING = "urn:oasis:names:tc:ebxml-regrep:ErrorSeverityType:Warning";

    /**
     * The problem reported is severe enough to generate a fault
     */
    public static final String ERROR = "urn:oasis:names:tc:ebxml-regrep:ErrorSeverityType:Error";
}
