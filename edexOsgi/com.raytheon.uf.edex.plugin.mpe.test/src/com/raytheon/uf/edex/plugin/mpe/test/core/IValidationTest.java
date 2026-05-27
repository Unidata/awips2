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
package com.raytheon.uf.edex.plugin.mpe.test.core;

import java.util.Calendar;

import com.raytheon.uf.edex.plugin.mpe.test.ValidationTestException;

/**
 * Interface describing the structure of a MPE conversion validation test.
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

public interface IValidationTest {

    /**
     * Returns the name of the validation test.
     * 
     * @return the name of the validation test.
     */
    public String getTestName();

    /**
     * Executes the validation test with the objective of validating data
     * generated for and/or associated with the specified validation date/time.
     * 
     * @param validationDateTime
     *            the specified validation date/time.
     * @throws ValidationTestException
     *             when validation fails to complete.
     */
    public void executeValidationTest(final Calendar validationDateTime)
            throws ValidationTestException;

}