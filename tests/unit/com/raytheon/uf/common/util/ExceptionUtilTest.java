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
package com.raytheon.uf.common.util;

import static org.junit.Assert.assertSame;

import java.io.FileNotFoundException;

import org.junit.Test;

/**
 * Test {@link ExceptionUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 5, 2012  740        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ExceptionUtilTest {

    @Test
    public void testGettingRootCauseGoesNLevelsDeep() {
        final FileNotFoundException root = new FileNotFoundException("root");
        final Throwable t = new Throwable(new IllegalArgumentException(
                new IllegalStateException(root)));

        assertSame("Didn't get the correct exception for the root cause!",
                root, ExceptionUtil.getRootCause(t));
    }

    @Test
    public void testGettingRootCauseWillReturnParameterIfNoCauseFound() {
        final FileNotFoundException root = new FileNotFoundException();

        assertSame("Didn't get the correct exception for the root cause!",
                root, ExceptionUtil.getRootCause(root));
    }
}
