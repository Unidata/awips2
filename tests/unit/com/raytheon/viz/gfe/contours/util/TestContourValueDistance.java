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
package com.raytheon.viz.gfe.contours.util;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.viz.gfe.contours.util.ContourValueDistance;

/**
 * JUnit test cases for selected methods in {@link ContourValueDistance}.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27Mar2008    968        MW Fegan    Initial implementation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class TestContourValueDistance {
    /* test objects */
    ContourValueDistance cvd1 = null;
    ContourValueDistance cvd2 = null;
    
    /* values for test objects */
    double dist1 = 3.7;
    double value1 = 50.0;
    double dist2 = 4.1;
    double value2 = 49.5;
    
    /* gradients */
    double grad12 = (value1 - value2) / (dist1 - dist2);
    double grad21 = (value2 - value1) / (dist2 - dist1);
    
    /* zero */
    final double D_Zero = (double)0.0; 
    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        cvd1 = new ContourValueDistance(value1,dist1);
        cvd2 = new ContourValueDistance(value2,dist2);
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        cvd1 = null;
        cvd2 = null;
    }

    /**
     * Test method for {@link ContourValueDistance#equals(java.lang.Object)}.
     */
    @Test
    public void testEqualsObject() {
        System.out.println("Testing equality");
        assertFalse("Testing rhs must be the correct type",cvd1.equals(new Object()));
        assertTrue("Testing object equals itself",cvd1.equals(cvd1));
        assertTrue("Testing objects have same component values",
                   cvd1.equals(new ContourValueDistance(value1,dist1)));
        assertFalse("Testing against a different object",cvd1.equals(cvd2));
    }

    /**
     * Test method for {@link ContourValueDistance#gradient(ContourValueDistance)}.
     */
    @Test
    public void testGradient() {
        System.out.println("Testing gradient computation");
        assertTrue("Testing gradient of object with self", 
                   cvd1.gradient(cvd1) == D_Zero);
        assertTrue("Testing gradient computation",cvd1.gradient(cvd2) == grad12 );
    }

    /**
     * Test method for {@link ContourValueDistance#reset()}.
     */
    @Test
    public void testReset() {
        System.out.println("Testing reset");
        ContourValueDistance defObj = new ContourValueDistance();
        assertFalse("Verifying test object not default",cvd1.equals(defObj));
        cvd1.reset();
        assertTrue("Verifying reset to default values",cvd1.equals(defObj));
    }

}
