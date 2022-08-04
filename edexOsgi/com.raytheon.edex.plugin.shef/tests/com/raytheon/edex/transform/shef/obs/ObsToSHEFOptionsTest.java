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
package com.raytheon.edex.transform.shef.obs;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import org.junit.Test;

/**
 * JUnit test for ShefOptions class.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2018    6843    mduff       Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class ObsToSHEFOptionsTest {

    private MetarToShefConfigReader configReader = mock(
            MetarToShefConfigReader.class);

    @Test
    public void testShefOptions() {
        ObsToSHEFOptions options = new ObsToSHEFOptions(
                " -a -b -p1 -y2k -salias -p6 -p24 -round -pct 8", true,
                configReader);
        assertTrue("-a should be true", options.isOptCheckAliasId());
        assertTrue("-b should be true", options.isOptCollectives());
        assertTrue("-p1 should be true", options.isOptZeroAuto1HourPrecip());
        assertTrue("-y2k should be true", options.isOptCentury());
        assertTrue("-salias should be true", options.isOptCheckAliasId());
        assertTrue("-p6 should be true", options.isOptZeroAuto6HourPrecip());
        assertTrue("-p24 should be true", options.isOptZeroAuto24HourPrecip());
        assertTrue("-round should be true", options.isOptRoundObsTime());
        assertTrue("-pct 8 should be 8", options.getOptPCT() == 8);
    }
}
