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
package com.raytheon.uf.common.datadelivery.registry;

import java.util.Random;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.util.AbstractFixture;

/**
 * Fixture for {@link Coverage}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2013   2292     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class CoverageFixture extends AbstractFixture<Coverage> {
    public static final CoverageFixture INSTANCE = new CoverageFixture();

    /**
     * Prevent construction.
     */
    private CoverageFixture() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Coverage getInstance(long seedValue, Random random) {
        Coverage coverage = new Coverage();

        ReferencedEnvelope env = new ReferencedEnvelope(20, 25, 20, 25,
                MapUtil.LATLON_PROJECTION);
        coverage.setEnvelope(env);
        coverage.setRequestEnvelope(env);
        return coverage;
    }
}
