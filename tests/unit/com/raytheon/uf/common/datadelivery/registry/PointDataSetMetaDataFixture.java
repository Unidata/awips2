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

import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.common.util.AbstractFixture;

/**
 * {@link AbstractFixture} implementation for {@link PointDataSetMetaData}
 * objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 05, 2013 2038       djohnson     Initial creation
 * Oct  3, 2013 1797       dhladky      generics
 * Oct 10, 2013 1797       bgonzale     Refactored registry Time objects.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class PointDataSetMetaDataFixture extends
        AbstractFixture<PointDataSetMetaData> {

    public static final PointDataSetMetaDataFixture INSTANCE = new PointDataSetMetaDataFixture();

    /**
     * Disabled constructor.
     */
    private PointDataSetMetaDataFixture() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PointDataSetMetaData getInstance(long seedValue, Random random) {
        final PointTime time = PointTimeFixture.INSTANCE.get(seedValue);
        final PointDataSet dataSet = WFSPointDataSetFixture.INSTANCE
                .get(seedValue);

        PointDataSetMetaData obj = new PointDataSetMetaData();
        obj.setDataSetDescription("description" + seedValue);
        obj.setDataSetName(dataSet.getDataSetName());
        obj.setDate(new ImmutableDate(GriddedTimeFixture.INSTANCE
                .get(seedValue).getStart()));
        obj.setProviderName(dataSet.getProviderName());
        obj.setTime(time);
        obj.setUrl("http://" + seedValue);

        return obj;
    }

}
