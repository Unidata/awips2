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
package com.raytheon.uf.common.datadelivery.service.subscription;

import java.util.Collection;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Checks for duplication among subscriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 02, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionDuplicateChecker implements
        ISubscriptionDuplicateChecker {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionDuplicateChecker.class);

    private static final String UNABLE_TO_DETERMINE_SPATIAL_OVERLAP = "Unable to determine spatial overlap.  "
            + "Subscriptions will not be considered to be overlapping spatially.";

    /**
     * {@inheritDoc}
     */
    @Override
    public int getParameterDuplicationPercent(Subscription sub1,
            Subscription sub2) {
        return getDuplicationPercent(sub1.getParameter(), sub2.getParameter());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getForecastHourDuplicationPercent(Subscription sub1,
            Subscription sub2) {
        return getDuplicationPercent(sub1.getTime().getSelectedTimeIndices(),
                sub2.getTime().getSelectedTimeIndices());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getCycleDuplicationPercent(Subscription sub1, Subscription sub2) {
        return getDuplicationPercent(sub1.getTime().getCycleTimes(), sub2
                .getTime().getCycleTimes());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getSpatialDuplicationPercent(Subscription sub1, Subscription sub2) {
        final Coverage sub1Coverage = sub1.getCoverage();
        final Coverage sub2Coverage = sub2.getCoverage();

        if (sub1Coverage != null && sub2Coverage != null) {
            final ReferencedEnvelope sub1Envelope = sub1Coverage
                    .getRequestEnvelope();
            final ReferencedEnvelope sub2Envelope = sub2Coverage
                    .getRequestEnvelope();

            if (sub1Envelope != null && sub2Envelope != null) {
                try {
                    ReferencedEnvelope intersection = MapUtil
                            .reprojectAndIntersect(sub1Envelope, sub2Envelope);
                    final double intersectionArea = intersection.getArea();
                    return (int) ((intersectionArea * 100) / sub2Envelope
                            .getArea());
                } catch (TransformException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            UNABLE_TO_DETERMINE_SPATIAL_OVERLAP, e);
                }
            }
        }
        return 0;
    }

    private <T> int getDuplicationPercent(Collection<T> coll1,
            Collection<T> coll2) {

        int numberSatisfiedByFirstCollection = 0;
        if (!CollectionUtil.isNullOrEmpty(coll1)
                && !CollectionUtil.isNullOrEmpty(coll2)) {
            for (T entry : coll2) {
                if (coll1.contains(entry)) {
                    numberSatisfiedByFirstCollection++;
                }
            }
            // Convert percent to 0-100
            return (numberSatisfiedByFirstCollection * 100) / (coll2.size());
        }

        return 0;
    }
}
