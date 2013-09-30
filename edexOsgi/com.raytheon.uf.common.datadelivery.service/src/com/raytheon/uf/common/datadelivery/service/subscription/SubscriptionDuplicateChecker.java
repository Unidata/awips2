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
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
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
 * Sept 24, 2013 2386       dhladky     Made multi-data type
 * Sept 25, 2013 1797       dhladky     separated time from griddedtime
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionDuplicateChecker<T extends Time, C extends Coverage> implements
        ISubscriptionDuplicateChecker<T, C> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionDuplicateChecker.class);

    private static final String UNABLE_TO_DETERMINE_SPATIAL_OVERLAP = "Unable to determine spatial overlap.  "
            + "Subscriptions will not be considered to be overlapping spatially.";

    /**
     * {@inheritDoc}
     */
    @Override
    public int getParameterDuplicationPercent(Subscription<T, C> sub1,
            Subscription<T, C> sub2) {
        return getDuplicationPercent(sub1.getParameter(), sub2.getParameter());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getForecastHourDuplicationPercent(Subscription<T, C> sub1,
            Subscription<T, C> sub2) {

        if (sub1.getTime() instanceof GriddedTime) {

            GriddedTime gtime1 = (GriddedTime) sub1.getTime();
            GriddedTime gtime2 = (GriddedTime) sub2.getTime();

            return getDuplicationPercent(gtime1.getSelectedTimeIndices(),
                    gtime2.getSelectedTimeIndices());
        } else {
            throw new IllegalArgumentException(sub1.getTime().getClass()
                    + " Config not yet Implemented!");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getCycleDuplicationPercent(Subscription<T, C> sub1,
            Subscription<T, C> sub2) {

        if (sub1.getTime() instanceof GriddedTime) {

            GriddedTime gtime1 = (GriddedTime) sub1.getTime();
            GriddedTime gtime2 = (GriddedTime) sub2.getTime();

            return getDuplicationPercent(gtime1.getCycleTimes(),
                    gtime2.getCycleTimes());
        } else {
            throw new IllegalArgumentException(sub1.getTime().getClass()
                    + " Config not yet Implemented!");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getTimeDuplicationPercent(Subscription<T, C> sub1,
            Subscription<T, C> sub2) {

        if (sub1.getTime() instanceof PointTime) {

            PointTime ptime1 = (PointTime) sub1.getTime();
            PointTime ptime2 = (PointTime) sub2.getTime();

            return getDuplicationPercent(ptime1.getTimes(), ptime2.getTimes());
        } else {
            return 0;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getSpatialDuplicationPercent(Subscription<T, C> sub1, Subscription<T, C> sub2) {
       
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

    private <M> int getDuplicationPercent(Collection<M> coll1,
            Collection<M> coll2) {

        int numberSatisfiedByFirstCollection = 0;
        if (!CollectionUtil.isNullOrEmpty(coll1)
                && !CollectionUtil.isNullOrEmpty(coll2)) {
            for (M entry : coll2) {
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
