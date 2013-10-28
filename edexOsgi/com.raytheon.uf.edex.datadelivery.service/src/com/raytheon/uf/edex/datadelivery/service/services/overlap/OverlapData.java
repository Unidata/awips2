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
package com.raytheon.uf.edex.datadelivery.service.services.overlap;

import java.util.Collection;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Parent Overlap Data Object. This object holds the data needed to calculate
 * overlapping of subscriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2013   2292     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 * @param <T>
 * @param <C>
 */

public abstract class OverlapData<T extends Time, C extends Coverage> {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OverlapData.class);

    protected final int ONE_HUNDRED_PERCENT = 100;

    private final String UNABLE_TO_DETERMINE_SPATIAL_OVERLAP = "Unable to determine spatial overlap.  "
            + "Subscriptions will not be considered to be overlapping spatially.";

    /** The number of common attributes (in this class) */
    protected final int numberOfCommonAttributes = 2;

    /** Spatial duplication percent */
    protected int spatialDuplication = -999;

    /** parameter duplication percent */
    protected int parameterDuplication = -999;

    /** Spatial pass flag */
    protected boolean spatialPass = false;

    /** Parameter pass flag */
    protected boolean parameterPass = false;

    /** The subscription match strategy */
    protected SubscriptionOverlapMatchStrategy matchStrategy;

    /** The subscription overlap config object */
    protected SubscriptionOverlapConfig config;

    /** Subscription 1 */
    protected Subscription<T, C> sub1;

    /** Subscription 2 */
    protected Subscription<T, C> sub2;

    /**
     * Constructor.
     * 
     * @param sub1
     * @param sub2
     * @param config
     */
    public OverlapData(Subscription<T, C> sub1, Subscription<T, C> sub2,
            SubscriptionOverlapConfig config) {
        this.sub1 = sub1;
        this.sub2 = sub2;
        this.config = config;
        this.matchStrategy = config.getMatchStrategy();
    }

    /**
     * Calculates the percent, 0-100, of how much spatial coverage from sub2 is
     * satisfied by sub1.
     * 
     * @param sub1
     * @param sub2
     */
    protected void calculateSpatialDuplicationPercent(Subscription<T, C> sub1,
            Subscription<T, C> sub2) {

        final Coverage sub1Coverage = sub1.getCoverage();
        final Coverage sub2Coverage = sub2.getCoverage();

        if (sub1Coverage != null && sub2Coverage != null) {
            final ReferencedEnvelope sub1Envelope = sub1Coverage
                    .getRequestEnvelope();
            final ReferencedEnvelope sub2Envelope = sub2Coverage
                    .getRequestEnvelope();

            if (sub1Envelope != null && sub2Envelope != null) {
                // try {
                ReferencedEnvelope intersection;
                try {
                    intersection = MapUtil.reprojectAndIntersect(sub1Envelope,
                            sub2Envelope);
                    final double intersectionArea = intersection.getArea();
                    spatialDuplication = (int) ((intersectionArea * 100) / sub2Envelope
                            .getArea());
                } catch (TransformException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            UNABLE_TO_DETERMINE_SPATIAL_OVERLAP, e);
                }
            }
        }
    }

    /**
     * Calculates the percent, 0-100, of how many parameters from sub2 are
     * satisfied by sub1.
     * 
     * @param sub1
     * @param sub2
     */
    protected void calculateParameterDuplicationPercent(
            Subscription<T, C> sub1, Subscription<T, C> sub2) {
        parameterDuplication = getDuplicationPercent(sub1.getParameter(),
                sub2.getParameter());
    }

    /**
     * Determine the overlap values
     */
    protected void determineOverlapping() {
        calculateParameterDuplicationPercent(sub1, sub2);
        calculateSpatialDuplicationPercent(sub1, sub2);
        this.parameterPass = this.parameterDuplication > config
                .getMaxAllowedParameterDuplication();

        this.spatialPass = this.spatialDuplication > config
                .getMaxAllowedSpatialDuplication();
    }

    /**
     * Calculate the duplication percent of the two collections.
     * 
     * @param coll1
     * @param coll2
     * @return
     */
    protected <M> int getDuplicationPercent(Collection<M> coll1,
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

    /**
     * Are the two subscriptions considered overlapping
     * 
     * @return true if overlapping
     */
    public abstract boolean isOverlapping();

    /**
     * Are the two subscriptions duplicates
     * 
     * @return true if duplicates
     */
    public abstract boolean isDuplicate();
}
