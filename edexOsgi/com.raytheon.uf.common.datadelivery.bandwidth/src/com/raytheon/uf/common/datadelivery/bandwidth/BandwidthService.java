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
package com.raytheon.uf.common.datadelivery.bandwidth;

import java.rmi.RemoteException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest.RequestType;
import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.serialization.ExceptionWrapper;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.LogUtil;

/**
 * Implementation of {@link IBandwidthService}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2012 1286       djohnson     Initial creation
 * Nov 15, 2012 1286       djohnson     No longer abstract.
 * Nov 20, 2012 1286       djohnson     Add proposeSchedule methods.
 * Dec 06, 2012 1397       djohnson     Add ability to get bandwidth graph data.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BandwidthService implements IBandwidthService {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BandwidthService.class);

    /**
     * {@inheritDoc}
     */
    @Override
    public final int getBandwidthForNetworkInKilobytes(Network network) {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.GET_BANDWIDTH);
        request.setNetwork(network);

        try {
            return ((Integer) sendRequest(request)).intValue();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to set available bandwidth for network [" + network
                            + "]", e);
            return 0;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings("unchecked")
    public Set<Subscription> proposeBandwidthForNetworkInKilobytes(
            Network network, int bandwidth) {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.PROPOSE_SET_BANDWIDTH);
        request.setNetwork(network);
        request.setBandwidth(bandwidth);

        try {
            return ((Set<Subscription>) sendRequest(request));
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to set available bandwidth for network [" + network
                            + "]", e);
            return null;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final boolean setBandwidthForNetworkInKilobytes(Network network,
            int bandwidth) {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.FORCE_SET_BANDWIDTH);
        request.setNetwork(network);
        request.setBandwidth(bandwidth);

        try {
            return ((Boolean) sendRequest(request)).booleanValue();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to set available bandwidth for network [" + network
                            + "]", e);
            return false;
        }
    }

    /**
     * {@inheritDoc}
     * 
     * @return
     */
    @Override
    public Set<String> schedule(Subscription subscription) {
        return schedule(Arrays.asList(subscription));
    }

    /**
     * Schedule the subscriptions for bandwidth management.
     * 
     * @param subscriptions
     *            the subscriptions
     * @return the set of unscheduled subscriptions
     */
    @Override
    public Set<String> schedule(List<Subscription> subscriptions) {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.SCHEDULE_SUBSCRIPTION);
        request.setSubscriptions(subscriptions);

        try {
            @SuppressWarnings("unchecked")
            Set<String> retVal = (Set<String>) sendRequest(request);
            return retVal;
        } catch (Exception e) {
            LogUtil.logIterable(
                    statusHandler,
                    Priority.PROBLEM,
                    "Unable to schedule the following subscriptions for bandwidth management:",
                    subscriptions);
            return Collections.emptySet();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IProposeScheduleResponse proposeSchedule(Subscription subscription) {
        return proposeSchedule(Arrays.asList(subscription));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IProposeScheduleResponse proposeSchedule(
            List<Subscription> subscriptions) {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.PROPOSE_SCHEDULE_SUBSCRIPTION);
        request.setSubscriptions(subscriptions);

        try {
            return (IProposeScheduleResponse) sendRequest(request);
        } catch (Exception e) {
            LogUtil.logIterable(
                    statusHandler,
                    Priority.PROBLEM,
                    "Returning null response object, unable to propose scheduling the following subscriptions for bandwidth management:",
                    subscriptions);
            return IProposeScheduleResponse.NULL_OBJECT;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void reinitialize() {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.REINITIALIZE);

        try {
            sendRequest(request);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to reinitialize the bandwidth manager.", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Date getEstimatedCompletionTime(AdhocSubscription sub) {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setSubscriptions(Arrays.<Subscription> asList(sub));
        request.setRequestType(RequestType.GET_ESTIMATED_COMPLETION);
        try {
            return (Date) sendRequest(request);
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Unable to retrieve the estimated completion time, returning the current time.",
                            e);
            return new Date();
        }
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthGraphData getBandwidthGraphData() {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.GET_BANDWIDTH_GRAPH_DATA);
        try {
            return (BandwidthGraphData) sendRequest(request);
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                    "Unable to retrieve bandwidth graph data, returning null.",
                            e);
            return null;
        }
    }

    /**
     * {@inheritDoc}
     */
    @VisibleForTesting
    protected Object sendRequest(IBandwidthRequest request) throws Exception {
        Object object = RequestRouter.route(request,
                DataDeliveryConstants.DATA_DELIVERY_SERVER);
        if (object instanceof SuccessfulExecution) {
            SuccessfulExecution response = (SuccessfulExecution) object;
            return response.getResponse();
        } else {
            throw new RemoteException(
                    "Error communicating with bandwidth service!",
                    ExceptionWrapper
                            .unwrapThrowable(((ServerErrorResponse) object)
                                    .getException()));
        }
    }
}
