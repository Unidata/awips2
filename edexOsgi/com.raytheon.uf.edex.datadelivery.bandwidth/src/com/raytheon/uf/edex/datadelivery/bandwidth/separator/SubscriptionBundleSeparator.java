package com.raytheon.uf.edex.datadelivery.bandwidth.separator;

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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Separate Subscription bundles in Queue
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2012            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class SubscriptionBundleSeparator implements
        Iterator<SubscriptionBundle> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionBundleSeparator.class);

    private List<SubscriptionBundle> reports = null;

    private int currentCount = -1;

    public static SubscriptionBundleSeparator separate(
            ArrayList<SubscriptionBundle> data, Headers headers)
            throws Exception {
        SubscriptionBundleSeparator sbs = new SubscriptionBundleSeparator();
        sbs.setData(data, headers);
        return sbs;
    }

    @Override
    public boolean hasNext() {

        boolean answer = ((reports != null) && (reports.size() > 0) && (currentCount < reports
                .size()));

        if (!answer) {
            reports.clear();
            reports = null;
        }

        return answer;
    }

    @Override
    public SubscriptionBundle next() {
        return reports.get(currentCount++);
    }

    public void setData(ArrayList<SubscriptionBundle> sbs, Headers headers) {

        try {
            if (sbs != null) {
                reports = new ArrayList<SubscriptionBundle>();
                separate(sbs);
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }

        if ((reports != null) && (reports.size() > 0)) {
            currentCount = 0;
        } else {
            System.err.println("No bundles found in data.");
        }
    }

    private void separate(ArrayList<SubscriptionBundle> sbs) {

        for (int i = 0; i < sbs.size(); i++) {
            try {
                reports.add(sbs.get(i));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    @Override
    public void remove() {

    }

}
