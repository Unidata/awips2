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
package com.raytheon.uf.common.dataplugin.text.db;

import java.util.Collections;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.text.request.MixedCaseProductIdsRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Utilities to support mixed case product generation on a per Product ID (nnn)
 * basis
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 01, 2014  3685     randerso  Initial creation
 * Mar 16, 2016  5411     randerso  Added null checks, code cleanup
 * Apr 27, 2020  8153     randerso  Refactor MixCaseProductSupport to use only
 *                                  AW_SITE_IDENTIFIER's copy of
 *                                  mixedCaseProductIds.txt
 *
 * </pre>
 *
 * @author randerso
 */

public class MixedCaseProductSupport {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MixedCaseProductSupport.class);

    /**
     * @return list of Product IDs enabled for mixed case transmission
     * @throws Exception
     */
    public static Set<String> getMixedCasePids() {
        try {
            Object retVal = RequestRouter
                    .route(new MixedCaseProductIdsRequest());
            if (retVal instanceof Set<?>) {
                @SuppressWarnings("unchecked")
                Set<String> mixedCasePids = (Set<String>) retVal;
                return mixedCasePids;
            } else {
                statusHandler.error(
                        "Unexpected type returned from MixedCaseProductIdsRequest: "
                                + (retVal == null ? "null"
                                        : retVal.getClass().getName()));
            }
        } catch (Exception e) {
            statusHandler.error("Error retreiving mixed case product IDs", e);
        }
        return Collections.emptySet();
    }

    public static boolean isMixedCase(String pid) {
        if (pid == null) {
            return false;
        }
        return getMixedCasePids().contains(pid.toUpperCase());
    }

    public static String conditionalToUpper(String pid, String text) {
        if (!isMixedCase(pid)) {
            if (text != null) {
                text = text.toUpperCase();
            }
        }

        return text;
    }
}
