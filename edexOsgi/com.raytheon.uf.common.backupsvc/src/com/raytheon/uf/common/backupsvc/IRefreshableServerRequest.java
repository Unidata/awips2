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
package com.raytheon.uf.common.backupsvc;

import java.util.Optional;

import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * A refreshable {@link IServerRequest}. Calling {@link #refresh()} allows the
 * request to update itself arbitrarily.
 *
 * Introduced for BackupService, which stores IServerRequests to be sent later.
 * Such requests may be "stale" for whatever reason by the time they'd be sent.
 * This mechanism allows for a stale request to be updated or even canceled
 * before it would be sent.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2019             tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public interface IRefreshableServerRequest extends IServerRequest {
    /**
     * @return the request itself updated arbitrarily, or a new/different
     *         request object of the same type, or a different type of request
     *         object, or nothing.
     */
    Optional<IServerRequest> refresh();
}
