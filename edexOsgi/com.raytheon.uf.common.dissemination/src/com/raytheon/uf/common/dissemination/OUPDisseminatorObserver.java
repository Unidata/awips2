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
package com.raytheon.uf.common.dissemination;

/**
 * Interface for objects that wish to be notified of text product dissemination.
 *
 * To get this functionality you have to implement this interface on an object
 * and then register that object by calling OUPHandler.addObserver. (There is
 * supposed to be only one OUPHandler instance in EDEX. It is accessible as a
 * Spring bean called "oupHandler".)
 *
 * IMPORTANT NOTE: This is only used by the Java OUPDisseminator. It is not
 * needed or used by handleOUP.py, which is still supported. If handleOUP.py is
 * being used instead of OUPDisseminator for text product dissemination, then no
 * observers will get called. So, don't implement this interface until
 * handleOUP.py has been removed.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2025 2038247    tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */
public interface OUPDisseminatorObserver {
    /**
     * Called immediately before the OUP is transmitted on the WAN. This is not
     * called for OUPs that are excluded from the WAN for whatever reason.
     *
     * @param oup
     *            The {@link OfficialUserProduct}
     * @throws Exception
     */
    void beforeSendToWAN(OfficialUserProduct oup) throws Exception;
}
