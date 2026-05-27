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
package com.raytheon.uf.edex.registry.ebxml.services;

import org.apache.cxf.clustering.FailoverTargetSelector;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.Message;

/**
 * <pre>
 *
 * Since CXF does not failover on Server Errors (500), this class overrides that behavior and forces a failover in that case.
 * This class only exists since several REST services have moved in version 16.1.1.
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * May 11, 2015  4448     bphillip  Initial creation
 * Aug 10, 2020  8203     randerso  Updated for CXF 3.3.7
 *
 * </pre>
 *
 * @author bphillip
 **/
public class RegistryFailoverTargetSelector extends FailoverTargetSelector {

    private static final Integer SERVER_ERROR_CODE = 500;

    @Override
    protected boolean requiresFailover(Exchange exchange, Exception ex) {
        if (!super.requiresFailover(exchange, ex)) {
            return SERVER_ERROR_CODE
                    .equals(exchange.get(Message.RESPONSE_CODE));
        }
        return true;
    }
}
