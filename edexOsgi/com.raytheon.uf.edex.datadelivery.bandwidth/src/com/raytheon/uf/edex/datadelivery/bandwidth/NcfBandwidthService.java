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
package com.raytheon.uf.edex.datadelivery.bandwidth;

import com.raytheon.uf.common.datadelivery.bandwidth.BandwidthService;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthService;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;

/**
 * Implementation of {@link IBandwidthService} that communicates with the NCF
 * bandwidth manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2013 1644       djohnson     Initial creation
 * OCt 2,  2013 1797       dhladky      Generics
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class NcfBandwidthService<T extends Time, C extends Coverage> extends BandwidthService<T, C> {

    /**
     * Constructor.
     */
    protected NcfBandwidthService() {
        super(DataDeliveryConstants.NCF_BANDWIDTH_MANAGER_SERVICE);
    }

}
