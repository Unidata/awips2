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
package com.raytheon.uf.viz.datadelivery.utils;

import com.raytheon.uf.common.registry.RegistryConstants;
import com.raytheon.uf.common.registry.ebxml.SOAPRegistryManager;
import com.raytheon.uf.viz.core.VizServers;

/**
 * CAVE specific version of the {@link SOAPRegistryManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2012 1167       djohnson     Extend {@link SOAPRegistryManager} to use localization.
 * Dec 03, 2012 1379       djohnson     Use registry service keys.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class VizSoapRegistryHandler extends SOAPRegistryManager {


    @Override
    protected String getQueryManagerUrl() {
        return VizServers.getInstance().getServerLocation(
                RegistryConstants.EBXML_QUERY_SERVICE);
    }

    @Override
    protected String getLifecycleManagerUrl() {
        return VizServers.getInstance().getServerLocation(
                RegistryConstants.EBXML_LCM_SERVICE);
    }
}
