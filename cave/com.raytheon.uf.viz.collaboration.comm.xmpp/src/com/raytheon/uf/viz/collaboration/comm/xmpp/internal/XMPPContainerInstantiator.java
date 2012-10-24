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
package com.raytheon.uf.viz.collaboration.comm.xmpp.internal;

import org.eclipse.ecf.core.ContainerCreateException;
import org.eclipse.ecf.core.ContainerTypeDescription;

import com.raytheon.uf.viz.collaboration.comm.xmpp.XMPPContainer;

/**
 * 
 * Extends the ECF XMPPContainerInstantiator to create a Viz specific container
 * with fixes needed for Viz.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@SuppressWarnings("restriction")
public class XMPPContainerInstantiator extends
        org.eclipse.ecf.internal.provider.xmpp.XMPPContainerInstantiator {

    /**
     * Adapted from super.createInstance, only change is that it returns the Viz
     * XMPPContainer instead of the ECF XMPPContainer
     */
    @Override
    public XMPPContainer createInstance(ContainerTypeDescription description,
            Object[] args) throws ContainerCreateException {
        try {
            Integer ka = new Integer(XMPPContainer.DEFAULT_KEEPALIVE);
            String name = null;
            if (args != null) {
                if (args.length > 0) {
                    name = (String) args[0];
                    if (args.length > 1) {
                        ka = getIntegerFromArg(args[1]);
                    }
                }
            }
            if (name == null) {
                if (ka == null) {
                    return new XMPPContainer();
                } else {
                    return new XMPPContainer(ka.intValue());
                }
            } else {
                if (ka == null) {
                    ka = new Integer(XMPPContainer.DEFAULT_KEEPALIVE);
                }
                return new XMPPContainer(name, ka.intValue());
            }
        } catch (Exception e) {
            throw new ContainerCreateException(
                    "Exception creating generic container", e);
        }
    }

}