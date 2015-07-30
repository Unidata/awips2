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
package com.raytheon.rcm.request;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.serialization.JAXBManager;

/**
 * Holds the JAXBManager used by other classes.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 09, 2015 4498       nabowle     Switch to JAXBManager.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
public class RpsXml {
    private static JAXBManager manager;

    private static Logger logger = LoggerFactory.getLogger(RpsXml.class);

    static {
        try {
            /* JAXBManager#marshalTo... formats output by default. */
            manager = new JAXBManager(RpsList.class);
        } catch (JAXBException e) {
            logger.error("Could not create JAXBManager.", e);
        }
    }

    public static JAXBManager getJAXBManager() {
        return manager;
    }
}
