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
package com.raytheon.uf.edex.plugin.bufrmos.decoder;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.bufrtools.descriptors.DescriptorFactory;
import com.raytheon.uf.edex.bufrtools.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.bufrtools.descriptors.IDescriptorFactorySelector;


/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         jkorman     Initial creation.
 * Sep 16, 2014 #3628      mapeters    Replaced static imports.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class MOSDescriptorDelegate implements IDescriptorFactoryDelegate {
    private static Log logger = LogFactory.getLog(MOSDescriptorDelegate.class);

    IDescriptorFactorySelector factorySelector = null;

    private static Map<String, DescriptorFactory> factories;
    static {
        factories = new HashMap<String, DescriptorFactory>();
        factories.put(BUFRMOSStaticData.MODEL_AVN,
                createFactory("AVNBufrTableB", "AVNBufrTableD"));
        factories.put(BUFRMOSStaticData.MODEL_ETA,
                createFactory("ETABufrTableB", "ETABufrTableD"));
        factories.put(BUFRMOSStaticData.MODEL_GFS,
                createFactory("GFSBufrTableB", "GFSBufrTableD"));
        factories.put(BUFRMOSStaticData.MODEL_HPC,
                createFactory("HPCBufrTableB", "HPCBufrTableD"));
        factories.put(BUFRMOSStaticData.MODEL_LAMP,
                createFactory("LAMPBufrTableB",
                "LAMPBufrTableD"));
        factories.put(BUFRMOSStaticData.MODEL_MRF,
                createFactory("MRFBufrTableB", "MRFBufrTableD"));
        factories.put(BUFRMOSStaticData.MODEL_NGM,
                createFactory("NGMBufrTableB", "NGMBufrTableD"));

    }

    public MOSDescriptorDelegate(IDescriptorFactorySelector selector) {
        factorySelector = selector;
    }

    /**
     *
     */
    @Override
    public DescriptorFactory getInstance() {
        DescriptorFactory factory = null; // defaultFactory;

        String selector = null;
        if (factorySelector != null) {
            selector = factorySelector.getSelector();
        }
        logger.debug("Getting DescriptorFactory for type " + selector);

        factory = factories.get(selector);
        if (factory == null) {
            factory = new DescriptorFactory();
        }
        return factory;
    }

    /**
     * 
     */
    @Override
    public void setDescriptorFactorySelector(IDescriptorFactorySelector selector) {
        factorySelector = selector;
    }

    /**
     * 
     * @param resourceClassReference
     *            Class reference table resources will be loaded from.
     * @param tableB
     *            Name of BUFR table B.
     * @param tableD
     *            Name of BUFR table D.
     * @return
     */
    private static final DescriptorFactory createFactory(String tableB,
            String tableD) {
        DescriptorFactory factory = null;
        logger.debug("Creating table " + tableB + " " + tableD);
        factory = new DescriptorFactory(MOSDescriptorDelegate.class, tableB,
                tableD);
        logger.debug("Table created = [" + (factory != null) + "]");
        return factory;
    }

}
