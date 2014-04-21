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

import static com.raytheon.uf.edex.plugin.bufrmos.decoder.BUFRMOSStaticData.MODEL_AVN;
import static com.raytheon.uf.edex.plugin.bufrmos.decoder.BUFRMOSStaticData.MODEL_ETA;
import static com.raytheon.uf.edex.plugin.bufrmos.decoder.BUFRMOSStaticData.MODEL_GFS;
import static com.raytheon.uf.edex.plugin.bufrmos.decoder.BUFRMOSStaticData.MODEL_HPC;
import static com.raytheon.uf.edex.plugin.bufrmos.decoder.BUFRMOSStaticData.MODEL_LAMP;
import static com.raytheon.uf.edex.plugin.bufrmos.decoder.BUFRMOSStaticData.MODEL_MRF;
import static com.raytheon.uf.edex.plugin.bufrmos.decoder.BUFRMOSStaticData.MODEL_NGM;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.DescriptorFactory;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;


/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
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
        factories.put(MODEL_AVN,
                createFactory("AVNBufrTableB", "AVNBufrTableD"));
        factories.put(MODEL_ETA,
                createFactory("ETABufrTableB", "ETABufrTableD"));
        factories.put(MODEL_GFS,
                createFactory("GFSBufrTableB", "GFSBufrTableD"));
        factories.put(MODEL_HPC,
                createFactory("HPCBufrTableB", "HPCBufrTableD"));
        factories.put(MODEL_LAMP, createFactory("LAMPBufrTableB",
                "LAMPBufrTableD"));
        factories.put(MODEL_MRF,
                createFactory("MRFBufrTableB", "MRFBufrTableD"));
        factories.put(MODEL_NGM,
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
