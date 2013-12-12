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
package com.raytheon.edex.plugin.bufrua.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.edex.plugin.bufrua.util.SigWindHeightConversionList.SigWindHeightConversion;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.localization.AutoUpdatingLocalizationFile;
import com.raytheon.uf.common.localization.AutoUpdatingLocalizationFile.AutoUpdatingFileChangedListener;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Some UAObs are incorrectly encoded so the Significant Wind heights are
 * multiples of 300 meters when in fact the observations were really taken at
 * multiples of 1000 feet. A conversion list is serialised and can be used to
 * determine which sites require unit conversion for the heights to match the
 * actual observations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 06, 2013  2612     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SigWindHeightConversionManager implements
        AutoUpdatingFileChangedListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SigWindHeightConversionList.class);

    private static final String FILE_NAME = "bufrua/sigWindHeightConversion.xml";

    private static final SigWindHeightConversionManager instance = new SigWindHeightConversionManager();

    /* Must keep reference to the file or listener doesn't work */
    @SuppressWarnings("unused")
    private final AutoUpdatingLocalizationFile file;

    private Map<String, Double> factors;

    private SigWindHeightConversionManager() {
        AutoUpdatingLocalizationFile file = null;
        try {
            file = new AutoUpdatingLocalizationFile(FILE_NAME,
                    LocalizationType.EDEX_STATIC);
            file.addListener(this);
            loadFactors(file);
        } catch (Throwable e) {
            /*
             * Do not allow exceptions to propogate, this would break class
             * loading, instead no conversion will ever occur.
             */
            statusHandler
                    .handle(Priority.PROBLEM,
                            "An error has occured loading the SigWind Height Conversion factors.",
                            e);
            factors = Collections.emptyMap();
        }
        this.file = file;
    }

    @Override
    public void fileChanged(AutoUpdatingLocalizationFile file) {
        try {
            loadFactors(file);
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "An error has occured reloading the SigWind Height Conversion factors.",
                            e);
        }
    }

    private void loadFactors(AutoUpdatingLocalizationFile file)
            throws SerializationException, JAXBException {
        SigWindHeightConversionList list = file.loadObject(
                new JAXBManager(SigWindHeightConversionList.class),
                SigWindHeightConversionList.class);
        Map<String, Double> result = new HashMap<String, Double>(list
                .getEntries().size(), 1.0f);
        for (SigWindHeightConversion entry : list.getEntries()) {
            result.put(entry.getStationId(), entry.getFactor());
        }
        factors = result;
    }

    public double getFactor(String stationId) {
        Double factor = factors.get(stationId);
        if (factor != null) {
            return factor.doubleValue();
        } else {
            return 1.0;
        }
    }

    public static double convertHeight(UAObs obs, double height) {
        return height * instance.getFactor(obs.getStationId());
    }

}