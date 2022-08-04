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
package com.raytheon.uf.edex.plugin.ffmp;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPConfigurationException;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceIngestConfigXML;

/**
 * Validates various aspects of the FFMP configurations, specifically the
 * FFMPRunConfig and FFMPSourceConfig.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 10, 2018  6695     njensen   Initial creation
 * Jul 18, 2018  6695     randerso  Fix validator to not modify original config
 *
 * </pre>
 *
 * @author njensen
 */

public class FFMPConfigValidator {

    private static final String DHR = "DHR";

    private static final String DPR = "DPR";

    private FFMPConfigValidator() {
        // don't allow instantiation
    }

    /**
     * Validates the configuration for FFMP. Throws an exception if the
     * configuration is determined to be bad. In general this should halt the
     * JVM from starting until someone fixes the configuration.
     *
     * @param runConfigMgr
     * @param sourceConfigMgr
     * @throws FFMPConfigurationException
     */
    public static void validateConfig(FFMPRunConfigurationManager runConfigMgr,
            FFMPSourceConfigurationManager sourceConfigMgr)
            throws FFMPConfigurationException {
        FFMPRunXML runner = runConfigMgr.getFFMPRunner();
        validateProducts(runner);
        validateSources(runner);
        validateDhrDpr(runner);
        validateProductToProduct(runner, sourceConfigMgr.getProducts());
        validateDomains(runner);
    }

    /**
     * Validates that each source has dataKeys. Throws an exception if the
     * configuration is invalid.
     *
     * @param runner
     * @throws FFMPConfigurationException
     */
    private static void validateSources(FFMPRunXML runner)
            throws FFMPConfigurationException {
        for (SourceIngestConfigXML source : runner.getSourceIngests()) {
            if (source.getDataKey() == null || source.getDataKey().isEmpty()) {
                StringBuilder sb = new StringBuilder();
                sb.append(source.getSourceName() + " sourceIngestConfig in ")
                        .append(FFMPRunConfigurationManager.CONFIG_FILE_NAME)
                        .append(" must have at least one dataKey.");
                throw new FFMPConfigurationException(sb.toString());
            }
        }
    }

    /**
     * Validates that the DHR and DPR sourceIngestConfig tags in FFMPRunConfig
     * are valid and have corresponding product tags in the FFMPRunConfig.
     * Throws an exception if the configuration is invalid.
     *
     * @param runner
     * @throws FFMPConfigurationException
     */
    private static void validateDhrDpr(FFMPRunXML runner)
            throws FFMPConfigurationException {
        List<String> dhrKeys = null;
        List<String> dprKeys = null;

        /*
         * Null lists will indicate the entire sourceIngestConfig for DHR or DPR
         * has been removed.
         */
        SourceIngestConfigXML dhrSrc = runner.getSourceIngest(DHR);
        if (dhrSrc != null) {
            if (dhrSrc.getDataKey() != null) {
                // make a copy of the list so we don't modify the original
                dhrKeys = new ArrayList<>(dhrSrc.getDataKey());
            }
        }
        SourceIngestConfigXML dprSrc = runner.getSourceIngest(DPR);
        if (dprSrc != null) {
            if (dprSrc.getDataKey() != null) {
                // make a copy of the list so we don't modify the original
                dprKeys = new ArrayList<>(dprSrc.getDataKey());
            }
        }

        boolean dhrIsBad = false;
        boolean dprIsBad = false;

        /*
         * Verify there are not DHR and DPR sourceIngestConfigs that have zero
         * dataKeys.
         */
        if (dhrKeys != null && dhrKeys.isEmpty()) {
            dhrIsBad = true;
        } else if (dprKeys != null && dprKeys.isEmpty()) {
            dprIsBad = true;
        }
        if (dhrIsBad || dprIsBad) {
            StringBuilder sb = new StringBuilder(
                    FFMPRunConfigurationManager.CONFIG_FILE_NAME);
            sb.append(" has sourceIngestConfig ");
            if (dhrIsBad) {
                sb.append(DHR);
            } else if (dprIsBad) {
                sb.append(DPR);
            }
            sb.append(" without any dataKeys (radars).");
            throw new FFMPConfigurationException(sb.toString());
        }

        /*
         * Remove dataKeys that have a matching product key, indicating that
         * menus will be created for those dataKeys. Any keys leftover will be
         * missing menu entries due to the missing product keys.
         */
        for (ProductRunXML product : runner.getProducts()) {
            if (dhrKeys != null && DHR.equals(product.getProductName())) {
                dhrKeys.remove(product.getProductKey());
            }
            if (dprKeys != null && DPR.equals(product.getProductName())) {
                dprKeys.remove(product.getProductKey());
            }
        }

        /*
         * Non-empty indicates that a sourceIngestConfig had a dataKey that has
         * no corresponding product entry.
         */
        if (dhrKeys != null && !dhrKeys.isEmpty()) {
            dhrIsBad = true;
        } else if (dprKeys != null && !dprKeys.isEmpty()) {
            dprIsBad = true;
        }

        if (dhrIsBad || dprIsBad) {
            StringBuilder sb = new StringBuilder();
            sb.append("sourceIngestConfig in ")
                    .append(FFMPRunConfigurationManager.CONFIG_FILE_NAME)
                    .append(" is configured to process ");
            if (dhrIsBad) {
                sb.append(DHR);
            } else if (dprIsBad) {
                sb.append(DPR);
            }
            sb.append(" data for site(s) ");
            if (dhrIsBad) {
                sb.append(runner.getSourceIngest(DHR).getDataKey());
            } else if (dprIsBad) {
                sb.append(runner.getSourceIngest(DPR).getDataKey());
            }
            sb.append(" but there is no corresponding ");
            if (dhrIsBad) {
                sb.append(DHR);
            } else if (dprIsBad) {
                sb.append(DPR);
            }
            sb.append(" product tag for ");
            if (dhrIsBad) {
                sb.append(dhrKeys);
            } else if (dprIsBad) {
                sb.append(dprKeys);
            }
            sb.append(".");
            throw new FFMPConfigurationException(sb.toString());
        }

    }

    /**
     * Validates that the product tags in the FFMPRunConfig have the required
     * attributes. Throws an exception if the configuration is invalid.
     *
     * @param runner
     * @throws FFMPConfigurationException
     */
    private static void validateProducts(FFMPRunXML runner)
            throws FFMPConfigurationException {
        for (ProductRunXML product : runner.getProducts()) {
            if (product.getProductName() == null
                    || product.getProductKey() == null) {
                throw new FFMPConfigurationException("Each product in "
                        + FFMPRunConfigurationManager.CONFIG_FILE_NAME
                        + " must have a name and key.");
            }
        }
    }

    /**
     * Validates that the product tags in the FFMPRunConfig have a corresponding
     * product tag in the FFMPSourceConfig. Throws an exception if the
     * configuration is invalid.
     *
     * @param runner
     * @param sourceProducts
     * @throws FFMPConfigurationException
     */
    private static void validateProductToProduct(FFMPRunXML runner,
            List<ProductXML> sourceProducts) throws FFMPConfigurationException {
        List<ProductRunXML> runProducts = runner.getProducts();

        /*
         * Verify that for each product in the run config, there is a
         * corresponding entry in the source config. Otherwise the menus will
         * not be correct.
         */
        for (ProductRunXML rp : runProducts) {
            String runName = rp.getProductName();
            boolean found = false;
            for (ProductXML sp : sourceProducts) {
                String sourceName = sp.getPrimarySource();
                if (runName.equals(sourceName)) {
                    found = true;
                    break;
                }
            }

            if (!found) {
                StringBuilder sb = new StringBuilder();
                sb.append(FFMPRunConfigurationManager.CONFIG_FILE_NAME)
                        .append(" has product name ").append(runName)
                        .append(" but there is no corresponding ")
                        .append(runName).append(" product primarySource in ")
                        .append(FFMPSourceConfigurationManager.CONFIG_FILE_NAME)
                        .append(".");
                throw new FFMPConfigurationException(sb.toString());
            }
        }
    }

    /**
     * Validates that the domain tags in the FFMPRunConfig have one primary
     * domain. Throws an exception if the configuration is invalid.
     *
     * @param runner
     * @throws FFMPConfigurationException
     */
    private static void validateDomains(FFMPRunXML runner)
            throws FFMPConfigurationException {
        List<DomainXML> domains = runner.getDomains();
        if (domains == null || domains.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append(FFMPRunConfigurationManager.CONFIG_FILE_NAME)
                    .append(" must have at least one domain.");
            throw new FFMPConfigurationException(sb.toString());
        }

        int primaryCount = 0;
        for (DomainXML dx : domains) {
            if (dx.isPrimary()) {
                primaryCount++;
            }
        }

        if (primaryCount != 1) {
            StringBuilder sb = new StringBuilder();
            sb.append(FFMPRunConfigurationManager.CONFIG_FILE_NAME)
                    .append(" must have exactly 1 primary domain.");
            throw new FFMPConfigurationException(sb.toString());
        }
    }

}
