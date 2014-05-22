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
package com.raytheon.uf.edex.plugin.hpe.util;

/**
 * HPE Enumerations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2014    3026    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HpeEnums {
    /**
     * HPE Data Source
     * 
     * <pre>
     * S for single pol 
     * D for dual pol
     * </pre>
     */
    public enum HpeDataSource {
        S, D
    }

    /**
     * HPE Bias Source.
     */
    public enum HpeBiasSource {
        RFC_MEAN_BIAS("RCF MEAN BIAS"), SITE_MEAN_BIAS("SITE MEAN BIAS"), SITE_LOCAL_BIAS(
                "SITE LOCAL BIAS"), NO_BIAS("NO BIAS");

        private final String biasSource;

        HpeBiasSource(String biasSource) {
            this.biasSource = biasSource;
        }

        public String getBiasSource() {
            return biasSource;
        }

        public static HpeBiasSource fromString(String source) {
            if (source != null) {
                for (HpeBiasSource h : HpeBiasSource.values()) {
                    if (source.equalsIgnoreCase(h.getBiasSource())) {
                        return h;
                    }
                }
            }

            throw new IllegalArgumentException(
                    "No souce found for HpeBiasSource value: " + source);
        }
    }
}
