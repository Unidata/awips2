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

import java.text.DecimalFormat;
import java.util.Date;
import java.util.List;
import java.util.Map.Entry;
import java.util.SortedMap;

import com.raytheon.uf.common.plugin.hpe.data.BiasDynRecord;
import com.raytheon.uf.common.plugin.hpe.data.HpeEnums.HpeBiasSource;
import com.raytheon.uf.common.plugin.hpe.data.HpeEnums.HpeDataSource;
import com.raytheon.uf.common.plugin.hpe.data.HpeRadarResult;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * HPE label generator. Creates labels based on the HPE bias source data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2014   3026     mpduff      Initial creation
 * Dec 16, 2014   3026     mpduff      Add default value if numPairs < npairBiasSelect
 * Oct 12, 2016   5631     bkowal      Cleanup. Relocated HPE Radar Result enums to a
 *                                     true common plugin.
 * May 23, 2018   6943     mduff       Changed to read Biases unavailable if no biases are available.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class HpeLabelGenerator {
    /**
     * 2 decimal place formatter
     */
    private final ThreadLocal<DecimalFormat> twoDeciamalFormatter = new ThreadLocal<DecimalFormat>() {

        @Override
        protected DecimalFormat initialValue() {
            DecimalFormat format = new DecimalFormat("0.00");
            return format;
        }
    };

    private static String SLASH = "/";

    private static String SPACE = " ";

    private static String Y = "Y";

    private static String N = "N";

    private final HpeDataAccessor dataAccessor;

    /**
     * Constructor.
     * 
     * @param dataAccessor
     */
    public HpeLabelGenerator(HpeDataAccessor dataAccessor) {
        this.dataAccessor = dataAccessor;
    }

    /**
     * Get the HPE label string.
     * 
     * @param recDate
     *            The date of the record
     * @param productName
     *            The name of the HPE product
     * 
     * @return the label
     * @throws Exception
     */
    public String getHpeLabel(Date recDate, String productName)
            throws Exception {
        HpeRadarResult hpeResult = dataAccessor.getHpeRadarResult(recDate,
                productName);
        if (hpeResult == null || hpeResult.isEmpty()) {
            return "No HPE bias source data";
        }

        HpeDataSource source = hpeResult.getRadarDataSource();
        HpeBiasSource biasSource = hpeResult.getBiasSource();

        // Site->BiasDynRecord list
        SortedMap<String, List<BiasDynRecord>> dataMap = dataAccessor
                .getBiasDynRecords(recDate, productName);

        StringBuilder label = new StringBuilder("Dual-Pol Source: ");

        // Label if single or dual
        if (source == HpeDataSource.S) {
            label.append(N);
        } else if (source == HpeDataSource.D) {
            label.append(Y);
        }

        label.append(StringUtil.NEWLINE);

        if (dataMap.isEmpty()) {
            label.append("Biases unavailable");
            return label.toString();
        }
        // Add bias value
        if (biasSource == HpeBiasSource.SITE_MEAN_BIAS) {
            label.append("Bias Source: ");
            label.append(EDEXUtil.getEdexSite());
        } else if (biasSource == HpeBiasSource.RFC_MEAN_BIAS) {
            label.append("Bias Source: RFC");
        } else if (biasSource == HpeBiasSource.SITE_LOCAL_BIAS) {
            label.append(EDEXUtil.getEdexSite()).append(" Local Bias ");
            return label.toString().trim();
        } else {
            label.append("Biases unavailable");
            return label.toString().trim();
        }

        label.append(StringUtil.NEWLINE).append(SPACE);

        // Process each radar to get it's part of the label
        for (Entry<String, List<BiasDynRecord>> entry : dataMap.entrySet()) {
            String labelEntry = getLabelEntry(entry.getKey(), entry.getValue());
            label.append(labelEntry).append(SPACE);
        }

        return label.toString().trim();
    }

    /**
     * Get the label entry for the radar.
     * 
     * @param radar
     *            radar id
     * @param records
     *            BiasDynRecord objects
     * @return the label for this radar
     * @throws Exception
     */
    private String getLabelEntry(String radar, List<BiasDynRecord> records)
            throws Exception {
        StringBuilder sb = new StringBuilder("k");
        sb.append(radar.toLowerCase()).append(SPACE);
        final int npairBiasSelect = dataAccessor.getNPairBiasSelect();

        // process the records
        if (!records.isEmpty()) {
            for (BiasDynRecord rec : records) {
                if (rec.getNumPairs() > npairBiasSelect) {
                    String bias = twoDeciamalFormatter.get()
                            .format(rec.getBias());
                    sb.append(bias).append(SLASH)
                            .append((int) rec.getNumPairs());
                    sb.append(StringUtil.NEWLINE);
                    return sb.toString();
                } else {
                    sb.append("1.00").append(SLASH)
                            .append((int) rec.getNumPairs());
                    sb.append(StringUtil.NEWLINE);
                    return sb.toString();
                }
            }
        }

        return sb.toString();
    }
}