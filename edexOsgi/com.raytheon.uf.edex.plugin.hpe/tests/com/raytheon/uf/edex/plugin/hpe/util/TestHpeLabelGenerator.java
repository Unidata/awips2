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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.plugin.hpe.data.BiasDynRecord;
import com.raytheon.uf.common.plugin.hpe.data.HpeEnums.HpeBiasSource;
import com.raytheon.uf.common.plugin.hpe.data.HpeEnums.HpeDataSource;
import com.raytheon.uf.common.plugin.hpe.data.HpeRadarResult;
import com.raytheon.uf.common.plugin.hpe.data.HpeRadarResultId;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Test class for HpeLabelGenerator.java.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2018   6943     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class TestHpeLabelGenerator {
    private final HpeDataAccessor dataAccessor = mock(HpeDataAccessor.class);

    private final Calendar cal = TimeUtil.newGmtCalendar();

    /** Copied name from an existing product. */
    private static final String productName = "ERMOSAICM60201403051225z";

    private final SortedMap<String, List<BiasDynRecord>> meanDataMap = new TreeMap<>();

    private final SortedMap<String, List<BiasDynRecord>> localDataMap = new TreeMap<>();

    @Before
    public void setup() throws Exception {
        setCalendar();
        createBiasDynRecords();
        System.setProperty("aw.site.identifier", "OAX");
        when(dataAccessor.getNPairBiasSelect()).thenReturn(10);
    }

    // Single pol tests
    @Test
    public void testLabelCreationForSinglePolSiteMeanBias() throws Exception {
        // Set up run specific data
        HpeRadarResult siteMeanBiasSingle = new HpeRadarResult();
        siteMeanBiasSingle.setBiasSource(HpeBiasSource.SITE_MEAN_BIAS);
        siteMeanBiasSingle.setNumRadarAvailable((short) 8);
        siteMeanBiasSingle.setRadarDataSource(HpeDataSource.S);
        HpeRadarResultId id = new HpeRadarResultId();
        id.setHpeProductName(productName);
        id.setProductTime(cal.getTime());
        siteMeanBiasSingle.setId(id);

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(siteMeanBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(meanDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);

        String expected = "Dual-Pol Source: N\nBias Source: OAX";
        assertTrue(label.startsWith(expected));
    }

    @Test
    public void testLabelCreationForSinglePolRfcMeanBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcMeanBiasSingle = new HpeRadarResult();
        HpeRadarResultId id = new HpeRadarResultId();
        id.setHpeProductName(productName);
        id.setProductTime(cal.getTime());
        rfcMeanBiasSingle.setId(id);
        rfcMeanBiasSingle.setBiasSource(HpeBiasSource.RFC_MEAN_BIAS);
        rfcMeanBiasSingle.setNumRadarAvailable((short) 8);
        rfcMeanBiasSingle.setRadarDataSource(HpeDataSource.S);

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcMeanBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(meanDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);
        String expected = "Dual-Pol Source: N\nBias Source: RFC";
        assertTrue(label.startsWith(expected));
    }

    @Test
    public void testLabelCreationForSinglePolLocalBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcLocalBiasSingle = new HpeRadarResult();
        HpeRadarResultId id = new HpeRadarResultId();
        id.setHpeProductName(productName);
        id.setProductTime(cal.getTime());
        rfcLocalBiasSingle.setId(id);
        rfcLocalBiasSingle.setBiasSource(HpeBiasSource.SITE_LOCAL_BIAS);
        rfcLocalBiasSingle.setNumRadarAvailable((short) 8);
        rfcLocalBiasSingle.setRadarDataSource(HpeDataSource.S);

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcLocalBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(localDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);
        String expected = "Dual-Pol Source: N\nOAX Local Bias";
        assertEquals("Labels do not match.", label, expected);
    }

    @Test
    public void testLabelCreationForSinglePolNoBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcLocalBiasSingle = new HpeRadarResult();
        HpeRadarResultId id = new HpeRadarResultId();
        id.setHpeProductName(productName);
        id.setProductTime(cal.getTime());
        rfcLocalBiasSingle.setId(id);
        rfcLocalBiasSingle.setBiasSource(HpeBiasSource.NO_BIAS);
        rfcLocalBiasSingle.setNumRadarAvailable((short) 8);
        rfcLocalBiasSingle.setRadarDataSource(HpeDataSource.S);

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcLocalBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(localDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);
        String expected = "Dual-Pol Source: N\nBiases unavailable";
        assertEquals(label, expected);
    }

    // Dual pol tests
    @Test
    public void testLabelCreationForDualPolSiteMeanBias() throws Exception {
        // Set up run specific data
        HpeRadarResult siteMeanBiasSingle = new HpeRadarResult();
        HpeRadarResultId id = new HpeRadarResultId();
        id.setHpeProductName(productName);
        id.setProductTime(cal.getTime());
        siteMeanBiasSingle.setId(id);
        siteMeanBiasSingle.setBiasSource(HpeBiasSource.SITE_MEAN_BIAS);
        siteMeanBiasSingle.setNumRadarAvailable((short) 8);
        siteMeanBiasSingle.setRadarDataSource(HpeDataSource.D);

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(siteMeanBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(meanDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);
        String expected = "Dual-Pol Source: Y\nBias Source: OAX";
        assertTrue(label.startsWith(expected));
    }

    @Test
    public void testLabelCreationForDualPolRfcMeanBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcMeanBiasSingle = new HpeRadarResult();
        HpeRadarResultId id = new HpeRadarResultId();
        id.setHpeProductName(productName);
        id.setProductTime(cal.getTime());
        rfcMeanBiasSingle.setId(id);
        rfcMeanBiasSingle.setBiasSource(HpeBiasSource.RFC_MEAN_BIAS);
        rfcMeanBiasSingle.setNumRadarAvailable((short) 8);
        rfcMeanBiasSingle.setRadarDataSource(HpeDataSource.D);

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcMeanBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(meanDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);
        String expected = "Dual-Pol Source: Y\nBias Source: RFC";
        assertTrue(label.startsWith(expected));
    }

    @Test
    public void testLabelCreationForDualPolLocalBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcLocalBiasSingle = new HpeRadarResult();
        HpeRadarResultId id = new HpeRadarResultId();
        id.setHpeProductName(productName);
        id.setProductTime(cal.getTime());
        rfcLocalBiasSingle.setId(id);
        rfcLocalBiasSingle.setBiasSource(HpeBiasSource.SITE_LOCAL_BIAS);
        rfcLocalBiasSingle.setNumRadarAvailable((short) 8);
        rfcLocalBiasSingle.setRadarDataSource(HpeDataSource.D);

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcLocalBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(localDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);
        String expected = "Dual-Pol Source: Y\nOAX Local Bias";

        assertEquals(label, expected);
    }

    @Test
    public void testLabelCreationForDualPolNoBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcLocalBiasSingle = new HpeRadarResult();
        HpeRadarResultId id = new HpeRadarResultId();
        id.setHpeProductName(productName);
        id.setProductTime(cal.getTime());
        rfcLocalBiasSingle.setId(id);
        rfcLocalBiasSingle.setBiasSource(HpeBiasSource.NO_BIAS);
        rfcLocalBiasSingle.setNumRadarAvailable((short) 8);
        rfcLocalBiasSingle.setRadarDataSource(HpeDataSource.D);

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcLocalBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(localDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);
        String expected = "Dual-Pol Source: Y\nBiases unavailable";
        assertEquals(label, expected);
    }

    @Test
    public void testLabelCreationForDualPolRFCNoBiasDataAvailable()
            throws Exception {
        // Set up run specific data
        HpeRadarResult rfcLocalBiasSingle = new HpeRadarResult();
        HpeRadarResultId id = new HpeRadarResultId();
        id.setHpeProductName(productName);
        id.setProductTime(cal.getTime());
        rfcLocalBiasSingle.setId(id);
        rfcLocalBiasSingle.setBiasSource(HpeBiasSource.NO_BIAS);
        rfcLocalBiasSingle.setNumRadarAvailable((short) 8);
        rfcLocalBiasSingle.setRadarDataSource(HpeDataSource.D);

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcLocalBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(new TreeMap<String, List<BiasDynRecord>>());

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);
        String expected = "Dual-Pol Source: Y\nBiases unavailable";
        assertEquals(label, expected);
    }

    private void setCalendar() {
        cal.set(Calendar.YEAR, 2014);
        cal.set(Calendar.MONTH, 3);
        cal.set(Calendar.DAY_OF_MONTH, 5);
        cal.set(Calendar.HOUR, 12);
        cal.set(Calendar.MINUTE, 25);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
    }

    private List<String> getRadarList() {
        List<String> radarList = new ArrayList<>();
        radarList.add("ABR");
        radarList.add("DMX");
        radarList.add("EAX");
        radarList.add("FSD");
        radarList.add("LNX");
        radarList.add("OAX");
        radarList.add("TWX");
        radarList.add("UEX");

        return radarList;
    }

    private void createBiasDynRecords() {
        List<String> radarList = getRadarList();

        float[] memSpanIdx = new float[] { 0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f,
                6.0f, 7.0f, 8.0f, 9.0f };

        double[] numPairs = new double[] { 0, 1.8534577634678e-36,
                3.9062266222742e-18, 5.102510849233e-12, 4.0175370594682e-07,
                0.0018974485809437, 7.045055608027, 40.795505532567,
                74.1231615108, 102.99244442947 };

        float[] sumGages = new float[] { 0.381f, 0.384065f, 0.387877f,
                0.389836f, 0.391715f, 0.393323f, 0.539044f, 1.08328f, 1.15115f,
                1.16888f };

        float[] sumRadars = new float[] { 1.21662f, 1.09099f, 1.03442f,
                1.01281f, 0.99475f, 0.980855f, 0.949928f, 0.990251f, 0.977503f,
                0.9625f };

        float[] bias = new float[] { 0.313163f, 0.352033f, 0.37497f, 0.384905f,
                0.393782f, 0.401f, 0.567458f, 1.09394f, 1.17764f, 1.21442f };

        // Create mean bias data
        for (String radar : radarList) {
            meanDataMap.put(radar, new ArrayList<BiasDynRecord>());
        }

        for (String radar : radarList) {
            for (int i = 0; i < memSpanIdx.length; i++) {
                BiasDynRecord r = new BiasDynRecord();
                r.setBias(bias[i]);
                r.setMemspanIndex(memSpanIdx[i]);
                r.setNumPairs(numPairs[i]);
                r.setObsTime(cal.getTime());
                r.setOfficeId("OAX");
                r.setRadarId(radar);
                r.setSumGages(sumGages[i]);
                r.setSumRadars(sumRadars[i]);
                meanDataMap.get(radar).add(r);
            }
        }

        // Create local bias data - local/no bias do not have bias data
        for (String radar : radarList) {
            localDataMap.put(radar, new ArrayList<>());
        }
    }
}
