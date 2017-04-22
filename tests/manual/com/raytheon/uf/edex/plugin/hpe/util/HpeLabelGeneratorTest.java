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
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.hpe.util.HpeEnums.HpeBiasSource;

/**
 * Test class for HpeLabelGenerator.java.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2014    3026    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HpeLabelGeneratorTest {

    private final HpeDataAccessor dataAccessor = mock(HpeDataAccessor.class);

    private final Calendar cal = TimeUtil.newGmtCalendar();

    private final String SINGLE_POL_TABLE = "RWBiasDyn";

    private final String DUAL_POL_TABLE = "DAABiasDyn";

    private final String productName = "ERMOSAICM60201403051225z";

    private final SortedMap<String, List<BiasDynRecord>> meanDataMap = new TreeMap<String, List<BiasDynRecord>>();

    private final SortedMap<String, List<BiasDynRecord>> localDataMap = new TreeMap<String, List<BiasDynRecord>>();

    @Before
    public void setup() throws Exception {
        setCalendar();
        createBiasDynRecords();
        List<String> radarList = getRadarList();
        System.setProperty("AW_SITE_IDENTIFIER", "OAX");
        when(dataAccessor.getNPairBiasSelect()).thenReturn(10);
        when(dataAccessor.getHpeRadars(cal.getTime(), SINGLE_POL_TABLE))
                .thenReturn(radarList);
        when(dataAccessor.getHpeRadars(cal.getTime(), DUAL_POL_TABLE))
                .thenReturn(radarList);
    }

    // Single pol tests
    @Test
    public void testLabelCreationForSinglePolSiteMeanBias() throws Exception {
        // Set up run specific data
        HpeRadarResult siteMeanBiasSingle = new HpeRadarResult();
        siteMeanBiasSingle.setHpeProductName(productName);
        siteMeanBiasSingle.setProductTime(cal.getTime());
        siteMeanBiasSingle.setBiasSource(HpeBiasSource.SITE_MEAN_BIAS
                .getBiasSource());
        siteMeanBiasSingle.setNumRadarAvailable(8);
        siteMeanBiasSingle.setRadarDataSource("S");

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(siteMeanBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(meanDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);

        System.out.println("");
        System.out.println("Single Pol Site Mean Bias");
        System.out.println(label);
    }

    @Test
    public void testLabelCreationForSinglePolRfcMeanBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcMeanBiasSingle = new HpeRadarResult();
        rfcMeanBiasSingle.setHpeProductName(productName);
        rfcMeanBiasSingle.setProductTime(cal.getTime());
        rfcMeanBiasSingle.setBiasSource(HpeBiasSource.RFC_MEAN_BIAS
                .getBiasSource());
        rfcMeanBiasSingle.setNumRadarAvailable(8);
        rfcMeanBiasSingle.setRadarDataSource("S");

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcMeanBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(meanDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label;
        label = labelGen.getHpeLabel(cal.getTime(), productName);

        System.out.println("");
        System.out.println("Single Pol RFC Mean Bias");
        System.out.println(label);
    }

    @Test
    public void testLabelCreationForSinglePolLocalBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcLocalBiasSingle = new HpeRadarResult();
        rfcLocalBiasSingle.setHpeProductName(productName);
        rfcLocalBiasSingle.setProductTime(cal.getTime());
        rfcLocalBiasSingle.setBiasSource(HpeBiasSource.SITE_LOCAL_BIAS
                .getBiasSource());
        rfcLocalBiasSingle.setNumRadarAvailable(8);
        rfcLocalBiasSingle.setRadarDataSource("S");

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcLocalBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(localDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);

        System.out.println("");
        System.out.println("Single Pol Local Site Bias");
        System.out.println(label);
    }

    @Test
    public void testLabelCreationForSinglePolNoBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcLocalBiasSingle = new HpeRadarResult();
        rfcLocalBiasSingle.setHpeProductName(productName);
        rfcLocalBiasSingle.setProductTime(cal.getTime());
        rfcLocalBiasSingle.setBiasSource(HpeBiasSource.NO_BIAS.getBiasSource());
        rfcLocalBiasSingle.setNumRadarAvailable(8);
        rfcLocalBiasSingle.setRadarDataSource("S");

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcLocalBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(localDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);

        System.out.println("");
        System.out.println("Single Pol No Bias");
        System.out.println(label);
    }

    // Dual pol tests
    @Test
    public void testLabelCreationForDualPolSiteMeanBias() throws Exception {
        // Set up run specific data
        HpeRadarResult siteMeanBiasSingle = new HpeRadarResult();
        siteMeanBiasSingle.setHpeProductName(productName);
        siteMeanBiasSingle.setProductTime(cal.getTime());
        siteMeanBiasSingle.setBiasSource(HpeBiasSource.SITE_MEAN_BIAS
                .getBiasSource());
        siteMeanBiasSingle.setNumRadarAvailable(8);
        siteMeanBiasSingle.setRadarDataSource("D");

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(siteMeanBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(meanDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);

        System.out.println("");
        System.out.println("Dual Pol Site Mean Bias");
        System.out.println(label);
    }

    @Test
    public void testLabelCreationForDualPolRfcMeanBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcMeanBiasSingle = new HpeRadarResult();
        rfcMeanBiasSingle.setHpeProductName(productName);
        rfcMeanBiasSingle.setProductTime(cal.getTime());
        rfcMeanBiasSingle.setBiasSource(HpeBiasSource.RFC_MEAN_BIAS
                .getBiasSource());
        rfcMeanBiasSingle.setNumRadarAvailable(8);
        rfcMeanBiasSingle.setRadarDataSource("D");

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcMeanBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(meanDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);

        System.out.println("");
        System.out.println("Dual Pol RFC Mean Bias");
        System.out.println(label);
    }

    @Test
    public void testLabelCreationForDualPolLocalBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcLocalBiasSingle = new HpeRadarResult();
        rfcLocalBiasSingle.setHpeProductName(productName);
        rfcLocalBiasSingle.setProductTime(cal.getTime());
        rfcLocalBiasSingle.setBiasSource(HpeBiasSource.SITE_LOCAL_BIAS
                .getBiasSource());
        rfcLocalBiasSingle.setNumRadarAvailable(8);
        rfcLocalBiasSingle.setRadarDataSource("D");

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcLocalBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(localDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);

        System.out.println("");
        System.out.println("Dual Pol Local Site Bias");
        System.out.println(label);
    }

    @Test
    public void testLabelCreationForDualPolNoBias() throws Exception {
        // Set up run specific data
        HpeRadarResult rfcLocalBiasSingle = new HpeRadarResult();
        rfcLocalBiasSingle.setHpeProductName(productName);
        rfcLocalBiasSingle.setProductTime(cal.getTime());
        rfcLocalBiasSingle.setBiasSource(HpeBiasSource.NO_BIAS.getBiasSource());
        rfcLocalBiasSingle.setNumRadarAvailable(8);
        rfcLocalBiasSingle.setRadarDataSource("D");

        when(dataAccessor.getHpeRadarResult(cal.getTime(), productName))
                .thenReturn(rfcLocalBiasSingle);

        when(dataAccessor.getBiasDynRecords(cal.getTime(), productName))
                .thenReturn(localDataMap);

        // Run the code
        HpeLabelGenerator labelGen = new HpeLabelGenerator(dataAccessor);
        String label = labelGen.getHpeLabel(cal.getTime(), productName);

        System.out.println("");
        System.out.println("Dual Pol No Bias");
        System.out.println(label);
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
        List<String> radarList = new ArrayList<String>();
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
            localDataMap.put(radar, new ArrayList<BiasDynRecord>());
        }
    }
}
