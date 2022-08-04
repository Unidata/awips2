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
package com.raytheon.edex.plugin.shef.database;

import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.data.ShefRecord.ShefType;
import com.raytheon.edex.plugin.shef.database.PostShef.Location;
import com.raytheon.edex.plugin.shef.util.SHEFDate;
import com.raytheon.edex.plugin.shef.util.ShefParm;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Duration;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Extremum;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Probability;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.TypeSource;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;

/**
 * JUnit test for PostShef.java. Due to the way the code is written this must be
 * run as a manual test. Examine the console output to verify posting of proper
 * data to correct table.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2018   6991     mduff       Initial creation
 * Jul 17, 2018   7003     dgilling    Add tests for Apps_defaults shef_winpast
 *                                     and shef_winfuture settings.
 * Sep 23, 2021   8608     mapeters    Handle PDO.traceId changes
 *
 * </pre>
 *
 * @author mpduff
 */
@RunWith(MockitoJUnitRunner.class)
public class PostShefTest {
    private static final String SHEF_ON = "ON";

    private static final String TRACE_ID = "traceId";

    private static final String INGEST_SQL = "select lid, pe, dur, ts, extremum, ts_rank, ingest, ofs_input, stg2_input from IngestFilter where lid = 'DNS'";

    private static final String ADJUST_PZ_SQL = "select divisor, base, multiplier, adder from adjustfactor where lid = 'DNS' and pe = 'SW' and dur = 0 and ts = 'PZ' and extremum = 'Z'";

    private static final String ADJUST_RZ_SQL = "select divisor, base, multiplier, adder from adjustfactor where lid = 'DNS' and pe = 'SW' and dur = 0 and ts = 'RZ' and extremum = 'Z'";

    private static final String QUALITY_SQL = "select monthdaystart, monthdayend, gross_range_min, gross_range_max, reason_range_min, reason_range_max, roc_max, alert_upper_limit, alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit, alarm_lower_limit, alert_diff_limit, alarm_diff_limit, pe, dur from locdatalimits where lid = 'DNS' and pe = 'SW' and dur = 0";

    private static final String TELEM_QUERY = "select lid, type from telem where lid = 'DNS'";

    private static final String INGEST_PE_QUERY = "select pe from IngestFilter where lid = 'DNS' and ingest = 'T'";

    private static final String LOC_TEST1_INGEST_SQL = "select lid, pe, dur, ts, extremum, ts_rank, ingest, ofs_input, stg2_input from IngestFilter where lid = 'TEST1'";

    private static final String LOC_TEST1_ADJUST_PZ_SQL = "select divisor, base, multiplier, adder from adjustfactor where lid = 'TEST1' and pe = 'SA' and dur = 0 and ts = 'PZ' and extremum = 'Z'";

    private static final String LOC_TEST1_TELEM_QUERY = "select lid, type from telem where lid = 'TEST1'";

    private static final String LOC_TEST1_INGEST_PE_QUERY = "select pe from IngestFilter where lid = 'TEST1' and ingest = 'T'";

    @Mock
    private AppsDefaults appsDefaults;

    @Mock
    private ShefParm shefParm;

    @Mock
    private HydroDataAccessor hydroDataAccessor;

    @Mock
    private PostTables postTables;

    @Mock
    private CoreDao dao;

    @Mock
    private CoreDao locDao;

    @Before
    public void setup() {
        when(appsDefaults.getToken(ShefConstants.SHEF_POST_UNKNOWN,
                ShefConstants.NONE)).thenReturn(ShefConstants.NONE);

        when(appsDefaults.getToken(ShefConstants.SHEF_DUPLICATE))
                .thenReturn("IF_DIFFERENT");

        when(appsDefaults.getToken(ShefConstants.SHEF_DUPLICATE,
                ShefConstants.IF_DIFFERENT)).thenReturn("IF_DIFFERENT");

        when(appsDefaults.getBoolean(ShefConstants.SHEF_ALERTALARM, false))
                .thenReturn(true);

        when(appsDefaults.getBoolean(ShefConstants.LOCMESS, false))
                .thenReturn(true);

        when(appsDefaults.getInt(ShefConstants.SHEF_WINPAST, 10))
                .thenReturn(10);
        when(appsDefaults.getInt(ShefConstants.SHEF_WINFUTURE, 30))
                .thenReturn(60);
        when(appsDefaults.getBoolean(ShefConstants.SHEF_POST_LINK, false))
                .thenReturn(true);

        when(appsDefaults.getToken(ShefConstants.SHEF_POST_LATEST))
                .thenReturn("ON");

        when(appsDefaults.getToken(ShefConstants.SHEF_LOAD_MAXFCST, SHEF_ON))
                .thenReturn("ON");

        when(appsDefaults.getToken(ShefConstants.SHEF_POST_BADDATA, "REJECT"))
                .thenReturn("REJECT");

        when(appsDefaults.getToken(ShefConstants.BASIS_HOURS_FILTER))
                .thenReturn("5");

        when(appsDefaults.getBoolean(ShefConstants.SHEF_LOAD_INGEST, false))
                .thenReturn(true);

        when(appsDefaults.getBoolean(ShefConstants.SHEF_PROCOBS, false))
                .thenReturn(false);

        when(appsDefaults.getBoolean(ShefConstants.SHEF_DATA_LOG, false))
                .thenReturn(true);

        when(appsDefaults.getBoolean(ShefConstants.SHEF_PERFLOG, false))
                .thenReturn(false);

        when(hydroDataAccessor.checkLocation("DNS", TRACE_ID))
                .thenReturn(Location.LOC_LOCATION);
        when(hydroDataAccessor.checkLocation("TEST1", TRACE_ID))
                .thenReturn(Location.LOC_LOCATION);

        when(dao.executeSQLQuery(INGEST_SQL)).thenReturn(getIngestData());
        when(dao.executeSQLQuery(ADJUST_PZ_SQL)).thenReturn(new Object[0]);
        when(dao.executeSQLQuery(ADJUST_RZ_SQL)).thenReturn(new Object[0]);
        when(dao.executeSQLQuery(QUALITY_SQL)).thenReturn(getQualityResults());
        when(dao.executeSQLQuery(TELEM_QUERY)).thenReturn(getTelemResults());
        when(dao.executeSQLQuery(INGEST_PE_QUERY))
                .thenReturn(getIngestPeResults());

        when(dao.executeSQLQuery(LOC_TEST1_INGEST_SQL))
                .thenReturn(getLidTEST1IngestData());
        when(dao.executeSQLQuery(LOC_TEST1_ADJUST_PZ_SQL))
                .thenReturn(new Object[0]);
        when(dao.executeSQLQuery(LOC_TEST1_TELEM_QUERY))
                .thenReturn(getLidTEST1TelemResults());
        when(dao.executeSQLQuery(LOC_TEST1_INGEST_PE_QUERY))
                .thenReturn(getLidTEST1IngestPeResults());
    }

    /*
     * This tests the gross range on both the high and low end for processed
     * data
     */
    @Test
    public void testProcessedShefDataFailGrossRange() {
        Date date = new Date();
        ShefRecord rec = getProcessedShefRecordFailGrossRange();
        PostShef postShef = new PostShef(date, shefParm, appsDefaults,
                hydroDataAccessor, dao, locDao, postTables);
        postShef.post(rec);

        // TODO further refactor PostShef so individual parts can be tested
    }

    /*
     * This tests the reasonable range on both the high and low end for
     * processed data
     */
    @Test
    public void testProcessedShefDataFailReasonableRange() {
        Date date = new Date();
        ShefRecord rec = getProcessedShefRecordFailReasonableRange();
        PostShef postShef = new PostShef(date, shefParm, appsDefaults,
                hydroDataAccessor, dao, locDao, postTables);
        postShef.post(rec);
        // TODO further refactor PostShef so individual parts can be tested
    }

    /*
     * This tests the gross range on both the high and low end for reading data
     */
    @Test
    public void testReadingShefDataFailGrossRange() {
        Date date = new Date();
        ShefRecord rec = getReadingShefRecordFailGrossRange();
        PostShef postShef = new PostShef(date, shefParm, appsDefaults,
                hydroDataAccessor, dao, locDao, postTables);
        postShef.post(rec);
        // TODO further refactor PostShef so individual parts can be tested
    }

    /*
     * This tests the reasonable range on both the high and low end for reading
     * data
     */
    @Test
    public void testReadingShefDataFailReasonableRange() {
        Date date = new Date();
        ShefRecord rec = getReadingShefRecordFailReasonableRange();
        PostShef postShef = new PostShef(date, shefParm, appsDefaults,
                hydroDataAccessor, dao, locDao, postTables);
        postShef.post(rec);
        // TODO further refactor PostShef so individual parts can be tested
    }

    @Test
    public void testProcessedShefDataPassRangeChecks() {
        Date date = new Date();
        ShefRecord rec = getProcessedShefRecordValidRange();
        PostShef postShef = new PostShef(date, shefParm, appsDefaults,
                hydroDataAccessor, dao, locDao, postTables);
        postShef.post(rec);
        // TODO further refactor PostShef so individual parts can be tested
    }

    @Test
    public void testReadingShefDataPassRangeChecks() {
        Date date = new Date();
        ShefRecord rec = getReadingShefRecordValidRange();
        PostShef postShef = new PostShef(date, shefParm, appsDefaults,
                hydroDataAccessor, dao, locDao, postTables);
        postShef.post(rec);
        // TODO further refactor PostShef so individual parts can be tested
    }

    @Test
    public void testProcessedShefDataPastDateChecks() {
        ShefRecord rec = getProcessedShefRecordPastDate();
        PostShef postShef = new PostShef(new Date(), shefParm, appsDefaults,
                hydroDataAccessor, dao, locDao, postTables);
        postShef.post(rec);
        // TODO further refactor PostShef so individual parts can be tested
    }

    @Test
    public void testProcessedShefDataFutureDateChecks() {
        ShefRecord rec = getProcessedShefRecordFutureDate();
        PostShef postShef = new PostShef(new Date(), shefParm, appsDefaults,
                hydroDataAccessor, dao, locDao, postTables);
        postShef.post(rec);
        // TODO further refactor PostShef so individual parts can be tested
    }

    private ShefRecord getProcessedShefRecordFailGrossRange() {
        ShefRecord rec = getShefRecord();
        rec.setDataValues(getProcessedShefDataFailGrossRange());

        return rec;
    }

    private ShefRecord getProcessedShefRecordFailReasonableRange() {
        ShefRecord rec = getShefRecord();
        rec.setDataValues(getProcessedShefDataFailReasonableRange());

        return rec;
    }

    private ShefRecord getReadingShefRecordFailReasonableRange() {
        ShefRecord rec = getShefRecord();
        rec.setDataValues(getReadingShefDataFailReasonableRange());

        return rec;
    }

    private ShefRecord getReadingShefRecordFailGrossRange() {
        ShefRecord rec = getShefRecord();
        rec.setDataValues(getReadingShefDataFailGrossRange());

        return rec;
    }

    private ShefRecord getProcessedShefRecordValidRange() {
        ShefRecord rec = getShefRecord();
        rec.setDataValues(getProcessedShefDataValidRange());

        return rec;
    }

    private ShefRecord getReadingShefRecordValidRange() {
        ShefRecord rec = getShefRecord();
        rec.setDataValues(getReadingShefDataValidRange());

        return rec;
    }

    private ShefRecord getProcessedShefRecordPastDate() {
        ShefRecord rec = getShefRecord();
        rec.setDataValues(getProcessedShefDataPastDate());
        return rec;
    }

    private ShefRecord getProcessedShefRecordFutureDate() {
        ShefRecord rec = getShefRecord();
        rec.setDataValues(getProcessedShefDataFutureDate());
        return rec;
    }

    private List<ShefData> getProcessedShefDataFailGrossRange() {
        List<ShefData> data = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            ShefData sd = new ShefData(shefParm);
            sd.setDataSource("DMX");
            sd.setDuration(Duration.INSTANTENOUS);
            sd.setDurationValue((short) 0);
            sd.setExtremum(Extremum.MAX_DAY);
            sd.setLocationId("DNS");
            sd.setObsTime(new SHEFDate());
            sd.setParameterCodeString("SW", "");
            sd.setPhysicalElement(PhysicalElement.SNOW_WATER_EQUIVALENT);
            sd.setProbability(Probability.NULL);
            sd.setQualifier("Z");
            sd.setTypeSource(TypeSource.PROCESSED_NONSPECIFIC);
            sd.setUnitsCode("E");
            if (i == 0) {
                sd.setValue(-100.0);
            } else {
                sd.setValue(1000.0);
            }
            SHEFDate date = new SHEFDate();
            date.setTimeZone(TimeZone.getTimeZone("GMT"));
            sd.setObsTime(date);
            sd.setStringValue(String.valueOf((sd.getValue())));
            data.add(sd);
        }

        return data;
    }

    private List<ShefData> getProcessedShefDataFailReasonableRange() {
        List<ShefData> data = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            ShefData sd = new ShefData(shefParm);
            sd.setDataSource("DMX");
            sd.setDuration(Duration.INSTANTENOUS);
            sd.setDurationValue((short) 0);
            sd.setExtremum(Extremum.MAX_DAY);
            sd.setLocationId("DNS");
            sd.setObsTime(new SHEFDate());
            sd.setParameterCodeString("SW", "");
            sd.setPhysicalElement(PhysicalElement.SNOW_WATER_EQUIVALENT);
            sd.setProbability(Probability.NULL);
            sd.setQualifier("Z");
            sd.setTypeSource(TypeSource.PROCESSED_NONSPECIFIC);
            sd.setUnitsCode("E");
            if (i == 0) {
                sd.setValue(5.0);
            } else {
                sd.setValue(50.0);
            }
            SHEFDate date = new SHEFDate();
            date.setTimeZone(TimeZone.getTimeZone("GMT"));
            sd.setObsTime(date);
            sd.setStringValue(String.valueOf((sd.getValue())));
            data.add(sd);
        }

        return data;
    }

    private List<ShefData> getReadingShefDataFailReasonableRange() {
        List<ShefData> data = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            ShefData sd = new ShefData(shefParm);
            sd.setDataSource("DMX");
            sd.setDuration(Duration.INSTANTENOUS);
            sd.setDurationValue((short) 0);
            sd.setExtremum(Extremum.MAX_DAY);
            sd.setLocationId("DNS");
            sd.setObsTime(new SHEFDate());
            sd.setParameterCodeString("SW", "");
            sd.setPhysicalElement(PhysicalElement.SNOW_WATER_EQUIVALENT);
            sd.setProbability(Probability.NULL);
            sd.setQualifier("Z");
            sd.setTypeSource(TypeSource.READING_NONSPECIFIC);
            sd.setUnitsCode("E");
            if (i == 0) {
                sd.setValue(5.0);
            } else {
                sd.setValue(50.0);
            }
            SHEFDate date = new SHEFDate();
            date.setTimeZone(TimeZone.getTimeZone("GMT"));
            sd.setObsTime(date);
            sd.setStringValue(String.valueOf((sd.getValue())));
            data.add(sd);
        }

        return data;
    }

    private List<ShefData> getReadingShefDataFailGrossRange() {
        List<ShefData> data = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            ShefData sd = new ShefData(shefParm);
            sd.setDataSource("DMX");
            sd.setDuration(Duration.INSTANTENOUS);
            sd.setDurationValue((short) 0);
            sd.setExtremum(Extremum.MAX_DAY);
            sd.setLocationId("DNS");
            sd.setObsTime(new SHEFDate());
            sd.setParameterCodeString("SW", "");
            sd.setPhysicalElement(PhysicalElement.SNOW_WATER_EQUIVALENT);
            sd.setProbability(Probability.NULL);
            sd.setQualifier("Z");
            sd.setTypeSource(TypeSource.READING_NONSPECIFIC);
            sd.setUnitsCode("E");
            if (i == 0) {
                sd.setValue(-100.0);
            } else {
                sd.setValue(150.0);
            }
            SHEFDate date = new SHEFDate();
            date.setTimeZone(TimeZone.getTimeZone("GMT"));
            sd.setObsTime(date);
            sd.setStringValue(String.valueOf((sd.getValue())));
            data.add(sd);
        }

        return data;
    }

    private List<ShefData> getProcessedShefDataValidRange() {
        List<ShefData> data = new ArrayList<>();
        ShefData sd = new ShefData(shefParm);
        sd.setDataSource("DMX");
        sd.setDuration(Duration.INSTANTENOUS);
        sd.setDurationValue((short) 0);
        sd.setExtremum(Extremum.MAX_DAY);
        sd.setLocationId("DNS");
        sd.setObsTime(new SHEFDate());
        sd.setParameterCodeString("SW", "");
        sd.setPhysicalElement(PhysicalElement.SNOW_WATER_EQUIVALENT);
        sd.setProbability(Probability.NULL);
        sd.setQualifier("Z");
        sd.setTypeSource(TypeSource.PROCESSED_NONSPECIFIC);
        sd.setUnitsCode("E");
        sd.setValue(25.0);
        SHEFDate date = new SHEFDate();
        date.setTimeZone(TimeZone.getTimeZone("GMT"));
        sd.setObsTime(date);
        sd.setStringValue(String.valueOf((sd.getValue())));
        data.add(sd);

        return data;
    }

    private List<ShefData> getReadingShefDataValidRange() {
        List<ShefData> data = new ArrayList<>();
        ShefData sd = new ShefData(shefParm);
        sd.setDataSource("DMX");
        sd.setDuration(Duration.INSTANTENOUS);
        sd.setDurationValue((short) 0);
        sd.setExtremum(Extremum.MAX_DAY);
        sd.setLocationId("DNS");
        sd.setObsTime(new SHEFDate());
        sd.setParameterCodeString("SW", "");
        sd.setPhysicalElement(PhysicalElement.SNOW_WATER_EQUIVALENT);
        sd.setProbability(Probability.NULL);
        sd.setQualifier("Z");
        sd.setTypeSource(TypeSource.READING_NONSPECIFIC);
        sd.setUnitsCode("E");
        sd.setValue(25.0);
        SHEFDate date = new SHEFDate();
        date.setTimeZone(TimeZone.getTimeZone("GMT"));
        sd.setObsTime(date);
        sd.setStringValue(String.valueOf((sd.getValue())));
        data.add(sd);

        return data;
    }

    private List<ShefData> getProcessedShefDataPastDate() {
        ShefData sd = new ShefData(shefParm);
        sd.setDataSource("DMX");
        sd.setDuration(Duration.INSTANTENOUS);
        sd.setDurationValue((short) 0);
        sd.setDurationCodeVariable("I");
        sd.setExtremum(Extremum.NULL);
        sd.setLocationId("TEST1");
        sd.setParameterCodeString("SA", "I");
        sd.setPhysicalElement(PhysicalElement.SNOW_AREAL_EXTENT);
        sd.setProbability(Probability.NULL);
        sd.setQualifier("Z");
        sd.setTypeSource(TypeSource.PROCESSED_NONSPECIFIC);
        sd.setUnitsCode("E");
        sd.setValue(92.0);
        sd.setDurationCodeVariable("I");

        Calendar obsTime = TimeUtil.newGmtCalendar();
        obsTime.add(Calendar.DAY_OF_MONTH, -11);
        obsTime.set(Calendar.HOUR, 18);
        obsTime.set(Calendar.MINUTE, 0);
        obsTime.set(Calendar.SECOND, 0);
        obsTime.set(Calendar.MILLISECOND, 0);
        SHEFDate date = new SHEFDate(obsTime);
        date.setTimeZone(TimeZone.getTimeZone("GMT"));
        sd.setObsTime(date);

        date = new SHEFDate();
        date.setTimeZone(TimeZone.getTimeZone("GMT"));
        sd.setCreateTime(date);

        sd.setStringValue(String.valueOf((sd.getValue())));

        return Collections.singletonList(sd);
    }

    private List<ShefData> getProcessedShefDataFutureDate() {
        ShefData sd = new ShefData(shefParm);
        sd.setDataSource("DMX");
        sd.setDuration(Duration.INSTANTENOUS);
        sd.setDurationValue((short) 0);
        sd.setDurationCodeVariable("I");
        sd.setExtremum(Extremum.NULL);
        sd.setLocationId("TEST1");
        sd.setParameterCodeString("SA", "I");
        sd.setPhysicalElement(PhysicalElement.SNOW_AREAL_EXTENT);
        sd.setProbability(Probability.NULL);
        sd.setQualifier("Z");
        sd.setTypeSource(TypeSource.PROCESSED_NONSPECIFIC);
        sd.setUnitsCode("E");
        sd.setValue(95.0);
        sd.setDurationCodeVariable("I");

        Calendar obsTime = TimeUtil.newGmtCalendar();
        obsTime.add(Calendar.DAY_OF_MONTH, 1);
        obsTime.set(Calendar.HOUR, 18);
        obsTime.set(Calendar.MINUTE, 0);
        obsTime.set(Calendar.SECOND, 0);
        obsTime.set(Calendar.MILLISECOND, 0);
        SHEFDate date = new SHEFDate(obsTime);
        date.setTimeZone(TimeZone.getTimeZone("GMT"));
        sd.setObsTime(date);

        date = new SHEFDate();
        date.setTimeZone(TimeZone.getTimeZone("GMT"));
        sd.setCreateTime(date);

        sd.setStringValue(String.valueOf((sd.getValue())));

        return Collections.singletonList(sd);
    }

    private ShefRecord getShefRecord() {
        ShefRecord rec = new ShefRecord();
        rec.setDataStringQualifier("Z");
        rec.setDataURI("KDMXRR4DMX");
        rec.setLocationId("DMX");
        rec.setProductTime(new Date());
        rec.setShefType(ShefType.B);
        rec.setSourceTraceId(TRACE_ID);
        return rec;
    }

    private Object[] getIngestData() {
        Object[] retArray = new Object[1];
        Object[] oa = new Object[9];
        oa[0] = "DNS";
        oa[1] = "SW";
        oa[2] = 0;
        oa[3] = "PZ";
        oa[4] = "Z";
        oa[5] = 1;
        oa[6] = "T";
        oa[7] = "F";
        oa[8] = "F";

        retArray[0] = oa;
        return retArray;
    }

    private Object[] getQualityResults() {
        Object[] retArray = new Object[1];
        Object[] oa = new Object[17];
        oa[0] = "01-01";
        oa[1] = "12-31";
        oa[2] = 0;
        oa[3] = 60;
        oa[4] = 10;
        oa[5] = 40;

        retArray[0] = oa;
        return retArray;
    }

    private Object[] getTelemResults() {
        Object[] retArray = new Object[1];
        Object[] oa = new Object[2];
        oa[0] = "DNS";
        oa[1] = "Telem Type";

        retArray[0] = oa;
        return retArray;
    }

    private Object[] getIngestPeResults() {
        Object[] oa = new Object[1];
        oa[0] = "SW";

        return oa;
    }

    private Object[] getLidTEST1TelemResults() {
        Object[] oa = { "TEST1", "DCP" };
        Object[] retArray = { oa };
        return retArray;
    }

    private Object[] getLidTEST1IngestData() {
        Object[] oa = new Object[9];
        oa[0] = "TEST1";
        oa[1] = "SA";
        oa[2] = 0;
        oa[3] = "PZ";
        oa[4] = "Z";
        oa[5] = 1;
        oa[6] = "T";
        oa[7] = "F";
        oa[8] = "F";

        Object[] retArray = { oa };
        return retArray;
    }

    private Object[] getLidTEST1IngestPeResults() {
        Object[] oa = { "SA" };
        return oa;
    }
}
