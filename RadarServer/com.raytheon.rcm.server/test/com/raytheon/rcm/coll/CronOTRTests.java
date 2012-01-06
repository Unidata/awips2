package com.raytheon.rcm.coll;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.EnumSet;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.SimpleTimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBException;

import org.junit.Assert;
import org.junit.Test;

import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.RadarProduct.Param;
import com.raytheon.rcm.request.Request;

public class CronOTRTests {
    private static final String TEST_INPUT_HOURS_AND_MINUTES =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<cronOTRConfiguration>" +
        "<cronOTR cron=\"0 1 * * * ?\" productCode=\"173\" hoursBack=\"3:42\" wmo=\"SDUS8\" nnn=\"DUA\"/>" +
        "<cronOTR cron=\"0 1 * * * ?\" productCode=\"31\" hoursBack=\"3\" wmo=\"SDUS9\" nnn=\"USP\"/>" +
        "<cronOTR cron=\"0 1 12 * * ?\" productCode=\"31\" wmo=\"SDUS9\" nnn=\"USP\"/>" +
        "</cronOTRConfiguration>";

    @Test
    public void testParseHoursAndMinutesBack() throws JAXBException {
        CronOTRConfiguration config = configFrom(TEST_INPUT_HOURS_AND_MINUTES);
        
        Assert.assertNotNull(config.cronOTRList.get(0).getMinutesBack());
        Assert.assertEquals(3 * 60 + 42, config.cronOTRList.get(0).getMinutesBack().intValue());
        
        Assert.assertNotNull(config.cronOTRList.get(1).getMinutesBack());
        Assert.assertEquals(3 * 60, config.cronOTRList.get(1).getMinutesBack().intValue());
        
        Assert.assertNull(config.cronOTRList.get(2).getMinutesBack());
        Assert.assertEquals(3 * 60, config.cronOTRList.get(1).getMinutesBack().intValue());
    }
    
    @Test
    public void testHoursAndMinutesBack() throws JAXBException {
        CronOTRConfiguration config = configFrom(TEST_INPUT_HOURS_AND_MINUTES);
        
        GregorianCalendar cal = new GregorianCalendar(
                new SimpleTimeZone(0, "GMT"));
        cal.set(GregorianCalendar.HOUR_OF_DAY, 7);
        cal.set(GregorianCalendar.MINUTE, 1);
        cal.set(GregorianCalendar.SECOND, 27);
        
        List<Request> reqs = config.cronOTRList.get(0).createRequests(RadarType.WSR, cal);
        Assert.assertEquals(1, reqs.size());
        Assert.assertEquals(7 * 60, reqs.get(0).getEndHour());
        Assert.assertEquals(3 * 60 + 42, reqs.get(0).getTimeSpan());
        
        reqs = config.cronOTRList.get(1).createRequests(RadarType.WSR, cal);
        Assert.assertEquals(1, reqs.size());
        Assert.assertEquals(7, reqs.get(0).getEndHour());
        Assert.assertEquals(3, reqs.get(0).getTimeSpan());
    }

    @Test
    public void testFormatHourseAndMinutesBack() throws JAXBException {
        CronOTRConfiguration config = new CronOTRConfiguration();
        CronOTR cronOTR = new CronOTR();
        cronOTR.setMinutesBack(3 * 60 + 42);
        config.cronOTRList.add(cronOTR); 
        
        StringWriter sw = new StringWriter();
        RequestScheduler.getJAXBContext().createMarshaller().
            marshal(config, sw);
        String text = sw.toString();
        Matcher m = Pattern.compile("hoursBack=['\"]([^'\"]+)['\"]").matcher(text);

        Assert.assertTrue(m.find());
        Assert.assertEquals("3:42", m.group(1));
    }
    
    private static final String TEST_RADAR_TYPE_RESTRICTIONS =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<cronOTRConfiguration>" +
        "<cronOTR cron=\"0 23,53 * * * ?\"     productCode=\"74\"  wmo=\"SDUS4\" nnn=\"RCM\"/>" +
        "<cronOTR cron=\"0 15 * * * ?\"        productCode=\"79\"  wmo=\"SDUS6\" nnn=\"N3P\" radarTypes=\"WSR\"/>" +
        "<cronOTR cron=\"0 35 * * * ?\"        productCode=\"136\" randomWait=\"600\"/>" +
        "<cronOTR cron=\"0 5 0,8,16 * * ?\"    productCode=\"152\" randomWait=\"600\"/>" +
        "<cronOTR cron=\"0 1 0,6,12,18 * * ?\">" +
        "<request> <productCode>34</productCode> <pdw20>2</pdw20> </request>" +
        "</cronOTR>" +
        "</cronOTRConfiguration>";

    @Test
    public void testRadarTypeRestrictions() throws JAXBException {
        CronOTRConfiguration config = configFrom(TEST_RADAR_TYPE_RESTRICTIONS);
        
        GregorianCalendar cal = new GregorianCalendar(
                new SimpleTimeZone(0, "GMT"));

        // RCM is only available for 88Ds.
        List<Request> reqs = config.cronOTRList.get(0).createRequests(RadarType.WSR, cal);
        Assert.assertEquals(1, reqs.size());
        
        reqs = config.cronOTRList.get(0).createRequests(RadarType.TDWR, cal);
        Assert.assertEquals(0, reqs.size());
        
        // THP is available for both 88Ds and TDWRs, but we only want it from 88Ds.
        reqs = config.cronOTRList.get(1).createRequests(RadarType.WSR, cal);
        Assert.assertEquals(1, reqs.size());
        
        reqs = config.cronOTRList.get(1).createRequests(RadarType.TDWR, cal);
        Assert.assertEquals(0, reqs.size());        
        
        // SO is only available for 88Ds.
        reqs = config.cronOTRList.get(2).createRequests(RadarType.WSR, cal);
        Assert.assertEquals(1, reqs.size());
        
        reqs = config.cronOTRList.get(2).createRequests(RadarType.TDWR, cal);
        Assert.assertEquals(0, reqs.size());

        // Sanity check: RSS is available for both 88Ds and TDWRs.
        reqs = config.cronOTRList.get(3).createRequests(RadarType.WSR, cal);
        Assert.assertEquals(1, reqs.size());
        
        reqs = config.cronOTRList.get(3).createRequests(RadarType.TDWR, cal);
        Assert.assertEquals(1, reqs.size());

        // CFC is only available for 88.
        reqs = config.cronOTRList.get(4).createRequests(RadarType.WSR, cal);
        Assert.assertEquals(1, reqs.size());
        
        reqs = config.cronOTRList.get(4).createRequests(RadarType.TDWR, cal);
        Assert.assertEquals(0, reqs.size());        
    }
    
    @Test
    public void testVolumeScanSelectionType() throws JAXBException {
        GregorianCalendar cal = new GregorianCalendar(new SimpleTimeZone(0,
                "GMT"));

        CronOTRConfiguration currentProdsConfig = 
            configFrom(TEST_INPUT_HOURS_AND_MINUTES);
        List<Request> currentReqs = currentProdsConfig.cronOTRList.get(0)
                .createRequests(RadarType.WSR, cal);

        CronOTRConfiguration latestProdsConfig = 
            configFrom(TEST_RADAR_TYPE_RESTRICTIONS);
        List<Request> latestReqs = latestProdsConfig.cronOTRList.get(0)
                .createRequests(RadarType.WSR, cal);

        // Sanity checks:
        EnumSet<Param> params;

        params = ProductInfo.getInstance().getPoductForCode(
                currentReqs.get(0).productCode).params;
        Assert.assertTrue(params.contains(Param.TIME_SPAN)
                || params.contains(Param.TIME_SPAN_MINUTES));

        params = ProductInfo.getInstance().getPoductForCode(
                latestReqs.get(0).productCode).params;
        Assert.assertTrue(params.isEmpty()
                || params.equals(EnumSet.of(Param.ELEVATION)));

        // Actual tests:
        Assert.assertEquals(Request.SELECT_CURRENT, currentReqs.get(0)
                .getVolumeScanSelection());

        Assert.assertEquals(Request.SELECT_LATEST, latestReqs.get(0)
                .getVolumeScanSelection());
    }
    
    private CronOTRConfiguration configFrom(String text) throws JAXBException {
        return (CronOTRConfiguration) RequestScheduler.getJAXBContext()
                .createUnmarshaller().unmarshal(new StringReader(text));
    }

}
