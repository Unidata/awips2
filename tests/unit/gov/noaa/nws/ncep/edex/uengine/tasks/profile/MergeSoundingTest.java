package gov.noaa.nws.ncep.edex.uengine.tasks.profile;

//import edu.emory.mathcs.backport.java.util.Collections;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

import java.util.ArrayList;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class MergeSoundingTest {
    @Before
    public void setUp() throws Exception {
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testMergeObservedSounding() {
        /* Case I: Good report with a string of five characters */
        MergeSounding ms = new MergeSounding();

        List<NcSoundingLayer> sls = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> ttaa = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> ttbb = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> ttcc = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> ttdd = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> ppaa = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> ppbb = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> ppcc = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> ppdd = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> trop_a = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> wmax_a = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> trop_c = new ArrayList<NcSoundingLayer>();
        List<NcSoundingLayer> wmax_c = new ArrayList<NcSoundingLayer>();

        NcSoundingLayer ta;
        ta = new NcSoundingLayer();
        ta.setPressure(200.f);
        ta.setTemperature(-54.9f);
        ta.setDewpoint(-64.9f);
        ta.setWindDirection(285.f);
        ta.setWindSpeed(58.0f);
        ta.setGeoHeight(11830.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(700.f);
        ta.setTemperature(0.2f);
        ta.setDewpoint(-19.8f);
        ta.setWindDirection(295.f);
        ta.setWindSpeed(37.0f);
        ta.setGeoHeight(3013.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(500.f);
        ta.setTemperature(-17.5f);
        ta.setDewpoint(-22.5f);
        ta.setWindDirection(290.f);
        ta.setWindSpeed(52.0f);
        ta.setGeoHeight(5620.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(1000.f);
        ta.setTemperature(-9999.f);
        ta.setDewpoint(-9999.f);
        ta.setWindDirection(-9999.f);
        ta.setWindSpeed(-9999.f);
        ta.setGeoHeight(71.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(150.f);
        ta.setTemperature(-54.5f);
        ta.setDewpoint(-69.5f);
        ta.setWindDirection(275.f);
        ta.setWindSpeed(52.0f);
        ta.setGeoHeight(13670.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(925.f);
        ta.setTemperature(-9999.f);
        ta.setDewpoint(-9999.f);
        ta.setWindDirection(-9999.f);
        ta.setWindSpeed(-9999.f);
        ta.setGeoHeight(731.f);
        ttaa.add(ta);

        // ta = new NcSoundingLayer();
        // ta.setPressure(997.f);
        // ta.setTemperature(17.6f);
        // ta.setDewpoint(9.6f);
        // ta.setWindDirection(0.f);
        // ta.setWindSpeed(3.00f);
        // ta.setGeoHeight(95.f);
        // ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(850.f);
        ta.setTemperature(7.8f);
        ta.setDewpoint(5.8f);
        ta.setWindDirection(255.f);
        ta.setWindSpeed(27.0f);
        ta.setGeoHeight(1435.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(100.f);
        ta.setTemperature(-57.3f);
        ta.setDewpoint(-74.3f);
        ta.setWindDirection(270.f);
        ta.setWindSpeed(27.0f);
        ta.setGeoHeight(16240.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(250.f);
        ta.setTemperature(-55.7f);
        ta.setDewpoint(-63.7f);
        ta.setWindDirection(270.f);
        ta.setWindSpeed(84.0f);
        ta.setGeoHeight(10420.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(400.f);
        ta.setTemperature(-29.7f);
        ta.setDewpoint(-35.7f);
        ta.setWindDirection(280.f);
        ta.setWindSpeed(61.0f);
        ta.setGeoHeight(7250.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(920.f);
        ta.setTemperature(17.6f);
        ta.setDewpoint(9.6f);
        ta.setWindDirection(0.f);
        ta.setWindSpeed(3.0f);
        ta.setGeoHeight(-9999.f);
        ttaa.add(ta);

        ta = new NcSoundingLayer();
        ta.setPressure(300.f);
        ta.setTemperature(-46.1f);
        ta.setDewpoint(-54.1f);
        ta.setWindDirection(275.f);
        ta.setWindSpeed(68.0f);
        ta.setGeoHeight(9230.f);
        ttaa.add(ta);

        NcSoundingLayer tropa;
        tropa = new NcSoundingLayer();
        tropa.setPressure(223.f);
        tropa.setTemperature(-59.5f);
        tropa.setDewpoint(-67.5f);
        tropa.setWindDirection(275.f);
        tropa.setWindSpeed(75.0f);
        tropa.setGeoHeight(-9999.f);
        trop_a.add(tropa);

        tropa = new NcSoundingLayer();
        tropa.setPressure(123.f);
        tropa.setTemperature(-60.5f);
        tropa.setDewpoint(-69.5f);
        tropa.setWindDirection(275.f);
        tropa.setWindSpeed(80.0f);
        tropa.setGeoHeight(-9999.f);
        trop_a.add(tropa);

        NcSoundingLayer tropc;
        tropc = new NcSoundingLayer();
        tropc.setPressure(63.f);
        tropc.setTemperature(-60.5f);
        tropc.setDewpoint(-69.5f);
        tropc.setWindDirection(275.f);
        tropc.setWindSpeed(80.0f);
        tropc.setGeoHeight(-9999.f);
        trop_c.add(tropc);

        NcSoundingLayer wmaxa = new NcSoundingLayer();
        wmaxa.setPressure(252.f);
        wmaxa.setTemperature(-9999.f);
        wmaxa.setDewpoint(-9999.f);
        wmaxa.setWindDirection(270.f);
        wmaxa.setWindSpeed(84.0f);
        wmaxa.setGeoHeight(-9999.f);
        wmax_a.add(wmaxa);

        NcSoundingLayer wmaxc = new NcSoundingLayer();
        wmaxc.setPressure(5.8f);
        wmaxc.setTemperature(-9999.f);
        wmaxc.setDewpoint(-9999.f);
        wmaxc.setWindDirection(270.f);
        wmaxc.setWindSpeed(67.f);
        wmaxc.setGeoHeight(-9999.f);
        wmax_c.add(wmaxc);

        NcSoundingLayer tb;
        tb = new NcSoundingLayer();
        tb.setPressure(420.f);
        tb.setTemperature(-26.5f);
        tb.setDewpoint(-34.5f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(769.f);
        tb.setTemperature(4.6f);
        tb.setDewpoint(-22.4f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(170.f);
        tb.setTemperature(-54.3f);
        tb.setDewpoint(-68.3f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(920.f);
        tb.setTemperature(17.6f);
        tb.setDewpoint(9.6f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(150.f);
        tb.setTemperature(-54.5f);
        tb.setDewpoint(-69.5f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        // tb = new NcSoundingLayer();
        // tb.setPressure(922.f);
        // tb.setTemperature(11.8f);
        // tb.setDewpoint(6.8f);
        // tb.setWindDirection(-9999.f);
        // tb.setWindSpeed(-9999.f);
        // tb.setGeoHeight(-9999.f);
        // ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(-9999.f);
        tb.setTemperature(17.6f);
        tb.setDewpoint(9.6f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(215.f);
        tb.setTemperature(-57.1f);
        tb.setDewpoint(-65.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(106.f);
        tb.setTemperature(-55.7f);
        tb.setDewpoint(-72.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(244.f);
        tb.setTemperature(-57.1f);
        tb.setDewpoint(-65.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(521.f);
        tb.setTemperature(-14.9f);
        tb.setDewpoint(-27.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(805.f);
        tb.setTemperature(5.f);
        tb.setDewpoint(-4.f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(194.f);
        tb.setTemperature(-53.9f);
        tb.setDewpoint(-64.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(109.f);
        tb.setTemperature(-56.9f);
        tb.setDewpoint(-73.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(189.f);
        tb.setTemperature(-52.5f);
        tb.setDewpoint(-64.5f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(436.f);
        tb.setTemperature(-25.1f);
        tb.setDewpoint(-30.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(198.f);
        tb.setTemperature(-53.9f);
        tb.setDewpoint(-63.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        // tb = new NcSoundingLayer();
        // tb.setPressure(965.f);
        // tb.setTemperature(15.f);
        // tb.setDewpoint(7.f);
        // tb.setWindDirection(-9999.f);
        // tb.setWindSpeed(-9999.f);
        // tb.setGeoHeight(-9999.f);
        // ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(772.f);
        tb.setTemperature(4.6f);
        tb.setDewpoint(-17.4f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(559.f);
        tb.setTemperature(-12.1f);
        tb.setDewpoint(-29.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(507.f);
        tb.setTemperature(-16.7f);
        tb.setDewpoint(-22.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(134.f);
        tb.setTemperature(-56.7f);
        tb.setDewpoint(-72.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(541.f);
        tb.setTemperature(-13.5f);
        tb.setDewpoint(-17.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(524.f);
        tb.setTemperature(-14.9f);
        tb.setDewpoint(-19.4f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setTemperature(-12.5f);
        tb.setPressure(554.f);
        tb.setDewpoint(-23.5f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(809.f);
        tb.setTemperature(4.6f);
        tb.setDewpoint(2.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(545.f);
        tb.setTemperature(-13.5f);
        tb.setDewpoint(-17.5f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(126.f);
        tb.setTemperature(-56.3f);
        tb.setDewpoint(-72.3f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(569.f);
        tb.setTemperature(-11.5f);
        tb.setDewpoint(-25.5f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(787.f);
        tb.setTemperature(4.8f);
        tb.setDewpoint(-19.2f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(803.f);
        tb.setTemperature(5.f);
        tb.setDewpoint(-7.f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(308.f);
        tb.setTemperature(-44.7f);
        tb.setDewpoint(-52.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(468.f);
        tb.setTemperature(-21.1f);
        tb.setDewpoint(-24.8f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(699.f);
        tb.setTemperature(.2f);
        tb.setDewpoint(-19.8f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(502.f);
        tb.setTemperature(-17.3f);
        tb.setDewpoint(-22.3f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(223.f);
        tb.setTemperature(-59.5f);
        tb.setDewpoint(-67.5f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(138.f);
        tb.setTemperature(-57.3f);
        tb.setDewpoint(-72.3f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(221.f);
        tb.setTemperature(-58.1f);
        tb.setDewpoint(-66.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(885.f);
        tb.setTemperature(10.4f);
        tb.setDewpoint(7.3999996f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(806.f);
        tb.setTemperature(4.6f);
        tb.setDewpoint(2.3f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(662.f);
        tb.setTemperature(-3.3f);
        tb.setDewpoint(-14.3f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        // tb = new NcSoundingLayer();
        // tb.setPressure(986.f);
        // tb.setTemperature(16.2f);
        // tb.setDewpoint(8.200001f);
        // tb.setWindDirection(-9999.f);
        // tb.setWindSpeed(-9999.f);
        // tb.setGeoHeight(-9999.f);
        // ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(601.f);
        tb.setTemperature(-8.9f);
        tb.setDewpoint(-18.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(204.f);
        tb.setTemperature(-56.9f);
        tb.setDewpoint(-65.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(519.f);
        tb.setTemperature(-15.1f);
        tb.setDewpoint(-30.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(551.f);
        tb.setTemperature(-12.7f);
        tb.setDewpoint(-19.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(537.f);
        tb.setTemperature(-13.7f);
        tb.setDewpoint(-18.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(212.f);
        tb.setTemperature(-55.7f);
        tb.setDewpoint(-64.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(202.f);
        tb.setTemperature(-55.3f);
        tb.setDewpoint(-65.3f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(391.f);
        tb.setTemperature(-31.1f);
        tb.setDewpoint(-39.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(560.f);
        tb.setTemperature(-12.1f);
        tb.setDewpoint(-30.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(142.f);
        tb.setTemperature(-56.9f);
        tb.setDewpoint(-71.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(234.f);
        tb.setTemperature(-58.7f);
        tb.setDewpoint(-66.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(120.f);
        tb.setTemperature(-57.1f);
        tb.setDewpoint(-73.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(399.f);
        tb.setTemperature(-29.9f);
        tb.setDewpoint(-35.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(484.f);
        tb.setTemperature(-18.9f);
        tb.setDewpoint(-24.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(558.f);
        tb.setTemperature(-12.1f);
        tb.setDewpoint(-23.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(627.f);
        tb.setTemperature(-6.7f);
        tb.setDewpoint(-15.7f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        // tb = new NcSoundingLayer();
        // tb.setPressure(997.f);
        // tb.setTemperature(17.6f);
        // tb.setDewpoint(9.6f);
        // tb.setWindDirection(-9999.f);
        // tb.setWindSpeed(-9999.f);
        // tb.setGeoHeight(-9999.f);
        // ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(585.f);
        tb.setTemperature(-9.9f);
        tb.setDewpoint(-23.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(130.f);
        tb.setTemperature(-57.1f);
        tb.setDewpoint(-73.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(459.f);
        tb.setTemperature(-22.1f);
        tb.setDewpoint(-27.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(115.f);
        tb.setTemperature(-56.1f);
        tb.setDewpoint(-73.1f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        tb = new NcSoundingLayer();
        tb.setPressure(513.f);
        tb.setTemperature(-15.9f);
        tb.setDewpoint(-29.9f);
        tb.setWindDirection(-9999.f);
        tb.setWindSpeed(-9999.f);
        tb.setGeoHeight(-9999.f);
        ttbb.add(tb);

        NcSoundingLayer pb = new NcSoundingLayer();
        pb.setWindDirection(280.f);
        pb.setWindSpeed(51.0f);
        pb.setGeoHeight(12192.0f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(275.f);
        pb.setWindSpeed(67.f);
        pb.setGeoHeight(9144.0f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(270.f);
        pb.setWindSpeed(84.f);
        pb.setGeoHeight(10363.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(250.f);
        pb.setWindSpeed(12.00f);
        pb.setGeoHeight(914.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(285.f);
        pb.setWindSpeed(33.f);
        pb.setGeoHeight(2438.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(250.f);
        pb.setWindSpeed(22.0f);
        pb.setGeoHeight(1219.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(275.f);
        pb.setWindSpeed(60.0f);
        pb.setGeoHeight(8839.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(295.f);
        pb.setWindSpeed(33.0f);
        pb.setGeoHeight(2743.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(280.f);
        pb.setWindSpeed(27.0f);
        pb.setDewpoint(-9999.f);
        pb.setGeoHeight(1829.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(285.f);
        pb.setDewpoint(-9999.f);
        pb.setWindSpeed(39.0f);
        pb.setGeoHeight(4877.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(305.f);
        pb.setDewpoint(-9999.f);
        pb.setWindSpeed(12.0f);
        pb.setGeoHeight(305.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(305.f);
        pb.setWindSpeed(15.0f);
        pb.setGeoHeight(610.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(285.f);
        pb.setDewpoint(-9999.f);
        pb.setWindSpeed(55.0f);
        pb.setGeoHeight(6096.0f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(285.f);
        pb.setDewpoint(-9999.f);
        pb.setWindSpeed(54.0f);
        pb.setGeoHeight(14326.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(295.f);
        pb.setWindSpeed(37.0f);
        pb.setGeoHeight(3048.f);
        pb.setPressure(-9999.f);
        pb.setDewpoint(-9999.f);
        pb.setTemperature(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(280.f);
        pb.setDewpoint(-9999.f);
        pb.setWindSpeed(39.0f);
        pb.setGeoHeight(15240.0f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setDewpoint(-9999.f);
        pb.setWindDirection(290.f);
        pb.setWindSpeed(37.0f);
        pb.setGeoHeight(3658.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(290.f);
        pb.setWindSpeed(55.0f);
        pb.setGeoHeight(5791.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(285.f);
        pb.setWindSpeed(34.0f);
        pb.setDewpoint(-9999.f);
        pb.setGeoHeight(4572.0f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(0.f);
        pb.setWindSpeed(3.00f);
        pb.setGeoHeight(0.f);
        pb.setTemperature(-9999.f);
        pb.setPressure(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(285.f);
        pb.setWindSpeed(34.0f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        pb.setGeoHeight(2134.f);
        pb.setPressure(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(280.f);
        pb.setTemperature(-9999.f);
        pb.setWindSpeed(61.0f);
        pb.setGeoHeight(7620.f);
        pb.setPressure(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(285.f);
        pb.setDewpoint(-9999.f);
        pb.setWindSpeed(34.0f);
        pb.setTemperature(-9999.f);
        pb.setGeoHeight(4267.f);
        pb.setPressure(-9999.f);
        ppbb.add(pb);

        pb = new NcSoundingLayer();
        pb.setWindDirection(265.f);
        pb.setWindSpeed(51.0f);
        pb.setGeoHeight(13411.f);
        pb.setPressure(-9999.f);
        pb.setTemperature(-9999.f);
        pb.setDewpoint(-9999.f);
        ppbb.add(pb);

        NcSoundingLayer tc = new NcSoundingLayer();
        tc.setPressure(7.f);
        tc.setTemperature(-51.5f);
        tc.setDewpoint(-9999.f);
        tc.setWindDirection(265.f);
        tc.setWindSpeed(60.f);
        tc.setGeoHeight(33260.f);
        ttcc.add(tc);

        tc = new NcSoundingLayer();
        tc.setPressure(20.f);
        tc.setTemperature(-53.1f);
        tc.setDewpoint(-85.1f);
        tc.setWindSpeed(22.0f);
        tc.setWindDirection(245.f);
        tc.setGeoHeight(26470.0f);
        ttcc.add(tc);

        tc = new NcSoundingLayer();
        tc.setPressure(30.f);
        tc.setTemperature(-55.1f);
        tc.setDewpoint(-81.1f);
        tc.setWindDirection(310.0f);
        tc.setWindSpeed(11.00f);
        tc.setGeoHeight(23860.0f);
        ttcc.add(tc);

        tc = new NcSoundingLayer();
        tc.setPressure(10.f);
        tc.setTemperature(-53.5f);
        tc.setDewpoint(-9999.f);
        tc.setWindDirection(265.0f);
        tc.setWindSpeed(48.0f);
        tc.setGeoHeight(30950.0f);
        ttcc.add(tc);

        tc = new NcSoundingLayer();
        tc.setPressure(70.f);
        tc.setTemperature(-56.9f);
        tc.setDewpoint(-75.9f);
        tc.setWindDirection(270.0f);
        tc.setWindSpeed(11.0f);
        tc.setGeoHeight(18500.0f);
        ttcc.add(tc);

        tc = new NcSoundingLayer();
        tc.setPressure(50.f);
        tc.setTemperature(-56.9f);
        tc.setDewpoint(-77.9f);
        tc.setWindDirection(275.0f);
        tc.setWindSpeed(9.00f);
        tc.setGeoHeight(20620.0f);
        ttcc.add(tc);

        NcSoundingLayer td = new NcSoundingLayer();
        td.setPressure(44.4f);
        td.setTemperature(-58.1f);
        td.setDewpoint(-79.1f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(22.0f);
        td.setTemperature(-54.5f);
        td.setDewpoint(-84.5f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(62.6f);
        td.setTemperature(-56.5f);
        td.setDewpoint(-76.5f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(7.3f);
        td.setTemperature(-52.3f);
        td.setDewpoint(-9999.f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(8.4f);
        td.setTemperature(-52.7f);
        td.setGeoHeight(-9999.f);
        td.setDewpoint(-9999.f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(54.9f);
        td.setTemperature(-58.9f);
        td.setDewpoint(-78.9f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(50.1f);
        td.setTemperature(-56.9f);
        td.setDewpoint(-77.9f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(71.4f);
        td.setTemperature(-57.9f);
        td.setDewpoint(-76.9f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(36.7f);
        td.setTemperature(-56.9f);
        td.setDewpoint(-78.9f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(10.4f);
        td.setTemperature(-54.1f);
        td.setWindDirection(-9999.f);
        td.setDewpoint(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(64.0f);
        td.setWindDirection(-9999.f);
        td.setTemperature(-59.1f);
        td.setDewpoint(-78.1f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(70.2f);
        td.setTemperature(-56.9f);
        td.setDewpoint(-75.9f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(13.7f);
        td.setTemperature(-51.9f);
        td.setDewpoint(-9999.f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(77.3f);
        td.setTemperature(-55.1f);
        td.setDewpoint(-74.1f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(84.7f);
        td.setTemperature(-56.7f);
        td.setDewpoint(-74.7f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(33.4f);
        td.setTemperature(-52.5f);
        td.setDewpoint(-79.5f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(82.8f);
        td.setTemperature(-57.5f);
        td.setDewpoint(-75.5f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(8.f);
        td.setTemperature(-49.9f);
        td.setDewpoint(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setWindDirection(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(8.9f);
        td.setTemperature(-51.9f);
        td.setDewpoint(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setWindDirection(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(90.4f);
        td.setTemperature(-59.9f);
        td.setDewpoint(-76.9f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        td.setWindDirection(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(29.8f);
        td.setTemperature(-55.1f);
        td.setWindDirection(-9999.f);
        td.setDewpoint(-81.1f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(12.8f);
        td.setTemperature(-50.1f);
        td.setDewpoint(-9999.f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(41.2f);
        td.setTemperature(-55.9f);
        td.setDewpoint(-77.9f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(6.8f);
        td.setTemperature(-50.7f);
        td.setDewpoint(-9999.f);
        td.setWindDirection(-9999.f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(27.8f);
        td.setTemperature(-53.9f);
        td.setDewpoint(-81.9f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        td = new NcSoundingLayer();
        td.setPressure(15.1f);
        td.setTemperature(-51.1f);
        td.setDewpoint(-9999.f);
        td.setWindDirection(-9999.f);
        td.setWindSpeed(-9999.f);
        td.setGeoHeight(-9999.f);
        ttdd.add(td);

        NcSoundingLayer pd = new NcSoundingLayer();
        pd.setWindDirection(300.0f);
        pd.setWindSpeed(12.00f);
        pd.setGeoHeight(24993.6f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(260.0f);
        pd.setWindSpeed(24.0f);
        pd.setGeoHeight(26517.6f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(235.0f);
        pd.setWindSpeed(18.0f);
        pd.setGeoHeight(26212.8f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(335.0f);
        pd.setWindSpeed(12.00f);
        pd.setGeoHeight(17983.2f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(280.0f);
        pd.setWindSpeed(12.0f);
        pd.setGeoHeight(19507.2f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(280.0f);
        pd.setWindSpeed(15.0f);
        pd.setGeoHeight(18897.6f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(280.0f);
        pd.setWindSpeed(11.00f);
        pd.setGeoHeight(19202.4f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(260.0f);
        pd.setWindSpeed(24.0f);
        pd.setGeoHeight(24384.0f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(285.0f);
        pd.setWindSpeed(22.0f);
        pd.setGeoHeight(17678.4f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        pd.setPressure(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(265.0f);
        pd.setWindSpeed(15.0f);
        pd.setGeoHeight(23164.8f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(290.0f);
        pd.setWindSpeed(9.00f);
        pd.setGeoHeight(23469.6f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(210.0f);
        pd.setWindSpeed(16.0f);
        pd.setGeoHeight(22555.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(20.0f);
        pd.setWindSpeed(1.000f);
        pd.setGeoHeight(20117.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(295.0f);
        pd.setWindSpeed(12.00f);
        pd.setGeoHeight(20422.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(250.0f);
        pd.setWindSpeed(33.0f);
        pd.setGeoHeight(27432.0f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(0.0f);
        pd.setWindSpeed(0.f);
        pd.setGeoHeight(22250.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(305.f);
        pd.setWindSpeed(11.f);
        pd.setGeoHeight(21031.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(280.0f);
        pd.setWindSpeed(3.f);
        pd.setGeoHeight(21336.0f);
        pd.setPressure(-9999.f);
        pd.setDewpoint(-9999.f);
        pd.setTemperature(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(255.0f);
        pd.setWindSpeed(18.0f);
        pd.setGeoHeight(24079.f);
        pd.setPressure(-9999.f);
        pd.setDewpoint(-9999.f);
        pd.setTemperature(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindSpeed(37.0f);
        pd.setWindDirection(240.f);
        pd.setGeoHeight(27127.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(325.0f);
        pd.setWindSpeed(12.00f);
        pd.setGeoHeight(18288.0f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(275.0f);
        pd.setWindSpeed(21.0f);
        pd.setGeoHeight(25603.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(315.0f);
        pd.setWindSpeed(11.f);
        pd.setGeoHeight(23774.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(255.0f);
        pd.setWindSpeed(11.f);
        pd.setGeoHeight(18593.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(270.0f);
        pd.setWindSpeed(34.0f);
        pd.setGeoHeight(28042.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(285.0f);
        pd.setWindSpeed(13.f);
        pd.setGeoHeight(20726.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(280.0f);
        pd.setWindSpeed(30.0f);
        pd.setGeoHeight(17374.f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(270.0f);
        pd.setWindSpeed(44.f);
        pd.setGeoHeight(30480.0f);
        pd.setPressure(-9999.f);
        pd.setDewpoint(-9999.f);
        pd.setTemperature(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(265.0f);
        pd.setDewpoint(-9999.f);
        pd.setWindSpeed(45.0f);
        pd.setGeoHeight(31090.0f);
        pd.setTemperature(-9999.f);
        pd.setPressure(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(270.f);
        pd.setWindSpeed(67.f);
        pd.setGeoHeight(32614.0f);
        pd.setPressure(-9999.f);
        pd.setDewpoint(-9999.f);
        pd.setTemperature(-9999.f);
        ppdd.add(pd);

        pd = new NcSoundingLayer();
        pd.setWindDirection(260.0f);
        pd.setWindSpeed(60.0f);
        pd.setGeoHeight(33223.0f);
        pd.setPressure(-9999.f);
        pd.setTemperature(-9999.f);
        pd.setDewpoint(-9999.f);
        ppdd.add(pd);

        NcSoundingLayer pa;
        pa = new NcSoundingLayer();

        pa.setPressure(700.f);
        pa.setTemperature(-9999.f);
        pa.setDewpoint(-9999.f);
        pa.setWindDirection(230.f);
        pa.setWindSpeed(30.0f);
        pa.setGeoHeight(-9999.f);
        ppaa.add(pa);

        pa = new NcSoundingLayer();
        pa.setPressure(300.f);
        pa.setWindDirection(250.f);
        pa.setWindSpeed(50.0f);
        pa.setTemperature(-9999.f);
        pa.setDewpoint(-9999.f);
        pa.setGeoHeight(-9999.f);
        ppaa.add(pa);

        pa = new NcSoundingLayer();
        pa.setPressure(850.f);
        pa.setTemperature(-9999.f);
        pa.setDewpoint(-9999.f);
        pa.setGeoHeight(-9999.f);
        pa.setWindDirection(220.f);
        pa.setWindSpeed(20.0f);
        ppaa.add(pa);

        pa = new NcSoundingLayer();
        pa.setPressure(500.f);
        pa.setTemperature(-9999.f);
        pa.setDewpoint(-9999.f);
        pa.setWindDirection(240.f);
        pa.setWindSpeed(40.0f);
        pa.setGeoHeight(-9999.f);
        ppaa.add(pa);

        NcSoundingLayer pc;
        pc = new NcSoundingLayer();

        pc.setPressure(30.f);
        pc.setTemperature(-9999.f);
        pc.setDewpoint(-9999.f);
        pc.setWindDirection(230.f);
        pc.setWindSpeed(30.0f);
        pc.setGeoHeight(-9999.f);
        ppcc.add(pc);

        pc = new NcSoundingLayer();
        pc.setPressure(70.f);
        pc.setTemperature(-9999.f);
        pc.setDewpoint(-9999.f);
        pc.setWindDirection(270.f);
        pc.setWindSpeed(70.0f);
        pc.setGeoHeight(-9999.f);
        ppcc.add(pc);

        pc = new NcSoundingLayer();
        pc.setPressure(50.f);
        pc.setTemperature(-9999.f);
        pc.setDewpoint(-9999.f);
        pc.setWindDirection(250.f);
        pc.setWindSpeed(50.0f);
        pc.setGeoHeight(-9999.f);
        ppcc.add(pc);

        // Sorting the data
        sls = ms.mergeUairSounding("", ttaa, ttbb, ttcc, ttdd, ppaa, ppbb,
                ppcc, ppdd, trop_a, trop_c, wmax_a, wmax_c, 770.f);

        ms.printOut(sls);

    }

}
