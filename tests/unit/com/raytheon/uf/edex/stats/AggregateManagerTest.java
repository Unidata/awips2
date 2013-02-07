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
package com.raytheon.uf.edex.stats;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.collect.Maps;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.stats.StatsGrouping;
import com.raytheon.uf.common.stats.StatsGroupingColumn;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;
import com.raytheon.uf.common.stats.xml.StatisticsEvent;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.stats.util.ConfigLoader;

/**
 * Test {@link AggregateManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2013 1487       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class AggregateManagerTest {
    private static JAXBManager jaxbManager;

    @BeforeClass
    public static void classSetUp() throws JAXBException {
        jaxbManager = new JAXBManager(StatisticsConfig.class,
                StatsGroupingColumn.class);
    }

    @Before
    public void setUp() {
        PathManagerFactoryTest.initLocalization();
    }

    @Test
    public void testDeterminingGroupForEvent() throws Exception {
        IPathManager pm = PathManagerFactory.getPathManager();
        final LocalizationFile lf = pm.getLocalizationFile(
                new LocalizationContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.BASE), FileUtil.join("stats",
                        "mockStats.xml"));

        final StatisticsConfig statisticsConfig = lf.jaxbUnmarshal(
                StatisticsConfig.class, jaxbManager);

        ConfigLoader.validate(Maps.<String, StatisticsEvent> newHashMap(),
                statisticsConfig);

        MockEvent mockEvent = new MockEvent();
        mockEvent.setPluginName("somePlugin");
        mockEvent.setFileName("someFileName");
        mockEvent.setProcessingTime(1000L);
        mockEvent.setProcessingLatency(500L);

        List<StatsGrouping> groupList = new ArrayList<StatsGrouping>();
        groupList.add(new StatsGrouping("pluginName", "somePlugin"));
        groupList.add(new StatsGrouping("fileName", "someFileName"));
        StatsGroupingColumn column = new StatsGroupingColumn();
        column.setGroup(groupList);

        final String expectedGroupRepresentation = jaxbManager
                .marshalToXml(column);
        final String actualGroupRepresentation = AggregateManager.determineGroupRepresentationForEvent(
                statisticsConfig.getEvents().iterator().next(), mockEvent);
        assertThat(actualGroupRepresentation,
                is(equalTo(expectedGroupRepresentation)));
    }

}
