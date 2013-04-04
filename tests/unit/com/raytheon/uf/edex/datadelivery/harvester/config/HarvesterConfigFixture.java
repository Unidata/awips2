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
package com.raytheon.uf.edex.datadelivery.harvester.config;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Collection;
import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Projection;
import com.raytheon.uf.common.datadelivery.registry.Projection.ProjectionType;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.AbstractFixture;

/**
 * {@link AbstractFixture} implementation for {@link HarvesterConfig}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012 1154       djohnson     Initial creation
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Jan 02, 2012 1345       djohnson     Fix broken code from removal of the setThreads method.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class HarvesterConfigFixture extends AbstractFixture<HarvesterConfig> {

    public static final HarvesterConfigFixture INSTANCE = new HarvesterConfigFixture();

    /**
     * Disabled constructor.
     */
    private HarvesterConfigFixture() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HarvesterConfig get(long seedValue) {
        HarvesterConfig config = new HarvesterConfig();

        CrawlAgent crawl = new CrawlAgent();
        crawl.setDateFormat("yyyyMMdd");
        crawl.setSearchKey(".info");
        crawl.setMaxMainDepth(2);
        crawl.setCrawlDir("/awips2/crawl/");
        crawl.setMaxMainPages(1000);
        crawl.setUseRobots(true);

        List<Collection> collections = new ArrayList<Collection>();
        Collection coll = new Collection("rap", "rap", "yyyyMMdd");
        coll.setIgnore(false);
        coll.setFirstDateAsDate(TimeUtil.newImmutableDate());
        coll.setLastDateAsDate(TimeUtil.newImmutableDate());
        collections.add(coll);
        crawl.setCollection(collections);

        // TODO: Once ticket 1102 is merged change to
        // ProviderFixture.INSTANCE.get(seedValue);
        Provider provider = new Provider();
        ArrayList<Projection> projections = new ArrayList<Projection>();
        Projection proj = new Projection();
        proj.setName(ProjectionType.LatLon.toString());
        proj.setType(ProjectionType.LatLon);
        projections.add(proj);
        provider.setConnection(new Connection());
        provider.setName("provider" + seedValue);

        config.setProvider(provider);
        config.setAgent(crawl);

        return config;
    }

}
