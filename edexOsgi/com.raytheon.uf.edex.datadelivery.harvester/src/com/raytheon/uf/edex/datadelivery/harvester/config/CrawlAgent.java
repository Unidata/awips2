package com.raytheon.uf.edex.datadelivery.harvester.config;

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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.datadelivery.registry.Collection;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * DD Crawler Agent
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 Sept, 2012   1038      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class CrawlAgent extends Agent implements ISerializableObject {

    // These top 3 have default settings
    @XmlElement(name = "crawlDir")
    @DynamicSerializeElement
    private String crawlDir = "/awips2/crawl";

    @XmlElement(name = "dateFormat")
    @DynamicSerializeElement
    private String dateFormat = "HHddMMMyyyy";

    @XmlElement(name = "ignore")
    @DynamicSerializeElement
    private List<String> ignore;

    @XmlElement(name = "useRobots")
    @DynamicSerializeElement
    private boolean useRobots = true;

    @XmlElement(name = "seedScan", required = true)
    @DynamicSerializeElement
    private String seedScan;

    @XmlElement(name = "mainScan", required = true)
    @DynamicSerializeElement
    private String mainScan;

    @XmlElement(name = "searchKey", required = true)
    @DynamicSerializeElement
    private String searchKey;

    @XmlElements({ @XmlElement(name = "collection") })
    @DynamicSerializeElement
    private List<Collection> collection;

    @XmlElement(name = "ingestNew", required = true)
    @DynamicSerializeElement
    private boolean ingestNew = true;

    @XmlElement(name = "maxSeedDepth")
    @DynamicSerializeElement
    private int maxSeedDepth = 2;

    @XmlElement(name = "maxSeedPages")
    @DynamicSerializeElement
    private int maxSeedPages = 1000;

    @XmlElement(name = "maxMainDepth")
    @DynamicSerializeElement
    private int maxMainDepth = 2;

    @XmlElement(name = "maxMainPages")
    @DynamicSerializeElement
    private int maxMainPages = 1000;

    public CrawlAgent() {

    }

    public List<Collection> getCollection() {
        return collection;
    }

    /**
     * Get a collection by it's name
     * 
     * @param name
     * @return
     */
    public Collection getCollectionByName(String name) {

        for (Collection collection : getCollection()) {
            if (collection.getName().equals(name)) {
                return collection;
            }
        }

        return null;
    }

    public String getCrawlDir() {
        return crawlDir;
    }

    public String getDateFormat() {
        return dateFormat;
    }

    public List<String> getIgnore() {
        return ignore;
    }

    public String getMainScan() {
        return mainScan;
    }

    public int getMaxMainDepth() {
        return maxMainDepth;
    }

    public int getMaxMainPages() {
        return maxMainPages;
    }

    public int getMaxSeedDepth() {
        return maxSeedDepth;
    }

    public int getMaxSeedPages() {
        return maxSeedPages;
    }

    public String getSearchKey() {
        return searchKey;
    }

    public String getSeedScan() {
        return seedScan;
    }

    public boolean isIngestNew() {
        return ingestNew;
    }

    /**
     * Check to see if it is up to date as far as seed scanning
     * 
     * @param collectionName
     * @return
     */
    public boolean isMature(String collectionName) {
        boolean isMature = true;

        Collection coll = getCollectionByName(collectionName);

        if (coll.getPeriodicity() == null) {
            isMature = false;
        }
        if (coll.getLastDate() == null) {
            isMature = false;
        }
        if (coll.getFirstDate() == null) {
            isMature = false;
        }

        return isMature;
    }

    public boolean isUseRobots() {
        return useRobots;
    }

    public void setCollection(List<Collection> collection) {
        this.collection = collection;
    }

    public void setCrawlDir(String crawlDir) {
        this.crawlDir = crawlDir;
    }

    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    public void setIgnore(List<String> ignore) {
        this.ignore = ignore;
    }

    public void setIngestNew(boolean ingestNew) {
        this.ingestNew = ingestNew;
    }

    public void setMainScan(String mainScan) {
        this.mainScan = mainScan;
    }

    public void setMaxMainDepth(int maxMainDepth) {
        this.maxMainDepth = maxMainDepth;
    }

    public void setMaxMainPages(int maxMainPages) {
        this.maxMainPages = maxMainPages;
    }

    public void setMaxSeedDepth(int maxSeedDepth) {
        this.maxSeedDepth = maxSeedDepth;
    }

    public void setMaxSeedPages(int maxSeedPages) {
        this.maxSeedPages = maxSeedPages;
    }

    public void setSearchKey(String searchKey) {
        this.searchKey = searchKey;
    }

    public void setSeedScan(String seedScan) {
        this.seedScan = seedScan;
    }

    public void setUseRobots(boolean useRobots) {
        this.useRobots = useRobots;
    }

}
