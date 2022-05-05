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
package com.raytheon.uf.common.dataplugin.text.db;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Main Text Editor dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 22, 2008           jkorman   Initial creation.
 * Oct 11, 2010  7294     cjeanbap  Changed access level of private member
 *                                  variable.
 * Apr 30, 2018  7278     randerso  Code cleanup and optimization. Fixed wild
 *                                  card logic for default versions to keep.
 * Jan 13, 2021  7864     randerso  Removed redundant check for null in
 *                                  getSiteNode as it is now done in
 *                                  SiteMap.getAFOSTableMap().
 *
 * </pre>
 *
 * @author cjeanbap
 */
@Entity
@Table
@XmlRootElement
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class TextProductInfo extends PersistableDataObject<TextProductInfoPK> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextProductInfo.class);

    private static final long serialVersionUID = 1L;

    private static final int DEFAULT_VERSIONS_TO_KEEP = 5;

    private static Map<TextProductInfoPK, Integer> defaultVersionsMap = new HashMap<>();

    private static Date lastRead = new Date(0);

    private static String localCCC = null;

    @EmbeddedId
    @DynamicSerializeElement
    @XmlElement
    private TextProductInfoPK prodId;

    /** persistent field */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private int versionstokeep;

    /**
     * full constructor
     *
     * @param cccid
     * @param nnnid
     * @param xxxid
     */
    public TextProductInfo(String cccid, String nnnid, String xxxid) {
        prodId = new TextProductInfoPK(cccid, nnnid, xxxid);

        Map<TextProductInfoPK, Integer> versionMap = getDefaultVersionsMap();
        TextProductInfoPK testId = new TextProductInfoPK(cccid, nnnid, xxxid);

        // Order of matching:

        // exact match
        Integer versions = versionMap.get(testId);

        // xxx wildcard
        if (versions == null) {
            testId.setXxxid("XXX");
            versions = versionMap.get(testId);
        }

        // ccc and xxx wildcards
        if (versions == null) {
            testId.setCccid("CCC");
            versions = versionMap.get(testId);
        }

        // Check for @@@NNNXXX
        if (versions == null) {
            testId.setCccid(localCCC);
            testId.setNnnid("NNN");
            versions = versionMap.get(testId);
        }

        if (versions == null) {
            this.versionstokeep = DEFAULT_VERSIONS_TO_KEEP;
        } else {
            this.versionstokeep = versions;
        }
    }

    /** default constructor */
    public TextProductInfo() {
        prodId = new TextProductInfoPK();
        this.versionstokeep = DEFAULT_VERSIONS_TO_KEEP;
    }

    /**
     * @return the product id
     */
    public TextProductInfoPK getProdId() {
        return prodId;
    }

    /**
     * @param prodId
     */
    public void setProdId(TextProductInfoPK prodId) {
        this.prodId = prodId;
    }

    /**
     * @return the versions to keep
     */
    public int getVersionstokeep() {
        return this.versionstokeep;
    }

    /**
     * @param versionstokeep
     */
    public void setVersionstokeep(int versionstokeep) {
        this.versionstokeep = versionstokeep;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("TextProductInfo {");

        sb.append(prodId.toString());
        sb.append(":");
        sb.append(versionstokeep);
        sb.append("}");
        return sb.toString();
    }

    private Map<TextProductInfoPK, Integer> getDefaultVersionsMap() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        ILocalizationFile lf = pathMgr.getLocalizationFile(lc,
                "versions_lookup_table.template");

        if (lf.getTimeStamp().after(lastRead)) {
            synchronized (TextProductInfo.class) {
                String localXXX = pathMgr
                        .getContext(LocalizationType.COMMON_STATIC,
                                LocalizationLevel.SITE)
                        .getContextName();

                localCCC = getSiteNode(localXXX);

                defaultVersionsMap.clear();
                String[] splitLine;
                String line;
                int lineNumber = 0;
                try (BufferedReader fis = new BufferedReader(
                        new InputStreamReader(lf.openInputStream()))) {
                    for (line = fis.readLine(); line != null; line = fis
                            .readLine()) {
                        lineNumber++;
                        splitLine = line.trim().split("\\s+");
                        try {
                            Integer versionsTK = Integer.valueOf(splitLine[1]);
                            TextProductInfoPK entry = new TextProductInfoPK(
                                    splitLine[0].substring(0, 3),
                                    splitLine[0].substring(3, 6),
                                    splitLine[0].substring(6, 9));
                            if ("@@@".equals(entry.getCccid())) {
                                entry.setCccid(localCCC);
                            }

                            if ("@@@".equals(entry.getXxxid())) {
                                entry.setXxxid(localXXX);
                            }
                            defaultVersionsMap.put(entry, versionsTK);
                        } catch (Exception e) {
                            statusHandler.error(
                                    String.format("Error in %d line of %s\n%s",
                                            lineNumber, lf, line),
                                    e);
                            continue;
                        }
                    }
                    lastRead = new Date();
                } catch (LocalizationException | IOException e) {
                    statusHandler.error("Error reading " + lf, e);
                }
            }
        }
        return defaultVersionsMap;
    }

    /**
     * Grabs the site node from the afos_lookup_table.dat file.
     *
     * @param threeLetterSiteId
     *            the 3 letter site id e.g. "OAX"
     *
     * @return The 3 letter site node, e.g. "OMA" or empty string when query
     *         does not return a result
     */
    private static String getSiteNode(String threeLetterSiteId) {
        if (threeLetterSiteId == null) {
            return "";
        }
        SiteMap siteMap = SiteMap.getInstance();
        String fourLetterId = siteMap.getSite4LetterId(threeLetterSiteId);
        String rval = siteMap.getAFOSTableMap(fourLetterId);
        return rval;
    }

}
