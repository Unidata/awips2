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
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
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
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Main Text Editor dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------  --------------------------
 * 9/22/08                  jkorman     Initial creation.
 * 10/11/10     7294        cjeanbap    Changed access level of private 
 *                                      member variable.
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
public class TextProductInfo extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    public static final int DEFAULT_VERSIONS_TO_KEEP = 5;

    @EmbeddedId
    @DynamicSerializeElement
    @XmlElement
    TextProductInfoPK prodId;

    /** persistent field */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private int versionstokeep;

    /** full constructor */
    public TextProductInfo(String cccid, String nnnid, String xxxid) {
        prodId = new TextProductInfoPK(cccid, nnnid, xxxid);

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File versionsTable = pathMgr.getFile(lc,
                "versions_lookup_table.template");
        Map<TextProductInfoPK, Integer> versionMap = new HashMap<TextProductInfoPK, Integer>();
        String[] splitLine;
        String line;
        BufferedReader fis;
        try {
            fis = new BufferedReader(new InputStreamReader(new FileInputStream(
                    versionsTable)));
            for (line = fis.readLine(); line != null; line = fis.readLine()) {
                splitLine = line.trim().split("\\s+");
                try {
                    Integer versionsTK = Integer.valueOf(splitLine[1]);
                    TextProductInfoPK entry = new TextProductInfoPK(
                            splitLine[0].substring(0, 3),
                            splitLine[0].substring(3, 6),
                            splitLine[0].substring(6, 9));
                    if (entry.getCccid().equals("@@@")) {
                        entry.setCccid(lc.getContextName());
                    }
                    if (entry.getNnnid().equals("@@@")) {
                        entry.setNnnid(lc.getContextName());
                    }
                    if (entry.getXxxid().equals("@@@")) {
                        entry.setXxxid(lc.getContextName());
                    }
                    versionMap.put(entry, versionsTK);
                } catch (Exception e) {
                    continue;
                }
            }
            // Order of matching:
            if (versionMap.get(prodId) != null) {
                // Exact Match
                this.versionstokeep = versionMap.get(prodId);
                return;
            }
            // XXX wildcard
            prodId.setXxxid("XXX");
            if (versionMap.get(prodId) != null) {
                this.versionstokeep = versionMap.get(prodId);
                prodId.setCccid(cccid);
                prodId.setNnnid(nnnid);
                prodId.setXxxid(xxxid);
                return;
            }
            // CCC and XXX wildcards
            prodId.setCccid("CCC");
            if (versionMap.get(prodId) != null) {
                this.versionstokeep = versionMap.get(prodId);
                prodId.setCccid(cccid);
                prodId.setNnnid(nnnid);
                prodId.setXxxid(xxxid);
                return;
            }
            // Check for @@@NNNXXX
            prodId.setCccid(lc.getContextName());
            prodId.setNnnid("NNN");
            prodId.setXxxid("XXX");
            if (versionMap.get(prodId) != null) {
                this.versionstokeep = versionMap.get(prodId);
                prodId.setCccid(cccid);
                prodId.setNnnid(nnnid);
                prodId.setXxxid(xxxid);
                return;
            }
        } catch (Exception e) {
        }

        this.versionstokeep = DEFAULT_VERSIONS_TO_KEEP;
    }

    /** default constructor */
    public TextProductInfo() {
        prodId = new TextProductInfoPK();
        this.versionstokeep = DEFAULT_VERSIONS_TO_KEEP;
    }

    public TextProductInfoPK getProdId() {
        return prodId;
    }

    public void setProdId(TextProductInfoPK prodId) {
        this.prodId = prodId;
    }

    public int getVersionstokeep() {
        return this.versionstokeep;
    }

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
}
