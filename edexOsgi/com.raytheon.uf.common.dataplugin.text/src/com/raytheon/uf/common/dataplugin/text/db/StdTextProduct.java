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

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.CRC32;

import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.Lob;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * The base Standard Text Product record
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 *                                  Initial Implementation
 * Oct 15, 2008  1538     jkorman   Core functions added.
 * Jul 10, 2009  2191     rjpeter   Reimplemented.
 * Apr 06, 2010  4734     mhuang    Moved from edex server
 * May 17, 2010  2187     cjeanbap  Change class to be Abstract
 * May 27, 2012  647      dgilling  Implement getIdentifier/setIdentifier.
 * Nov 05, 2013  2499     rjpeter   Fix generics.
 * May 14, 2014  2536     bclement  moved WMO Header to common, removed
 *                                  ISerializableObject
 * Oct 29, 2014  3454     bphillip  Fixed text archiver
 * Apr 26, 2018  6966     randerso  Code cleanup.
 * Dec 04, 2018  7665     tgurney   getASCIIProduct delete control characters
 *                                  instead of replacing them
 * Dec 04, 2018  7665     tgurney   Rename getASCIIProduct to
 *                                  getPrintableProduct
 * Jun 28, 2019  7625     randerso  Changed WMO_END_OF_LINE_PATTERN to match
 *                                  newline preceded by 1 or more carriage
 *                                  returns.
 * May 16, 2019 21048 mgamazaychikov Added wmoid and cccid,
 *                                  changed how datacrc is generated
 *
 * </pre>
 *
 * @author jkorman
 */
@Entity
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class StdTextProduct
        extends PersistableDataObject<StdTextProductId> {

    private static final long serialVersionUID = 1L;

    private static final Pattern WMO_END_OF_LINE_PATTERN = Pattern
            .compile("\r+\n");
    
    private static final Pattern WMOHEADING_END_OF_LINE_PATTERN = Pattern
            .compile("\r?\n");

    private static final Pattern NULL_PATTERN = Pattern.compile("\0");

    private static final Pattern CONTROL_CHARACTER_PATTERN = Pattern
            .compile("[\\x00-\\x08\\x0E-\\x1F\\x7F]");

    @EmbeddedId
    @DynamicSerializeElement
    @XmlElement
    private StdTextProductId prodId;

    /** nullable persistent field */
    @Column(length = 3)
    @DynamicSerializeElement
    @XmlAttribute
    private String bbbid;

    /** persistent field */
    @Column(nullable = false)
    @DynamicSerializeElement
    @XmlAttribute
    private Long refTime;

    /**
     * The timestamp denoting when this record was inserted into the database
     */
    @Column(columnDefinition = "timestamp without time zone", nullable = false)
    @Index(name = "insertTimeIndex")
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar insertTime;

    /** persistent field */
    @Column(nullable = false)
    @Lob
    @Type(type = "org.hibernate.type.TextType")
    @DynamicSerializeElement
    @XmlElement
    private String product;

    @Column(nullable = false, length = 6)
    @DynamicSerializeElement
    @XmlAttribute
    private String wmoid;

    @Column(nullable = false, length = 3)
    @DynamicSerializeElement
    @XmlAttribute
    private String cccid;

    @Transient
    private String wrkwgn;

    public StdTextProduct() {
        setProdId(new StdTextProductId());
    }

    public StdTextProduct(WMOHeader wmoHeader, AFOSProductId afosId,
            String product) {
        this(wmoHeader.getTtaaii(), wmoHeader.getCccc(), afosId.getCcc(),
                afosId.getNnn(), afosId.getXxx(), wmoHeader.getYYGGgg(),
                wmoHeader.getBBBIndicator(), System.currentTimeMillis(),
                product);
    }

    public StdTextProduct(String wmoid, String site, String cccid, String nnnid,
            String xxxid, String hdrtime, String bbbid, Long createtime,
            String product) {
        setWmoid(wmoid);
        setCccid(cccid);
        setProdId(new StdTextProductId(site, nnnid, xxxid,
                hdrtime));
        setProduct(product);
        setRefTime(createtime);
        setBbbid(bbbid);
    }

    public StdTextProduct(StdTextProductId prodIdToCopy, String bbbid, Long createtime, String product, String wmoid,
            String cccid) {
        setWmoid(wmoid);
        setCccid(cccid);
        setProdId(prodIdToCopy);
        setProduct(product);
        setRefTime(createtime);
        setBbbid(bbbid);
    }

    public StdTextProduct(String site, String nnnid, String xxxid, String hdrtime, String bbbid, Long createtime,
            String product) {
        setProdId(new StdTextProductId(site, nnnid, xxxid,
                hdrtime));
        setProduct(product);
        setRefTime(createtime);
        setBbbid(bbbid);
    }

    public StdTextProduct(StdTextProductId prodIdToCopy, String bbbid,
            Long createtime, String product) {
        this(prodIdToCopy.getSite(), prodIdToCopy.getNnnid(),
                prodIdToCopy.getXxxid(), prodIdToCopy.getHdrtime(), bbbid,
                createtime, product);
    }

    /**
     * copy constructor
     *
     * All elements are immutable so no need to do new operations.
     */
    public StdTextProduct(StdTextProduct aProductToCopy) {
        this(aProductToCopy.getProdId(), aProductToCopy.getBbbid(),
                aProductToCopy.getRefTime(), aProductToCopy.getProduct());
    }

    @Override
    public StdTextProductId getIdentifier() {
        return this.prodId;
    }

    @Override
    public void setIdentifier(StdTextProductId identifier) {
        setProdId(identifier);
    }

    public String getBbbid() {
        return this.bbbid;
    }

    public void setBbbid(String bbbid) {
        this.bbbid = bbbid;
    }

    public Long getRefTime() {
        return this.refTime;
    }

    public void setRefTime(Long refTime) {
        this.refTime = refTime;
    }

    public void setInsertTime(Calendar insertTime) {
        this.insertTime = insertTime;
    }

    public Calendar getInsertTime() {
        return insertTime;
    }

    public String getProduct() {
        return this.product;
    }

    /**
     * @return The product with control characters removed (excluding backspace,
     *         horizontal and vertical tab, carriage return, newline, and form
     *         feed).
     */
    public String getPrintableProduct() {
        Matcher m = CONTROL_CHARACTER_PATTERN.matcher(this.product);
        return m.replaceAll("");
    }

    public StdTextProductId getProdId() {
        return prodId;
    }

    public void setProdId(StdTextProductId id) {
        this.prodId = id;
    }

    public String getWmoid() {
        return wmoid;
    }

    public void setWmoid(String wmoid) {
        this.wmoid = wmoid;
    }

    public String getSite() {
        return this.prodId.getSite();
    }

    public void setSite(String site) {
        this.prodId.setSite(site);
    }

    public String getCccid() {
        return cccid;
    }

    public void setCccid(String cccid) {
        this.cccid = cccid;
    }

    public String getNnnid() {
        return this.prodId.getNnnid();
    }

    public void setNnnid(String nnnid) {
        this.prodId.setNnnid(nnnid);
    }

    public String getXxxid() {
        return this.prodId.getXxxid();
    }

    public void setXxxid(String xxxid) {
        this.prodId.setXxxid(xxxid);
    }

    public String getHdrtime() {
        return this.prodId.getHdrtime();
    }

    public void setHdrtime(String hdrtime) {
        this.prodId.setHdrtime(hdrtime);
    }

    public Long getDataCrc() {
        return this.prodId.getDataCrc();
    }

    public void setDataCrc(Long dataCrc) {
        this.prodId.setDataCrc(dataCrc);
    }

    public String getWrkwgn() {
        return wrkwgn;
    }

    public void setWrkwgn(String wrkwgn) {
        this.wrkwgn = wrkwgn;
    }

    private void generateCrc() {
        if (prodId != null) {
            if (product != null) {
                String nnnid = prodId.getNnnid();
                CRC32 crc = new CRC32();
                if (SiteMap.getInstance().isDuplicateNNN(nnnid)) {
                    // calculate the datacrc based on the text of the message
                    // only
                    Matcher matcher = WMOHEADING_END_OF_LINE_PATTERN.matcher(product);
                    int indx =  matcher.find() ? matcher.start() : -1;
                    String productBody = null;
                    if (indx !=-1) {
                        productBody = product.substring(indx+1).replaceAll("\\s+", " ").trim();
                    } else {
                        productBody = product.replaceAll("\\s+", " ").trim();
                    }
                    crc.update(productBody.getBytes());
                } else {
                    crc.update(product.getBytes());
                }
                prodId.setDataCrc(crc.getValue());
            } else {
                prodId.setDataCrc(null);
            }
        }
    }

    public void setProduct(String product) {
        Matcher matcher = WMO_END_OF_LINE_PATTERN.matcher(product);
        matcher = NULL_PATTERN.matcher(matcher.replaceAll("\n"));
        this.product = matcher.replaceAll("");
        generateCrc();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result) + ((bbbid == null) ? 0 : bbbid.hashCode());
        result = (prime * result)
                + ((refTime == null) ? 0 : refTime.hashCode());
        result = (prime * result) + ((prodId == null) ? 0 : prodId.hashCode());
        result = (prime * result)
                + ((product == null) ? 0 : product.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        StdTextProduct other = (StdTextProduct) obj;
        if (bbbid == null) {
            if (other.bbbid != null) {
                return false;
            }
        } else if (!bbbid.equals(other.bbbid)) {
            return false;
        }
        if (refTime == null) {
            if (other.refTime != null) {
                return false;
            }
        } else if (!refTime.equals(other.refTime)) {
            return false;
        }
        if (prodId == null) {
            if (other.prodId != null) {
                return false;
            }
        } else if (!prodId.equals(other.prodId)) {
            return false;
        }
        if (product == null) {
            if (other.product != null) {
                return false;
            }
        } else if (!product.equals(other.product)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this)
                .append("id", (prodId == null) ? null : prodId.toString())
                .toString();
    }
}
