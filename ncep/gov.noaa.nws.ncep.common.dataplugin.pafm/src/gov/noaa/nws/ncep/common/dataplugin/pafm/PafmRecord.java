/*
 * 
 * PafmRecord
 * 
 * This class performs the mapping to the database tables for the Point/Area
 * Forecast Matrices (PAFM) Decoder Plug-In
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer		Description
 * ------------	----------- --------------	-----------------------------------
 * 08/05/2009	126			F. J. Yen		Initial creation
 * 01/06/2010   126	 		F. J. Yen		Migrated and refactored from to11dr3 to to11dr11
 * *
 * This code has been develped by the SIB for use in the AWIPS2 system.
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1
 
 */

package gov.noaa.nws.ncep.common.dataplugin.pafm;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * PafmRecord is the Data Access component for PAFM Point/Area Forecast Matrices
 * data. This contains getters and setters for the main parent table pafm. This
 * code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 23Jun2009    126			F. J. Yen	Initial Coding.
 * 22Sep2009	126			F. J. Yen	Increase size of column bullMessage
 * 28Sep2009	126			F. J. Yen	Delete column bullMessage
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1.0
 */
@Entity
@Table(name = "pafm", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PafmRecord extends PluginDataObject {

    private static final long serialVersionUID = 1L;

    /** Report type */
    @Column(length = 32)
    @XmlElement
    @DynamicSerializeElement
    private String reportType;

    // WMO header
    @DataURI(position = 4)
    @Column(length = 32)
    @XmlElement
    @DynamicSerializeElement
    private String wmoHeader;

    // The issue office where the report from
    @Column(length = 32)
    @DataURI(position = 1)
    @XmlElement
    @DynamicSerializeElement
    private String issueOffice;

    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    @XmlElement
    private Calendar issueTime;

    @DataURI(position = 3)
    @Column(length = 8)
    @DynamicSerializeElement
    @XmlElement
    private String designatorBBB;

    // The mndTime
    @Column(length = 72)
    @XmlElement
    @DynamicSerializeElement
    private String mndTime;

    /** Matrix type */
    @DataURI(position = 5)
    @Column(length = 32)
    @XmlElement
    @DynamicSerializeElement
    private String matrixType;

    /** PAFM UGC Child Table */
    @XmlElement
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JoinColumn(name = "parentId", nullable = false)
    @Index(name = "pafm_ugc_parentid_idex")
    private Set<PafmUgc> pafmUGC = new HashSet<PafmUgc>();

    /**
     * Default Constructor
     */
    public PafmRecord() {
        this.issueOffice = null;
        this.reportType = "PAFM";
        this.matrixType = " ";
        this.issueTime = null;
        this.wmoHeader = null;
        this.designatorBBB = " ";
        this.mndTime = null;
    }

    /**
     * Constructs a pafm record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public PafmRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    public String getReportType() {
        return reportType;
    }

    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    public String getMatrixType() {
        return matrixType;
    }

    public void setMatrixType(String matrixType) {
        this.matrixType = matrixType;
    }

    public String getWmoHeader() {
        return wmoHeader;
    }

    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    public String getIssueOffice() {
        return issueOffice;
    }

    public void setIssueOffice(String issueOffice) {
        this.issueOffice = issueOffice;
    }

    public Calendar getIssueTime() {
        return issueTime;
    }

    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    public String getDesignatorBBB() {
        return designatorBBB;
    }

    public void setDesignatorBBB(String designatorBBB) {
        this.designatorBBB = designatorBBB;
    }

    public String getMndTime() {
        return mndTime;
    }

    public void setMndTime(String mndTime) {
        this.mndTime = mndTime;
    }

    /**
     * return the set of pafmUgc
     */
    public Set<PafmUgc> getPafmUGC() {
        return pafmUGC;
    }

    /**
     * @param pafmUgc
     *            the pafmUgc to set
     */
    public void setPafmUgc(Set<PafmUgc> pafmUgcs) {
        this.pafmUGC = pafmUgcs;
    }

    /*
     * Add pafmUgc to set
     */
    public void addPafmUGC(PafmUgc pugc) {
        pafmUGC.add(pugc);
    }

    /**
     * Override existing set method to modify any classes that use the dataURI
     * as a foreign key
     */
    public void setIdentifier(Object dataURI) {
        this.identifier = dataURI;
    }

}
