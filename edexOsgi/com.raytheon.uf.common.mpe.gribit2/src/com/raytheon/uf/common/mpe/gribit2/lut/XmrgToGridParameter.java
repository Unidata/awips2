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
package com.raytheon.uf.common.mpe.gribit2.lut;

import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang3.StringUtils;

/**
 * POJO representing a single xmrg to grid lookup parameter. Based on:
 * /rary.ohd.pproc.gribit/TEXT/bdxmrg.f.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "parameter")
public class XmrgToGridParameter {

    public static final String WILDCARD_CHAR = "*";

    /*
     * Possible package flags.
     */
    // simple
    public static final int PACKAGE_SIMPLE = 0;

    // complex (second order)
    public static final int PACKAGE_COMPLEX = 1;

    // second order spatial differencing
    public static final int PACKAGE_SPATIAL = 3;

    @XmlElement(required = true)
    private String proc;

    /*
     * A few proc Strings end with a '*' indicating that they will match any
     * proc that starts with everything before the '*'.
     */
    private boolean wildcardMatch;

    @XmlElement(required = true)
    private String gribParam;

    @XmlElement(required = true)
    private String wmoid;

    @XmlElement(required = true)
    private int modlid;

    @XmlElement(required = true)
    private int ngrid;

    @XmlElement(required = true)
    private int tunit;

    @XmlElement(required = true)
    private int nturef;

    @XmlElement(required = true)
    private int ntufc;

    @XmlElement(required = true)
    private int trang;

    @XmlElement(required = true)
    private int width;

    @XmlElement(required = true)
    private int binf;

    @XmlElement(required = true)
    private int dec;

    @XmlElement(required = true)
    private int pkflg;

    /**
     * Determines if {@link #proc} includes {@link #WILDCARD_CHAR}. When the
     * wildcard character is present, it is removed from {@link #proc} and the
     * {@link #wildcardMatch} flag is set.
     */
    public void checkForWildcard() {
        if (proc.endsWith(WILDCARD_CHAR)) {
            this.wildcardMatch = true;
            proc = proc.replaceAll(Pattern.quote(WILDCARD_CHAR),
                    StringUtils.EMPTY);
        }
    }

    /**
     * Determines if the specified proc {@link String} matches {@link #proc}.
     * The match must be a direct equals match if {@link #wildcardMatch} has not
     * been set. Or the specific proc must start with {@link #proc} when the
     * {@link #wildcardMatch} flag has been set.
     * 
     * @param compareProc
     *            the specified proc
     * @return {@code} true, when matching. {@code false}, otherwise.
     */
    public boolean procMatches(final String compareProc) {
        if (compareProc == null) {
            throw new IllegalArgumentException(
                    "Required argument 'compareProc' cannot be NULL.");
        }
        if (this.wildcardMatch) {
            return compareProc.startsWith(proc);
        } else {
            return proc.equals(compareProc);
        }
    }

    public String getProc() {
        return (this.wildcardMatch) ? proc + WILDCARD_CHAR : proc;
    }

    public void setProc(String proc) {
        this.proc = proc;
    }

    public String getGribParam() {
        return gribParam;
    }

    public void setGribParam(String gribParam) {
        this.gribParam = gribParam;
    }

    public String getWmoid() {
        return wmoid;
    }

    public void setWmoid(String wmoid) {
        this.wmoid = wmoid;
    }

    public int getModlid() {
        return modlid;
    }

    public void setModlid(int modlid) {
        this.modlid = modlid;
    }

    public int getNgrid() {
        return ngrid;
    }

    public void setNgrid(int ngrid) {
        this.ngrid = ngrid;
    }

    public int getTunit() {
        return tunit;
    }

    public void setTunit(int tunit) {
        this.tunit = tunit;
    }

    public int getNturef() {
        return nturef;
    }

    public void setNturef(int nturef) {
        this.nturef = nturef;
    }

    public int getNtufc() {
        return ntufc;
    }

    public void setNtufc(int ntufc) {
        this.ntufc = ntufc;
    }

    public int getTrang() {
        return trang;
    }

    public void setTrang(int trang) {
        this.trang = trang;
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public int getBinf() {
        return binf;
    }

    public void setBinf(int binf) {
        this.binf = binf;
    }

    public int getDec() {
        return dec;
    }

    public void setDec(int dec) {
        this.dec = dec;
    }

    public int getPkflg() {
        return pkflg;
    }

    public void setPkflg(int pkflg) {
        this.pkflg = pkflg;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("XmrgToGridParameter [");
        String originalProc = (wildcardMatch) ? proc + WILDCARD_CHAR : proc;
        sb.append("proc=").append(originalProc);
        sb.append(", gribParam=").append(gribParam);
        sb.append(", wmoid=").append(wmoid);
        sb.append(", modlid=").append(modlid);
        sb.append(", ngrid=").append(ngrid);
        sb.append(", tunit=").append(tunit);
        sb.append(", nturef=").append(nturef);
        sb.append(", ntufc=").append(ntufc);
        sb.append(", trang=").append(trang);
        sb.append(", width=").append(width);
        sb.append(", binf=").append(binf);
        sb.append(", dec=").append(dec);
        sb.append(", pkflg=").append(pkflg);
        sb.append("]");
        return sb.toString();
    }
}