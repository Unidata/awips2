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
package com.raytheon.uf.common.dataplugin.scan.data;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import org.locationtech.jts.geom.Coordinate;

/**
 *
 * SCAN Cell Table Data Row
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------------
 * Apr 29, 2009  2037     dhladky   Initial creation
 * Feb 01, 2013  1569     dhladky   removed XML where not needed
 * May 13, 2014  3133     njensen   Use ScanUtils instead of ScanConfig
 * Apr 04, 2018  6696     randerso  Code cleanup
 *
 * </pre>
 *
 * @author dhladky
 *
 */
@DynamicSerialize
public class CellTableDataRow extends ScanTableDataRow {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /** cell rank in table **/
    @DynamicSerializeElement
    private Integer rank = 0;

    /** cell TVS **/
    @DynamicSerializeElement
    private String tvs = TVS_TYPE.NONE.getTVSName();

    /** meso cyclone detection alg rank **/
    @DynamicSerializeElement
    private String mdaSR = null;

    /** prob of severe hail **/
    @DynamicSerializeElement
    private Integer posh = 0;

    /** prob of hail **/
    @DynamicSerializeElement
    private Integer poh = 0;

    /** prob of large hail **/
    @DynamicSerializeElement
    private Integer polh = 0;

    /** hail size, inches **/
    @DynamicSerializeElement
    private Double hsize = 0.0;

    /** Vertically Integrated Liquid **/
    @DynamicSerializeElement
    private Double vil = 0.0;

    /** max reflectivity Decibel return **/
    @DynamicSerializeElement
    private Double dbz = 0.0;

    /** height of max reflectivity dbz return **/
    @DynamicSerializeElement
    private Double dbzHt = 0.0;

    /** top of storm in kft **/
    @DynamicSerializeElement
    private Double top = 0.0;

    /** 15 min forecast azm **/
    @DynamicSerializeElement
    private Double azm15 = 0.0;

    /** 15 min forecast range **/
    @DynamicSerializeElement
    private Double rng15 = 0.0;

    /** 30 min forecast azm **/
    @DynamicSerializeElement
    private Double azm30 = 0.0;

    /** 30 min forecast range **/
    @DynamicSerializeElement
    private Double rng30 = 0.0;

    /** 45 min forecast azm **/
    @DynamicSerializeElement
    private Double azm45 = 0.0;

    /** 45 min forecast range **/
    @DynamicSerializeElement
    private Double rng45 = 0.0;

    /** 60 min forecast azm **/
    @DynamicSerializeElement
    private Double azm60 = 0.0;

    /** 60 min forecast range **/
    @DynamicSerializeElement
    private Double rng60 = 0.0;

    /** move error **/
    @DynamicSerializeElement
    private Double mvtErr = 0.0;

    /** move error mean **/
    @DynamicSerializeElement
    private Double mvtMn = 0.0;

    /** severe wx prob **/
    @DynamicSerializeElement
    private Integer svrwx = 0;

    /** heavy precip % **/
    @DynamicSerializeElement
    private Integer hvyPr = 0;

    /** % positive ligtning strikes **/
    @DynamicSerializeElement
    private Double pos = 0.0;

    /** % cloud to ground strikes **/
    @DynamicSerializeElement
    private Double cgRate = 0.0;

    /** Volume Coverage Pattern **/
    @DynamicSerializeElement
    private Integer vcp = 0;

    /** CAPE Convective Available Potential Energy **/
    @DynamicSerializeElement
    private Double cape = -99999.0;

    /** Storm Relative Helicity **/
    @DynamicSerializeElement
    private Double sreh = -99999.0;

    @DynamicSerializeElement
    private Map<Date, Coordinate> pastCoordinates;

    /**
     * Default constructor for serialization
     */
    public CellTableDataRow() {

    }

    /**
     * Constructor
     *
     * @param time
     */
    public CellTableDataRow(DataTime time) {
        super(time);
    }

    /**
     * @return the rank
     */
    public Integer getRank() {
        return rank;
    }

    /**
     * Sets the rank
     *
     * @param rank
     */
    public void setRank(Integer rank) {
        this.rank = rank;
    }

    /**
     * @return the TVS
     */
    public String getTvs() {
        return tvs;
    }

    /**
     * Sets the TVS
     *
     * @param tvs
     */
    public void setTvs(String tvs) {
        this.tvs = tvs;
    }

    /**
     * @return the Meso detection strength
     */
    public String getMdaSR() {
        return mdaSR;
    }

    /**
     * Set Meso Detection Strength
     *
     * @param mdaSR
     */
    public void setMdaSR(String mdaSR) {
        this.mdaSR = mdaSR;
    }

    /**
     * @return the Probability of severe hail
     */
    public Integer getPosh() {
        return posh;
    }

    /**
     * Set the posh
     *
     * @param posh
     */
    public void setPosh(Integer posh) {
        this.posh = posh;
    }

    /**
     * @return the probability of hail
     */
    public Integer getPoh() {
        return poh;
    }

    /**
     * Set the poh
     *
     * @param poh
     */
    public void setPoh(Integer poh) {
        this.poh = poh;
    }

    /**
     * @return the probability of large hail
     */
    public Integer getPolh() {
        return polh;
    }

    /**
     * Set the polh
     *
     * @param polh
     */
    public void setPolh(Integer polh) {
        this.polh = polh;
    }

    /**
     * @return the hail size
     */
    public Double getHsize() {
        return hsize;
    }

    /**
     * Set the hail size
     *
     * @param hsize
     */
    public void setHsize(Double hsize) {
        this.hsize = hsize;
    }

    /**
     * @return the Vertically integrated liquid
     */
    public Double getVil() {
        return vil;
    }

    /**
     * Sets the VIL
     *
     * @param vil
     */
    public void setVil(Double vil) {
        this.vil = vil;
    }

    /**
     * @return the max Decibel return of radar
     */
    public Double getDbz() {
        return dbz;
    }

    /**
     * Set the max reflectivity decibel
     *
     * @param dbz
     */
    public void setDbz(Double dbz) {
        this.dbz = dbz;
    }

    /**
     * @return the max decibel return height
     */
    public Double getDbzHt() {
        return dbzHt;
    }

    /**
     * Set the height of the max reflectivity decibel
     *
     * @param dbzHt
     */
    public void setDbzHt(Double dbzHt) {
        this.dbzHt = dbzHt;
    }

    /**
     * @return the storm top in kft
     */
    public Double getTop() {
        return top;
    }

    /**
     * Gets the cell (storm) height
     *
     * @param top
     */
    public void setTop(Double top) {
        this.top = top;
    }

    /**
     * @return the 15 min forecast azm
     */
    public Double getAzm15() {
        return azm15;
    }

    /**
     * Set the 15 min azimuth
     *
     * @param azm15
     */
    public void setAzm15(Double azm15) {
        this.azm15 = azm15;
    }

    /**
     * @return the 15 min forecast range
     */
    public Double getRng15() {
        return rng15;
    }

    /**
     * Set the 15 minute range
     *
     * @param rng15
     */
    public void setRng15(Double rng15) {
        this.rng15 = rng15;
    }

    /**
     * @return the 30 min forecast azm
     */
    public Double getAzm30() {
        return azm30;
    }

    /**
     * Set the 15 minute azimuth
     *
     * @param azm30
     */
    public void setAzm30(Double azm30) {
        this.azm30 = azm30;
    }

    /**
     * @return the 30 min forecast range
     */
    public Double getRng30() {
        return rng30;
    }

    /**
     * Set the 30 minute range
     *
     * @param rng30
     */
    public void setRng30(Double rng30) {
        this.rng30 = rng30;
    }

    /**
     * @return the 45 min forecast azm
     */
    public Double getAzm45() {
        return azm45;
    }

    /**
     * Set the 45 minute azimuth
     *
     * @param azm45
     */
    public void setAzm45(Double azm45) {
        this.azm45 = azm45;
    }

    /**
     * @return the 45 min forecast range
     */
    public Double getRng45() {
        return rng45;
    }

    /**
     * Set the 45 minute range
     *
     * @param rng45
     */
    public void setRng45(Double rng45) {
        this.rng45 = rng45;
    }

    /**
     * @return the 60 min forecast azm
     */
    public Double getAzm60() {
        return azm60;
    }

    /**
     * Set the 60 minute range
     *
     * @param azm60
     */
    public void setAzm60(Double azm60) {
        this.azm60 = azm60;
    }

    /**
     * @return the 60 min forecast range
     */
    public Double getRng60() {
        return rng60;
    }

    /**
     * Set the 60 minute range
     *
     * @param rng60
     */
    public void setRng60(Double rng60) {
        this.rng60 = rng60;
    }

    /**
     * @return the movement error
     */
    public Double getMvtErr() {
        return mvtErr;
    }

    /**
     * Set the movement error
     *
     * @param mvtErr
     */
    public void setMvtErr(Double mvtErr) {
        this.mvtErr = mvtErr;
    }

    /**
     * @return the movement mean error
     */
    public Double getMvtMn() {
        return mvtMn;
    }

    /**
     * set movement mean error
     *
     * @param mvtMn
     */
    public void setMvtMn(Double mvtMn) {
        this.mvtMn = mvtMn;
    }

    /**
     * @return the latitude
     */
    @Override
    public Double getLat() {
        return lat;
    }

    /**
     * set the latitude
     *
     * @param lat
     */
    @Override
    public void setLat(Double lat) {
        this.lat = lat;
    }

    /**
     * @return the longitude
     */
    @Override
    public Double getLon() {
        return lon;
    }

    /**
     * set the longitude
     *
     * @param lon
     */
    @Override
    public void setLon(Double lon) {
        this.lon = lon;
    }

    /**
     * @return the Heavy precip %
     */
    public Integer getHvyPr() {
        return hvyPr;
    }

    /**
     * Set the Heavy precip %
     *
     * @param hvyPr
     */
    public void setHvyPr(Integer hvyPr) {
        this.hvyPr = hvyPr;
    }

    /**
     * @return the sever WX prob
     */
    public Integer getSvrwx() {
        return svrwx;
    }

    /**
     * Set the sever WX prob
     *
     * @param svrwx
     */
    public void setSvrwx(Integer svrwx) {
        this.svrwx = svrwx;
    }

    /**
     * @return the positive lght strikes
     */
    public Double getPos() {
        return pos;
    }

    /**
     * set the positive lght strikes
     *
     * @param pos
     */
    public void setPos(Double pos) {
        this.pos = pos;
    }

    /**
     * @return the CG strikes
     */
    public Double getCgRate() {
        return cgRate;
    }

    /**
     * Sets the CG strikes
     *
     * @param cgRate
     */
    public void setCgRate(Double cgRate) {
        this.cgRate = cgRate;
    }

    /**
     * @return the VCP
     */
    public Integer getVcp() {
        return vcp;
    }

    /**
     * Sets the VCP (coverage)
     *
     * @param vcp
     */
    public void setVcp(Integer vcp) {
        this.vcp = vcp;
    }

    /**
     * @return the CAPE
     */
    public Double getCape() {
        return cape;
    }

    /**
     * Sets the CAPE
     *
     * @param cape
     */
    public void setCape(Double cape) {
        this.cape = cape;
    }

    /**
     * @return the storm relative helicity
     */
    public Double getSreh() {
        return sreh;
    }

    /**
     * set the storm relative helicity
     *
     * @param sreh
     */
    public void setSreh(Double sreh) {
        this.sreh = sreh;
    }

    /**
     * Set the past Coordinates
     *
     * @param pastCoordinates
     */
    public void setPastCoordinates(Map<Date, Coordinate> pastCoordinates) {
        this.pastCoordinates = pastCoordinates;
    }

    /**
     * @return the past coordinates
     */
    public Map<Date, Coordinate> getPastCoordinates() {
        return pastCoordinates;
    }

    /**
     * Gets the value by column
     *
     * @param column
     * @return the value
     */
    @Override
    public Double getValue(String column) {
        double value = 0.0;
        if (column.equals(SCANConfigEnums.CELLTable.MDASR.getColName())) {
            value = ScanUtils.convertStrankValue(mdaSR);
        } else if (column.equals(SCANConfigEnums.CELLTable.POSH.getColName())) {
            value = posh;
        } else if (column.equals(SCANConfigEnums.CELLTable.POH.getColName())) {
            value = poh;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.HSIZE.getColName())) {
            value = hsize;
        } else if (column.equals(SCANConfigEnums.CELLTable.VIL.getColName())) {
            value = vil;
        } else if (column.equals(SCANConfigEnums.CELLTable.DBZ.getColName())) {
            value = dbz;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.DBZHT.getColName())) {
            value = dbzHt;
        } else if (column.equals(SCANConfigEnums.CELLTable.POLH.getColName())) {
            value = polh;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.SVRWX.getColName())) {
            value = svrwx;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.HVYPR.getColName())) {
            value = hvyPr;
        } else if (column.equals(SCANConfigEnums.CELLTable.PPOS.getColName())) {
            value = pos;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.CGRATE.getColName())) {
            value = cgRate;
        } else if (column.equals(SCANConfigEnums.CELLTable.CAPE.getColName())) {
            value = cape;
        } else if (column.equals(SCANConfigEnums.CELLTable.SREH.getColName())) {
            value = sreh;
        } else if (column.equals(SCANConfigEnums.CELLTable.AZM.getColName())) {
            value = azm;
        } else if (column.equals(SCANConfigEnums.CELLTable.RNG.getColName())) {
            value = rng;
        } else if (column.equals(SCANConfigEnums.CELLTable.DIR.getColName())) {
            value = dir;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.AZM15.getColName())) {
            value = azm15;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.AZM30.getColName())) {
            value = azm30;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.AZM45.getColName())) {
            value = azm45;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.AZM60.getColName())) {
            value = azm60;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.RNG60.getColName())) {
            value = rng60;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.RNG45.getColName())) {
            value = rng45;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.RNG30.getColName())) {
            value = rng30;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.RNG15.getColName())) {
            value = rng15;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.MVTERR.getColName())) {
            value = mvtErr;
        } else if (column
                .equals(SCANConfigEnums.CELLTable.MVTMN.getColName())) {
            value = mvtMn;
        } else if (column.equals(SCANConfigEnums.CELLTable.SPD.getColName())) {
            value = spd;
        } else if (column.equals(SCANConfigEnums.CELLTable.TOP.getColName())) {
            value = top;
        }

        return value;
    }

    /**
     * used for debugging / screen mouseover
     */
    @Override
    public String toString() {
        DecimalFormat format = new DecimalFormat();
        format.setMaximumFractionDigits(1);
        StringBuilder buf = new StringBuilder();
        buf.append("Storm ID: ").append(ident).append("\n");
        if (rank > 0) {
            buf.append("Rank: ").append(rank).append("\n");
        }
        if (!"NONE".equals(tvs)) {
            buf.append("TVS: ").append(tvs).append("\n");
        }
        if (mdaSR != null) {
            buf.append("mdaSR: ").append(mdaSR).append("\n");
        }
        if (posh > 0) {
            buf.append("POSH: ").append(posh).append("\n");
        }
        if (poh > 0) {
            buf.append("POH: ").append(poh).append("\n");
        }
        if (polh > 0) {
            buf.append("POLH: ").append(polh).append("\n");
        }
        if (hsize > 0) {
            buf.append("Hail Size: ").append(format.format(hsize)).append("\n");
        }
        if (dbz > 0) {
            buf.append("dbz: ").append(format.format(dbz)).append("\n");
        }
        if (dbzHt > 0) {
            buf.append("dbzHt: ").append(format.format(dbzHt)).append("\n");
        }
        if (top > 0) {
            buf.append("top: ").append(format.format(top)).append("\n");
        }
        if (azm15 > 0) {
            buf.append("azm15: ").append(format.format(azm15)).append("\n");
        }
        if (rng15 > 0) {
            buf.append("rng15: ").append(format.format(rng15)).append("\n");
        }
        if (azm30 > 0) {
            buf.append("azm30: ").append(format.format(azm30)).append("\n");
        }
        if (rng30 > 0) {
            buf.append("rng30: ").append(format.format(rng30)).append("\n");
        }
        if (azm45 > 0) {
            buf.append("azm45: ").append(format.format(azm45)).append("\n");
        }
        if (rng45 > 0) {
            buf.append("rng45: ").append(format.format(rng45)).append("\n");
        }
        if (azm60 > 0) {
            buf.append("azm60: ").append(format.format(azm60)).append("\n");
        }
        if (rng60 > 0) {
            buf.append("rng60: ").append(format.format(rng60)).append("\n");
        }
        if (mvtErr > 0) {
            buf.append("mvtErr: ").append(format.format(mvtErr)).append("\n");
        }
        if (mvtMn > 0) {
            buf.append("mvtMn: ").append(format.format(mvtMn)).append("\n");
        }
        if (svrwx > 0) {
            buf.append("svrwx: ").append(svrwx).append("\n");
        }
        if (hvyPr > 0) {
            buf.append("hvyPr: ").append(hvyPr).append("\n");
        }
        if (pos > 0) {
            buf.append("%Pos: ").append(format.format(pos)).append("\n");
        }
        if (cgRate > 0) {
            buf.append("cgRate: ").append(format.format(cgRate)).append("\n");
        }
        if (vcp > 0) {
            buf.append("VCP: ").append(vcp).append("\n");
        }
        if (cape > 0) {
            buf.append("CAPE: ").append(format.format(cape)).append("\n");
        }
        if (sreh > 0) {
            buf.append("SREH: ").append(format.format(sreh)).append("\n");
        }

        return buf.toString();
    }

    @Override
    public ScanTableDataRow copy() {
        CellTableDataRow row = new CellTableDataRow(this.getTime());
        row = (CellTableDataRow) copyCommon(row);
        row.setRank(this.getRank());
        row.setTvs(this.getTvs());
        row.setMdaSR(this.getMdaSR());
        row.setPosh(this.getPosh());
        row.setPoh(this.getPoh());
        row.setPolh(this.getPolh());
        row.setHsize(this.getHsize());
        row.setVil(this.getVil());
        row.setDbz(this.getDbz());
        row.setDbzHt(this.getDbzHt());
        row.setTop(this.getTop());
        row.setAzm15(this.getAzm15());
        row.setRng15(this.getRng15());
        row.setAzm30(this.getAzm30());
        row.setRng30(this.getRng30());
        row.setAzm45(this.getAzm45());
        row.setRng45(this.getRng45());
        row.setAzm60(this.getAzm60());
        row.setRng60(this.getRng60());
        row.setMvtErr(this.getMvtErr());
        row.setMvtMn(this.getMvtMn());
        row.setSvrwx(this.getSvrwx());
        row.setHvyPr(this.getHvyPr());
        row.setPos(this.getPos());
        row.setCgRate(this.getCgRate());
        row.setVcp(this.getVcp());
        row.setCape(this.getCape());
        row.setSreh(this.getSreh());
        row.setPastCoordinates(this.getPastCoordinates());

        return row;
    }

    @Override
    public void clear() {
        this.setRank(0);
        this.setTvs(TVS_TYPE.NONE.getTVSName());
        this.setMdaSR(null);
        this.setPosh(0);
        this.setPoh(0);
        this.setPolh(0);
        this.setHsize(0.0);
        this.setVil(0.0);
        this.setDbz(0.0);
        this.setDbzHt(0.0);
        this.setTop(0.0);
        this.setAzm15(0.0);
        this.setRng15(0.0);
        this.setAzm30(0.0);
        this.setRng30(0.0);
        this.setAzm45(0.0);
        this.setRng45(0.0);
        this.setAzm60(0.0);
        this.setRng60(0.0);
        this.setMvtErr(0.0);
        this.setMvtMn(0.0);
        this.setSvrwx(0);
        this.setHvyPr(0);
        this.setPos(0.0);
        this.setCgRate(0.0);
        this.setVcp(0);
        this.setCape(-99999.0);
        this.setSreh(-99999.0);
    }

    /**
     * Clear out past Coordinates older than barrier time
     *
     * @param currdate
     */
    public void purgeCoordinates(Date currdate) {
        long barrier = currdate.getTime() - (30 * 60 * 1000);
        Date barrierDate = new Date(barrier);
        List<Date> removes = new ArrayList<>();
        if (pastCoordinates != null) {
            for (Date date : pastCoordinates.keySet()) {
                if (date.before(barrierDate)) {
                    removes.add(date);
                }
            }

            if (!removes.isEmpty()) {
                for (Date date : removes) {
                    pastCoordinates.remove(date);
                }
            }
        }
    }

}
