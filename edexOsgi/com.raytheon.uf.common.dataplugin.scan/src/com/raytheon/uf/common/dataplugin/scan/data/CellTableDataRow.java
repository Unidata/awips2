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
import java.util.HashMap;

import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * SCAN Cell Table Data Row
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 29, 2009   2037    dhladky     Initial creation
 * 02/01/13     1569        D. Hladky   removed XML where not needed
 * May 13, 2014 3133        njensen     Use ScanUtils instead of ScanConfig
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */
@DynamicSerialize
public class CellTableDataRow extends ScanTableDataRow {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public CellTableDataRow(DataTime time) {
        super(time);
    }

    public CellTableDataRow() {

    }

    /** cell rank in table **/
    @DynamicSerializeElement
    public Integer rank = 0;

    /** cell TVS **/
    @DynamicSerializeElement
    public String tvs = TVS_TYPE.NONE.getTVSName();

    /** meso cyclone detection alg rank **/
    @DynamicSerializeElement
    public String mdaSR = null;

    /** prob of severe hail **/
    @DynamicSerializeElement
    public Integer posh = 0;

    /** prob of hail **/
    @DynamicSerializeElement
    public Integer poh = 0;

    /** prob of large hail **/
    @DynamicSerializeElement
    public Integer polh = 0;

    /** hail size, inches **/
    @DynamicSerializeElement
    public Double hsize = 0.0;

    /** Vertically Integrated Liquid **/
    @DynamicSerializeElement
    public Double vil = 0.0;

    /** max reflectivity Decibel return **/
    @DynamicSerializeElement
    public Double dbz = 0.0;

    /** height of max reflectivity dbz return **/
    @DynamicSerializeElement
    public Double dbzHt = 0.0;

    /** top of storm in kft **/
    @DynamicSerializeElement
    public Double top = 0.0;

    /** 15 min forecast azm **/
    @DynamicSerializeElement
    public Double azm15 = 0.0;

    /** 15 min forecast range **/
    @DynamicSerializeElement
    public Double rng15 = 0.0;

    /** 30 min forecast azm **/
    @DynamicSerializeElement
    public Double azm30 = 0.0;

    /** 30 min forecast range **/
    @DynamicSerializeElement
    public Double rng30 = 0.0;

    /** 45 min forecast azm **/
    @DynamicSerializeElement
    public Double azm45 = 0.0;

    /** 45 min forecast range **/
    @DynamicSerializeElement
    public Double rng45 = 0.0;

    /** 60 min forecast azm **/
    @DynamicSerializeElement
    public Double azm60 = 0.0;

    /** 60 min forecast range **/
    @DynamicSerializeElement
    public Double rng60 = 0.0;

    /** move error **/
    @DynamicSerializeElement
    public Double mvtErr = 0.0;

    /** move error mean **/
    @DynamicSerializeElement
    public Double mvtMn = 0.0;

    /** severe wx prob **/
    @DynamicSerializeElement
    public Integer svrwx = 0;

    /** heavy precip % **/
    @DynamicSerializeElement
    public Integer hvyPr = 0;

    /** % positive ligtning strikes **/
    @DynamicSerializeElement
    public Double pos = 0.0;

    /** % cloud to ground strikes **/
    @DynamicSerializeElement
    public Double cgRate = 0.0;

    /** Volume Coverage Pattern **/
    @DynamicSerializeElement
    public Integer vcp = 0;

    /** CAPE Convective Available Potential Energy **/
    @DynamicSerializeElement
    public Double cape = -99999.0;

    /** Storm Relative Helicity **/
    @DynamicSerializeElement
    public Double sreh = -99999.0;

    @DynamicSerializeElement
    public HashMap<Date, Coordinate> pastCoordinates;

    /**
     * gets the rank
     * 
     * @return
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
     * Get the TVS
     * 
     * @return
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
     * Get Meso detection strength
     * 
     * @return
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
     * Probability of severe hail
     * 
     * @return
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
     * probability of hail
     * 
     * @return
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
     * probability of large hail
     * 
     * @return
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
     * hail size
     * 
     * @return
     */
    public Double getHsize() {
        return hsize;
    }

    /**
     * Set the hail size
     * 
     * @param size
     */
    public void setHsize(Double hsize) {
        this.hsize = hsize;
    }

    /**
     * Vertically integrated liquid
     * 
     * @return
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
     * max Decibel return of radar
     * 
     * @return
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
     * max decibel return height
     * 
     * @return
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
     * storm top in kft
     * 
     * @return
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
     * 15 min forecast azm
     * 
     * @return
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
     * 15 min forecast range
     * 
     * @return
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
     * 30 min forecast azm
     * 
     * @return
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
     * 30 min forecast range
     * 
     * @return
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
     * 45 min forecast azm
     * 
     * @return
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
     * 45 min forecast range
     * 
     * @return
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
     * 60 min forecast azm
     * 
     * @return
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
     * 60 min forecast range
     * 
     * @return
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
     * movement error
     * 
     * @return
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
     * get movement mean error
     * 
     * @return
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
     * get the lat
     * 
     * @return
     */
    @Override
    public Double getLat() {
        return lat;
    }

    /**
     * set the lat
     * 
     * @param lat
     */
    @Override
    public void setLat(Double lat) {
        this.lat = lat;
    }

    /**
     * get the lon
     * 
     * @return
     */
    @Override
    public Double getLon() {
        return lon;
    }

    /**
     * set the lon
     * 
     * @param lon
     */
    @Override
    public void setLon(Double lon) {
        this.lon = lon;
    }

    /**
     * Get the Heavy precip %
     * 
     * @return
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
     * Get the sever WX prob
     * 
     * @return
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
     * get the positive lght strikes
     * 
     * @return
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
     * Gets the CG strikes
     * 
     * @return
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
     * Get the VCP
     * 
     * @return
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
     * Get the CAPE
     * 
     * @return
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
     * Set the storm relative helicity
     * 
     * @return
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
     * Set the past Coors
     * 
     * @param pastCoors
     */
    public void setPastCoordinates(HashMap<Date, Coordinate> pastCoordinates) {
        this.pastCoordinates = pastCoordinates;
    }

    /**
     * past coordinates
     * 
     * @return
     */
    public HashMap<Date, Coordinate> getPastCoordinates() {
        return pastCoordinates;
    }

    /**
     * Gets the value by column
     * 
     * @param column
     * @return
     */
    @Override
    public Double getValue(String column) {
        double value = 0.0;
        if (column.equals(SCANConfigEnums.CELLTable.MDASR.getColName())) {
            // if (mdaSR.length() > 1) {
            // value = new Double(mdaSR.substring(0, 1));
            // }
            value = ScanUtils.convertStrankValue(mdaSR);
        } else if (column.equals(SCANConfigEnums.CELLTable.POSH.getColName())) {
            value = posh;
        } else if (column.equals(SCANConfigEnums.CELLTable.POH.getColName())) {
            value = poh;
        } else if (column.equals(SCANConfigEnums.CELLTable.HSIZE.getColName())) {
            value = hsize;
        } else if (column.equals(SCANConfigEnums.CELLTable.VIL.getColName())) {
            value = vil;
        } else if (column.equals(SCANConfigEnums.CELLTable.DBZ.getColName())) {
            value = dbz;
        } else if (column.equals(SCANConfigEnums.CELLTable.DBZHT.getColName())) {
            value = dbzHt;
        } else if (column.equals(SCANConfigEnums.CELLTable.POLH.getColName())) {
            value = polh;
        } else if (column.equals(SCANConfigEnums.CELLTable.SVRWX.getColName())) {
            value = svrwx;
        } else if (column.equals(SCANConfigEnums.CELLTable.HVYPR.getColName())) {
            value = hvyPr;
        } else if (column.equals(SCANConfigEnums.CELLTable.PPOS.getColName())) {
            value = pos;
        } else if (column.equals(SCANConfigEnums.CELLTable.CGRATE.getColName())) {
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
        } else if (column.equals(SCANConfigEnums.CELLTable.AZM15.getColName())) {
            value = azm15;
        } else if (column.equals(SCANConfigEnums.CELLTable.AZM30.getColName())) {
            value = azm30;
        } else if (column.equals(SCANConfigEnums.CELLTable.AZM45.getColName())) {
            value = azm45;
        } else if (column.equals(SCANConfigEnums.CELLTable.AZM60.getColName())) {
            value = azm60;
        } else if (column.equals(SCANConfigEnums.CELLTable.RNG60.getColName())) {
            value = rng60;
        } else if (column.equals(SCANConfigEnums.CELLTable.RNG45.getColName())) {
            value = rng45;
        } else if (column.equals(SCANConfigEnums.CELLTable.RNG30.getColName())) {
            value = rng30;
        } else if (column.equals(SCANConfigEnums.CELLTable.RNG15.getColName())) {
            value = rng15;
        } else if (column.equals(SCANConfigEnums.CELLTable.MVTERR.getColName())) {
            value = mvtErr;
        } else if (column.equals(SCANConfigEnums.CELLTable.MVTMN.getColName())) {
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
        StringBuffer buf = new StringBuffer();
        buf.append("Storm ID: " + ident + "\n");
        if (rank > 0) {
            buf.append("Rank: " + rank + "\n");
        }
        if (!tvs.equals("NONE")) {
            buf.append("TVS: " + tvs + "\n");
        }
        if (mdaSR != null) {
            buf.append("mdaSR: " + mdaSR + "\n");
        }
        if (posh > 0) {
            buf.append("POSH: " + posh + "\n");
        }
        if (poh > 0) {
            buf.append("POH: " + poh + "\n");
        }
        if (polh > 0) {
            buf.append("POLH: " + polh + "\n");
        }
        if (hsize > 0) {
            buf.append("Hail Size: " + format.format(hsize) + "\n");
        }
        if (dbz > 0) {
            buf.append("dbz: " + format.format(dbz) + "\n");
        }
        if (dbzHt > 0) {
            buf.append("dbzHt: " + format.format(dbzHt) + "\n");
        }
        if (top > 0) {
            buf.append("top: " + format.format(top) + "\n");
        }
        if (azm15 > 0) {
            buf.append("azm15: " + format.format(azm15) + "\n");
        }
        if (rng15 > 0) {
            buf.append("rng15: " + format.format(rng15) + "\n");
        }
        if (azm30 > 0) {
            buf.append("azm30: " + format.format(azm30) + "\n");
        }
        if (rng30 > 0) {
            buf.append("rng30: " + format.format(rng30) + "\n");
        }
        if (azm45 > 0) {
            buf.append("azm45: " + format.format(azm45) + "\n");
        }
        if (rng45 > 0) {
            buf.append("rng45: " + format.format(rng45) + "\n");
        }
        if (azm60 > 0) {
            buf.append("azm60: " + format.format(azm60) + "\n");
        }
        if (rng60 > 0) {
            buf.append("rng60: " + format.format(rng60) + "\n");
        }
        if (mvtErr > 0) {
            buf.append("mvtErr: " + format.format(mvtErr) + "\n");
        }
        if (mvtMn > 0) {
            buf.append("mvtMn: " + format.format(mvtMn) + "\n");
        }
        if (svrwx > 0) {
            buf.append("svrwx: " + svrwx + "\n");
        }
        if (hvyPr > 0) {
            buf.append("hvyPr: " + hvyPr + "\n");
        }
        if (pos > 0) {
            buf.append("%Pos: " + format.format(pos) + "\n");
        }
        if (cgRate > 0) {
            buf.append("cgRate: " + format.format(cgRate) + "\n");
        }
        if (vcp > 0) {
            buf.append("VCP: " + vcp + "\n");
        }
        if (cape > 0) {
            buf.append("CAPE: " + format.format(cape) + "\n");
        }
        if (sreh > 0) {
            buf.append("SREH: " + format.format(sreh) + "\n");
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
        ArrayList<Date> removes = new ArrayList<Date>();
        if (pastCoordinates != null) {
            for (Date date : pastCoordinates.keySet()) {
                if (date.before(barrierDate)) {
                    removes.add(date);
                }
            }

            if (removes.size() > 0) {
                for (Date date : removes) {
                    pastCoordinates.remove(date);
                }
            }
        }
    }

}
