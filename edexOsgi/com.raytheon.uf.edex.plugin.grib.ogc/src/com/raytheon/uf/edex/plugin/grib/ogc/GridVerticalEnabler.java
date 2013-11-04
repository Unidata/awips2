/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.grib.ogc;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalEnabled;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GridVerticalEnabler implements VerticalEnabled<GridRecord> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.spatial.VerticalEnabled#getVerticalCoordinate
     * (java.lang.Object)
     */
    @Override
    public VerticalCoordinate getVerticalCoordinate(GridRecord rec) {
        if (rec.getInfo() == null) {
            return null;
        }
        Level level = rec.getInfo().getLevel();
        if (level == null) {
            return null;
        }
        double l1 = level.getLevelonevalue();
        double l2 = level.getLeveltwovalue();
        MasterLevel master = level.getMasterLevel();
        VerticalCoordinate.Reference ref;
        if (master.getUnit() == null) {
            ref = Reference.UNKNOWN;
        } else if (SI.PASCAL.isCompatible(master.getUnit())) {
            ref = Reference.PRESSURE_LEVEL;
        } else if ("FHAG".equalsIgnoreCase(master.getName())) {
            ref = Reference.ABOVE_GROUND;
        } else if (SI.METER.isCompatible(master.getUnit())) {
            ref = Reference.ABOVE_MSL;
        } else {
            ref = Reference.UNKNOWN;
        }
        if (l2 == Level.INVALID_VALUE) {
            return new VerticalCoordinate(l1, master.getUnit(), ref);
        } else {
            return new VerticalCoordinate(l1, l2, master.getUnit(), ref);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.spatial.VerticalEnabled#getSupportedClass
     * ()
     */
    @Override
    public Class<GridRecord> getSupportedClass() {
        return GridRecord.class;
    }

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.ogc.common.spatial.VerticalEnabled#
	 * getDefaultVerticalUnit()
	 */
	@Override
	public Unit<?> getDefaultVerticalUnit() {
		return null;
	}

}
