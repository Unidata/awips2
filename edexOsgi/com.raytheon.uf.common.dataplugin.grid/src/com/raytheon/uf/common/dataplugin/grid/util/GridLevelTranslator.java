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
package com.raytheon.uf.common.dataplugin.grid.util;

import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Pressure;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.style.level.Level;
import com.raytheon.uf.common.style.level.SingleLevel;

import si.uom.NonSI;
import tec.uom.se.AbstractConverter;
import tec.uom.se.unit.MetricPrefix;

/**
 * Grid Level Translator
 * 
 * Translates grib levels into level types
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ ------------------
 * Jul 30, 2007           chammack     Initial Creation.
 * May 01, 2014  DCS 027  MPorricelli  Add WBZ level
 * Aug 15, 2016  5821     bsteffen     Add TROP level
 * May 18, 2018  20395    wkwock       Add CBL and CLG level
 * Apr 15, 2019  7596     lsingh       Updated units framework to JSR-363.
 * 
 * </pre>
 * 
 * @author chammack
 */
public class GridLevelTranslator {

    private GridLevelTranslator() {

    }

    public static SingleLevel construct(String levelType) {

        SingleLevel level = null;

        if (levelType.equalsIgnoreCase("MB")
                || levelType.equalsIgnoreCase("LYRMB")) {
            level = new SingleLevel(Level.LevelType.PRESSURE);
        } else if (levelType.equalsIgnoreCase("FHAG")
                || levelType.equalsIgnoreCase("LYRFHAG")) {
            level = new SingleLevel(Level.LevelType.HEIGHT_AGL);
        } else if (levelType.equalsIgnoreCase("FH")
                || levelType.equalsIgnoreCase("LYRFH")) {
            level = new SingleLevel(Level.LevelType.HEIGHT_MSL);
        } else if (levelType.equalsIgnoreCase("SFC")) {
            level = new SingleLevel(Level.LevelType.SURFACE);
        } else if (levelType.equalsIgnoreCase("TROP")) {
            level = new SingleLevel(Level.LevelType.TROP);
        } else if (levelType.equalsIgnoreCase("MAXW")) {
            level = new SingleLevel(Level.LevelType.MAXW);
        } else if (levelType.equalsIgnoreCase("K")) {
            level = new SingleLevel(Level.LevelType.THETA);
        } else if (levelType.equals("BL")) {
            level = new SingleLevel(Level.LevelType.MB_AGL);
        } else if (levelType.equals("TILT")) {
            level = new SingleLevel(Level.LevelType.TILT);
        } else if (levelType.equalsIgnoreCase("TW0")) {
            level = new SingleLevel(Level.LevelType.TW0);
        } else if (levelType.equalsIgnoreCase("TEMP")) {
            level = new SingleLevel(Level.LevelType.TEMP);
        } else if (levelType.equalsIgnoreCase("FRZ")) {
            level = new SingleLevel(Level.LevelType.FRZ);
        } else if (levelType.equalsIgnoreCase("WBZ")) {
            level = new SingleLevel(Level.LevelType.WBZ);            
        } else if (levelType.equalsIgnoreCase("CBL")) {
            level = new SingleLevel(Level.LevelType.CBL);
        } else if (levelType.equalsIgnoreCase("CLG")) {
            level = new SingleLevel(Level.LevelType.CLG);
        } else {
            return new SingleLevel(Level.LevelType.DEFAULT);
        }

        return level;

    }

    public static SingleLevel constructMatching(GridRecord record) {
        return constructMatching(record.getLevel());
    }

    public static SingleLevel constructMatching(
            com.raytheon.uf.common.dataplugin.level.Level inputLevel) {
        Unit<?> levelUnits = inputLevel.getMasterLevel().getUnit();// record.getModelInfo().getLevelUnitObject();

        UnitConverter levelConverter = AbstractConverter.IDENTITY;
        if (levelUnits != null && levelUnits.isCompatible(MetricPrefix.MILLI(NonSI.BAR))) {
            levelConverter = levelUnits.asType(Pressure.class).getConverterTo(MetricPrefix.MILLI(NonSI.BAR));
        }

        double convertedLevel = 0;
        if (inputLevel.isLevelOneValid())
            convertedLevel = levelConverter.convert(inputLevel
                    .getLevelonevalue()); // record.getModelInfo().getLevelOneValue()
        if (inputLevel.isLevelTwoValid()) { // record.getModelInfo().getLevelTwoValue()!=null
            double convertedLevel2 = 0;
            convertedLevel2 = levelConverter.convert(inputLevel
                    .getLeveltwovalue()); // record.getModelInfo().getLevelTwoValue()
            convertedLevel = (1000 * convertedLevel + convertedLevel2);
        }

        SingleLevel level = construct(inputLevel.getMasterLevel().getName()); // record.getModelInfo().getLevelName()
        level.setValue(convertedLevel);
        return level;
    }
}
