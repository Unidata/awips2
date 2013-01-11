/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;

/**
 * Grid Level Translator
 * 
 * Translates grib levels into level types
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jul 30, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class GridLevelTranslator {

    private GridLevelTranslator() {

    }

    public static SingleLevel construct(String levelType) {

        SingleLevel level = null;

        if (levelType.equalsIgnoreCase("MB")
                || levelType.equalsIgnoreCase("LYRMB")
                || levelType.equals("BL")) {
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
            level = new SingleLevel(Level.LevelType.SURFACE);
        } else {
            return new SingleLevel(Level.LevelType.SURFACE);
        }

        return level;

    }

    public static SingleLevel construct(GridRecord record) {
        Unit<?> levelUnits = record.getLevel().getMasterLevel().getUnit();

        UnitConverter levelConverter = UnitConverter.IDENTITY;
        if (levelUnits.isCompatible(SI.MILLI(NonSI.BAR))) {
            levelConverter = levelUnits.getConverterTo(SI.MILLI(NonSI.BAR));
        }

        float convertedLevel = (float) levelConverter.convert(record
                .getLevel().getLevelonevalue());
        SingleLevel level = construct(record.getLevel().getMasterLevel().getName());
        level.setValue(convertedLevel);
        return level;
    }

}
