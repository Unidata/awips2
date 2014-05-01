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
package com.raytheon.edex.plugin.gfe.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmStorageInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.weather.WxDefinition;

/**
 * Aggregate class that contains all the info needed to create a GridDatabase.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/14/08     #1030      randerso    Initial port
 * 04/8/08      #875       bphillip    Added getter for Grid Parm Info dictionary
 * 08/05/2013   #1571      randerso    Made GridParmInfo a field in ParmStorageInfo
 *                                     Cloned ParmStorageInfo when requested so we have
 *                                     a unique instance per database.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GridDbConfig {
    private final Log theLogger = LogFactory.getLog(this.getClass());

    private HashMap<String, ParmStorageInfo> _parmInfoDict;

    private WxDefinition _wxDefinition;

    private DiscreteDefinition _discreteDefinition;

    private ProjectionData _projectionData;

    private class StorageFormat {
        float dataOffset;

        float dataMultiplier;

        String storageFormat;
    }

    /**
     * Determines the possible compression factors for data storage. Given the
     * dataType, min and max possible values and precision, returns the
     * storageFormat, dataOffset and dataMultiplier. If offset and multiplier is
     * zero, then data is stored as is.
     * 
     * Based on calcKrunchValues() in ifpnetCDF.py
     * 
     * @param dataFormat
     * @param maxV
     * @param minV
     * @param precision
     * @param dataOffset
     * @param dataMultiplier
     * @param storageFormat
     */
    private void calcKrunchValues(final String dataFormat, final float maxV,
            final float minV, final int precision, StorageFormat format) {
        // check for dataFormat of byte (WEATHER/DISCRETE)
        if (dataFormat == "byte") {
            format.dataOffset = 0.0f;
            format.dataMultiplier = 0.0f;
            format.storageFormat = "byte";
            return;
        }

        // TODO: can probably clean this up mathematically by determining number
        // of bits required using log(range)/log(2)

        float dprecision = (float) Math.pow(10.0, precision);

        int nentries = (int) (((maxV - minV) * dprecision));

        // check for byte possibilities
        if (nentries <= (Math.pow(2.0, 8.0) - 1)) {
            format.dataMultiplier = dprecision;
            format.dataOffset = 0;
            int minVarValue = 0;
            int maxVarValue = 255;
            if ((minV * format.dataMultiplier) < minVarValue) {
                format.dataOffset = minV
                        - (minVarValue / format.dataMultiplier);
            }
            if ((maxV * format.dataMultiplier) > maxVarValue) {
                format.dataOffset = maxV
                        - (maxVarValue / format.dataMultiplier);
            }
            format.storageFormat = "byte";
        }

        // check for short possibilities
        else if (nentries <= (Math.pow(2.0, 16.0) - 2)) {
            format.dataMultiplier = dprecision;
            format.dataOffset = 0;
            double maxVarValue = Math.pow(2.0, 15.0) - 1;
            double minVarValue = -(Math.pow(2.0, 15.0) - 2);
            if ((minV * format.dataMultiplier) < minVarValue) {
                format.dataOffset = (float) (minV - (minVarValue / format.dataMultiplier));
            }
            if ((maxV * format.dataMultiplier) > maxVarValue) {
                format.dataOffset = (float) (maxV - (maxVarValue / format.dataMultiplier));
            }
            format.storageFormat = "short";
        }

        // else full 32-bit float processing, no krunching needed
        else {
            format.dataMultiplier = 0.0f;
            format.dataOffset = 0.0f;
            format.storageFormat = "float";
        }
    }

    /**
     * Adds the specified GridParmInfo and ParmStorageInfo to the dictionaries.
     * Note that if the info for the parm was added previously, the new info
     * will replace the old info.
     * 
     * @param gridParmInfo
     * @param parmStorageInfo
     * @return
     */
    private boolean addParmInfo(final ParmStorageInfo parmStorageInfo) {
        String key = parmStorageInfo.getParmName() + "_"
                + parmStorageInfo.getParmLevel();
        _parmInfoDict.put(key, parmStorageInfo);

        return true;
    }

    /**
     * Adds the specified SimpleGridParmConfig to the dictionaries.
     * 
     * Extracts out the GridParmInfo and ParmStorageInfo from the
     * SImpleGridParmConfig, and then calls the addParmInfo(gpi, psi). Note:
     * SimpleGridParmConfig parmName is really parmName_level.
     * 
     * @param databaseID
     * @param config
     * @param extraWEPrecision
     * @return
     */
    private boolean addParmInfo(final String databaseID,
            final SimpleGridParmConfig config,
            final Map<String, Integer> extraWEPrecision) {
        GridType gridType = GridType.NONE;
        if (config == null) {
            return false;
        }

        String gridTypeInConfig = config.gridType.toLowerCase();
        if ("scalar".equals(gridTypeInConfig)) {
            gridType = GridType.SCALAR;
        } else if ("vector".equals(gridTypeInConfig)) {
            gridType = GridType.VECTOR;
        } else if ("weather".equals(gridTypeInConfig)) {
            gridType = GridType.WEATHER;
        } else if ("discrete".equals(gridTypeInConfig)) {
            gridType = GridType.DISCRETE;
        } else {
            theLogger.error("Invalid gridType (" + config.gridType + ") "
                    + " in configuration for database: " + databaseID
                    + " and parmName=" + config.parmName);
            return false; // error condition
        }

        // construct a GridParmInfo
        // check for a level, if not assign SFC
        ParmID parmID;
        int pos = config.parmName.indexOf('_');
        if (pos != -1) {
            parmID = new ParmID(config.parmName.substring(0, pos),
                    new DatabaseID(databaseID),
                    config.parmName.substring(config.parmName.length() - pos
                            - 1));
        } else {
            parmID = new ParmID(config.parmName, new DatabaseID(databaseID));
        }
        String siteId = parmID.getDbId().getSiteId();
        GridLocation gridLoc = new GridLocation(siteId, _projectionData,
                config.gridSize, config.domainOrigin, config.domainExtent,
                "GMT");

        TimeConstraints timeConstraints = new TimeConstraints(
                config.durationConstraint, config.repeatConstraint,
                config.startConstraint);

        GridParmInfo gpi = new GridParmInfo(parmID, gridLoc, gridType,
                config.units, config.descriptiveName, config.minAllowedValue,
                config.maxAllowedValue, config.precision,
                config.timeIndependentParm, timeConstraints, config.rateParm);

        // determine data format based on grid type
        String dataFormat = null;
        switch (gridType) {
        case SCALAR: // intentional fallthrough
        case VECTOR:
            dataFormat = "float";
            break;
        case WEATHER: // intentional fallthrough
        case DISCRETE:
            dataFormat = "byte";
            break;
        default:
            break;
        }

        // determine storage compression technique
        StorageFormat format = new StorageFormat();
        format.dataOffset = 0.0f;
        format.dataMultiplier = 0.0f;
        format.storageFormat = dataFormat;
        int precision = config.precision;
        Integer extraPrecision = extraWEPrecision.get(config.parmName);
        if (extraPrecision != null) {
            if (extraPrecision > 0) {
                precision += extraPrecision;
            }
        }
        calcKrunchValues(dataFormat, config.maxAllowedValue,
                config.minAllowedValue, precision, format);

        ParmStorageInfo psi = new ParmStorageInfo(dataFormat, gpi,
                format.dataOffset, format.dataMultiplier, format.storageFormat);
        return addParmInfo(psi);
    }

    /**
     * Constructor for the GridDbConfig class.
     * 
     * @param smc
     * @param wxDef
     * @param projectionData
     * @param dDef
     * @param extraWEPrecision
     */
    public GridDbConfig(final SimpleModelConfig smc, final WxDefinition wxDef,
            final ProjectionData projectionData, final DiscreteDefinition dDef,
            final Map<String, Integer> extraWEPrecision) {

        this._wxDefinition = wxDef;
        this._discreteDefinition = dDef;
        this._projectionData = projectionData;
        this._parmInfoDict = new HashMap<String, ParmStorageInfo>();

        for (int i = 0; i < smc.grids.size(); i++) {
            String tmp = smc.siteID + '_' + smc.format + '_' + smc.type + '_'
                    + smc.modelName + "_" + DatabaseID.NO_MODEL_TIME;
            DatabaseID id = new DatabaseID(tmp);
            if (!id.isValid()) {
                continue;
            }
            addParmInfo(tmp, smc.grids.get(i), extraWEPrecision);
        }
    }

    /**
     * @param config
     */
    @SuppressWarnings("unchecked")
    public GridDbConfig(GridDbConfig orig) {
        this._wxDefinition = orig._wxDefinition;
        this._discreteDefinition = orig._discreteDefinition;
        this._projectionData = orig._projectionData;
        this._parmInfoDict = (HashMap<String, ParmStorageInfo>) orig._parmInfoDict
                .clone();
    }

    /**
     * Returns the list of parmNames/levels that are currently stored in the
     * dictionaries. Returns a list of parmName_level.
     * 
     * Simply step through one of the dictionaries and extract the key. Append
     * each entry to the SeqOf<TextString> and return it.
     * 
     * @return
     */
    public List<String> parmAndLevelList() {
        List<String> parmAndLevels = new ArrayList<String>();

        for (String key : _parmInfoDict.keySet()) {
            parmAndLevels.add(key);
        }

        return parmAndLevels;
    }

    /**
     * Returns the GridParmInfo that corresponds to the specified parmName.
     * 
     * Lookup the GridParmInfo in the _parmInfo dictionary and return its value
     * or null if not found.
     * 
     * @param parmName
     * @param level
     * @return
     */
    public GridParmInfo getGridParmInfo(final String parmName,
            final String level) {
        String composite = parmName + "_" + level;
        return _parmInfoDict.get(composite).getGridParmInfo();
    }

    /**
     * Returns the ParmStorageInfo that corresponds to the specified parmName.
     * 
     * Lookup the GridParmInfo in the _parmInfo dictionary and return its value
     * or null if not found.
     * 
     * @param parmName
     * @param level
     * @return the ParmStorageInfo
     */
    public ParmStorageInfo getParmStorageInfo(final String parmName,
            final String level) {
        String composite = parmName + "_" + level;
        return _parmInfoDict.get(composite).clone();
    }

    @Override
    public String toString() {
        StringBuffer s = new StringBuffer();

        s.append("GRID INFO DICT\n");
        for (Map.Entry<String, ParmStorageInfo> entry : _parmInfoDict
                .entrySet()) {
            s.append(entry.getKey() + ' ' + entry.getValue().getGridParmInfo()
                    + '\n');
        }

        s.append("PARM INFO DICT\n");
        for (Map.Entry<String, ParmStorageInfo> entry : _parmInfoDict
                .entrySet()) {
            s.append(entry.getKey() + ' ' + entry.getValue() + '\n');
        }
        return s.toString();
    }

    /**
     * Returns the WxDefinition.
     * 
     * @return
     */
    public WxDefinition wxDefinition() {
        return this._wxDefinition;
    }

    /**
     * Returns the DiscreteDefinition.
     * 
     * @return
     */
    public DiscreteDefinition discreteDefinition() {
        return this._discreteDefinition;
    }

    /**
     * Returns the ProjectionData
     * 
     * @return
     */
    public ProjectionData projectionData() {
        return this._projectionData;
    }
}
