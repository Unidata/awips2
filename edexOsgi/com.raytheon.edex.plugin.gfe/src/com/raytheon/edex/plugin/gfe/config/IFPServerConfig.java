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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.gfe.DiscreteTerm;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKeyDef;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherAttribute;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherCoverage;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherIntensity;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherType;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherVisibility;
import com.raytheon.uf.common.dataplugin.gfe.weather.WxDefinition;

/**
 * This object contains general information needed to configure the IFP server.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/11/08     #1030      randerso    Initial port	
 * 04/08/08     #875       bphillip    Changed exception handling
 * 06/24/08     #1160      randerso    Added a method to get the Topo dbId
 * 07/09/09     #2590      njensen     No longer singleton
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class IFPServerConfig {
    private static final Log theLogger = LogFactory
            .getLog(IFPServerConfig.class);

    private String _serverHost, _mhsid;

    private long _rpcPort;

    private List<String> _allowedNodes;

    private List<DatabaseID> _singletonDatabases;

    private List<DatabaseID> _officialDatabases;

    private Map<String, String> _d2dModels;

    /**
     * Reverse mapping of _d2dModels
     */
    private Map<String, String> gfeModels;

    private Map<String, List<String>> _initMethods;

    private Map<String, List<String>> _accumulativeD2DElements;

    private Map<String, List<Integer>> _initSkips;

    private Map<String, Integer> _desiredDbVersions, _gridPurgeAge;

    private Map<String, GridDbConfig> _gridDbConfig;

    private String _prdDir;

    private List<WeatherVisibility> _weatherVisibilities;

    private List<WeatherType> _weatherTypes;

    private WxDefinition _wxDefinition;

    private DiscreteDefinition _discreteDefinition;

    private List<ProjectionData> _projectionData;

    private static final ProjectionData emptyProj = new ProjectionData();

    /**
     * @param projID
     * @return
     */
    public ProjectionData getProjectionData(final String projID) {
        for (int i = 0; i < _projectionData.size(); i++) {
            if (_projectionData.get(i).getProjectionID().equals(projID)) {
                return _projectionData.get(i);
            }
        }
        // ProjectionID was not found so return a default ProjectionData
        theLogger.error(projID + " was not found in IFPServerConfig.");
        return emptyProj;
    }

    private List<String> _siteID;

    private List<String> _timeZones;

    private List<String> _officeTypes;

    private boolean _topoAllowedBelowZero;

    private int _logFilePurgeAfter;

    private String _baseDir;

    private long _iscPort;

    private List<String> _allSites;

    private GridLocation _domain;

    private Map<String, String> _netCDFDirs;

    private Map<String, String> _satDirs;

    private int _tableFetchTime;

    private boolean _autoConfigureNotifyTextProd;

    private Map<String, String> _iscRoutingTableAddress;

    private List<String> _requestedISCsites;

    private boolean _requestISC;

    private boolean _sendiscOnSave;

    private boolean _sendiscOnPublish;

    private List<String> _requestedISCparms;

    private String _transmitScript;

    private long _protocolVersion = 20080905;

    private String convertToString(final DatabaseID id) {
        return id.getModelId();
    }

    private DatabaseID stripTime(final DatabaseID id) {
        return new DatabaseID(id.getSiteId(), id.getFormat(), id.getDbType(),
                id.getModelName());
    }

    // private DatabaseID stripType(final DatabaseID id) {
    // if (DatabaseID.NO_MODEL_TIME.equals(id.getModelTime())) {
    // return new DatabaseID(id.getSiteId(), id.getFormat(), "", id
    // .getModelName());
    // } else {
    // return new DatabaseID(id.getSiteId(), id.getFormat(), "", id
    // .getModelName(), id.getModelTime());
    // }
    // }

    private IFPServerConfig() {
        _rpcPort = 0;
        _iscPort = 0;
        _tableFetchTime = 0;
        _autoConfigureNotifyTextProd = false;
        _requestISC = false;
        _sendiscOnSave = false;
        _sendiscOnPublish = false;
        _desiredDbVersions = new HashMap<String, Integer>();
        _gridPurgeAge = new HashMap<String, Integer>();
        _gridDbConfig = new HashMap<String, GridDbConfig>();
    }

    protected IFPServerConfig(final SimpleServerConfig config) {
        this();
        if (config != null) {
            setServerCharacteristics(config);
        }
    }

    // network characteristics
    public String getServerHost() {
        return _serverHost;
    }

    public String getMhsid() {
        return _mhsid;
    }

    /**
     * Returns the rpc port.
     * 
     * @return
     */
    public long getRpcPort() {
        return _rpcPort;
    }

    /**
     * Returns the protocol version
     * 
     * @return
     */
    public long getProtocolVersion() {
        return _protocolVersion;
    }

    /**
     * Returns the allowed network nodes to access the server.
     * 
     * @return
     */
    public List<String> getAllowedNodes() {
        return _allowedNodes;
    }

    public String getBaseDir() {
        return _baseDir;
    }

    /**
     * Returns the list of singleton databases. (The modeltime is not useful.)
     * 
     * @return
     */
    public List<DatabaseID> getSingletonDatabases() {
        return _singletonDatabases;
    }

    /**
     * Return the list of official databases. (The modeltime is not useful.)
     * 
     * @return
     */
    public List<DatabaseID> getOfficialDatabases() {
        return _officialDatabases;
    }

    /**
     * Return number of desired database versions for the given database id. The
     * modelTime is ignored.
     * 
     * Looks up the entry and returns the value. If not found, returns 2.
     * 
     * @param id
     * @return
     */
    public int desiredDbVersions(final DatabaseID id) {
        Integer versions = _desiredDbVersions
                .get(convertToString(stripTime(id)));
        return versions == null ? 2 : versions;
    }

    /**
     * Return the number of hours before purging "old" grids.
     * 
     * The modelTime is ignored.
     * 
     * Looks up the entry and returns the value. If not found, returns -1 to
     * indicate no purging.
     * 
     * @param id
     * @return
     */
    public int gridPurgeAgeInHours(final DatabaseID id) {
        Integer numHours = _gridPurgeAge.get(convertToString(stripTime(id)));
        return numHours == null ? -1 : numHours;
    }

    /**
     * Returns a pointer to the GridDbConfig object for the given database. If
     * not defined, then returns NULL. (The modelTime is ignored.)
     * 
     * @param id
     * @return
     */
    public GridDbConfig gridDbConfig(final DatabaseID id) {
        return _gridDbConfig.get(convertToString(stripTime(id)));
    }

    /**
     * Returns the known database configurations for the server. Note that the
     * modelTime is ignored.
     * 
     * Go through the _gridDbConfig dictionaries and extract out the databaseid
     * which is a String, and then that is converted to a DatabaseID.
     * 
     * @return
     */
    public List<DatabaseID> knownDBConfig() {
        List<DatabaseID> ids = new ArrayList<DatabaseID>();
        // DatabaseID id;
        // for (int i = 0; i < _gridDbConfig.size(); i++)
        // {
        // id = _gridDbConfig.key(i);
        // if (id.isValid())
        // ids.append(id);
        // else
        // logProblem + "Invalid database identifier ["
        // + _gridDbConfig.key(i) + ']' + std::endl;
        // }
        return ids;
    }

    /**
     * Returns list of d2d models to process.
     * 
     * @return
     */
    public List<String> getD2dModels() {
        return new ArrayList<String>(_d2dModels.keySet());
    }

    /**
     * Maps a d2dModelName to its gfeModelName. If no mapping exists an empty
     * string is returned.
     * 
     * @param d2dModelName
     * @return
     */
    public String gfeModelNameMapping(final String d2dModelName) {
        // now handle the mapping of directory to optional model name
        String mapping = _d2dModels.get(d2dModelName);
        return mapping == null ? "" : mapping;
    }

    /**
     * Maps a gfedModelName to its d2dModelName. If no mapping exists an empty
     * string is returned.
     * 
     * @param d2dModelName
     * @return
     */
    public String d2dModelNameMapping(final String gfeModelName) {
        // now handle the mapping of directory to optional model name
        String mapping = gfeModels.get(gfeModelName);
        return mapping == null ? "" : mapping;
    }

    /**
     * Returns the weather visibilities.
     * 
     * @return
     */
    public List<WeatherVisibility> getWeatherVisibilities() {
        return _weatherVisibilities;
    }

    public List<WeatherType> getWeatherTypes() {
        return _weatherTypes;
    }

    /**
     * Returns the weather definition.
     * 
     * @return
     */
    public WxDefinition getWxDefinition() {
        return _wxDefinition;
    }

    /**
     * Returns the discrete definition.
     * 
     * @return
     */
    public DiscreteDefinition getDiscreteDefinition() {
        return _discreteDefinition;
    }

    public List<ProjectionData> projectionData() {
        return _projectionData;
    }

    /**
     * Returns the list of site identifiers known by this server.
     * 
     * @return
     */
    public List<String> getSiteID() {
        return _siteID;
    }

    /**
     * Returns the list of timeZones associated with the known sites.
     * 
     * @return
     */
    public List<String> getTimeZones() {
        return _timeZones;
    }

    /**
     * Returns true if topography data should show below zero values.
     * 
     * @return
     */
    public boolean isTopoAllowedBelowZero() {
        return _topoAllowedBelowZero;
    }

    public void setServerCharacteristics(final SimpleServerConfig config) {
        _iscRoutingTableAddress = config.iscRoutingTableAddress;
        _requestedISCsites = config.requestedISCsites;
        _requestISC = config.requestISC;
        _sendiscOnSave = config.sendiscOnSave;
        _sendiscOnPublish = config.sendiscOnPublish;
        _requestedISCparms = config.requestedISCparms;
        _transmitScript = config.transmitScript;
        _serverHost = config.serverHost;
        _mhsid = config.mhsid;

        _tableFetchTime = config.tableFetchTime;
        _satDirs = config.satDirs;
        _netCDFDirs = config.netCDFDirs;
        _prdDir = config.prdDir;
        _logFilePurgeAfter = config.logFilePurgeAfter;
        _initMethods = config.initMethods;
        _accumulativeD2DElements = config.accumulativeD2DElements;
        _initSkips = config.initSkips;
        _d2dModels = config.d2dModels;
        gfeModels = new HashMap<String, String>();
        for (Entry<String, String> d2dModelEntry : _d2dModels.entrySet()) {
            gfeModels.put(d2dModelEntry.getValue(), d2dModelEntry.getKey());
        }
        _baseDir = config.baseDir;
        _allSites = config.allSites;
        _officeTypes = config.officeTypes;
        _autoConfigureNotifyTextProd = config.autoConfigureNotifyTextProd;

        // common database grid location
        ProjectionData dProj = new ProjectionData(config.domain.projectionID,
                config.domain.projectionType.ordinal(), config.domain.latLonLL,
                config.domain.latLonUR, config.domain.latLonOrigin,
                config.domain.stdParallelOne, config.domain.stdParallelTwo,
                config.domain.gridPointLL, config.domain.gridPointUR,
                config.domain.latIntersect, config.domain.lonCenter,
                config.domain.lonOrigin);

        _domain = new GridLocation(config.siteID.get(0), dProj,
                config.domain.gridSize, config.domain.domainOrigin,
                config.domain.domainExtent, config.timeZone.get(0));

        // Assign the projection data in the config to private data
        _projectionData = config.projectionData;

        // Fill up the projection class with the data defined in config land.
        // if (!Projection::addProjectionData(_projectionData))
        // {
        // logFatal << "Attempt to modify existing projection data is not
        // allowed"
        // << std::endl;
        // logFatal << "Server aborting..." << std::endl;
        // exit(1);
        // }

        setRpcPort(config.rpcPort);
        setAllowedNodes(config.allowedNodes);

        // set Weather Visibility and Weather Type
        setWeatherVisibilities(config.weatherVisibilities);
        setWeatherTypes(config.weatherTypes);

        // make the WxDefinition object
        _wxDefinition = new WxDefinition(_weatherTypes, _weatherVisibilities);

        // make the DiscreteDefinition object
        _discreteDefinition = new DiscreteDefinition();
        for (Map.Entry<String, List<String>> entry : config.discreteDefinitions
                .entrySet()) {
            String key = entry.getKey();
            List<String> value = entry.getValue();
            boolean overlaps = "OVERLAPS".equals(value.get(0));
            int auxLength = Integer.parseInt(value.get(1));
            List<DiscreteKeyDef> keyDefs = new ArrayList<DiscreteKeyDef>();
            for (int j = 2; j < value.size(); j += 2) {
                keyDefs.add(new DiscreteKeyDef(value.get(j), value.get(j + 1)));
            }
            _discreteDefinition
                    .addDefinition(key, overlaps, auxLength, keyDefs);
        }

        List<DatabaseID> singleton = new ArrayList<DatabaseID>();
        List<DatabaseID> official = new ArrayList<DatabaseID>();
        for (int i = 0; i < config.models.size(); i++) {
            SimpleModelConfig model = config.models.get(i);
            String tmp = model.siteID + '_' + model.format + '_' + model.type
                    + '_' + model.modelName + "_" + DatabaseID.NO_MODEL_TIME;
            DatabaseID id = new DatabaseID(tmp);
            if (!id.isValid()) {
                theLogger.error("Invalid model definition in config file: "
                        + tmp);
                continue;
            }

            if (model.singleton) {
                singleton.add(id);
            }
            if (model.official) {
                official.add(id);
            }

            setDesiredDbVersions(id, model.numVersions);
            setGridPurgeAge(id, model.gridPurgeAge);

            // get the correct ProjectionData object
            ProjectionData projData = getProjectionData(model.projectionID);

            setGridDbConfig(id, new GridDbConfig(model, getWxDefinition(),
                    projData, getDiscreteDefinition(), config.extraWEPrecision));
        }

        setSingletonDatabases(singleton);
        setOfficialDatabases(official);

        _siteID = config.siteID;
        _timeZones = config.timeZone;
        _topoAllowedBelowZero = config.allowTopoBelowZero;

        // add in the d2d specified version values, if any
        for (Map.Entry<String, Integer> entry : config.d2dVersions.entrySet()) {
            String key = entry.getKey();
            int versions = entry.getValue();
            if (versions < 1) {
                versions = 1;
            }

            // create DatabaseID from the key
            DatabaseID dbid = new DatabaseID(_siteID.get(0),
                    DatabaseID.DataType.GRID, "D2D", key,
                    DatabaseID.NO_MODEL_TIME);
            setDesiredDbVersions(dbid, versions);
        }
    }

    /**
     * @param port
     */
    public void setRpcPort(long port) {
        _rpcPort = port;
    }

    /**
     * @param nodes
     */
    public void setAllowedNodes(final List<String> nodes) {
        _allowedNodes = nodes;
    }

    /**
     * Set the singleton databases.
     * 
     * @param ids
     */
    public void setSingletonDatabases(final List<DatabaseID> ids) {
        _singletonDatabases = new ArrayList<DatabaseID>();
        for (DatabaseID id : ids) {
            _singletonDatabases.add(stripTime(id));
        }
    }

    /**
     * Set the official databases.
     * 
     * @param ids
     */
    public void setOfficialDatabases(final List<DatabaseID> ids) {
        _officialDatabases = new ArrayList<DatabaseID>();
        for (DatabaseID id : ids) {
            _officialDatabases.add(stripTime(id));
        }
    }

    /**
     * Sets desired database versions for the given id.
     * 
     * @param id
     * @param nver
     */
    public void setDesiredDbVersions(final DatabaseID id, int nver) {
        _desiredDbVersions.put(convertToString(stripTime(id)), nver);
    }

    /**
     * Set the desired grid purging age in hours for the given id.
     * 
     * @param id
     * @param purgeHours
     */
    public void setGridPurgeAge(final DatabaseID id, int purgeHours) {
        _gridPurgeAge.put(convertToString(stripTime(id)), purgeHours);
    }

    public void setGridDbConfig(final DatabaseID id, final GridDbConfig config) {
        GridDbConfig newConfig = new GridDbConfig(config);
        _gridDbConfig.put(convertToString(stripTime(id)), newConfig);
    }

    /**
     * Sets the weather visibilities.
     * 
     * @param vis
     */
    public void setWeatherVisibilities(final List<String> vis) {
        _weatherVisibilities = new ArrayList<WeatherVisibility>(vis.size());
        for (String v : vis) {
            _weatherVisibilities.add(new WeatherVisibility(v));
        }
    }

    public void setWeatherTypes(final List<SimpleWeatherTypeConfig> types) {
        _weatherTypes = new ArrayList<WeatherType>();

        for (SimpleWeatherTypeConfig type : types) {
            _weatherTypes.add(weatherType(type));
        }
    }

    private WeatherType weatherType(SimpleWeatherTypeConfig type) {
        List<WeatherCoverage> coverages = new ArrayList<WeatherCoverage>(
                type.coverages.size());
        for (DiscreteTerm cov : type.coverages) {
            coverages.add(new WeatherCoverage(cov.getSymbol(), cov
                    .getDescription()));
        }

        List<WeatherIntensity> intensities = new ArrayList<WeatherIntensity>(
                type.intensities.size());
        for (DiscreteTerm inten : type.intensities) {
            intensities.add(new WeatherIntensity(inten.getSymbol(), inten
                    .getDescription()));
        }

        List<WeatherAttribute> attributes = new ArrayList<WeatherAttribute>(
                type.attributes.size());
        for (DiscreteTerm attr : type.attributes) {
            attributes.add(new WeatherAttribute(attr.getSymbol(), attr
                    .getDescription()));
        }

        return new WeatherType(type.symbol, type.description, coverages,
                intensities, attributes);
    }

    /**
     * @param baseModel
     * @return
     */
    public List<String> initModels(final String baseModel) {
        List<String> rval = new ArrayList<String>();
        for (Map.Entry<String, List<String>> entry : _initMethods.entrySet()) {
            if (entry.getValue().contains(baseModel)) {
                rval.add(entry.getKey());
            }
        }

        return rval;
    }

    /**
     * Retrieves a list of GFE model names that are sources for a given smart
     * initialization module name.
     * 
     * @param initModule
     *            The init module name.
     * @return the models to which the specified smart init module is mapped, or
     *         null if there are no mappings for the module.
     */
    public List<String> gfeModelsFromInit(final String initModule) {
        return _initMethods.get(initModule);
    }

    /**
     * Returns true if the model is to be skipped for this hour.
     * 
     * @param baseModel
     * @param hour
     * @return
     */
    public boolean initSkip(final String baseModel, int hour) {
        List<Integer> hoursToSkip = _initSkips.get(baseModel);
        if (hoursToSkip != null) {
            if (hoursToSkip.contains(hour)) {
                return true;
            }
        }
        return false;
    }

    public void setProtocolVersion(long ver) {
        _protocolVersion = ver;
    }

    /**
     * Given the model name, returns list of accumulative weather elements.
     * These weather elements do not contain the level information.
     * 
     * @param baseModel
     * @return
     */
    public List<String> accumulativeD2DElements(final String baseModel) {
        List<String> elements = _accumulativeD2DElements.get(baseModel);
        if (elements == null) {
            elements = Collections.emptyList();
        }
        return elements;
    }

    public int logFilePurgeAfter() {
        return _logFilePurgeAfter;
    }

    public int tableFetchTime() {
        return _tableFetchTime;
    }

    public boolean autoConfigureNotifyTextProd() {
        return _autoConfigureNotifyTextProd;
    }

    public String prdDir() {
        return _prdDir;
    }

    public List<String> allSites() {
        return _allSites;
    }

    public List<String> officeTypes() {
        return _officeTypes;
    }

    public long iscPort() {
        return _iscPort;
    }

    public void iscPort(long p) {
        _iscPort = p;
    }

    public Map<String, String> satDirs() {
        return _satDirs;
    }

    public Map<String, String> netCDFDirs() {
        return _netCDFDirs;
    }

    public GridLocation dbDomain() {
        return _domain;
    }

    // isc configuration
    public Map<String, String> iscRoutingTableAddress() {
        return _iscRoutingTableAddress;
    }

    public List<String> requestedISCsites() {
        return _requestedISCsites;
    }

    public void setRequestedISCsites(final List<String> sites) {
        _requestedISCsites = sites;
    }

    public boolean requestISC() {
        return _requestISC;
    }

    public boolean sendiscOnSave() {
        return _sendiscOnSave;
    }

    public boolean sendiscOnPublish() {
        return _sendiscOnPublish;
    }

    public List<String> requestedISCparms() {
        return _requestedISCparms;
    }

    public void setRequestedISCparms(final List<String> parms) {
        _requestedISCparms = parms;
    }

    public String transmitScript() {
        return _transmitScript;
    }

    /**
     * Gets my office type
     * 
     * @return
     */
    public String officeType() {
        int index = allSites().indexOf(this.getSiteID().get(0));
        if (index != -1) {
            return officeTypes().get(index);
        }
        return "";
    }

    public String getOfficeType(String siteID) {
        int index = allSites().indexOf(siteID);
        if (index != -1) {
            return officeTypes().get(index);
        }
        return "";
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "RPC port: " + _rpcPort + "\n" + "Allowed Nodes: "
                + _allowedNodes + "\n" + "Singleton Databases: "
                + _singletonDatabases + "\n" + "Official Databases: "
                + _officialDatabases + "\n" + "Sites: " + _siteID + "\n"
                + "TimeZones: " + _timeZones + "\n";
    }
}
