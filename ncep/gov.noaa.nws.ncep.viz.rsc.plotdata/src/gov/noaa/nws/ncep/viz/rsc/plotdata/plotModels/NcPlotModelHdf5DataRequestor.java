package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;

import gov.noaa.nws.ncep.edex.common.metparameters.AbstractMetParameter;
import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.NotDerivableException;
import gov.noaa.nws.ncep.edex.common.metparameters.StationID;
import gov.noaa.nws.ncep.edex.common.metparameters.StationLatitude;
import gov.noaa.nws.ncep.edex.common.metparameters.StationLongitude;
import gov.noaa.nws.ncep.edex.common.metparameters.StationNumber;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube.QueryStatus;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer2;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery2;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefn;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefns;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefnsMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.NcPlotImageCreator.IPointInfoRenderingListener;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModelElement;
import gov.noaa.nws.ncep.viz.rsc.plotdata.pluginplotproperties.PluginPlotProperties;
import gov.noaa.nws.ncep.viz.rsc.plotdata.queue.QueueEntry;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.NcPlotResource2.Station;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.TimeLogger;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.Tracer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Semaphore;

import javax.measure.unit.NonSI;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.viz.pointdata.PointDataRequest;

public class NcPlotModelHdf5DataRequestor {
    StringBuffer sb = new StringBuffer();

    private Map<String, RequestConstraint> constraintMap;

    private Map<DataTime, Map<String, RequestConstraint>> mapOfFrameTimeToRC;

    public Map<DataTime, Map<String, RequestConstraint>> getMapOfFrameTimeToRC() {
        return mapOfFrameTimeToRC;
    }

    public void setMapOfFrameTimeToRC(
            Map<DataTime, Map<String, RequestConstraint>> mapOfFrameTimeToRC) {
        this.mapOfFrameTimeToRC = mapOfFrameTimeToRC;
    }

    private String[] parameters = null;

    private String plugin;

    private PlotModel plotModel;

    private double plotDensity = Double.MIN_VALUE;

    private String levelStr;

    public void setLevelStr(String levelStr) {
        this.levelStr = levelStr;
    }

    // map from the metParam name to an abstractMetParameter. The met parameter
    // will have a value set either from the pointDataContainer query or derived
    // from the db pointData. This map is passed to the PlotModelFactory.
    //
    private HashMap<String, AbstractMetParameter> paramsToPlot = null;

    // a map from the dbParam name to a list of AbstractMetParameter objects
    // used to hold the values from the pointDataContainer.
    //
    private HashMap<String, AbstractMetParameter> dbParamsMap = null;

    // A list of derivedParameters that need to be derived from the dbParamsMap.
    //
    private ArrayList<AbstractMetParameter> derivedParamsList = null;

    // a map for those parameters that determine their value from an
    // array of values from the DB. (this implements functionality previously
    // done
    // in the PlotModelFactory for the plotFunctionTable tag and now done with
    // the arrayIndex tag in the plotParameterDefn)
    //
    private HashMap<String, PlotParameterDefn> prioritySelectionsMap = null;

    private String latDbName = "latitude";

    private String lonDbName = "longitude";

    private Map<String, String> metParamNameToDbNameMap = null;

    private HashMap<String, AbstractMetParameter> allMetParamsMap = null;

    private PlotParameterDefns plotPrmDefns = null;

    private Set<String> setOfDBParamNamesForHdf5Query = null;

    private Set<String> setOfCondColoringParamNames = null;

    private JobPool dataRequestJobPool = null;

    private ConcurrentLinkedQueue<QueueEntry> queueOfStations = null;

    private TimeLogger timeLogger;

    public NcPlotImageCreator imageCreator = null;

    private Set<String> setOfCondDerivedMetParamNames = null;

    private Semaphore sem1 = new Semaphore(1);

    // private NCMapDescriptor mapDescriptor;
    ConditionalFilter conditionalFilter = null;

    Map<String, RequestConstraint> condFilterMap = null;

    // Map< AbstractMetParameter, RequestConstraint >
    // condMetParamReqConstraintMap = null;
    public void queueStationsForHdf5Query(DataTime dt,
            Collection<Station> listOfStations) {
        Tracer.print("> Entry");
        QueueEntry queueEntry = new QueueEntry(dt, listOfStations);
        queueOfStations.add(queueEntry);
        Tracer.print("About to query HDF5 data for frame: "
                + Tracer.shortTimeString(dt));

        runDataQuery();
        Tracer.print("< Exit");
    }

    private void runDataQuery() {
        Tracer.print("> Entry");

        if (queueOfStations.peek() == null)
            return;

        while (queueOfStations.peek() != null) {
            QueueEntry qe = queueOfStations.poll();
            if (qe != null) {
                GetDataTask task = new GetDataTask(qe.getStations(),
                        qe.getDataTime());
                dataRequestJobPool.schedule(task);
            }
        }
        Tracer.print("< Exit");
    }

    public NcPlotModelHdf5DataRequestor(PlotModel plotModel, String level,
            Map<String, RequestConstraint> constraintMap,
            IPointInfoRenderingListener listener, double initialPlotDensity,
            ConditionalFilter cf) {

        Tracer.print("> Entry");
        this.plugin = plotModel.getPlugin();
        this.levelStr = level;
        this.constraintMap = constraintMap;
        paramsToPlot = new HashMap<String, AbstractMetParameter>();
        derivedParamsList = new ArrayList<AbstractMetParameter>();
        dbParamsMap = new HashMap<String, AbstractMetParameter>();
        prioritySelectionsMap = new HashMap<String, PlotParameterDefn>();
        allMetParamsMap = new HashMap<String, AbstractMetParameter>();
        plotPrmDefns = PlotParameterDefnsMngr.getInstance().getPlotParamDefns(
                plotModel.getPlugin());
        dataRequestJobPool = new JobPool("Requesting HDF5 data...", 8, false);
        queueOfStations = new ConcurrentLinkedQueue<QueueEntry>();
        parameters = new String[0];
        metParamNameToDbNameMap = new HashMap<String, String>();
        setOfDBParamNamesForHdf5Query = new HashSet<String>(2);
        imageCreator = new NcPlotImageCreator(listener, plotModel,
                initialPlotDensity);
        timeLogger = TimeLogger.getInstance();
        setOfCondColoringParamNames = new HashSet<String>(0);
        plotDensity = initialPlotDensity;
        conditionalFilter = cf;
        mapOfFrameTimeToRC = new HashMap<DataTime, Map<String, RequestConstraint>>();
        try {
            setOfCondDerivedMetParamNames = new HashSet<String>(0);
            establishPlotParamDefnToMetParamMappings();
            updateListOfParamsToPlotFromCurrentPlotModel(plotModel);
            if (conditionalFilter != null) {
                setUpConditionalFilterParameters();
            }
            if (plotModel.hasAdvancedSettings())
                determineConditionalColoringParameters(plotModel);
        } catch (VizException e) {
            e.printStackTrace();
        }
        Tracer.print("< Exit");

    }

    public ConditionalFilter getConditionalFilter() {
        return conditionalFilter;
    }

    public void setConditionalFilter(ConditionalFilter conditionalFilter) {
        if (conditionalFilter != null)
            this.conditionalFilter = new ConditionalFilter(conditionalFilter);
        else
            this.conditionalFilter = null;
    }

    public void setPlotModel(PlotModel pm) {
        plotModel = pm;
    }

    public void determineConditionalColoringParameters(PlotModel plotModel) {
        PlotParameterDefns plotParamDefns = PlotParameterDefnsMngr
                .getInstance().getPlotParamDefns(plotModel.getPlugin());

        Tracer.print("> Entry");
        List<PlotModelElement> listOfPlotModelElements = plotModel
                .getAllPlotModelElements();
        if (listOfPlotModelElements != null
                && !listOfPlotModelElements.isEmpty()) {
            for (PlotModelElement pme : listOfPlotModelElements) {
                String condParamName = pme.getConditionalParameter();
                if (condParamName != null) {
                    PlotParameterDefn thisPlotParamDefn = plotParamDefns
                            .getPlotParamDefn(condParamName);
                    if (thisPlotParamDefn != null) {
                        if (thisPlotParamDefn.getDeriveParams() != null) {
                            addToDerivedParamsList(
                                    thisPlotParamDefn.getDeriveParams(),
                                    thisPlotParamDefn);
                        } else {
                            String dbPrmName = thisPlotParamDefn
                                    .getDbParamName();
                            MetParameterFactory.getInstance().alias(
                                    thisPlotParamDefn.getMetParamName(),
                                    dbPrmName);
                            AbstractMetParameter condColoringParam = MetParameterFactory
                                    .getInstance()
                                    .createParameter(
                                            thisPlotParamDefn.getMetParamName(),
                                            thisPlotParamDefn.getPlotUnit());
                            if (!dbParamsMap.containsKey(dbPrmName))
                                dbParamsMap.put(dbPrmName, condColoringParam);
                        }

                        setOfCondColoringParamNames.add(thisPlotParamDefn
                                .getMetParamName());
                    }

                }
            }

        }
        Tracer.print("< Exit");
    }

    public void updateConditionalFilterMapFromConditionalFilter(
            ConditionalFilter cf) {
        Tracer.print("> Entry");
        if (cf != null)
            condFilterMap = new HashMap<String, RequestConstraint>(
                    cf.getConditionalFilterMap());
        else {
            if (condFilterMap != null)
                condFilterMap.clear();
        }
        Tracer.print("< Exit");
    }

    public Map<String, RequestConstraint> getConditionalFilterMap() {
        return condFilterMap;
    }

    /**
     * Filters the stations assuming that the conditional filter has parameters
     * that are available in the plot model
     * 
     * @param dataTime
     * @param stationSet
     */
    public synchronized void updateListOfStationsPerConditionalFilter(
            DataTime dataTime, Set<Station> stationSet) {
        Tracer.print("> Entry");
        Set<Station> filteredSetOfStations = new HashSet<Station>(0);
        if (conditionalFilter != null) {
            updateConditionalFilterMapFromConditionalFilter(this.conditionalFilter);

            synchronized (stationSet) {
                for (Station station : stationSet) {
                    if (station.listOfParamsToPlot == null
                            || station.listOfParamsToPlot.isEmpty())
                        continue;

                    List<Boolean> displayPlotBoolList = new ArrayList<Boolean>(
                            station.listOfParamsToPlot.size());
                    boolean displayStation = true;
                    synchronized (station.listOfParamsToPlot) {
                        for (AbstractMetParameter metPrm : station.listOfParamsToPlot) {
                            displayPlotBoolList
                                    .add(doesStationPassTheFilterForThisMetParam(metPrm));
                        }
                    }
                    synchronized (displayPlotBoolList) {
                        for (Boolean b : displayPlotBoolList) {
                            displayStation &= b;
                        }

                        if (displayStation) {
                            synchronized (filteredSetOfStations) {
                                filteredSetOfStations.add(station);
                            }
                        }

                    }
                }
            }
        }
        imageCreator.isThereAConditionalFilter = true;
        imageCreator.queueStationsToCreateImages(dataTime,
                filteredSetOfStations, plotDensity);
        // return filteredSetOfStations;
        Tracer.print("< Exit");
    }

    public void setUpConditionalFilterParameters() {
        Tracer.print("> Entry");
        if (conditionalFilter != null) {
            updateConditionalFilterMapFromConditionalFilter(this.conditionalFilter);
            if (condFilterMap == null || condFilterMap.isEmpty())
                return;
            if (!setOfCondDerivedMetParamNames.isEmpty())
                setOfCondDerivedMetParamNames.clear();

            List<PlotParameterDefn> listOfAllPlotParamDefnsForThisPlugin = plotPrmDefns
                    .getParameterDefns();
            for (PlotParameterDefn eachPlotParamDefn : listOfAllPlotParamDefnsForThisPlugin) {
                String plotParamName = eachPlotParamDefn.getPlotParamName();
                if (condFilterMap.containsKey(plotParamName)) {
                    AbstractMetParameter condMetParam = null;
                    if (eachPlotParamDefn.getDeriveParams() != null) {
                        setOfCondDerivedMetParamNames.add(eachPlotParamDefn
                                .getMetParamName());
                        condMetParam = addToDerivedParamsList(
                                eachPlotParamDefn.getDeriveParams(),
                                eachPlotParamDefn);

                    } else {
                        MetParameterFactory.getInstance().alias(
                                eachPlotParamDefn.getMetParamName(),
                                eachPlotParamDefn.getDbParamName());
                        condMetParam = MetParameterFactory.getInstance()
                                .createParameter(
                                        eachPlotParamDefn.getMetParamName(),
                                        eachPlotParamDefn.getPlotUnit());
                        String dbParamName = eachPlotParamDefn.getDbParamName();
                        if (!dbParamsMap.containsKey(dbParamName)) {
                            dbParamsMap.put(dbParamName, condMetParam);
                            setOfDBParamNamesForHdf5Query.add(dbParamName);
                        }

                    }

                    determineParameterNamesForHdf5Query();
                }

            }

        }
        Tracer.print("< Exit");

    }

    public Boolean doesStationPassTheFilterForThisMetParam(
            AbstractMetParameter metPrm) {
        Tracer.print("> Entry");
        Boolean displayStationPlot = true;

        Set<String> condPlotParamNameSet = condFilterMap.keySet();
        List<PlotParameterDefn> listOfPlotParamDefns = plotPrmDefns
                .getParameterDefns();

        for (PlotParameterDefn plotPrmDefn : listOfPlotParamDefns) {
            if (plotPrmDefn.getMetParamName().compareTo(
                    metPrm.getMetParamName()) == 0) {
                String plotParamName = plotPrmDefn.getPlotParamName();
                for (String condPlotParamName : condPlotParamNameSet) {
                    if (condPlotParamName.compareTo(plotParamName) == 0) {

                        RequestConstraint reqConstraint = condFilterMap
                                .get(condPlotParamName);
                        if (reqConstraint == null)
                            continue;

                        AbstractMetParameter condMetParam = MetParameterFactory
                                .getInstance().createParameter(
                                        plotPrmDefn.getMetParamName(),
                                        plotPrmDefn.getPlotUnit());

                        try {
                            if (!condMetParam.hasStringValue())
                                condMetParam.setValue(metPrm
                                        .getValueAs(condMetParam.getUnitStr()),
                                        condMetParam.getUnit());
                            else
                                condMetParam.setStringValue(metPrm
                                        .getStringValue());

                            String formattedPlotString = null;
                            String plotFormat = plotPrmDefn.getPlotFormat();
                            if (plotFormat != null) {
                                formattedPlotString = new String(
                                        condMetParam
                                                .getFormattedString(plotFormat));
                            } else {
                                if (condMetParam.hasStringValue())
                                    formattedPlotString = new String(
                                            condMetParam.getStringValue());
                                else
                                    formattedPlotString = new String(
                                            Double.toString(condMetParam
                                                    .getValueAs(
                                                            condMetParam
                                                                    .getUnitStr())
                                                    .doubleValue()));
                            }

                            int plotTrim = 0;
                            if (plotPrmDefn.getPlotTrim() == null) {
                                plotTrim = 0;
                            } else {
                                plotTrim = Integer.parseInt(plotPrmDefn
                                        .getPlotTrim());
                            }

                            if (plotTrim != 0) {
                                formattedPlotString = formattedPlotString
                                        .substring(plotTrim);
                            }

                            boolean result = condMetParam.hasStringValue() ? reqConstraint
                                    .evaluate(formattedPlotString)
                                    : reqConstraint.evaluate(Double
                                            .parseDouble(formattedPlotString));

                            if (result) {
                                displayStationPlot = true;
                                break;
                            } else {
                                displayStationPlot = false;
                                break;

                            }
                        } catch (Exception e) {
                            displayStationPlot = false;
                            break;
                        }

                    }
                }
            }
        }
        Tracer.print("< Exit");

        return displayStationPlot;
    }

    private void establishPlotParamDefnToMetParamMappings() throws VizException {
        Tracer.print("> Entry");
        long t0 = System.nanoTime();
        List<PlotParameterDefn> listOfAllPlotParamDefnsForThisPlugin = plotPrmDefns
                .getParameterDefns();
        if (listOfAllPlotParamDefnsForThisPlugin != null
                && !listOfAllPlotParamDefnsForThisPlugin.isEmpty()) {
            for (PlotParameterDefn plotPrmDefn : listOfAllPlotParamDefnsForThisPlugin) {
                metParamNameToDbNameMap.put(plotPrmDefn.getMetParamName(),
                        plotPrmDefn.getDbParamName());
                // if this is a 'vector' parameter (ie windBarb or arrow) then
                // get the 2
                // component metParameters and make sure they exist.
                if (plotPrmDefn.isVectorParameter()) {
                    String[] vectParamNames = plotPrmDefn
                            .getMetParamNamesForVectorPlot();

                    if (vectParamNames == null) {
                        throw new VizException(
                                "Error plotting WindBarb or Arrow: Can't get components metParameters for "
                                        + plotPrmDefn.getPlotParamName());
                    }

                    for (String vectParam : vectParamNames) {

                        if (plotPrmDefns
                                .getPlotParamDefnsForMetParam(vectParam)
                                .isEmpty()) {
                            throw new VizException(
                                    "Error plotting WindBarb or Arrow : Can't find definition for component metParameter "
                                            + vectParam);
                        }
                    }
                } else { // if not a vector parameter
                    String dbPrmName = plotPrmDefn.getDbParamName();

                    if (dbPrmName == null) {
                        // ??derived
                        if (plotPrmDefn.getDeriveParams() != null) {
                            // TODO Do anything here at all?
                        } else
                            continue;
                    } else if (dbParamsMap.containsKey(dbPrmName)) {
                        continue;
                    } else { // if( !dbPrmName.equals("derived" ) ) {

                        // alias the db param name to the ncep param name.
                        // (This eliminates the need to have a direct mapping
                        // from the db name to
                        // the ncep param name.)
                        MetParameterFactory.getInstance().alias(
                                plotPrmDefn.getMetParamName(),
                                plotPrmDefn.getDbParamName());

                        // create a metParam that will hold the value from the
                        // db and which will
                        // be used to plot the plotParameter and possibly derive
                        // other parameter values.
                        //
                        AbstractMetParameter dbParam = MetParameterFactory
                                .getInstance().createParameter(
                                        plotPrmDefn.getMetParamName(),
                                        plotPrmDefn.getPlotUnit());
                        if (dbParam == null) {
                            System.out.println("Error creating metParameter "
                                    + plotPrmDefn.getMetParamName());
                        } else {
                            // add this prm to a map to tell us which db params
                            // are needed
                            // when querying the db
                            dbParamsMap.put(plotPrmDefn.getDbParamName(),
                                    dbParam);

                            // for parameters that need to lookup their value
                            // from an
                            // array of values based on a priority. (ie for
                            // skyCover to
                            // determine the highest level of cloud cover at any
                            // level)
                            //
                            prioritySelectionsMap.put(dbPrmName, plotPrmDefn);

                            // else TODO : check for arrayIndex
                        }
                    }
                }
            }

            // if the station lat/long is not in the defns file, add them here
            // since they
            // are needed by the PlotModelFactory to plot the data
            //
            if (!dbParamsMap.containsKey(latDbName)) {
                MetParameterFactory.getInstance().alias(
                        StationLatitude.class.getSimpleName(), latDbName);
                AbstractMetParameter latPrm = MetParameterFactory.getInstance()
                        .createParameter(StationLatitude.class.getSimpleName(),
                                NonSI.DEGREE_ANGLE);
                dbParamsMap.put(latDbName, latPrm);

            }

            if (!dbParamsMap.containsKey(lonDbName)) {
                MetParameterFactory.getInstance().alias(
                        StationLongitude.class.getSimpleName(), lonDbName);

                AbstractMetParameter longPrm = MetParameterFactory
                        .getInstance().createParameter(
                                StationLongitude.class.getSimpleName(),
                                NonSI.DEGREE_ANGLE);

                dbParamsMap.put(lonDbName, longPrm);

            }

            setOfDBParamNamesForHdf5Query.add(latDbName);
            setOfDBParamNamesForHdf5Query.add(lonDbName);

        }

        long t1 = System.nanoTime();
        Tracer.print(" establishPlotParamDefnToMetParamMappings() took "
                + (t1 - t0) / 1000000 + " ms");
        Tracer.print("< Exit");
    }

    public Set<String> getNamesOfParamsPreviouslyPlotted() {
        return paramsToPlot.keySet();
    }

    public void updateListOfParamsToPlotFromCurrentPlotModel(PlotModel plotModel)
            throws VizException {
        Tracer.print("> Entry");
        long t0 = System.nanoTime();

        if (setOfDBParamNamesForHdf5Query != null
                && !setOfDBParamNamesForHdf5Query.isEmpty())
            setOfDBParamNamesForHdf5Query.clear();

        setOfDBParamNamesForHdf5Query.add(latDbName);
        setOfDBParamNamesForHdf5Query.add(lonDbName);

        if (derivedParamsList != null && !derivedParamsList.isEmpty())
            derivedParamsList.clear();

        // if ( dbParamsMap != null && !dbParamsMap.isEmpty())
        // dbParamsMap.clear();

        if (paramsToPlot != null & !paramsToPlot.isEmpty())
            paramsToPlot.clear();

        paramsToPlot.put(StationLatitude.class.getSimpleName(),
                dbParamsMap.get(latDbName));

        paramsToPlot.put(StationLongitude.class.getSimpleName(),
                dbParamsMap.get(lonDbName));

        if (setOfCondColoringParamNames != null
                && !setOfCondColoringParamNames.isEmpty())
            setOfCondColoringParamNames.clear();

        // if(metParamNameToDbNameMap != null &&
        // !metParamNameToDbNameMap.isEmpty())
        // metParamNameToDbNameMap.clear();

        List<String> listOfSelectedPlotParameters = plotModel
                .getPlotParamNames(true);
        for (String pltPrmName : listOfSelectedPlotParameters) {

            // get the dbParamName and determine if derived parameter
            //
            PlotParameterDefn plotPrmDefn = plotPrmDefns
                    .getPlotParamDefn(pltPrmName);

            if (plotPrmDefn == null) {
                throw new VizException("Error creating plot metParameter "
                        + pltPrmName);
            } else if (plotPrmDefn.isVectorParameter()) {
                // 'Vector' parameters for windBarbs and arrows are required to
                // be in the center (WD) position
                // Also, no plotDefns should have a plotMode of barb or arrow if
                // not in the center position.

                // add the 2 metParameters to paramsToPlot.
                String[] vectParamNames = plotPrmDefn
                        .getMetParamNamesForVectorPlot();
                for (String vectParam : vectParamNames) {

                    PlotParameterDefn vectPrmDefn = plotPrmDefns
                            .getPlotParamDefnsForMetParam(vectParam).get(0);
                    addToParamsToPlot(vectPrmDefn);
                }
            } else {
                addToParamsToPlot(plotPrmDefn);
            }
        }

        determineParameterNamesForHdf5Query();
        long t1 = System.nanoTime();
        Tracer.print(" updateListOfParamsToPlotFromCurrentPlotModel() took "
                + (t1 - t0) / 1000000 + " ms");
        Tracer.print("< Exit");
    }

    public void setPlotDensity(double density) {
        Tracer.print("> Entry");
        plotDensity = density;
        Tracer.print("< Exit");
    }

    public void setDefaultConstraintsMap(Map<String, RequestConstraint> inMap) {
        Tracer.print("> Entry");
        this.constraintMap = new HashMap<String, RequestConstraint>(inMap);
        Tracer.print("< Exit");
    }

    public void dispose() {
        Tracer.print("> Entry");
        Tracer.print("Invoking NcPlotModelHdf5DataRequestor.dispose()");
        if (dataRequestJobPool != null) {
            dataRequestJobPool.cancel();
            dataRequestJobPool = null;
        }
        imageCreator.dispose();
        Tracer.print("< Exit");
    }

    private void addToParamsToPlot(PlotParameterDefn plotPrmDefn) {
        Tracer.print("> Entry");
        long t0 = System.nanoTime();
        String dbParamName = plotPrmDefn.getDbParamName();
        String metParamName = plotPrmDefn.getMetParamName();
        String[] deriveParams = plotPrmDefn.getDeriveParams();// the input args
                                                              // to derive()

        // if this is a derived parameter, create a metParameter to hold the
        // derived
        // value to be computed and plotted.
        //
        if (deriveParams != null) { // dbParamName.equals( "derived" ) ) {

            AbstractMetParameter derivedMetParam = addToDerivedParamsList(
                    deriveParams, plotPrmDefn);
            if (derivedMetParam == null)
                return;

            paramsToPlot.put(metParamName, derivedMetParam);
        }
        // if this is a dbParameter then save the metParameter from the
        // dbParamsMap
        // in the paramsToPlot map.
        //
        else if (dbParamName != null && dbParamsMap.containsKey(dbParamName)) {
            setOfDBParamNamesForHdf5Query.add(dbParamName);

            // if it is already in the map then we don't need to save it twice.
            if (!paramsToPlot.containsKey(metParamName)) {
                paramsToPlot.put(metParamName, dbParamsMap.get(dbParamName));
            }
        } else {
            System.out
                    .println("Sanity check : dbParamName is not in dbParamsMap");
        }

        // System.out.println("ParamsToPlot KeySet: "+paramsToPlot.keySet());
        long t1 = System.nanoTime();
        Tracer.print("addToParamsToPlot() took " + (t1 - t0) / 1000000 + " ms");
        Tracer.print("< Exit");
    }

    private AbstractMetParameter addToDerivedParamsList(String[] deriveParams,
            PlotParameterDefn plotPrmDefn) {
        Tracer.print("> Entry");
        long t0 = System.nanoTime();
        // if this is a derived parameter, create a metParameter to hold the
        // derived
        // value to be computed and plotted.
        //
        AbstractMetParameter derivedMetParam = MetParameterFactory
                .getInstance().createParameter(plotPrmDefn.getMetParamName(),
                        plotPrmDefn.getPlotUnit());

        if (derivedMetParam == null) {
            System.out.println("Error creating derived metParameter "
                    + plotPrmDefn.getMetParamName());
            return null;
        } else {
            // If all is set then all of the
            // available metParameters from the db query are used
            // when attempting to derive the parameter.
            // Otherwise, we are expecting a comma separated list of parameters
            //
            if ( // deriveParams.length > 1 &&
            !deriveParams[0].equalsIgnoreCase("all")) {

                ArrayList<String> preferedDeriveParameterNames = new ArrayList<String>();
                ArrayList<AbstractMetParameter> preferedDeriveParameters = new ArrayList<AbstractMetParameter>();

                for (String dPrm : deriveParams) {
                    AbstractMetParameter deriveInputParam = MetParameterFactory
                            .getInstance().createParameter(dPrm);

                    if (deriveInputParam != null) {
                        // MetParameterFactory.getInstance().isValidMetParameterName(
                        // dPrm ) ) {
                        preferedDeriveParameters.add(deriveInputParam);
                        preferedDeriveParameterNames.add(dPrm);
                    } else {
                        System.out.println("Warning : '" + dPrm
                                + " is not a valid metParameter name");
                        return null;
                    }
                }

                derivedMetParam
                        .setPreferedDeriveParameters(preferedDeriveParameterNames);
            }

            if (derivedMetParam.getDeriveMethod(dbParamsMap.values()) == null) {
                System.out.println("Unable to derive "
                        + derivedMetParam.getMetParamName()
                        + " from available parameters.");
                return null;
            }
            if (derivedParamsList.isEmpty())
                derivedParamsList.add(derivedMetParam);
            else {
                boolean addParam = true;
                for (AbstractMetParameter derivedMetPrmToCheck : derivedParamsList) {

                    if (derivedMetPrmToCheck.getMetParamName().compareTo(
                            derivedMetParam.getMetParamName()) == 0) {
                        // derivedMetParam.getListOfInputMetPrmNamesForDerivingThisMetParameter();
                        addParam = false;
                        break;
                    }

                }

                if (addParam)
                    derivedParamsList.add(derivedMetParam);
            }

        }
        long t1 = System.nanoTime();
        Tracer.print("addToDerivedParamsList() took " + (t1 - t0) / 1000000
                + " ms for " + derivedMetParam.getMetParamName());
        Tracer.print("< Exit");
        return derivedMetParam;

    }

    private void determineDBParamNamesForDerivedParameters() {
        Tracer.print("> Entry");
        long t0 = System.nanoTime();
        synchronized (derivedParamsList) {
            for (AbstractMetParameter derivedMetParameter : derivedParamsList) {
                List<String> inputPrmsList = derivedMetParameter
                        .getListOfInputMetPrmNamesForDerivingThisMetParameter();
                if (inputPrmsList != null && !inputPrmsList.isEmpty()) {
                    for (String metPrmName : inputPrmsList) {
                        setOfDBParamNamesForHdf5Query.add(metPrmName);
                    }
                }
            }
        }

        long t1 = System.nanoTime();
        Tracer.print("getListOfDbParamNamesForDerivedMetParamsArgs() "
                + (t1 - t0) / 1000000 + " ms");
        Tracer.print("< Exit");

    }

    private void determineParameterNamesForHdf5Query() {
        Tracer.print("> Entry");
        long t0 = System.nanoTime();

        Collection<AbstractMetParameter> metParamsToPlotCollection = paramsToPlot
                .values();
        synchronized (metParamsToPlotCollection) {
            for (AbstractMetParameter metPrm : metParamsToPlotCollection) {
                String dbName = metParamNameToDbNameMap.get(metPrm.getClass()
                        .getSimpleName());
                if (dbName != null)
                    setOfDBParamNamesForHdf5Query.add(dbName);
            }
        }
        if (this.derivedParamsList != null && !this.derivedParamsList.isEmpty()) {
            determineDBParamNamesForDerivedParameters();

        }

        this.parameters = new String[setOfDBParamNamesForHdf5Query.size()];
        setOfDBParamNamesForHdf5Query.toArray(parameters);
        long t1 = System.nanoTime();
        Tracer.print("determineParameterNamesForHdf5Query() took " + (t1 - t0)
                / 1000000 + " ms");
        Tracer.print("< Exit");
    }

    private Collection<Station> requestUpperAirData(
            List<Station> listOfStationsRequestingForData) {
        Tracer.print("> Entry");
        List<Boolean> displayStationPlotBoolList = new ArrayList<Boolean>(0);
        boolean displayStationPlot = false;
        int listSize = listOfStationsRequestingForData.size();
        long beginTime = 0;
        long endTime = Long.MAX_VALUE;
        Date refTime = null;
        List<String> stnIdLst = new ArrayList<String>(listSize);
        List<Long> rangeTimeLst = new ArrayList<Long>(listSize);
        Map<String, Station> mapOfStnidsWithStns = new HashMap<String, Station>();
        synchronized (listOfStationsRequestingForData) {
            for (Station currentStation : listOfStationsRequestingForData) {
                refTime = currentStation.info.dataTime.getRefTime();
                long stnTime = currentStation.info.dataTime.getValidTime()
                        .getTimeInMillis();
                beginTime = (beginTime < stnTime ? stnTime : beginTime);
                endTime = (endTime > stnTime ? stnTime : endTime);
                String stnId = new String(currentStation.info.stationId);
                stnIdLst.add(stnId);
                mapOfStnidsWithStns.put(stnId, currentStation);
                if (rangeTimeLst.contains(stnTime) == false) {
                    rangeTimeLst.add(stnTime);
                }

            }
        }
        NcSoundingQuery2 sndingQuery;
        try {
            sndingQuery = new NcSoundingQuery2(plugin, true, levelStr);
        } catch (Exception e1) {
            System.out.println("Error creating NcSoundingQuery2: "
                    + e1.getMessage());
            return null;
        }

        sndingQuery.setStationIdConstraints(stnIdLst);
        sndingQuery.setRangeTimeList(rangeTimeLst);
        sndingQuery.setRefTimeConstraint(refTime);
        sndingQuery.setTimeRangeConstraint(new TimeRange(beginTime, endTime));

        // for modelsounding data we need to set the name of the model (ie the
        // reportType)
        if (plugin.equals("modelsounding")) {
            if (!constraintMap.containsKey("reportType")) {
                System.out
                        .println("Error creating NcSoundingQuery2: missing modelName (reportType) for modelsounding plugin");
                return null;
            }
            sndingQuery.setModelName(constraintMap.get("reportType")
                    .getConstraintValue());
        }

        long t004 = System.nanoTime();
        NcSoundingCube sndingCube = sndingQuery.query();
        long t005 = System.nanoTime();
        Tracer.print("requestUpperAirData()-->sndingQuery.query() took "
                + (t005 - t004) / 1000000 + " ms");

        //
        // TODO -- This shouldn't be necessary, given Amount.getUnit() should
        // now heal itself
        // from a null unit by using the String.
        // Repair the 'unit' in the met params, if damaged (as in, nulled) in
        // transit.
        // System.out.println("PlotModelGenerator2.plotUpperAirData() begin fixing returned data...");
        if (sndingCube != null && sndingCube.getRtnStatus() == QueryStatus.OK) {
            List<NcSoundingProfile> listOfSoundingProfiles = sndingCube
                    .getSoundingProfileList();
            synchronized (listOfSoundingProfiles) {
                for (NcSoundingProfile sndingProfile : listOfSoundingProfiles) {
                    List<NcSoundingLayer2> listOfSoundingLayer = sndingProfile
                            .getSoundingLyLst2();
                    synchronized (listOfSoundingLayer) {
                        for (NcSoundingLayer2 sndingLayer : listOfSoundingLayer) {
                            Collection<AbstractMetParameter> metParamColl = sndingLayer
                                    .getMetParamsMap().values();
                            synchronized (metParamColl) {
                                for (AbstractMetParameter metPrm : metParamColl) {
                                    metPrm.syncUnits();
                                }
                            }
                        }
                    }
                }
            }
        }
        // System.out.println("PlotModelGenerator2.plotUpperAirData() done fixing returned data");
        // TODO -- End
        //

        if (sndingCube != null && sndingCube.getRtnStatus() == QueryStatus.OK) {
            List<NcSoundingProfile> listOfSoundingProfiles = sndingCube
                    .getSoundingProfileList();
            if (listOfSoundingProfiles == null
                    || listOfSoundingProfiles.isEmpty())
                return null;
            synchronized (listOfSoundingProfiles) {
                for (NcSoundingProfile sndingProfile : listOfSoundingProfiles) {
                    Station currentStation = mapOfStnidsWithStns
                            .get(sndingProfile.getStationId());

                    /*
                     * Clear the existing list of parameters to plot in each
                     * station - to guarantee an updated list if there is a
                     * re-query for parameters by editing the plot model
                     */
                    if (currentStation.listOfParamsToPlot != null
                            && !currentStation.listOfParamsToPlot.isEmpty()) {
                        synchronized (currentStation.listOfParamsToPlot) {
                            currentStation.listOfParamsToPlot.clear();
                        }
                    }

                    if (sndingProfile.getSoundingLyLst2().isEmpty()
                            || sndingProfile.getSoundingLyLst2().size() != 1) {
                        continue;
                    }

                    NcSoundingLayer2 sndingLayer = sndingProfile
                            .getSoundingLyLst2().get(0);
                    Map<String, AbstractMetParameter> soundingParamsMap = sndingLayer
                            .getMetParamsMap();
                    // set all the paramsToPlot values to missing. (All the
                    // metParams in the paramsToPlot map are references into the
                    // derivedParamsMap and the dbParamsMap.)
                    //
                    for (AbstractMetParameter metPrm : derivedParamsList) {
                        metPrm.setValueToMissing();
                    }
                    synchronized (setOfDBParamNamesForHdf5Query) {
                        for (String dbPrmName : setOfDBParamNamesForHdf5Query) {
                            AbstractMetParameter metPrm = dbParamsMap
                                    .get(dbPrmName);
                            if (metPrm == null)
                                continue;

                            AbstractMetParameter newInstance = newInstance(metPrm);
                            if (newInstance == null)
                                continue;
                            // TODO : the station lat/lon, elev, name and id
                            // should be set in the sounding profile
                            // but currently isn't. So instead we will get the
                            // lat/lon and id from the DBQuery.
                            String key = newInstance.getMetParamName();
                            if (soundingParamsMap.containsKey(key)) {
                                AbstractMetParameter queriedParam = soundingParamsMap
                                        .get(key);
                                if (newInstance.hasStringValue())
                                    newInstance.setStringValue(queriedParam
                                            .getStringValue());
                                else
                                    newInstance.setValue(
                                            queriedParam.getValue(),
                                            queriedParam.getUnit());
                            }

                            else if (newInstance.getMetParamName().equals(
                                    StationLatitude.class.getSimpleName())) {
                                newInstance.setValue(new Amount(sndingProfile
                                        .getStationLatitude(),
                                        NonSI.DEGREE_ANGLE));
                            } else if (newInstance.getMetParamName().equals(
                                    StationLongitude.class.getSimpleName())) {
                                newInstance.setValue(new Amount(sndingProfile
                                        .getStationLongitude(),
                                        NonSI.DEGREE_ANGLE));
                            }
                            // else if( metPrm.getMetParamName().equals(
                            // StationElevation.class.getSimpleName() ) ) {
                            // // metPrm.setValue( new Amount(
                            // // sndingProfile.getStationElevation(), SI.METER
                            // ) );
                            // }
                            else if (newInstance.getMetParamName().equals(
                                    StationID.class.getSimpleName())) {
                                if (!sndingProfile.getStationId().isEmpty()) {
                                    newInstance.setStringValue(sndingProfile
                                            .getStationId());
                                } else {
                                    newInstance.setValueToMissing();
                                }
                                // if( stnInfo.stationId != null &&
                                // !stnInfo.stationId.isEmpty() ) {
                                // metPrm.setStringValue( stnInfo.stationId );
                                // }
                            } else if (newInstance.getMetParamName().equals(
                                    StationNumber.class.getSimpleName())) {
                                if (sndingProfile.getStationNum() != 0) {
                                    newInstance.setStringValue(new Integer(
                                            sndingProfile.getStationNum())
                                            .toString());
                                } else {
                                    newInstance.setValueToMissing();
                                }
                            } else {
                                // System.out.println("Sanity check: " +
                                // metPrm.getMetParamName() +
                                // " is not available in the sounding data");
                            }

                            // newInstance.setValueToMissing();
                            if (condFilterMap != null
                                    && !condFilterMap.isEmpty()) {
                                displayStationPlotBoolList
                                        .add(doesStationPassTheFilterForThisMetParam(metPrm));
                            }

                            // boolean found = false;
                            if (paramsToPlot.containsKey(newInstance
                                    .getMetParamName())) {

                                currentStation.listOfParamsToPlot
                                        .add(newInstance);
                            }

                            allMetParamsMap.put(newInstance.getMetParamName(),
                                    newInstance);
                            // paramsToPlot = new HashMap<String,
                            // AbstractMetParameter>(paramsToPlot);

                            // TODO : for modelsoundings. what are the units?
                            // else if( metPrm.getMetParamName().equals(
                            // VerticalVelocity.class.getSimpleName() ) ) {
                            // metPrm.setValue( new Amount(
                            // sndingLayer.getOmega(), ) );
                            // }

                        }

                    }
                    Collection<AbstractMetParameter> metPrmCollection = soundingParamsMap
                            .values();// dbParamsMap.values();
                    synchronized (derivedParamsList) {
                        for (AbstractMetParameter derivedParam : derivedParamsList) {
                            try {
                                synchronized (metPrmCollection) {
                                    derivedParam.derive(metPrmCollection);
                                }
                                AbstractMetParameter clonedDerivedPrm = newInstance(derivedParam);// .getClass().newInstance();

                                if (clonedDerivedPrm == null)
                                    continue;

                                if (paramsToPlot.containsKey(derivedParam
                                        .getMetParamName())) {
                                    // if (
                                    // !currentStation.listOfParamsToPlot.contains(
                                    // clonedDerivedPrm ))
                                    currentStation.listOfParamsToPlot
                                            .add(clonedDerivedPrm);
                                }

                                allMetParamsMap.put(
                                        clonedDerivedPrm.getMetParamName(),
                                        clonedDerivedPrm);

                            } catch (NotDerivableException e) {
                                e.printStackTrace();
                            }

                        }
                    }
                    /*
                     * Validate the station against conditionally derived
                     * MetParameters
                     */
                    if (condFilterMap != null && !condFilterMap.isEmpty()) {
                        synchronized (setOfCondDerivedMetParamNames) {
                            for (String condMetParamName : setOfCondDerivedMetParamNames) {
                                synchronized (derivedParamsList) {
                                    for (AbstractMetParameter condDerivedParamToCheck : derivedParamsList) {
                                        if (condDerivedParamToCheck
                                                .getMetParamName().compareTo(
                                                        condMetParamName) == 0) {
                                            if (condDerivedParamToCheck
                                                    .hasValidValue())
                                                displayStationPlotBoolList
                                                        .add(doesStationPassTheFilterForThisMetParam(condDerivedParamToCheck));
                                        }
                                    }

                                }
                            }
                        }
                    }

                    /*
                     * Process the conditional parameter(s) (if any) for the
                     * station
                     */
                    if (setOfCondColoringParamNames != null
                            && !setOfCondColoringParamNames.isEmpty()) {
                        Collection<AbstractMetParameter> dbMetParamColl = dbParamsMap
                                .values();
                        synchronized (setOfCondColoringParamNames) {
                            for (String condColorParamName : setOfCondColoringParamNames) {

                                currentStation = processConditionalParameterForEachStation(
                                        dbMetParamColl, currentStation,
                                        condColorParamName);

                                currentStation = processConditionalParameterForEachStation(
                                        derivedParamsList, currentStation,
                                        condColorParamName);
                            }
                        }
                    }

                    /*
                     * Evaluate the station against the conditional filter to
                     * decide if it needs to be plotted at all
                     */
                    if (condFilterMap != null && !condFilterMap.isEmpty()) {
                        displayStationPlot = true;
                        synchronized (displayStationPlotBoolList) {
                            for (Boolean b : displayStationPlotBoolList) {
                                displayStationPlot = displayStationPlot && b;
                            }
                        }

                        synchronized (mapOfStnidsWithStns) {
                            if (displayStationPlot)
                                mapOfStnidsWithStns.put(
                                        currentStation.info.stationId,
                                        currentStation);
                            else
                                mapOfStnidsWithStns
                                        .remove(currentStation.info.stationId);
                        }
                    } else
                        mapOfStnidsWithStns.put(currentStation.info.stationId,
                                currentStation);
                }

            }
        }

        Tracer.print("< Exit");

        return (mapOfStnidsWithStns.values());
    }

    private Collection<Station> requestSurfaceData(DataTime time,
            List<Station> listOfStationsRequestingForData) {
        Tracer.print("> Entry  " + Tracer.shortTimeString(time));
        // sem1.acquireUninterruptibly();
        Map<String, Station> stationMap = new HashMap<String, Station>(
                listOfStationsRequestingForData.size());
        if (listOfStationsRequestingForData != null
                && !listOfStationsRequestingForData.isEmpty()) {
            try {
                int listSize = listOfStationsRequestingForData.size();
                Tracer.print(Tracer.shortTimeString(time)
                        + " listOfStationsRequesting for data has " + listSize
                        + " entries");
                Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();

                map.put("pluginName", constraintMap.get("pluginName"));
                Tracer.print(Tracer.shortTimeString(time) + " putting "
                        + constraintMap.get("pluginName")
                        + " as pluginName entry in map");

                DataTime[] dt = new DataTime[listSize];
                String[] stationIds = new String[listSize];
                RequestConstraint rc = new RequestConstraint();
                RequestConstraint timeConstraint = new RequestConstraint();
                timeConstraint.setConstraintType(ConstraintType.IN);
                rc.setConstraintType(ConstraintType.IN);
                int index = 0;
                PluginPlotProperties plotProp = PluginPlotProperties
                        .getPluginProperties(map);
                Tracer.print(Tracer.shortTimeString(time) + " plotProp "
                        + plotProp);

                synchronized (listOfStationsRequestingForData) {

                    for (Station currentStation : listOfStationsRequestingForData) {

                        dt[index] = currentStation.info.dataTime;
                        // if( index == 0 ){
                        // System.out.println("\n for the frame " +
                        // time.toString() + "dt[0] = " + dt[0].toString());
                        // }
                        stationIds[index] = currentStation.info.stationId;

                        if (plotProp.hasDistinctStationId) {
                            Tracer.print(Tracer.shortTimeString(time)
                                    + " "
                                    + currentStation.info.stationId
                                    + " plotProp.hasDistinctStationId TRUE; adding stationId to constraint value list ");
                            rc.addToConstraintValueList(currentStation.info.stationId);
                            timeConstraint
                                    .addToConstraintValueList(currentStation.info.dataTime
                                            .toString());
                        } else
                            Tracer.print(Tracer.shortTimeString(time)
                                    + " "
                                    + currentStation.info.stationId
                                    + " plotProp.hasDistinctStationId FALSE; adding dataURI "
                                    + currentStation.info.dataURI
                                    + " to constraint value list");
                        rc.addToConstraintValueList(currentStation.info.dataURI);

                        Tracer.print(Tracer.shortTimeString(time)
                                + " "
                                + currentStation.info.stationId
                                + " station entered into stationMap with key "
                                + formatLatLonKey(currentStation.info.latitude,
                                        currentStation.info.longitude));
                        stationMap.put(
                                formatLatLonKey(currentStation.info.latitude,
                                        currentStation.info.longitude),
                                currentStation);

                        ++index;
                    }
                }

                if (plotProp.hasDistinctStationId) {
                    Tracer.print(Tracer.shortTimeString(time)
                            + " Done with station loop; plotProp.hasDistinctStationId TRUE; adding location.stationId-to-rc entry to map");
                    map.put("location.stationId", rc);
                    // if (dt.length > 1 && !dt[0].equals(dt[1])) { //TODO: wtf?
                    // Tracer.print(Tracer.shortTimeString(time)
                    // + " dt.length ("
                    // + dt.length
                    // + ") > 1 AND dt[0] ("
                    // + dt[0]
                    // + ") !=  dt[1] ("
                    // + dt[1]
                    // + "); replacing dataTime-to-rc entry in map with RC "
                    // + timeConstraint);
                    map.put("dataTime", timeConstraint);
                    // } else {
                    // map.remove("dataTime");
                    // String timeStr = new String(dt[0].toString());
                    // RequestConstraint rct = new RequestConstraint(timeStr);
                    // // TODO
                    // // temporary
                    // // for
                    // // debug
                    // Tracer.print(Tracer.shortTimeString(time)
                    // + " dt.length ("
                    // + dt.length
                    // + ") == 0 OR dt[0] ("
                    // + dt[0]
                    // +
                    // ") == dt[1]; replacing dataTime-to-rc entry in map with RC based on timeStr "
                    // + timeStr);
                    // map.put("dataTime", rct);
                    // // map.put("dataTime", new RequestConstraint(timeStr));
                    // }
                } else {
                    Tracer.print(Tracer.shortTimeString(time)
                            + " Done with station loop; plotProp.hasDistinctStationId FALSE; putting dataURI-to-rc entry in map with rc "
                            + rc);
                    map.put("dataURI", rc);
                }

                sem1.acquireUninterruptibly();

                Tracer.print("About to query HDF5 data for frame: "
                        + Tracer.shortTimeString(time) + " HDF5 query map = "
                        + map);
                boolean displayStationPlot = false;
                long t0 = System.nanoTime();
                PointDataContainer pdc = null;
                pdc = DataCubeContainer.getPointData(plugin, this.parameters,
                        null, map);
                long t1 = System.nanoTime();
                Tracer.print("DataCubeContainer.getPointData() took "
                        + (t1 - t0) / 1000000 + " ms for frame "
                        + Tracer.shortTimeString(time));
                Tracer.print("Done with query HDF5 data for frame: "
                        + Tracer.shortTimeString(time) + " HDF5 query map = "
                        + map);

                sem1.release();

                int pdcSize = -1;
                if (pdc == null) {
                    if (dt != null && dt.length > 0) {

                        sem1.acquireUninterruptibly();

                        Tracer.print("About to call PointDataRequest.requestPointDataAllLevels(...) for frame: "
                                + Tracer.shortTimeString(time)
                                + "HDF5 query map = "
                                + map
                                + " Plugin "
                                + this.plugin
                                + " Parameters "
                                + this.parameters + " Stations " + stationIds);
                        pdc = PointDataRequest.requestPointDataAllLevels(
                                this.plugin, this.parameters, stationIds, map);
                        Tracer.print("Done with call PointDataRequest.requestPointDataAllLevels(...) for frame: "
                                + Tracer.shortTimeString(time)
                                + "HDF5 query map = "
                                + map
                                + " Plugin "
                                + this.plugin
                                + " Parameters "
                                + this.parameters + " Stations " + stationIds);

                        sem1.release();
                    }

                }

                if (pdc != null) {
                    Tracer.print("We have a non-null PDC for frame: "
                            + Tracer.shortTimeString(time)
                            + "HDF5 query map = " + map + " Plugin "
                            + this.plugin + " Parameters " + this.parameters
                            + " Stations " + stationIds + " PDC content:  "
                            + pdc);
                    pdcSize = pdc.getAllocatedSz();
                    Tracer.print("PDC for frame "
                            + Tracer.shortTimeString(time)
                            + " has allocated size " + pdc.getAllocatedSz()
                            + " and current size " + pdc.getCurrentSz());
                    pdc.setCurrentSz(pdcSize);
                    Tracer.print("PDC for frame "
                            + Tracer.shortTimeString(time)
                            + " now has allocated size " + pdc.getAllocatedSz()
                            + " and current size " + pdc.getCurrentSz());
                } else {
                    Tracer.print("< Exit  " + Tracer.shortTimeString(time)
                            + " ABNORMAL?  PDC is null");
                    return stationMap.values();
                }

                int stationMapSize = stationIds.length;
                if (pdcSize > stationMapSize) {
                    Tracer.print(Tracer.shortTimeString(time) + " pdcSize "
                            + pdcSize + " > stationMapSize " + stationMapSize
                            + " setting pdcSize = stationMapSize");
                    pdcSize = stationMapSize;
                }
                for (int uriCounter = 0; uriCounter < pdcSize; uriCounter++) {

                    PointDataView pdv = pdc.readRandom(uriCounter);
                    if (pdv == null) { // ???
                        Tracer.print(Tracer.shortTimeString(time)
                                + " PDV is null for station " + uriCounter
                                + " -- skipping");
                        continue;
                    }

                    DataTime dataTime = dt[uriCounter];

                    String key = new String(formatLatLonKey(
                            pdv.getFloat(latDbName), pdv.getFloat(lonDbName)));

                    Station currentStation = stationMap.get(key);
                    if (currentStation == null) {
                        Tracer.print(Tracer.shortTimeString(time) + " "
                                + Tracer.shortTimeString(dataTime)
                                + " stationMap entry not found for key " + key
                                + " -- skipping");
                        continue;
                    }
                    // TODO remove
                    boolean jfk = currentStation.info.stationId
                            .equalsIgnoreCase("KJFK");

                    Semaphore sm = new Semaphore(1);
                    sm.acquireUninterruptibly();
                    synchronized (paramsToPlot) {
                        Set<String> pkeySet = paramsToPlot.keySet();
                        Tracer.printX(Tracer.shortTimeString(time) + " "
                                + Tracer.shortTimeString(dataTime)
                                + " paramsToPlot for "
                                + currentStation.info.stationId + ":  "
                                + pkeySet);
                        synchronized (pkeySet) {
                            try {
                                for (String prmToPlotKey : pkeySet) {
                                    if (jfk)
                                        Tracer.print(Tracer
                                                .shortTimeString(time)
                                                + " "
                                                + Tracer.shortTimeString(dataTime)
                                                + currentStation.info.stationId
                                                + " "
                                                + " from pkeySet updating prmToPlot(Key) "
                                                + prmToPlotKey);
                                    AbstractMetParameter prmToPlot = paramsToPlot
                                            .get(prmToPlotKey);
                                    if (prmToPlot != null) {
                                        if (jfk)
                                            Tracer.print(Tracer
                                                    .shortTimeString(time)
                                                    + " "
                                                    + Tracer.shortTimeString(dataTime)
                                                    + currentStation.info.stationId
                                                    + " "
                                                    + " prmToPlot non-null "
                                                    + prmToPlot);
                                        prmToPlot.setValueToMissing();
                                        paramsToPlot.put(prmToPlot.getClass()
                                                .getSimpleName(), prmToPlot);
                                    } else {
                                        if (jfk)
                                            Tracer.print(Tracer
                                                    .shortTimeString(time)
                                                    + " "
                                                    + Tracer.shortTimeString(dataTime)
                                                    + currentStation.info.stationId
                                                    + " "
                                                    + " from pkeySet updating prmToPlot(Key) "
                                                    + " -- prmToPlot is NULL -- skipping!!!");
                                    }
                                }

                            } catch (Exception e) {
                                sm.release();
                            }
                        }
                    }
                    sm.release();
                    List<Boolean> displayStationPlotBoolList = new ArrayList<Boolean>(
                            0);

                    synchronized (setOfDBParamNamesForHdf5Query) {
                        Tracer.print(Tracer.shortTimeString(time) + " "
                                + Tracer.shortTimeString(dataTime)
                                + " setOfDBParamNamesForHdf5Query for "
                                + currentStation.info.stationId + ":  "
                                + setOfDBParamNamesForHdf5Query);

                        for (String dbPrm : setOfDBParamNamesForHdf5Query) {
                            AbstractMetParameter metPrm = dbParamsMap
                                    .get(dbPrm);
                            if (metPrm == null) {
                                if (jfk)
                                    Tracer.print(Tracer.shortTimeString(time)
                                            + " "
                                            + Tracer.shortTimeString(dataTime)
                                            + currentStation.info.stationId
                                            + " "
                                            + " NULL metPrm return from dbParamsMap for key -- skipping"
                                            + dbPrm);
                                continue;
                            }

                            // get the fillValue from the parameterDescription
                            // and use it to set the missingValue
                            // Sentinel for the metParameter
                            try {
                                ParameterDescription pDesc = pdc
                                        .getDescription(dbPrm);
                                if (pDesc != null) {
                                    if (pdv.getType(dbPrm) == null) {
                                        continue;
                                    }
                                    if (pDesc.getFillValue() == null) {
                                        System.out
                                                .println("Sanity Check: ParameterDescription fill Value is null");
                                        System.out
                                                .println("Update the DataStoreFactory.py and H5pyDataStore.py files");
                                        continue;
                                    }
                                    switch (pdv.getType(dbPrm)) {
                                    case FLOAT:
                                        metPrm.setMissingDataSentinel(pDesc
                                                .getFillValue().floatValue());
                                        break;
                                    case DOUBLE:
                                        metPrm.setMissingDataSentinel(pDesc
                                                .getFillValue());
                                        break;
                                    case INT:
                                        metPrm.setMissingDataSentinel(pDesc
                                                .getFillValue().intValue());
                                        break;
                                    case STRING:
                                        break;
                                    default:
                                        break;
                                    }
                                }
                            } catch (Exception e) {
                                Tracer.print("param " + dbPrm + " not found.");
                            }
                            if (jfk)
                                Tracer.print(Tracer.shortTimeString(time) + " "
                                        + Tracer.shortTimeString(dataTime)
                                        + currentStation.info.stationId + " "
                                        + " before setMetParamFromPDV " + dbPrm
                                        + " " + metPrm);

                            /*
                             * Set the value for Met parameters from the
                             * corresponding database value
                             */
                            setMetParamFromPDV(metPrm, pdv, dbPrm, dataTime);
                            if (jfk)
                                Tracer.print(Tracer.shortTimeString(time) + " "
                                        + Tracer.shortTimeString(dataTime)
                                        + currentStation.info.stationId + " "
                                        + " after setMetParamFromPDV " + dbPrm
                                        + " " + metPrm);

                            // if(
                            // metPrm.getMetParamName().compareTo("StationID")
                            // == 0 ){
                            // if(metPrm.getStringValue().compareTo(currentStation.info.stationId)
                            // != 0 ){
                            // System.out.println("ttd");
                            // }
                            // }

                            if (paramsToPlot.containsKey(metPrm
                                    .getMetParamName())) {
                                if (jfk)
                                    Tracer.print(Tracer.shortTimeString(time)
                                            + " "
                                            + Tracer.shortTimeString(dataTime)
                                            + " "
                                            + currentStation.info.stationId
                                            + " "
                                            + " paramsToPlot contains metPrm; putting "
                                            + metPrm.getMetParamName()
                                            + " into paramsToPlot");
                                paramsToPlot.put(metPrm.getMetParamName(),
                                        metPrm);
                            }

                            dbParamsMap.put(dbPrm, metPrm);
                            if (jfk)
                                Tracer.print(Tracer.shortTimeString(time) + " "
                                        + Tracer.shortTimeString(dataTime)
                                        + currentStation.info.stationId + " "
                                        + " after put " + dbPrm + " " + metPrm
                                        + " into dbParamsMap");

                            if (condFilterMap != null
                                    && !condFilterMap.isEmpty()) {
                                displayStationPlotBoolList
                                        .add(doesStationPassTheFilterForThisMetParam(metPrm));
                            }

                        }
                    }
                    List<AbstractMetParameter> metParamsToDisplay;
                    Collection<AbstractMetParameter> collectionOfMetParamsWithDBValues = dbParamsMap
                            .values();

                    synchronized (derivedParamsList) {
                        for (AbstractMetParameter derivedParam : derivedParamsList) {
                            try {
                                synchronized (collectionOfMetParamsWithDBValues) {
                                    derivedParam
                                            .derive(collectionOfMetParamsWithDBValues);
                                }
                                AbstractMetParameter clonedDerivedPrm = newInstance(derivedParam);// .getClass().newInstance();

                                if (clonedDerivedPrm == null) {
                                    Tracer.print(Tracer.shortTimeString(time)
                                            + " "
                                            + Tracer.shortTimeString(dataTime)
                                            + " clonedDerivedPrm NULL "
                                            + currentStation.info.stationId
                                            + " " + derivedParam
                                            + " -- skipping");
                                    continue;
                                }

                                clonedDerivedPrm.setValidTime(dataTime);
                                if (paramsToPlot.containsKey(derivedParam
                                        .getMetParamName())) {
                                    currentStation.listOfParamsToPlot
                                            .add(clonedDerivedPrm);
                                }

                                allMetParamsMap.put(
                                        clonedDerivedPrm.getMetParamName(),
                                        clonedDerivedPrm);

                            } catch (NotDerivableException e) {
                                e.printStackTrace();
                            }

                        }
                    }

                    /*
                     * Validate the station against a conditional derived
                     * MetParameter
                     */
                    if (condFilterMap != null && !condFilterMap.isEmpty()) {
                        for (String condMetParamName : setOfCondDerivedMetParamNames) {
                            synchronized (derivedParamsList) {
                                for (AbstractMetParameter condDerivedParamToCheck : derivedParamsList) {
                                    if (condDerivedParamToCheck
                                            .getMetParamName().compareTo(
                                                    condMetParamName) == 0) {
                                        if (condDerivedParamToCheck
                                                .hasValidValue())
                                            displayStationPlotBoolList
                                                    .add(doesStationPassTheFilterForThisMetParam(condDerivedParamToCheck));
                                    }
                                }

                            }
                        }
                    }

                    /*
                     * Clear the existing list of parameters to plot in each
                     * station - to guarantee an updated list if there is a
                     * re-query for parameters by editing the plot model
                     */
                    if (!currentStation.listOfParamsToPlot.isEmpty()) {
                        currentStation.listOfParamsToPlot.clear();
                        if (jfk)
                            Tracer.print(Tracer.shortTimeString(time)
                                    + " "
                                    + Tracer.shortTimeString(dataTime)
                                    + " CLEARING nonempty listOfParamsToPlot for "
                                    + currentStation.info.stationId);
                    }
                    sm.acquireUninterruptibly();
                    metParamsToDisplay = new ArrayList<AbstractMetParameter>(
                            paramsToPlot.values());
                    if (jfk)
                        Tracer.print(Tracer.shortTimeString(time) + " "
                                + Tracer.shortTimeString(dataTime)
                                + " metParamsToDisplay for "
                                + currentStation.info.stationId + " "
                                + metParamsToDisplay);
                    synchronized (metParamsToDisplay) {
                        try {
                            for (AbstractMetParameter metParam : metParamsToDisplay) {
                                /*
                                 * Creating a fresh copy of the met parameter
                                 * seems to be the only way that each station
                                 * retains a unique set of values as queried (or
                                 * derived).Otherwise all stations in a frame
                                 * get the MetParameter values of the last
                                 * station being processed since the list
                                 * currentStation.listOfParamsToPlot references
                                 * the AbstractMetParametervalues from
                                 * paramsToPlot
                                 */
                                if (jfk)
                                    Tracer.print(Tracer.shortTimeString(time)
                                            + " "
                                            + Tracer.shortTimeString(dataTime)
                                            + currentStation.info.stationId
                                            + " " + " trying to add metParam "
                                            + metParam);
                                AbstractMetParameter newPrm = newInstance(metParam);
                                if (newPrm == null) {
                                    if (jfk)
                                        Tracer.print(Tracer
                                                .shortTimeString(time)
                                                + " "
                                                + Tracer.shortTimeString(dataTime)
                                                + currentStation.info.stationId
                                                + " "
                                                + " newPrm NULL from newInstance -- skipping!!! "
                                                + metParam);
                                    continue;
                                }
                                currentStation.listOfParamsToPlot.add(newPrm);
                                if (jfk)
                                    Tracer.print(Tracer.shortTimeString(time)
                                            + " "
                                            + Tracer.shortTimeString(dataTime)
                                            + currentStation.info.stationId
                                            + " " + " added newPrm " + newPrm);
                            }

                        } catch (Exception e) {
                            sm.release();
                        }
                    }
                    sm.release();
                    /*
                     * Process the conditional parameter(s) (if any) for the
                     * station
                     */
                    if (setOfCondColoringParamNames != null
                            && !setOfCondColoringParamNames.isEmpty()) {
                        Collection<AbstractMetParameter> dbMetParamColl = dbParamsMap
                                .values();
                        synchronized (setOfCondColoringParamNames) {
                            for (String condColorParamName : setOfCondColoringParamNames) {

                                currentStation = processConditionalParameterForEachStation(
                                        dbMetParamColl, currentStation,
                                        condColorParamName);

                                currentStation = processConditionalParameterForEachStation(
                                        derivedParamsList, currentStation,
                                        condColorParamName);
                            }
                        }
                    }

                    /*
                     * Evaluate the station against the conditional filter to
                     * decide if it needs to be plotted at all
                     */

                    if (condFilterMap != null && !condFilterMap.isEmpty()) {

                        displayStationPlot = true;
                        synchronized (displayStationPlotBoolList) {
                            for (Boolean b : displayStationPlotBoolList) {
                                displayStationPlot = displayStationPlot && b;
                            }
                        }

                        synchronized (stationMap) {
                            if (displayStationPlot)
                                stationMap.put(key, currentStation);
                            else
                                stationMap.remove(key);
                        }
                    } else
                        stationMap.put(key, currentStation);

                    // if( currentStation.info.stationId.compareTo("KJFK") == 0
                    // || currentStation.info.stationId.compareTo("KEYW") == 0 )
                    // System.out.println( "\nFor frame: " + time.toString() +
                    // " stn dataTime: "
                    // + currentStation.info.dataTime.toString() +":"
                    // +
                    // "\nList of AbstractMetParameter values from the station itself:\n"
                    // + currentStation.listOfParamsToPlot);

                    sm.acquireUninterruptibly();

                    synchronized (paramsToPlot) {
                        Set<String> pkeySet = paramsToPlot.keySet();
                        synchronized (pkeySet) {
                            try {
                                for (String prmToPlotKey : pkeySet) {
                                    // System.out.println("paramsToPlotKey = " +
                                    // prmToPlotKey);
                                    AbstractMetParameter prmToPlot = paramsToPlot
                                            .get(prmToPlotKey);
                                    if (prmToPlot != null) {
                                        prmToPlot.setValueToMissing();
                                        paramsToPlot.put(prmToPlot.getClass()
                                                .getSimpleName(), prmToPlot);
                                    }
                                }

                            } catch (Exception e) {
                                sm.release();
                            }
                        }
                    }
                    sm.release();

                }
            } catch (VizException e) {

                e.printStackTrace();
            }

        }

        // sem1.release();

        Tracer.print("< Exit    " + Tracer.shortTimeString(time));

        return (stationMap.values());
    }

    private void setMetParamFromPDV(AbstractMetParameter metPrm,
            PointDataView pdv, String dbParam, DataTime dt) {

        Tracer.printX("> Entry");
        metPrm.setValueToMissing();
        metPrm.setValidTime(dt);
        Type pType = pdv.getType(dbParam);

        // if this is an array then attempt to determine which
        // value in the array to use to set the metParameter.
        //
        if (pdv.getDimensions(dbParam) > 1) {

            PlotParameterDefn pltPrmDefn = prioritySelectionsMap.get(dbParam);
            if (pltPrmDefn == null) {
                return;
            }

            // if there is a priority ranking for this parameter
            //
            if (pltPrmDefn.getPrioritySelector() != null) {

                // S2N only for string lookups
                if (metPrm.hasStringValue()) {
                    String dbVals[] = pdv.getStringAllLevels(dbParam);

                    String rankedValue = pltPrmDefn.getPrioritySelector()
                            .getRankedField(dbVals);

                    metPrm.setStringValue(rankedValue);
                    return;
                } else {
                    System.out.println("Param " + dbParam
                            + " must be a string to do a priority select from "
                            + "the array of values.");
                    metPrm.setValueToMissing();
                    return;
                }
            }

            // if no arrayIndex given, just get the first in the list
            int arrayIndex = pltPrmDefn.getArrayIndex();

            if (pType == Type.STRING) {
                String dbVals[] = pdv.getStringAllLevels(dbParam);

                if (arrayIndex >= dbVals.length) {
                    metPrm.setValueToMissing();
                    return;
                }

                if (metPrm.hasStringValue()) {
                    metPrm.setStringValue(dbVals[arrayIndex]);
                } else { // parse a number from the string
                    metPrm.setValueFromString(dbVals[arrayIndex].toString(),
                            pdv.getUnit(dbParam));
                }
            } else {
                Number dbVals[] = pdv.getNumberAllLevels(dbParam);

                if (arrayIndex >= dbVals.length) {
                    metPrm.setValueToMissing();
                    return;
                }

                // TODO : should we allow this?
                if (metPrm.hasStringValue()) {
                    metPrm.setStringValue(dbVals[arrayIndex].toString());
                } else {
                    metPrm.setValue(dbVals[arrayIndex], pdv.getUnit(dbParam));
                }
            }
        } else { // set the metParam

            if (metPrm.hasStringValue()) {
                if (pType == Type.STRING) {
                    metPrm.setStringValue(pdv.getString(dbParam));
                } else {

                    if (pType == Type.INT) {
                        Integer tempInt = new Integer(pdv.getInt(dbParam));
                        metPrm.setStringValue(tempInt.toString());
                    }
                }
            } else { // metPrm is a number
                if (pType == Type.STRING) {
                    // parse a number from the string
                    metPrm.setValueFromString(pdv.getString(dbParam),
                            pdv.getUnit(dbParam));
                } else {
                    metPrm.setValue(pdv.getNumber(dbParam),
                            pdv.getUnit(dbParam));
                }
            }
        }
        Tracer.printX("< Exit");
    }

    public Station processConditionalParameterForEachStation(
            Collection<AbstractMetParameter> metPrmCollection,
            Station currentStation, String condColorParamName) {

        Tracer.print("> Entry  " + currentStation.info.toString());
        synchronized (metPrmCollection) {
            for (AbstractMetParameter thisCondColorParam : metPrmCollection) {
                if (condColorParamName.compareTo(thisCondColorParam
                        .getMetParamName()) == 0) {

                    AbstractMetParameter newPrm = newInstance(thisCondColorParam);
                    if (newPrm == null)
                        continue;
                    currentStation.setOfConditionalColorParams.add(newPrm);

                }
            }
        }
        Tracer.print("< Exit  " + currentStation.info.toString());
        return currentStation;
    }

    private AbstractMetParameter newInstance(
            AbstractMetParameter paramToInstantiate) {
        Tracer.printX("> Entry");
        AbstractMetParameter instantiatedPrm = null;
        try {
            instantiatedPrm = paramToInstantiate.getClass().newInstance();
            if (paramToInstantiate.hasValidValue()) {
                instantiatedPrm.setValidTime(paramToInstantiate.getValidTime());

                if (!paramToInstantiate.isUseStringValue()) {
                    instantiatedPrm.setValueAs(paramToInstantiate.getValue(),
                            paramToInstantiate.getUnitStr());
                } else {
                    instantiatedPrm.setUseStringValue(paramToInstantiate
                            .isUseStringValue());
                    instantiatedPrm.setStringValue(paramToInstantiate
                            .getStringValue());
                }
            }
        } catch (InstantiationException ie) {
            return null;
        } catch (IllegalAccessException iae) {
            return null;
        }
        Tracer.printX("< Exit");

        return instantiatedPrm;
    }

    private String formatLatLonKey(Number lat, Number lon) {

        return new String("" + Math.round(lat.doubleValue() * 1000.0) + ","
                + Math.round(lon.doubleValue() * 1000.0));
    }

    private final class GetDataTask implements Runnable {
        List<Station> listOfStationsRequestingForData;

        DataTime time;

        GetDataTask(Collection<Station> listOfStationsRequestingForData,
                DataTime time) {
            Tracer.print("> Entry");
            Tracer.print("Creating a Get[HDF5]DataTask for the frame time: "
                    + Tracer.shortTimeString(time) + " with "
                    + listOfStationsRequestingForData.size() + " stations");
            this.time = new DataTime(time.getRefTime());
            this.listOfStationsRequestingForData = new ArrayList<Station>(
                    listOfStationsRequestingForData);
            Tracer.print("< Exit");
        }

        @Override
        public void run() {
            Tracer.print("> Entry  START TASK " + Tracer.shortTimeString(time));
            if (levelStr == null)
                return;

            Collection<Station> stationsWithData = new ArrayList<Station>(0);
            long t0 = System.nanoTime();

            if (listOfStationsRequestingForData.size() == 0)
                return;

            // Tracer.sanityCheckStationSet(listOfStationsRequestingForData);
            // parameters to plot not populated yet

            if (levelStr.compareTo("Surface") == 0) {
                stationsWithData = requestSurfaceData(time,
                        listOfStationsRequestingForData);

            } else {
                stationsWithData = requestUpperAirData(listOfStationsRequestingForData);
            }

            long t1 = System.nanoTime();

            Tracer.print("Finished getting data for " + stationsWithData.size()
                    + " stations in " + (t1 - t0) / 1000000 + " ms for frame: "
                    + Tracer.shortTimeString(time));

            Tracer.sanityCheckStationSet(stationsWithData);

            if (stationsWithData.size() > 0)
                imageCreator.queueStationsToCreateImages(time,
                        stationsWithData, plotDensity);

            Tracer.print("< Exit   END TASK   " + Tracer.shortTimeString(time));

        }

    }

}
