package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;

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

import gov.noaa.nws.ncep.edex.common.metparameters.AbstractMetParameter;
import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory;
import gov.noaa.nws.ncep.edex.common.metparameters.StationElevation;
import gov.noaa.nws.ncep.edex.common.metparameters.StationID;
import gov.noaa.nws.ncep.edex.common.metparameters.StationLatitude;
import gov.noaa.nws.ncep.edex.common.metparameters.StationLongitude;
import gov.noaa.nws.ncep.edex.common.metparameters.StationNumber;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.NotDerivableException;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer2;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube.QueryStatus;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery2;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefn;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefns;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefnsMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotData;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.PointDataRequest;

/**
 * Job separated from PlotModelGenerator2 that requests plot data and passes it
 * on to the NcPlotModelGeneratorJob.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            njensen     Initial creation
 * 09/2012      896        sgurung     Refactored raytheon's class PlotModelDataRequestJob and added
 * 									   code from ncep's PlotModelGenerator2	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class NcPlotModelDataRequestJob extends Job {

    private PlotModelFactory2 plotCreator;

    private Map<String, RequestConstraint> constraintMap;

    private String plugin;
    
    private final String levelStr;

    private NcPlotModelGeneratorJob generatorJob;

    private NcPlotDataThreadPool parent;
    
    // map from the metParam name to an abstractMetParameter. The met parameter
    // will have a value set either from the pointDataContainer query or derived
    // from the db pointData. This map is passed to the PlotModelFactory. All the 
    // AbstractMetParameter are references to objects in either the dbParamsMap or
    // the derivedParamsList. 
    // 
    private HashMap<String,AbstractMetParameter> paramsToPlot = null;

    // a map from the dbParam name to a list of AbstractMetParameter objects used
    // to hold the values from the pointDataContainer. This map is used to 
    // derive parameter values to be plotted. 
    //
    private HashMap<String,AbstractMetParameter> dbParamsMap = null;

    // A list of derivedParameters that need to be derived from the dbParamsMap.
    //
    private ArrayList<AbstractMetParameter> derivedParamsList = null;

    // a map for those parameters that determine their value from an 
    // array of values from the DB. (this implements functionality previously done
    // in the PlotModelFactory for the plotFunctionTable tag and now done with
    // the arrayIndex tag in the plotParameterDefn)
    // 
    private HashMap<String, PlotParameterDefn> prioritySelectionsMap = null;
    
    // 
    private String latDbName = "latitude";
    private String lonDbName = "longitude";
    
    private HashMap<String, RequestConstraint> conditionalFilterMap = null;
    
    private HashMap<String,AbstractMetParameter> allMetParamsMap = null;
    
    private PlotParameterDefns plotPrmDefns = null;
    
    private IPlotModelGeneratorCaller caller;
    
    private boolean applyAdvancedSettings = false;
    
    public class PlotImageInfo {
        PlotInfo info;

        HashMap<String,AbstractMetParameter> paramsToPlot;
        
        HashMap<String,AbstractMetParameter> allMetParamsMap;
        
        protected PlotImageInfo(PlotInfo info, HashMap<String,AbstractMetParameter> paramsToPlot, HashMap<String,AbstractMetParameter> allMetParamsMap) {
        	this.info = info;
        	this.paramsToPlot = paramsToPlot;
        	this.allMetParamsMap = allMetParamsMap;
        }
    }

    public NcPlotModelDataRequestJob(IGraphicsTarget aTarget,
            IMapDescriptor mapDescriptor, PlotModel plotModel,
            String level, String plugin,
            Map<String, RequestConstraint> constraintMap,
            ConditionalFilter condFilter,
            IPlotModelGeneratorCaller caller, NcPlotDataThreadPool parent)
            throws VizException {
    	super("Requesting Plot Data...");
        this.plugin = plugin;
        this.levelStr = level;
        this.constraintMap = constraintMap;
        this.parent = parent;
        this.caller = caller; 
        this.conditionalFilterMap = new HashMap<String, RequestConstraint>();
        if (condFilter != null)
        	this.conditionalFilterMap = condFilter.getConditionalFilterMap();
        this.applyAdvancedSettings = plotModel.hasAdvancedSettings();
        
        paramsToPlot = new HashMap<String,AbstractMetParameter>();        
        derivedParamsList = new ArrayList<AbstractMetParameter>();
        dbParamsMap = new HashMap<String,AbstractMetParameter>();
        prioritySelectionsMap = new HashMap<String,PlotParameterDefn>();
        allMetParamsMap =  new HashMap<String,AbstractMetParameter>();
        
        ArrayList<PlotParameterDefn> toBeDerivedParamsList = new ArrayList<PlotParameterDefn>();
        
    	// get the parameter definitions for this plugin. 
    	// the definitions give the list of available nmap parameters as well
    	// as a mapping to the db param name an plot mode, units...
    	//    	
        //PlotParameterDefns plotPrmDefns = 
        plotPrmDefns = PlotParameterDefnsMngr.getInstance().getPlotParamDefns(  plotModel.getPlugin() );
        
        for( PlotParameterDefn plotPrmDefn : plotPrmDefns.getParameterDefns() ) { 
        	 		
        	// if this is a 'vector' parameter (ie windBarb or arrow) then get the 2
        	// component metParameters and make sure they exist.
        	if( plotPrmDefn.isVectorParameter() ) {
    			String[] vectParamNames = plotPrmDefn.getMetParamNamesForVectorPlot();
    			
    			if( vectParamNames == null ) {
					throw new VizException("Error plotting WindBarb or Arrow: Can't get components metParameters for "+
							plotPrmDefn.getPlotParamName() );
    			}
    			
    			for( String vectParam : vectParamNames ) {    				
//    				PlotParameterDefn vectPrmDefn = plotParamDefns.getPlotParamDefn( vectParam );

//    				if( vectPrmDefn == null ) {
//    					throw new VizException("Error plotting WindBarb or Arrow: Can't find parameter "+vectParam );
//    				}
//    				else if( vectPrmDefn.getDbParamName().equals( "derived" ) ) {
//    					throw new VizException("Error plotting WindBarb or Arrow: Can't plot derived vector parameter "+vectParam );
//    				}
    				// check that this metParam is
    				if( plotPrmDefns.getPlotParamDefnsForMetParam( vectParam ).isEmpty() ) {
    					throw new VizException("Error plotting WindBarb or Arrow : Can't find definition for component metParameter "+
    							vectParam );
    				}
    			}
        	}
        	else { // if not a vector parameter
        		String   dbPrmName = plotPrmDefn.getDbParamName();
        		String[] deriveArgs = plotPrmDefn.getDeriveParams();
        		String plotPrmName = plotPrmDefn.getPlotParamName();
        		
        		if( dbPrmName == null ) { // derived
        			//System.out.println("sanity check: can't find plotPrmDefn for :"+dbPrmName );
        			if (deriveArgs != null && conditionalFilterMap != null && conditionalFilterMap.containsKey(plotPrmName))
        				//needs to be derived so that it can be used when applying conditional filters
        				toBeDerivedParamsList.add(plotPrmDefn); 
        			continue;
        		}
        		// if there is already a plot param mapped to this dbParam
        		else if( dbParamsMap.containsKey( dbPrmName ) ) {
        			continue;
        		}
        		else { //if( !dbPrmName.equals("derived" ) ) {

        			// alias the db param name to the ncep param name. 
        			// (This eliminates the need to have a direct mapping from the db name to
        			// the ncep param name.)
        			MetParameterFactory.getInstance().alias( plotPrmDefn.getMetParamName(), plotPrmDefn.getDbParamName() );

        			// create a metParam that will hold the value from the db and which will
        			// be used to plot the plotParameter and possibly derive other parameter values.
        			//
        			AbstractMetParameter dbParam = MetParameterFactory.getInstance().
        						createParameter(  plotPrmDefn.getMetParamName(), plotPrmDefn.getPlotUnit() );
        			if( dbParam == null ) {
        				System.out.println("Error creating metParameter "+ plotPrmDefn.getMetParamName() );
        			}
        			else {
        				// add this prm to a map to tell us which db params are needed  
        				// when querying the db
        				dbParamsMap.put( plotPrmDefn.getDbParamName(), dbParam ); 
        				
        				// for parameters that need to lookup their value from an
        				// array of values based on a priority. (ie for skyCover to
        				// determine the highest level of cloud cover at any level)
        				//
        					prioritySelectionsMap.put( dbPrmName, plotPrmDefn );
        				
        				// else TODO : check for arrayIndex 
        			}
        		}
        	}
        }
        
        // if the station lat/long is not in the defns file, add them here since they
        // are needed by the PlotModelFactory to plot the data
        //
        if( !dbParamsMap.containsKey( latDbName ) ) {
    		MetParameterFactory.getInstance().alias( StationLatitude.class.getSimpleName(), latDbName );    		    		
    		AbstractMetParameter latPrm = MetParameterFactory.getInstance().
    							createParameter( StationLatitude.class.getSimpleName(), NonSI.DEGREE_ANGLE );        			    		
    		dbParamsMap.put( latDbName, latPrm );        
        }
        
        if( !dbParamsMap.containsKey( lonDbName ) ) {
    		MetParameterFactory.getInstance().alias( StationLongitude.class.getSimpleName(), lonDbName );    		
    		
    		AbstractMetParameter longPrm = MetParameterFactory.getInstance().
    							createParameter( StationLongitude.class.getSimpleName(), NonSI.DEGREE_ANGLE );        			
    		
    		dbParamsMap.put( lonDbName, longPrm );
        }

        paramsToPlot.put( StationLatitude.class.getSimpleName(),
    				dbParamsMap.get( latDbName ) );
    
        paramsToPlot.put(StationLongitude.class.getSimpleName(),
        			dbParamsMap.get( lonDbName ) );        
        
        // loop thru the non-vector (ie center position windBarb) plotParameters in the plotModel and
        //    
        // Once created this map will be (re)used to create each station plot.
        // The parameter values will be either set or derived from the database parameters.
        //
        for( String pltPrmName : plotModel.getPlotParamNames( true ) ) { // don't include vector(ie windBarb) params
        		
        	// get the dbParamName and determine if derived parameter
        	//
        	PlotParameterDefn plotPrmDefn = plotPrmDefns.getPlotParamDefn( pltPrmName );
        	
        	if( plotPrmDefn == null ) {
    			throw new VizException("Error creating plot metParameter "+ pltPrmName );
        	}
        	else if( plotPrmDefn.isVectorParameter() ) { 
            	// 'Vector' parameters for windBarbs and arrows are required to be in the center (WD) position 
            	//  Also, no plotDefns should have a plotMode of barb or arrow if not in the center position.         		

        		// add the 2 metParameters to paramsToPlot. 
        		 String[] vectParamNames = plotPrmDefn.getMetParamNamesForVectorPlot();

        		for( String vectParam : vectParamNames ) {
        			// already sanity checked this...	
        			PlotParameterDefn vectPrmDefn = plotPrmDefns.getPlotParamDefnsForMetParam( vectParam ).get(0);
        			addToParamsToPlot( vectPrmDefn );
        		}
        	}
        	else {
        		addToParamsToPlot( plotPrmDefn );
        	}        	
        }
        
        // add the plotParams from conditional filter (that needs to be derived) to derivedParamsList
        for (PlotParameterDefn plotPrmDefn: toBeDerivedParamsList) {
        	String[] deriveArgs = plotPrmDefn.getDeriveParams();    		
    		addToDerivedParamsList(deriveArgs, plotPrmDefn);    		
        }
                
        plotCreator = new PlotModelFactory2( mapDescriptor, plotModel, plotPrmDefns  );
        this.generatorJob = new NcPlotModelGeneratorJob(plotCreator, caller, aTarget);
        this.generatorJob.setSystem(false);
    }

    private void addToParamsToPlot( PlotParameterDefn plotPrmDefn ) {
		String   dbParamName =  plotPrmDefn.getDbParamName();
		String   metParamName = plotPrmDefn.getMetParamName();
		String[] deriveParams = plotPrmDefn.getDeriveParams();// the input args to derive()
		
		// if this is a derived parameter, create a metParameter to hold the derived
		// value to be computed and plotted.
		//
		if( deriveParams != null ) { //dbParamName.equals( "derived" ) ) {
			
			AbstractMetParameter derivedMetParam = addToDerivedParamsList(deriveParams, plotPrmDefn);
			if (derivedMetParam == null)
				return;
			
			paramsToPlot.put( metParamName, derivedMetParam );
		}
		// if this is a dbParameter then save the metParameter from the dbParamsMap
		// in the paramsToPlot map.
		//
		else if( dbParamName != null &&
				 dbParamsMap.containsKey( dbParamName ) ) {
			
			// if it is already in the map then we don't need to save it twice.
			if( !paramsToPlot.containsKey( dbParamName ) ) {
				paramsToPlot.put( metParamName, 
									  dbParamsMap.get( dbParamName ) );
			}
		}
		else {
			System.out.println("Sanity check : dbParamName is not in dbParamsMap");
		}	    			
	}
	
	private AbstractMetParameter addToDerivedParamsList(String[] deriveParams, PlotParameterDefn plotPrmDefn ) {
		
		// if this is a derived parameter, create a metParameter to hold the derived
		// value to be computed and plotted.
		//
		AbstractMetParameter derivedMetParam = MetParameterFactory.getInstance().        			
					createParameter( plotPrmDefn.getMetParamName(), plotPrmDefn.getPlotUnit() );

		if( derivedMetParam == null ) {
			System.out.println("Error creating derived metParameter "+
						     plotPrmDefn.getMetParamName() );
			return null;
		}      
		else {
			// If all is set then all of the  
			// available metParameters from the db query are used
			// when attempting to derive the parameter.
			// Otherwise, we are expecting a comma separated list of parameters
			// 	
			if( //deriveParams.length > 1 &&
			    !deriveParams[0].equalsIgnoreCase("all") ) {
				
				ArrayList<String> preferedDeriveParameterNames = new ArrayList<String>();
				ArrayList<AbstractMetParameter> preferedDeriveParameters = new ArrayList<AbstractMetParameter>();
				
				for( String dPrm : deriveParams ) {
					AbstractMetParameter deriveInputParam = 
						     MetParameterFactory.getInstance().createParameter( dPrm );

					if( deriveInputParam != null ) {
						//MetParameterFactory.getInstance().isValidMetParameterName( dPrm ) ) {
						preferedDeriveParameters.add( deriveInputParam );
						preferedDeriveParameterNames.add( dPrm );
					}
					else {
						System.out.println("Warning : '"+dPrm+" is not a valid metParameter name");
						return null;
					}
				}

				derivedMetParam.setPreferedDeriveParameters( preferedDeriveParameterNames );
			}

			if( derivedMetParam.getDeriveMethod( dbParamsMap.values() ) == null ) {
				System.out.println("Unable to derive "+ derivedMetParam.getMetParamName() +" from available parameters.");
				return null;
			}
			
			if (!derivedParamsList.contains(derivedMetParam))
				derivedParamsList.add( derivedMetParam );
			
		}
		return derivedMetParam;
			
	}
	
    @Override
    protected IStatus run(IProgressMonitor monitor) {
       
        while (parent.stationQueue.size() > 0) {
            List<PlotInfo> stationQuery = new ArrayList<PlotInfo>();

            List<PlotInfo> batch = null;
            synchronized (this) {
            	batch = parent.stationQueue.poll();
                if (batch == null) {
                    continue;
                }
                
                for (PlotInfo infos : batch) {
                    stationQuery.add(infos);
                }
            }
        
            List<PlotImageInfo> stations = new ArrayList<PlotImageInfo>();            

        	if( levelStr != null ) {
        		stations = requestUpperAirData(stationQuery);
        	}
        	else {
        		stations =  requestData(stationQuery);
        	}
             
             if (stations == null) {
            	 return Status.OK_STATUS;
             }

            for (PlotImageInfo pm : stations) {
                // schedule next work for other jobs    	
                this.generatorJob.enqueue(pm);
            }
        } 

        return Status.OK_STATUS;
    }

    private List<PlotImageInfo> requestData(List<PlotInfo> stationQuery) {
    	
   	    List<PlotImageInfo> toPlotImageStations = new ArrayList<PlotImageInfo>();
   	     	     
        // key is a formatted lat/lon string
        Map<String, PlotInfo> plotMap = new HashMap<String, PlotInfo>();
        Map<String, DataTime> timeMap = new HashMap<String, DataTime>();

        List<String> str = new ArrayList<String>(stationQuery.size());
        
        for( PlotInfo info : stationQuery ) {        	
    		str.add(info.dataURI);
            plotMap.put( 
            		formatLatLonKey(info.latitude, info.longitude ), info);
            timeMap.put( info.dataURI, info.dataTime);
        }
        
        // TODO : change to get all of the available prms for the plugin.
        // OR  call method on DerivedParams to determine which prms are needed to 
        // compute the set of params
        //            
        List<String> params = new ArrayList<String>();
        
        // TODO : is there a way to just tell the request to get all the parameters???
        // OR, we can determine the list of all the base parameters needed to 
        // compute all the derived parameters. Or we can specify the parameters needed
        // to compute the derived parameter in the plotParameterDefn.
        //    Currently, besides likely requesting more data than is needed, this 
        // means that if a parameter is needed to derive another then the deriving
        // parameter must also be in the PlotParamDefns (and available for display itself)
        // (seems reasonable though)
        for( String dbParam : dbParamsMap.keySet() ) {
        	params.add( dbParam );
        }

        if( !params.contains( latDbName ) ) {
        	params.add( latDbName ); // create MetParameter for dbParamsMap??
        }
        if( !params.contains( lonDbName ) ) {
        	params.add( lonDbName );
        }

        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        setMapConstraints(map);
        RequestConstraint rc = new RequestConstraint();
        rc.setConstraintType( ConstraintType.IN );

        int index = 0;
        int j = 0;
        int numOfValues = 500;
        PointDataContainer pdc = null;
        long hdf5t001 = 0;
        
        while (index < str.size()) {
            while (index < str.size() && j < numOfValues) {
                if (j == 0) {
                    rc.setConstraintValue(str.get(index));
                } else {
                    rc.addToConstraintValueList(str.get(index));
                }
                index++;
                j++;
            }
            
            map.put("dataURI", rc);
            
            try {
            	long t001 = System.currentTimeMillis();
                // Try and get data from datacube
                PointDataContainer tempPdc = DataCubeContainer
                        .getPointData(this.plugin, params
                                .toArray(new String[params.size()]),
                                null, map); //levelKey

                if (tempPdc == null) {
                    // Datacube didn't have proper plugin; going directly
                    // to the data store
                    tempPdc = PointDataRequest.requestPointDataAllLevels(
                            null, this.plugin, 
                            params.toArray( new String[params.size()] ), 
                            null, map );
                }
                long t002 = System.currentTimeMillis();
                
                hdf5t001 = hdf5t001 + (t002-t001);

                // I think this happens when  the data has been purged
                //
                if (tempPdc == null) {
                	System.out.println("PointDataRequest failed to find point data for known station ids. "+
                					   "Has data been purged?");
                    return null;
                }

                if (pdc == null) {
                    pdc = new PointDataContainer();
                    pdc.setAllocatedSz(tempPdc.getAllocatedSz());
                    pdc.setPointDataTypes(tempPdc.getPointDataTypes());
                } else {
                    pdc.combine(tempPdc);
                }
            } catch (VizException e1) {
                e1.printStackTrace();
                return null;
            }
            
            j = 0;
            map.clear();                
            setMapConstraints(map);
            
            if( pdc != null ) {
            	pdc.setCurrentSz(pdc.getAllocatedSz());
            }
        }
        //System.out.println("~~~ Total Time to get data from HDF5: " + (hdf5t001));
                                    	            
        int counter = 0;
        
        for( int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++ ) {
            PointDataView pdv = pdc.readRandom(uriCounter);
            if( pdv == null ) { // ???
            	continue;
            }
            	
            // set all the paramsToPlot values to missing. (All the  
            // metParams in the paramsToPlot map are references into the
            // derivedParamsMap and the dbParamsMap.)
            //
            for( AbstractMetParameter metPrm : derivedParamsList ) {
            	metPrm.setValueToMissing();
            	metPrm.setValidTime( null );
            }
            
            for( String dbPrm : dbParamsMap.keySet() ) {
            	AbstractMetParameter metPrm = dbParamsMap.get(dbPrm);
            	metPrm.setValidTime( null );            	
            
            	// get the fillValue from the parameterDescription and use it to set the missingValue
            	// Sentinel for the metParameter
            	try {
            	ParameterDescription pDesc = pdc.getDescription(dbPrm);
                if( pDesc != null ) {                    	
                	if( pdv.getType( dbPrm ) == null ) {
                		continue;
                	}
                	if( pDesc.getFillValue() == null ) {
                		System.out.println("Sanity Check: ParameterDescription fill Value is null");
                		System.out.println("Update the DataStoreFactory.py and H5pyDataStore.py files");
                		continue;
                	}
                	switch( pdv.getType( dbPrm ) ) {
                	case FLOAT :
                		metPrm.setMissingDataSentinel( 
                				pDesc.getFillValue().floatValue() );
                		break;
                	case DOUBLE :
                		metPrm.setMissingDataSentinel( 
                				pDesc.getFillValue() );
                		break;
                	case INT :
                		metPrm.setMissingDataSentinel( 
                				pDesc.getFillValue().intValue() );
                		break;
                	case STRING :
                		break;
                	}
                } 
            	} catch ( Exception e ) {
            		System.out.println("param "+dbPrm +" not found.");
            	}

            	metPrm.setValueToMissing();                	                	
            }

            // loop thru the dbparams from the point query
            //
            for( String dbParam : pdc.getParameters() ) {            	   

            	// find a metParam to hold the value for this dbParam 
            	// 
            	if( dbParamsMap.containsKey( dbParam ) ) { // id, lat, and longitude are not in the map.
            		
        			int dbid = pdv.getInt("id");
        			DataTime dataTime = timeMap.get( dbid );

        			AbstractMetParameter metPrm = dbParamsMap.get( dbParam );

        			setMetParamFromPDV( metPrm, pdv, dbParam, dataTime );
        			
        			allMetParamsMap.put(metPrm.getMetParamName(), metPrm);            			
            	}
            }
            
            // loop thru the derived params, and derive the values using value in 
            // the dbParamsMap 
            //
            for( AbstractMetParameter derivedParam : derivedParamsList ) {
            	
            	try {
            		derivedParam.derive( dbParamsMap.values() );
            		allMetParamsMap.put(derivedParam.getMetParamName(), derivedParam);

            	} catch( NotDerivableException e ) {
            		e.printStackTrace();
            	}                		
            }
            
            String latLonKey = formatLatLonKey( pdv.getFloat( latDbName ), 
            								    pdv.getFloat( lonDbName ) ); 
            PlotInfo pltInfo = plotMap.get( latLonKey );
            HashMap<String,AbstractMetParameter> pltInfoParamsToPlot = null;
            HashMap<String,AbstractMetParameter> pltInfoAllMetParamsMap = null;
            
            if( pltInfo == null ) { // ???
            	System.out.println("Error looking up plotInfo for map key:"+ latLonKey );
            }
            else {
            	boolean toPlot = (conditionalFilterMap != null)? applyConditionalFilters(): true;
            	
            	if (toPlot) {
                    synchronized (pltInfo) {
                       /* if (pltInfo.pdv == null) {
                        	pltInfo.pdv = new PlotData();
                        }
                        pltInfo.pdv.addData(pdv);*/
                        pltInfoParamsToPlot = new HashMap<String,AbstractMetParameter>();
                        pltInfoAllMetParamsMap = new HashMap<String,AbstractMetParameter>();
                        
                        
                        for (String key: paramsToPlot.keySet()) {
                        	AbstractMetParameter origMetParam = paramsToPlot.get(key);
                        	AbstractMetParameter metParam = MetParameterFactory.getInstance().createParameter( origMetParam.getClass().getSimpleName(), origMetParam.getUnit() );
                        	metParam.setDataTime(origMetParam.getDataTime());
                        	metParam.setMissing_data_value(origMetParam.getMissing_data_value());
                        	metParam.setPreferedDeriveParameters(origMetParam.getPreferedDeriveParameters());
                        	metParam.setUnitStr(origMetParam.getUnitStr());
                        	metParam.setUseStringValue(origMetParam.isUseStringValue());
                        	metParam.setValue(origMetParam.getValue());
                        	metParam.setValueString(origMetParam.getValueString());
                        	pltInfoParamsToPlot.put(key, metParam);                        	
                        }
                        
                        if (applyAdvancedSettings) {
	                        for (String key: allMetParamsMap.keySet()) {
	                        	AbstractMetParameter origMetParam = allMetParamsMap.get(key);
	                        	AbstractMetParameter metParam = MetParameterFactory.getInstance().createParameter( origMetParam.getClass().getSimpleName(), origMetParam.getUnit() );
	                        	metParam.setDataTime(origMetParam.getDataTime());
	                        	metParam.setMissing_data_value(origMetParam.getMissing_data_value());
	                        	metParam.setPreferedDeriveParameters(origMetParam.getPreferedDeriveParameters());
	                        	metParam.setUnitStr(origMetParam.getUnitStr());
	                        	metParam.setUseStringValue(origMetParam.isUseStringValue());
	                        	metParam.setValue(origMetParam.getValue());
	                        	metParam.setValueString(origMetParam.getValueString());
	                        	pltInfoAllMetParamsMap.put(key, metParam);
	                        }
                        }
                        toPlotImageStations.add(new PlotImageInfo(pltInfo, pltInfoParamsToPlot, pltInfoAllMetParamsMap));
                    }
            	}
            }

            counter++;
            
        }
    	return toPlotImageStations;
    }
    
    private List<PlotImageInfo> requestUpperAirData(List<PlotInfo> stationQuery) {
       	 	
   	    List<PlotImageInfo> toPlotImageStations = new ArrayList<PlotImageInfo>();
   	 
        long beginTime = 0;
        long endTime   = Long.MAX_VALUE;
        Date refTime=null;       
        Map<String, PlotInfo> plotMap = new HashMap<String, PlotInfo>();
        
        List<String> stnIdLst = new ArrayList<String>( stationQuery.size() );
        List<Long> rangeTimeLst = new ArrayList<Long>( stationQuery.size() );
        // get the start and end time for the query. And get a list of
        // coordinates for the query.
        for( PlotInfo stnInfo : stationQuery ) {
           
        	plotMap.put(formatLatLonKey(stnInfo.latitude, stnInfo.longitude ), stnInfo );
            
            refTime = stnInfo.dataTime.getRefTime();
            long stnTime = stnInfo.dataTime.getValidTime().getTimeInMillis();                
            beginTime = ( beginTime < stnTime ? stnTime : beginTime );
            endTime   = ( endTime   > stnTime ? stnTime : endTime );

            //chin use station id instead of lat/lon for query
            String stnId = new String(stnInfo.stationId);
            stnIdLst.add(stnId);
            if(rangeTimeLst.contains(stnTime) == false){
            	rangeTimeLst.add(stnTime);
            }
        }

        // TODO if this is an UpperAir FcstPlotResource then we will need to 
        // get the validTime and query for it. 
		NcSoundingQuery2 sndingQuery;
		try {
			sndingQuery = new NcSoundingQuery2( plugin, true, levelStr );
		} catch (Exception e1) {
        	System.out.println("Error creating NcSoundingQuery2: "+e1.getMessage() );
        	return null;
		}
		//chin sndingQuery.setLatLonConstraints( latLonCoords );
		sndingQuery.setStationIdConstraints(stnIdLst);
		sndingQuery.setRangeTimeList(rangeTimeLst);
		sndingQuery.setRefTimeConstraint(refTime);
		sndingQuery.setTimeRangeConstraint( 
				   new TimeRange( beginTime, endTime ) );
		
		// for modelsounding data we need to set the name of the model (ie the reportType)
		if( plugin.equals("modelsounding") ) {
			if( !constraintMap.containsKey( "reportType" ) ) {
            	System.out.println("Error creating NcSoundingQuery2: missing modelName (reportType) for modelsounding plugin" );
            	return null;
			}
			sndingQuery.setModelName(
					constraintMap.get("reportType" ).getConstraintValue() );
		}
		long t004 = System.currentTimeMillis();
		NcSoundingCube sndingCube = sndingQuery.query();
		long t005 = System.currentTimeMillis();
		System.out.println("plotUpperAirData sndingQuery query  took "+(t005-t004)+" ms");
		
		//
		//TODO -- This shouldn't be necessary, given Amount.getUnit() should now heal itself
		//        from a null unit by using the String.  
		//        Repair the 'unit' in the met params, if damaged (as in, nulled) in transit.
		//System.out.println("PlotModelGenerator2.plotUpperAirData() begin fixing returned data...");
		if( sndingCube != null && sndingCube.getRtnStatus() == QueryStatus.OK ) {
			for(NcSoundingProfile sndingProfile : sndingCube.getSoundingProfileList() ) {
				for(NcSoundingLayer2 sndingLayer : sndingProfile.getSoundingLyLst2()) {
					for(AbstractMetParameter metPrm : sndingLayer.getMetParamsMap().values()) {
						metPrm.syncUnits();
					}
				}
			}
		}
		//System.out.println("PlotModelGenerator2.plotUpperAirData() done fixing returned data");
		//TODO -- End
		//
		
//    		List<NcSoundingLayer> sndingLayers = new ArrayList<NcSoundingLayer>(0);

//            	sndingCube = NcSoundingQuery.soundingQueryByLatLon(
//            		beginTime, endTime, latLonCoords, 
//            		soundingType, DataType.ALLDATA, true, levelStr );

        if( sndingCube != null && sndingCube.getRtnStatus() == QueryStatus.OK ) {
            // Has to be just one Layer.
        	for(NcSoundingProfile sndingProfile : sndingCube.getSoundingProfileList() ) {  
            	if( sndingProfile.getSoundingLyLst2().size() != 1 ) {
//                		System.out.println("Sanity Check : SoundingQuery return Profile with != 1 Layer " );
                	if( sndingProfile.getSoundingLyLst2().isEmpty() ) {
                		continue;
                	}
            	}
            	
            	NcSoundingLayer2 sndingLayer = sndingProfile.getSoundingLyLst2().get(0);

//                	if( sndingProfile.getStationLatitude() != -9999 || 
//                	    sndingProfile.getStationLongitude() != -9999 ) {
//                		System.out.println("Station latlon is "+ sndingProfile.getStationLatitude() +
//                				"/"+ sndingProfile.getStationLongitude() );
//                	}
            	 Map<String, AbstractMetParameter>  soundingParamsMap=  sndingLayer.getMetParamsMap();
            	
                // set all the paramsToPlot values to missing. (All the  
                // metParams in the paramsToPlot map are references into the
                // derivedParamsMap and the dbParamsMap.)
                //
                for( AbstractMetParameter metPrm : derivedParamsList ) {
                	metPrm.setValueToMissing();
                }
                
                // loop thru all of the db params and set the values from
                // the sounding layer.
                //
                for( AbstractMetParameter metPrm : dbParamsMap.values() ) {
                	metPrm.setValueToMissing();

                	
                	// TODO : the station lat/lon, elev, name and id should be set in the sounding profile
                	// but currently isn't. So instead we will get the lat/lon and id from the DBQuery.
                	String key = metPrm.getMetParamName();
                	
                	if ( soundingParamsMap.containsKey(key ) ){
                		if  ( metPrm.hasStringValue() )
                			metPrm.setStringValue(soundingParamsMap.get(key).getStringValue() );
                		else	
                			 metPrm.setValue(soundingParamsMap.get(key));
                	}
                	else if( metPrm.getMetParamName().equals( StationLatitude.class.getSimpleName() ) ) {
                		metPrm.setValue( new Amount( 
                				sndingProfile.getStationLatitude(), NonSI.DEGREE_ANGLE ) );
                	}
                	else if( metPrm.getMetParamName().equals( StationLongitude.class.getSimpleName() ) ) {
                		metPrm.setValue( new Amount( 
                				sndingProfile.getStationLongitude(), NonSI.DEGREE_ANGLE ) );
                	}
                	else if( metPrm.getMetParamName().equals( StationElevation.class.getSimpleName() ) ) {
                		//                      metPrm.setValue( new Amount( 
                		//                      		sndingProfile.getStationElevation(), SI.METER ) );
                	}
                	else if( metPrm.getMetParamName().equals( StationID.class.getSimpleName() ) ) {
                		if( !sndingProfile.getStationId().isEmpty() ) {
                			metPrm.setStringValue( sndingProfile.getStationId() );
                		}
                		else {
                			metPrm.setValueToMissing();
                		}
                		//              		if( stnInfo.stationId != null && !stnInfo.stationId.isEmpty() ) {
                		//              			metPrm.setStringValue( stnInfo.stationId );
                		//              		}
                	}
                	else if( metPrm.getMetParamName().equals( StationNumber.class.getSimpleName() ) ) {
                		if( sndingProfile.getStationNum() != 0 ) {
                			metPrm.setStringValue( new Integer (sndingProfile.getStationNum()).toString() );
                		}
                		else {
                			metPrm.setValueToMissing();
                		}

                	}
                	
                	
                	else{
//                    		System.out.println("Sanity check: " + metPrm.getMetParamName() + " is not available in the sounding data");
                	}
                	// TODO : for modelsoundings. what are the units?
//                    	else if( metPrm.getMetParamName().equals(   VerticalVelocity.class.getSimpleName() ) ) {
//                            metPrm.setValue( new Amount( 
//                            		sndingLayer.getOmega(),  ) );
//                    	}
                	
                	allMetParamsMap.put(metPrm.getMetParamName(), metPrm);
                }
                
                // loop thru the derived params, and derive the values using value in 
                // the dbParamsMap 
                //
                for( AbstractMetParameter derivedParam : derivedParamsList ) {
                	try {
                		derivedParam.derive( dbParamsMap.values() );
                		allMetParamsMap.put(derivedParam.getMetParamName(), derivedParam);
                		
                	} catch( NotDerivableException e ) {
                		e.printStackTrace();
                	}                		
                }

                String latLonKey = formatLatLonKey( sndingProfile.getStationLatitude(), 
                									sndingProfile.getStationLongitude() ); 
                PlotInfo pltInfo = plotMap.get( latLonKey );
                HashMap<String,AbstractMetParameter> pltInfoParamsToPlot = null;
                HashMap<String,AbstractMetParameter> pltInfoAllMetParamsMap = null;

                if( pltInfo == null ) { // ???
                	System.out.println("Error looking up plotInfo for map key:"+ latLonKey );
                }
                else {
                	boolean toPlot = (conditionalFilterMap != null)? applyConditionalFilters(): true;
                	
                	if (toPlot) {
	                    synchronized (pltInfo) {
	                      /*  if (pltInfo.pdv == null) {
	                        	pltInfo.pdv = new PlotData();
	                        }
	                        pltInfo.pdv.addData(pdv);*/
	                    	pltInfoParamsToPlot = new HashMap<String,AbstractMetParameter>();
	                        pltInfoAllMetParamsMap = new HashMap<String,AbstractMetParameter>();
	                        
	                        for (String key: paramsToPlot.keySet()) {
	                        	AbstractMetParameter origMetParam = paramsToPlot.get(key);
	                        	AbstractMetParameter metParam = MetParameterFactory.getInstance().createParameter( origMetParam.getClass().getSimpleName(), origMetParam.getUnit() );
	                        	metParam.setDataTime(origMetParam.getDataTime());
	                        	metParam.setMissing_data_value(origMetParam.getMissing_data_value());
	                        	metParam.setPreferedDeriveParameters(origMetParam.getPreferedDeriveParameters());
	                        	metParam.setUnitStr(origMetParam.getUnitStr());
	                        	metParam.setUseStringValue(origMetParam.isUseStringValue());
	                        	metParam.setValue(origMetParam.getValue());
	                        	metParam.setValueString(origMetParam.getValueString());
	                        	pltInfoParamsToPlot.put(key, metParam);
	                        }
	                        
	                        if (applyAdvancedSettings) {
		                        for (String key: allMetParamsMap.keySet()) {
		                        	AbstractMetParameter origMetParam = allMetParamsMap.get(key);
		                        	AbstractMetParameter metParam = MetParameterFactory.getInstance().createParameter( origMetParam.getClass().getSimpleName(), origMetParam.getUnit() );
		                        	metParam.setDataTime(origMetParam.getDataTime());
		                        	metParam.setMissing_data_value(origMetParam.getMissing_data_value());
		                        	metParam.setPreferedDeriveParameters(origMetParam.getPreferedDeriveParameters());
		                        	metParam.setUnitStr(origMetParam.getUnitStr());
		                        	metParam.setUseStringValue(origMetParam.isUseStringValue());
		                        	metParam.setValue(origMetParam.getValue());
		                        	metParam.setValueString(origMetParam.getValueString());
		                        	pltInfoAllMetParamsMap.put(key, metParam);
		                        }
	                        }
	                        toPlotImageStations.add(new PlotImageInfo(pltInfo, pltInfoParamsToPlot, pltInfoAllMetParamsMap));
	                    }
                	}
                }

            } // end loop thru returned profiles

        }
  
		return toPlotImageStations;
    }

    public int getPlotModelWidth() {
        return this.plotCreator.getDefinedPlotModelWidth();
    }

    public void setPlotModelSize(long width) {
        this.plotCreator.setPlotDimensions(width, width);
    }

    public void setPlotMissingData(boolean b) {
        this.plotCreator.setPlotMissingData(b);
    }

    public void shutdown() {
        this.cancel();
        this.generatorJob.shutdown();
        this.caller.clearImages();
    }
    
    private void setMetParamFromPDV( AbstractMetParameter metPrm, PointDataView pdv, String dbParam, DataTime dt) {

    	Type pType = pdv.getType(dbParam);

    	metPrm.setValidTime( dt );

		// check that the types match.
//		if( !metPrm.hasStringValue() && pType == Type.STRING ||
//			 metPrm.hasStringValue() && pType != Type.STRING ) {
//			System.out.println("The metParameter ("+metPrm.getMetParamName()+
//					") type and db param ("+dbParam+") type don't match. String != Number");
//			metPrm.setValueToMissing();
//			return;			
//		}


		// if this is an array then attempt to determine which 
		// value in the array to use to set the metParameter.
		//  
		if( pdv.getDimensions( dbParam ) > 1 ) {

			PlotParameterDefn pltPrmDefn = prioritySelectionsMap.get( dbParam );
			if( pltPrmDefn == null ) {
				return; // ?????
			}

			// if there is a priority ranking for this parameter
			// 
			if( pltPrmDefn.getPrioritySelector() != null ) {
			
				// S2N only for string lookups
				if( metPrm.hasStringValue() ) {
					String dbVals[] = pdv.getStringAllLevels( dbParam );
					
					String rankedValue = 
						 pltPrmDefn.getPrioritySelector().getRankedField( dbVals );

					metPrm.setStringValue( rankedValue );
					return;
				}
				else {
					System.out.println("Param "+dbParam+" must be a string to do a priority select from "+
							"the array of values.");
					metPrm.setValueToMissing();
					return;
				}
			}

			//  if no arrayIndex given, just get the first in the list
			int arrayIndex = pltPrmDefn.getArrayIndex();
			
			if( pType == Type.STRING ) {				
				String dbVals[] = pdv.getStringAllLevels( dbParam );

				if( arrayIndex >= dbVals.length ) {
					metPrm.setValueToMissing();
					return;			
				}

//				if( !dbVals[arrayIndex].isEmpty() ) {
//					System.out.println( "index "+arrayIndex+ " is "  + dbVals[arrayIndex] );
//				}

				if( metPrm.hasStringValue() ) {
					metPrm.setStringValue( dbVals[arrayIndex] );
				}		
				else { // parse a number from the string
					metPrm.setValueFromString( dbVals[arrayIndex].toString(),  pdv.getUnit( dbParam ) );
				}
			}
			else {
				Number dbVals[] = pdv.getNumberAllLevels( dbParam );
				
				if( arrayIndex >= dbVals.length ) {
					metPrm.setValueToMissing();
					return;			
				}

				// TODO : should we allow this?
				if( metPrm.hasStringValue() ) {
					metPrm.setStringValue( dbVals[arrayIndex].toString() );
				}
				else {
					metPrm.setValue( dbVals[arrayIndex],  pdv.getUnit( dbParam ) );					
				}
			}
		}
		else { // set the metParam
    	
		   if( metPrm.hasStringValue() ) {
			   if( pType == Type.STRING ) {
				   metPrm.setStringValue( pdv.getString( dbParam ) );				   
			   }
			   else {
// This could really be a sanity-check??			   
//			   	   System.out.println("DB parameter "+dbParam+" of type  is not compatible with "+
//					   "metParameter "+metPrm.getClass().getSimpleName() );
				   if(pType == Type.INT){
					   Integer tempInt = new Integer (pdv.getInt(dbParam));
				         metPrm.setStringValue( tempInt.toString() );
				   }
			   }
		   }
		   else { // metPrm is a number
			   if( pType == Type.STRING ) {
				   // parse a number from the string
				   metPrm.setValueFromString( pdv.getString( dbParam ), pdv.getUnit( dbParam ) );
			   }			   
			   else {
				   metPrm.setValue( pdv.getNumber( dbParam ),  pdv.getUnit( dbParam ) );
			   }
		   }
		}		
    }    
    
    private void setMapConstraints(Map<String, RequestConstraint> map){
        for (Entry<String, RequestConstraint> constraint : 
        			this.constraintMap.entrySet()) {
            map.put(constraint.getKey(), constraint.getValue());
        }
    }

    private String formatLatLonKey( Number lat, Number lon ) {
//    	Double dlat = lat.doubleValue();
//    	Double dlon = lon.doubleValue();
//    	
    	return new String( ""+Math.round(lat.doubleValue()*1000.0)  + ","+
    						  Math.round(lon.doubleValue()*1000.0) ); 
    }
    
    public synchronized boolean applyConditionalFilters() {
    	if( conditionalFilterMap == null ||
        		conditionalFilterMap.isEmpty() ) {
        		return true;
        	}
    	
    	try {
						
	    	List<Boolean> displayStationPlotBoolList = new ArrayList<Boolean>();
	    	
	    	for( PlotParameterDefn plotPrmDefn : plotPrmDefns.getParameterDefns() ) { 
	    		String plotParamName = plotPrmDefn.getPlotParamName();
				String metParamName = plotPrmDefn.getMetParamName(); 
				
				// apply filter conditions            			
				if( conditionalFilterMap.containsKey(plotParamName) && conditionalFilterMap.get(plotParamName) != null){ 				
				//if (conditionalFilterMap != null && conditionalFilterMap.containsKey(plotParamName) && conditionalFilterMap.get(plotParamName) != null) {
					
					RequestConstraint reqConstraint = conditionalFilterMap.get(plotParamName);
					
					AbstractMetParameter metParam = allMetParamsMap.get(metParamName);
					
					if (metParam == null) {
						continue;
					}
					
					if (plotPrmDefn.getPlotUnit() != null) {
						Unit<?>  pltParamUnit = new UnitAdapter().unmarshal(plotPrmDefn.getPlotUnit().toString().trim());
						
						// if the units are not same, convert value to desired unit 
			    		if( pltParamUnit != metParam.getUnit()) {			    			
			    			try {
			    				metParam.setValue(metParam.getValueAs( pltParamUnit), pltParamUnit );
			    			} catch (Exception e) {
			    				metParam.setValueToMissing();
			    			}
			    		}
					}					
					
					if (reqConstraint != null) {
						try {
							String formattedPlotString = metParam.getFormattedString(
									plotPrmDefn.getPlotFormat() );
	
							int plotTrim = 0;
							if( plotPrmDefn.getPlotTrim() == null ) {
			        			plotTrim = 0;
			        		}
			        		else {
			        			plotTrim = Integer.parseInt( plotPrmDefn.getPlotTrim() );
			        		}
							
							if( plotTrim != 0 ) {
								formattedPlotString = formattedPlotString.substring(plotTrim );
							}
							
							boolean result = metParam.hasStringValue() ? reqConstraint.evaluate(formattedPlotString) : reqConstraint.evaluate(Double.parseDouble(formattedPlotString));
							if (result) {
								displayStationPlotBoolList.add(true);
							} else {
								displayStationPlotBoolList.add(false);
							}
						}
						catch (Exception e) {
							displayStationPlotBoolList.add(false);
							continue;							
						}
					}					
		    	}  
	
	    	}
	    	
	    	// AND the filter results
	        boolean displayStationPlot = true;
	        for (Boolean b: displayStationPlotBoolList) {
	        	displayStationPlot = displayStationPlot && b;
	        }
	       
	        if (!displayStationPlot) {
	        	return false;
	        }
    	} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}     
       return true;
    }
       
}
