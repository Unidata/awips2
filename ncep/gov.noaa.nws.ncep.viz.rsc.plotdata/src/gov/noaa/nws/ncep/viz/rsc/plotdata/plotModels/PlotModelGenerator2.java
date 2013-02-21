/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
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

package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;

import java.awt.image.BufferedImage;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.IllegalFormatConversionException;
import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.UUID;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.sound.midi.MidiDevice.Info;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.PointDataRequest;

import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery2;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefn;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefns;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefnsMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.PlotModelFactory2.PlotElement;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.edex.common.metparameters.AbstractMetParameter;
//import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory;
//import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
//import gov.noaa.nws.ncep.edex.common.metparameters.RelativeHumidity;
//import gov.noaa.nws.ncep.edex.common.metparameters.StationElevation;
//import gov.noaa.nws.ncep.edex.common.metparameters.StationID;
import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.StationElevation;
import gov.noaa.nws.ncep.edex.common.metparameters.StationID;
import gov.noaa.nws.ncep.edex.common.metparameters.StationLatitude;
import gov.noaa.nws.ncep.edex.common.metparameters.StationLongitude;
import gov.noaa.nws.ncep.edex.common.metparameters.StationNumber;
//import gov.noaa.nws.ncep.edex.common.metparameters.StationName;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.NotDerivableException;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
//import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
//import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer.DataType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer2;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube.QueryStatus;

import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.vividsolutions.jts.geom.Coordinate;
/**
 * A Eclipse Job thread that will listen for new stations on a queue and request
 * the data to create the plots.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/20/2006              brockwoo    Initial creation
 * 12/06/2006              brockwoo    Implemented code review changes
 * 05/11/2007   #273       brockwoo    Implemented JavaScript obs request
 * 04/16/2008              njensen     createActionASCII uses ScriptCreator
 * 05/01/2010   #275       ghull   
 * 07/26/2010   T285       qzhou       Modified run() for uair    
 * 05/26/2011   #441       ghull       use metParameters for non-uair data
 * 06/11/2011   #441       ghull       use NcSoundingQuery to get uair data
 * 08/20/2011   #450       ghull       use PlotParameterDefns instead of PlotParameterDefnsMngr
 * 09/07/2011   #294       ghull       setMissingDataSentinel for ncscd
 * 09/14/2011   #457       sgurung     Renamed h5uair and h5scd to ncuair and ncscd
 * 09/20/2011   #459       ghull       use new NcSoundingQuery2 for modelsoundings
 * 10/05/2011   #465     archana      replaced instances of the individual met parameters with
 *                                                         instances in the met parameters map in the NcSoundingLayer2 class.  
 * 10/05/2011   #259       ghull       set missing sentinel from ParameterDescription, set dataTime in metParameters
 * 10/17/2011   #          ghull       don't use slat,slon for ncuair
 * 11/01/2011   #482       ghull       move S2N to PlotParameterDefn
 * 11/01/2011   #482       ghull       move array lookup to setMetParamFromPDV
 * 11/15/2011              bhebbard    ensure metParm units valid after query return
 * 02/21/2012              Chin Chen    Modified plotUpperAirData() for performance improvement
 * 04/02/2012   #615       sgurung     Modified code to support conditional filtering: added conditionalFilterMap, 
 * 									   applyConditionalFilters(), and modified constructor to take ConditionalFilter
 * 12/19/2012   #947       ghull       check for empty conditional filter before applying
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class PlotModelGenerator2 extends Job {

    private final ConcurrentLinkedQueue<PlotInfo> stationQueue;
    
    private final PlotModelFactory2 plotCreator;

    private final IGraphicsTarget target;

    private final String plugin;
    
    private final String levelStr;

    private final Map<String, RequestConstraint> constraintMap;

    private IPlotModelGeneratorCaller caller;

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
    

    /**
     * Initializes the thread with the station's reference time and the target
     * to create textures for.
     * 
     * @param aTarget
     *            The graphic target to create the tiles for
     * @param mapDescriptor
     * @param plotModelFile
     * @param plugin
     *            the name of the plugin
     * @param constraintMap
     *            the constraints from the resource
     * @throws VizException
     */
    public PlotModelGenerator2(IGraphicsTarget aTarget,
            IMapDescriptor mapDescriptor, PlotModel plotModel,
            String level, //String plugin,
            Map<String, RequestConstraint> constraintMap,
            ConditionalFilter condFilter,
            IPlotModelGeneratorCaller caller) throws VizException {
        super("Creating Plots...");
        stationQueue = new ConcurrentLinkedQueue<PlotInfo>();

        this.target = aTarget;
        this.plugin = plotModel.getPlugin();
        this.levelStr = level;
        this.constraintMap = constraintMap;
        this.caller = caller;
        
        this.conditionalFilterMap = new HashMap<String, RequestConstraint>();
        if (condFilter != null)
        	this.conditionalFilterMap = condFilter.getConditionalFilterMap();
       
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
	
    public int getPlotModelWidth() {
        return this.plotCreator.getDefinedPlotModelWidth();
    }

    public void setPlotModelSize(long width) {
        this.plotCreator.setPlotDimensions(width, width);
        this.cleanImages();
    }

    @Override
    protected IStatus run( IProgressMonitor monitor) {
    	if( levelStr != null ) {
    		return plotUpperAirData();
    	}
    	else {
    		return plotSurfaceData();
    	}
    }
    
    private IStatus plotSurfaceData() {
    	while (stationQueue.size() > 0) {
            List<PlotInfo> stationQuery = new ArrayList<PlotInfo>();
            
            // key is a formatted lat/lon string
            Map<String, PlotInfo> plotMap = new HashMap<String, PlotInfo>();
            Map<String, DataTime> timeMap = new HashMap<String, DataTime>();

            for( PlotInfo info : stationQueue ) {
                plotMap.put( 
                		formatLatLonKey(info.latitude, info.longitude ), info);
                timeMap.put( info.dataURI, info.dataTime);
                stationQuery.add(info);            		
            }
            

            IImage image = null;

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
            for( String dbParam : dbParamsMap.keySet() ) { //plotParamDefns.getPlotParamDefns() ) {
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
            String[] str = new String[stationQuery.size()];
            
            for (int z = 0; z < str.length; z++) {
                str[z] = "" + stationQuery.get(z).dataURI;
            }

            int index = 0;
            int j = 0;
            int numOfValues = 500;
            PointDataContainer pdc = null;

            while (index < str.length) {
                while (index < str.length && j < numOfValues) {
                    if (j == 0) {
                        rc.setConstraintValue(str[index]);
                    } else {
                        rc.addToConstraintValueList(str[index]);
                    }
                    index++;
                    j++;
                }
                
                map.put("dataURI", rc);
                
                try {
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

                    // I think this happens when  the data has been purged
                    //
                    if (tempPdc == null) {
                    	System.out.println("PointDataRequest failed to find point data for known station ids. "+
                    					   "Has data been purged?");
                        return Status.OK_STATUS;
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
                    return Status.OK_STATUS;
                }
                
                j = 0;
                map.clear();                
                setMapConstraints(map);
                
                if( pdc != null ) {
                	pdc.setCurrentSz(pdc.getAllocatedSz());
                }
            }
                                        	            
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
                for( AbstractMetParameter metPrm : derivedParamsList ) {//derivedParamsMap.values() ) {
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
                
                if( pltInfo == null ) { // ???
                	System.out.println("Error looking up plotInfo for map key:"+ latLonKey );
                }
                else {

                	BufferedImage bImage = null;
                	
                	if( applyConditionalFilters() ) {
                		bImage = plotCreator.getStationPlot( paramsToPlot ); // don't need this anymore, id);
                	}

                	if (bImage != null ) {
                		image = target.initializeRaster(new IODataPreparer(
                				bImage, UUID.randomUUID().toString(), 0), null);
                		caller.modelGenerated(new PlotInfo[] {pltInfo}, image);
                	}
                }

                counter++;
            }
            this.stationQueue.removeAll(stationQuery);
        }

        return Status.OK_STATUS;
    }
    
    private IStatus plotUpperAirData() {
    	
    	while (stationQueue.size() > 0) {
            List<PlotInfo> stationQuery = new ArrayList<PlotInfo>();            

            long beginTime = 0;
            long endTime   = Long.MAX_VALUE;
            Date refTime=null;
            //ArrayList<Coordinate> latLonCoords = new ArrayList<Coordinate>( stationQueue.size() );
            Map<String, PlotInfo> plotMap = new HashMap<String, PlotInfo>();
            
            List<String> stnIdLst = new ArrayList<String>( stationQueue.size() );
            List<Long> rangeTimeLst = new ArrayList<Long>( stationQueue.size() );
            // get the start and end time for the query. And get a list of
            // coordinates for the query.
            for( PlotInfo stnInfo : stationQueue ) {
               
            	plotMap.put( 
                		formatLatLonKey(stnInfo.latitude, stnInfo.longitude ), stnInfo );
                stationQuery.add( stnInfo );
                
                refTime = stnInfo.dataTime.getRefTime();
                long stnTime = stnInfo.dataTime.getValidTime().getTimeInMillis();                
                beginTime = ( beginTime < stnTime ? stnTime : beginTime );
                endTime   = ( endTime   > stnTime ? stnTime : endTime );

               
                                //                 
                //latLonCoords.add( new Coordinate( stnInfo.longitude, stnInfo.latitude ) );
                //System.out.println("PlotModelGenerator2.plotUpperAirData(): lat = "+stnInfo.latitude+ " lon="+
                //		stnInfo.longitude);
                //chin use station id instead of lat/lon for query
                String stnId = new String(stnInfo.stationId);
                stnIdLst.add(stnId);
                if(rangeTimeLst.contains(stnTime) == false){
                	rangeTimeLst.add(stnTime);
                	//System.out.println("PlotModelGenerator2.plotUpperAirData(): add rangeTime="+stnInfo.dataTime.getValidTime().getTime().toString());
                }
            }

            // TODO if this is an UpperAir FcstPlotResource then we will need to 
            // get the validTime and query for it. 
    		NcSoundingQuery2 sndingQuery;
			try {
				sndingQuery = new NcSoundingQuery2( plugin, true, levelStr );
			} catch (Exception e1) {
            	System.out.println("Error creating NcSoundingQuery2: "+e1.getMessage() );
            	return Status.CANCEL_STATUS;
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
                	return Status.CANCEL_STATUS;
    			}
    			sndingQuery.setModelName(
    					constraintMap.get("reportType" ).getConstraintValue() );
    		}
    		//long t004 = System.currentTimeMillis();
			NcSoundingCube sndingCube = sndingQuery.query();
    		//long t005 = System.currentTimeMillis();
			//System.out.println("plotUpperAirData sndingQuery query  took "+(t005-t004)+" ms");
    		
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

                    if( pltInfo == null ) {
                    	System.out.println("Error looking up plotInfo for map key:"+ latLonKey );
                    }
                    else {
                    	IImage image = null;
                    	
                    	BufferedImage bImage = null;
                    	
                    	if (applyConditionalFilters()) {
                    		bImage = plotCreator.getStationPlot( paramsToPlot ); // don't need this anymore, id);
                    	}

                    	if (bImage != null ) {
                    		image = target.initializeRaster(new IODataPreparer(
                    				bImage, UUID.randomUUID().toString(), 0), null);
                    		caller.modelGenerated(new PlotInfo[] {pltInfo}, image);
                    	}
                    } 
                } // end loop thru returned profiles

            } // end loop thru stationQueue

            this.stationQueue.removeAll(stationQuery);
    	}

        return Status.OK_STATUS;
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
//				System.out.print ( "values for "+dbParam );


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
				   metPrm.setStringValue( pType.toString() );			   
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

    /**
     * Adds a station to the queue
     * 
     * @param s
     * @return If the station was added to the queue properly
     */
    public void queueStations(List<PlotInfo> stations) {
        stationQueue.addAll(stations);
        if (!(this.getState() == Job.RUNNING)) {
            this.schedule();
        }
    }

    public boolean isQueued(PlotInfo info) {
        return stationQueue.contains(info);
    }

    /**
     * Kills the thread.
     */
    public void shutdown() {
        this.cancel();
        cleanImages();
    }

    public void cleanImages() {
        caller.clearImages();
    }

    public void setPlotMissingData(boolean b) {
        this.plotCreator.setPlotMissingData(b);
    }

    // use 
    private String formatLatLonKey( Number lat, Number lon ) {
//    	Double dlat = lat.doubleValue();
//    	Double dlon = lon.doubleValue();
//    	
    	return new String( ""+Math.round(lat.doubleValue()*1000.0)  + ","+
    						  Math.round(lon.doubleValue()*1000.0) ); 
    }
    
//    public String getStationMessage(int id) {
//        String message = null;
//        if (plotCreator.rawMessageMap.containsKey(id)
//                && plotCreator.rawMessageMap.get(id) != null) {
//            message = plotCreator.rawMessageMap.get(id);
//        } else {
//            message = "No Data Available";
//        }
//        return message;
//    }
  
    
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
				if( conditionalFilterMap.containsKey(plotParamName) && 
					conditionalFilterMap.get(plotParamName) != null) {
					
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
