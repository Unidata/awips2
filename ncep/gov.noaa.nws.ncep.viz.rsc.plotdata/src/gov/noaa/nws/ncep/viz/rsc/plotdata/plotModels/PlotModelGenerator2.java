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
import com.raytheon.uf.common.pointdata.PointDataContainer;
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

import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefn;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefnsMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.PlotModelFactory2.PlotElement;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer.DataType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube.QueryStatus;
import gov.noaa.nws.ncep.metParameters.parameterConversion.NcUnits;
import gov.noaa.nws.ncep.metparameters.AbstractMetParameter;
import gov.noaa.nws.ncep.metparameters.Amount;
import gov.noaa.nws.ncep.metparameters.DewPointTemp;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.NotDerivableException;
import gov.noaa.nws.ncep.metparameters.RelativeHumidity;
import gov.noaa.nws.ncep.metparameters.StationElevation;
import gov.noaa.nws.ncep.metparameters.StationID;
import gov.noaa.nws.ncep.metparameters.StationLatitude;
import gov.noaa.nws.ncep.metparameters.StationLongitude;
import gov.noaa.nws.ncep.metparameters.StationName;
import gov.noaa.nws.ncep.metparameters.AirTemperature;
import gov.noaa.nws.ncep.metparameters.PressureLevel;
import gov.noaa.nws.ncep.metparameters.HeightAboveSeaLevel;
import gov.noaa.nws.ncep.metparameters.WindDirectionUComp;
import gov.noaa.nws.ncep.metparameters.WindDirectionVComp;
import gov.noaa.nws.ncep.metparameters.WindDirection;
import gov.noaa.nws.ncep.metparameters.WindSpeed;

import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.time.DataTime;
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
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class PlotModelGenerator2 extends Job {

    private final ConcurrentLinkedQueue<PlotInfo> stationQueue;

    private final PlotParameterDefnsMngr plotParamDefns;
    
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
    private HashMap<String, S2N> prioritySelectionsMap = null;
    
    // 
    private String latDbName = "latitude";
    private String lonDbName = "longitude";
    

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
            IPlotModelGeneratorCaller caller) throws VizException {
        super("Creating Plots...");
        stationQueue = new ConcurrentLinkedQueue<PlotInfo>();

        this.target = aTarget;
        this.plugin = plotModel.getPlugin();
        this.levelStr = level;
        this.constraintMap = constraintMap;
        this.caller = caller;

        paramsToPlot = new HashMap<String,AbstractMetParameter>();        
        derivedParamsList = new ArrayList<AbstractMetParameter>();
        dbParamsMap = new HashMap<String,AbstractMetParameter>();
        prioritySelectionsMap = new HashMap<String,S2N>();
        
        // TODO : if we change the h5uair decoder to decode the station
        // location and elevation data like raytheon does then we won't 
        // need to do this.
        latDbName = (plugin.equals("h5uair") ? "SLAT" : "latitude" );
        lonDbName = (plugin.equals("h5uair") ? "SLON" : "longitude" );

    	// get the parameter definitions for this plugin. 
    	// the definitions give the list of available nmap parameters as well
    	// as a mapping to the db param name an plot mode, units...
    	//    	
        plotParamDefns = new PlotParameterDefnsMngr( plotModel.getPlugin() );

        for( PlotParameterDefn plotPrmDefn : plotParamDefns.getPlotParamDefns() ) {
        	
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
    				if( plotParamDefns.getPlotParamDefnsForMetParam( vectParam ).isEmpty() ) {
    					throw new VizException("Error plotting WindBarb or Arrow : Can't find definition for component metParameter "+
    							vectParam );
    				}
    			}
        	}
        	else { // if not a vector parameter
        		String   dbPrmName = plotPrmDefn.getDbParamName();
        		String[] deriveArgs = plotPrmDefn.getDeriveParams();
        		
        		if( dbPrmName == null ) { // derived
        			//System.out.println("sanity check: can't find plotPrmDefn for :"+dbPrmName );
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
        				if( plotPrmDefn.getPrioritySelectTable() != null ) {
        					S2N priorSel = S2N.readS2NFile( 
        							plotPrmDefn.getPrioritySelectTable() );
        					prioritySelectionsMap.put( dbPrmName, priorSel );
        				}
        				// else TODO : check for arrayIndex 
        			}
        		}
        	}
        }
        
        // if the station lat/long is not in the defns file, add them here since they
        // are needed to plot the data
        //
        if( !dbParamsMap.containsKey( latDbName ) ) {
    		MetParameterFactory.getInstance().alias( StationLatitude.class.getSimpleName(), latDbName );    		    		
    		AbstractMetParameter latPrm = MetParameterFactory.getInstance().
    							createParameter( StationLatitude.class.getSimpleName(), Unit.ONE );        			    		
    		dbParamsMap.put( latDbName, latPrm );        
        }
        
        if( !dbParamsMap.containsKey( lonDbName ) ) {
    		MetParameterFactory.getInstance().alias( StationLongitude.class.getSimpleName(), lonDbName );    		
    		
    		AbstractMetParameter longPrm = MetParameterFactory.getInstance().
    							createParameter( StationLongitude.class.getSimpleName(), Unit.ONE );        			
    		
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
        	PlotParameterDefn plotPrmDefn = plotParamDefns.getPlotParamDefn( pltPrmName );
        	
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
        			PlotParameterDefn vectPrmDefn = plotParamDefns.getPlotParamDefnsForMetParam( vectParam ).get(0);
        			addToParamsToPlot( vectPrmDefn );
        		}
        	}
        	else {
        		addToParamsToPlot( plotPrmDefn );
        	}        	
        }
        
        plotCreator = new PlotModelFactory2( mapDescriptor, plotModel, 
			     plotParamDefns, levelStr );
    }

	private void addToParamsToPlot( PlotParameterDefn plotPrmDefn ) {
		String   dbParamName =  plotPrmDefn.getDbParamName();
		String   metParamName = plotPrmDefn.getMetParamName();
		String[] deriveParams = plotPrmDefn.getDeriveParams();// the input args to derive()
		
		// if this is a derived parameter, create a metParameter to hold the derived
		// value to be computed and plotted.
		//
		if( deriveParams != null ) { //dbParamName.equals( "derived" ) ) {
			
			AbstractMetParameter derivedMetParam = MetParameterFactory.getInstance().        			
						createParameter( plotPrmDefn.getMetParamName(), plotPrmDefn.getPlotUnit() );

			if( derivedMetParam == null ) {
				System.out.println("Error creating derived metParameter "+
							     plotPrmDefn.getMetParamName() );
				return;
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
//								MetParameterFactory.getInstance().isValidMetParameterName( dPrm ) ) {
							preferedDeriveParameters.add( deriveInputParam );
							preferedDeriveParameterNames.add( dPrm );
						}
						else {
							System.out.println("Warning : '"+dPrm+" is not a valid metParameter name");
							return;
						}
					}

					derivedMetParam.setPreferedDeriveParameters( preferedDeriveParameterNames );
				}

				if( derivedMetParam.getDeriveMethod( dbParamsMap.values() ) == null ) {
					System.out.println("Unable to derive "+ derivedMetParam.getMetParamName() +" from available parameters.");
					return;
				}

				derivedParamsList.add( derivedMetParam );

				paramsToPlot.put( metParamName, derivedMetParam );
			}
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
            
            for( PlotInfo info : stationQueue ) {
                plotMap.put( 
                		formatLatLonKey(info.latitude, info.longitude ), info);
                stationQuery.add(info);            		
            }
            

            IImage image = null;

            // TODO : change to get all of the available prms for the plugin.
            // OR  call method on DerivedParams to determine which prms are needed to 
            // compute the set of params
            //            
            List<String> params = new ArrayList<String>();
            params.add( latDbName ); // create MetParameter for dbParamsMap??
            params.add( lonDbName );
                        
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

            Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
            setMapConstraints(map);
            RequestConstraint rc = new RequestConstraint();
            rc.setConstraintType( ConstraintType.IN );
            String[] str = new String[stationQuery.size()];
            
            for (int z = 0; z < str.length; z++) {
                str[z] = "" + stationQuery.get(z).id;
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
                
                map.put("id", rc);
                
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
                }
                
                for( AbstractMetParameter metPrm : dbParamsMap.values() ) {
                	metPrm.setValueToMissing();
                }

                // loop thru the dbparams from the point query
                //
                for( String dbParam : pdc.getParameters() ) {            	   

                	// find a metParam to hold the value for this dbParam 
                	// 
                	if( dbParamsMap.containsKey( dbParam ) ) { // id, lat, and longitude are not in the map.
                		AbstractMetParameter metPrm = dbParamsMap.get( dbParam );
                		
                		Type pType = pdv.getType(dbParam);

                		// if this is an array then attempt to determine which 
                		// value in the array to use to set the metParameter.
                		//  
                		if( pdv.getDimensions( dbParam ) > 1 ) {

                			// if there is a priority ranking for this parameter
                			// 
                			if( prioritySelectionsMap.containsKey( dbParam ) ) {
                			
                				// S2N only for string lookups
                				if( metPrm.hasStringValue() ) {

                					S2N priortySelection = prioritySelectionsMap.get( dbParam );

                					String dbVals[] = pdv.getStringAllLevels( dbParam );

                					String rankedValue = priortySelection.getRankedField( dbVals );

                					metPrm.setStringValue( rankedValue );
                				}
                			}
                			// TODO : implement arrayIndex (old plotIndex)
                			//  else if( arrayIndex... )
                		}
                		else {
                			setMetParamFromPDV( metPrm, pdv, dbParam );
                		}
                	}
                }
                
                // loop thru the derived params, and derive the values using value in 
                // the dbParamsMap 
                //
                for( AbstractMetParameter derivedParam : derivedParamsList ) {
                	
                	try {
                		derivedParam.derive( dbParamsMap.values() );

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
                	BufferedImage bImage = plotCreator.getStationPlot( paramsToPlot ); // don't need this anymore, id);

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
            
            // Get the SoundingType from the plugin.
            // TODO : Why not just pass in the plugin instead of a 'SoundingType'
            //
            String soundingType=null;

            if( plugin.equals("h5uair") ) {
            	soundingType = "H5UAIR";
            }
            // TODO : fill these in when implementing the Fcst
            else if( plugin.equals("???")) {
            	soundingType = "RUC2SND";
            }
            else {
            	System.out.println("Unable to determine SoundingType for plugin: "+plugin );
            	continue;
//            	throw new VizException("Unable to determine SoundingType for plugin: "+plugin );                	
            }

            long beginTime = 0;
            long endTime   = Long.MAX_VALUE;
            ArrayList<Coordinate> latLonCoords = new ArrayList<Coordinate>( stationQueue.size() );
            Map<String, PlotInfo> plotMap = new HashMap<String, PlotInfo>();

            // get the start and end time for the query. And get a list of
            // coordinates for the query.
            for( PlotInfo stnInfo : stationQueue ) {
               
            	plotMap.put( 
                		formatLatLonKey(stnInfo.latitude, stnInfo.longitude ), stnInfo );
                stationQuery.add( stnInfo );
                
                long stnTime = stnInfo.dataTime.getValidTime().getTimeInMillis();                
                beginTime = ( beginTime < stnTime ? stnTime : beginTime );
                endTime   = ( endTime   > stnTime ? stnTime : endTime );

                DataTime fcstTime = null; // for forecast model plotResources
                // Can query by lat/lon or stnNum or stnId.
                // String[] stnIds = new String[stationQuery.size()];
                //                 
                latLonCoords.add( new Coordinate( stnInfo.longitude, stnInfo.latitude ) );
            }

            // TODO if this is an UpperAir FcstPlotResource then we will need to 
            // get the validTime and query for it. 
            List<NcSoundingLayer> sndingLayers = new ArrayList<NcSoundingLayer>(0);

            NcSoundingCube sndingCube = NcSoundingQuery.soundingQueryByLatLon(
            		beginTime, endTime, latLonCoords, 
            		soundingType, DataType.ALLDATA, true, levelStr );

            if( sndingCube != null && sndingCube.getRtnStatus() == QueryStatus.OK ) {
                // Has to be just one Layer.
            	for(NcSoundingProfile sndingProfile : sndingCube.getSoundingProfileList() ) {  
                	if( sndingProfile.getSoundingLyLst().size() != 1 ) {
                		System.out.println("Sanity Check : SoundingQuery return Profile with != 1 Layer " );
                    	if( sndingProfile.getSoundingLyLst().isEmpty() ) {
                    		continue;
                    	}
                	}
                	
                	NcSoundingLayer sndingLayer = sndingProfile.getSoundingLyLst().get(0);

//                	if( sndingProfile.getStationLatitude() != -9999 || 
//                	    sndingProfile.getStationLongitude() != -9999 ) {
//                		System.out.println("Station latlon is "+ sndingProfile.getStationLatitude() +
//                				"/"+ sndingProfile.getStationLongitude() );
//                	}
                	
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
                    	if( metPrm.getMetParamName().equals( StationLatitude.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingProfile.getStationLatitude(), Unit.ONE ) );
                    	}
                    	else if( metPrm.getMetParamName().equals( StationLongitude.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingProfile.getStationLongitude(), Unit.ONE ) );
                    	}
                    	else if( metPrm.getMetParamName().equals( StationElevation.class.getSimpleName() ) ) {
//                            metPrm.setValue( new Amount( 
//                            		sndingProfile.getStationElevation(), SI.METER ) );
                    	}
                    	else if( metPrm.getMetParamName().equals( StationID.class.getSimpleName() ) ) {
                    		if( sndingProfile.getStationId().isEmpty() ) {
                    			metPrm.setStringValue( sndingProfile.getStationId() );
                    		}
                    		else {
                    			metPrm.setValueToMissing();
                    		}
//                    		if( stnInfo.stationId != null && !stnInfo.stationId.isEmpty() ) {
//                    			metPrm.setStringValue( stnInfo.stationId );
//                    		}
                    	}
                    	else if( metPrm.getMetParamName().equals( StationName.class.getSimpleName() ) ) {
//                            metPrm.setStringValue( Integer.toString( sndingProfile.getStationNum() ) );
                    	}
                    	else if( metPrm.getMetParamName().equals( AirTemperature.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingLayer.getTemperature(), SI.CELSIUS ) );
                    	}
                    	else if( metPrm.getMetParamName().equals( PressureLevel.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingLayer.getPressure(), NcUnits.MILLIBAR ) );
                    	}
                    	else if( metPrm.getMetParamName().equals(  RelativeHumidity.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingLayer.getRelativeHumidity(), Unit.ONE ) );
                    	}
                    	else if( metPrm.getMetParamName().equals( DewPointTemp.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingLayer.getDewpoint(), SI.CELSIUS ) );
                    	}
                    	else if( metPrm.getMetParamName().equals( WindSpeed.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingLayer.getWindSpeed(), NonSI.KNOT ) );
                    	}
                    	else if( metPrm.getMetParamName().equals( WindDirection.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingLayer.getWindDirection(), NonSI.DEGREE_ANGLE ) );
                    	}
                    	else if( metPrm.getMetParamName().equals(  HeightAboveSeaLevel.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingLayer.getGeoHeight(), SI.METER ) );
                    	}
                    	else if( metPrm.getMetParamName().equals(  WindDirectionUComp.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingLayer.getWindU(), NonSI.KNOT ) );
                    	}
                    	else if( metPrm.getMetParamName().equals(  WindDirectionVComp.class.getSimpleName() ) ) {
                            metPrm.setValue( new Amount( 
                            		sndingLayer.getWindV(), NonSI.KNOT ) );
                    	}
      // TODO : for modelsoundings. what are the units?
//                    	else if( metPrm.getMetParamName().equals(   VerticalVelocity.class.getSimpleName() ) ) {
//                            metPrm.setValue( new Amount( 
//                            		sndingLayer.getOmega(),  ) );
//                    	}
//                    	else if( metPrm.getMetParamName().equals(  SpecificHumidity.class.getSimpleName() ) ) {
//                            metPrm.setValue( new Amount( 
//                            		sndingLayer.getSpecHumidity(), NonSI.  ) );
//                    	}
                    }
                    
                    // loop thru the derived params, and derive the values using value in 
                    // the dbParamsMap 
                    //
                    for( AbstractMetParameter derivedParam : derivedParamsList ) {
                    	try {
                    		derivedParam.derive( dbParamsMap.values() );

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
                    	BufferedImage bImage = plotCreator.getStationPlot( paramsToPlot ); // don't need this anymore, id);

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

    private void setMetParamFromPDV( AbstractMetParameter metPrm, PointDataView pdv, String dbParam) {
		   Type pType = pdv.getType(dbParam);

		   if( metPrm.hasStringValue() && pType != Type.STRING ||
				   !metPrm.hasStringValue() && pType == Type.STRING ) {
			   System.out.println("type for DB parameter "+dbParam+" is not compatible with "+
					   "metParameter "+metPrm.getClass().getSimpleName() );
			   return;
		   }
		   else if( metPrm.hasStringValue() ) {
			   metPrm.setStringValue( pdv.getString( dbParam ) );                		   
		   }
		   else {
			   metPrm.setValue( pdv.getNumber( dbParam ),
					   pdv.getUnit( dbParam ) );
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
}
