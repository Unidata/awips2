package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import gov.noaa.nws.ncep.edex.common.dataRecords.NcFloatDataRecord;
import gov.noaa.nws.ncep.gempak.parameters.colors.COLORS;
import gov.noaa.nws.ncep.gempak.parameters.hilo.HILOBuilder;
import gov.noaa.nws.ncep.gempak.parameters.hilo.HILOStringParser;
import gov.noaa.nws.ncep.gempak.parameters.hlsym.HLSYM;
import gov.noaa.nws.ncep.gempak.parameters.intext.TextStringParser;
import gov.noaa.nws.ncep.gempak.parameters.title.TITLE;
import gov.noaa.nws.ncep.viz.common.ui.HILORelativeMinAndMaxLocator;
import gov.noaa.nws.ncep.viz.common.ui.ModelListInfo;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;
import gov.noaa.nws.ncep.viz.gempak.util.DatatypeTable;
import gov.noaa.nws.ncep.viz.gempak.util.GempakGrid;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.AbstractFrameData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.ContourAttributes;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.ContourRenderable;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.GridIndicesDisplay;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.GridPointMarkerDisplay;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.GridPointValueDisplay;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.GridRelativeHiLoDisplay;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.contours.GriddedVectorDisplay;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv.Dgdriv;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv.DgdrivException;

import java.io.FileNotFoundException;
import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.style.level.SingleLevel;

/**
 * Grid contour Resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb, 2010           		M. Li      		Initial creation
 * Jun, 2010			    M. Li	   		Retrieve grid data from Grid Diagnostic instead of HDF5
 * Oct, 2010     #307       G. Hull    		use NcGridDataProxy to support correct time matching
 * Oct, 2010     #320       X. Guo     		Replace special characters in TITLE parameter
 * Oct, 2010	 #307       m.gamazaychikov	Add handling of DgdrivException in getDataRecord method
 * Oct, 2010                X. Guo          Rename getCycleTimeStringFromDataTime to getTimeStringFromDataTime
 * Oct, 2010	 #277		M. Li			Parsed first model name from ensemble model list
 * Nov, 2010			    M. Li			modified for new vector algorithm
 * 11/29/2010				mgamazaychikov	Updated queryRecords and updateFrameData
 * 12/06/2010    #363       X. Guo          Plot relative minima and maxima gridded data
 * 12/19/2010     365       Greg Hull       Replace dataSource with pluginName
 * 01/03/11				    M. Li			Add hilo and hlsysm to contourAttributes
 * 01/07/11				    M. Li			Use Vector array
 * 03/08/11					M. Li			refactor ContourRenderable
 * 04/29/11				    M. Li			move gridIndiceDisplay to a separate class
 * 06/2011					mgamazaychikov	Add spatialObject to the Dgdriv fields.
 * 07/2011					mgamazaychikov	Add substituteAlias method.
 * 08/2011					mgamazaychikov	Add dispose method to FrameData class;
 * 											change disposeInternal method of NcgridResource class;
 * 											change aDgdriv from the NcgridResource class variable to local variable.
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */
public class NcgridResource extends AbstractNatlCntrsResource<NcgridResourceData, MapDescriptor> implements 
   		INatlCntrsResource {

	protected NcgridResourceData gridRscData;
	
    protected SingleLevel[] levels;

    protected float displayedLevel;

    protected boolean ready = false;

    protected IGraphicsTarget lastTarget;

    protected SingleLevel level;

    protected String levelUnits;

    protected UnitConverter conversion;

    private boolean needsUpdate = false;
    
    private ArrayList<DataTime> dataTimesForDgdriv = new ArrayList<DataTime>();
    
//    private MathTransform trans;
    
//    private SymbolLocationSet gridPointMarkerSet = null;
    
//    private RGB gridIndiceColor = new RGB(255, 255, 0);
    
    private ContourAttributes[] contourAttributes;
    
    
    // These objects are used as proxys to time match the frames.
    // These are created by querying the db for available times and then 
    // they are assiged to the FrameData's which will then use the time
    // in a separate query to the DgDriv to get the grid data.
    //    The dataTime is set first and then the spatial object since this
    // is determined later in updateFrameData after the time matching is done.
    // This is because, for the GHM model, the spatialObjects will be different
    // for each time. 
    private class NcGridDataProxy implements IRscDataObject {
    	
    	private DataTime       dataTime;
    	private ISpatialObject spatialObj;
    	
    	public NcGridDataProxy( DataTime dt ) {
    		dataTime = new DataTime( dt.getRefTime(), dt.getFcstTime() );
    		spatialObj = null;
    	}
    	
		@Override
		public DataTime getDataTime() {		
			return dataTime;
		}    	
		
		public void setSpatialObject( ISpatialObject spatObj ) {
			spatialObj = spatObj;
		}
		public ISpatialObject getSpatialObject() {
			return spatialObj;
		}
    }
    
    /*
    private class VectorContourRenderable extends ContourRenderable {

       // private PluginDataObject pdo;
        private NcGridDataProxy gridDataPrxy;
        
        private IDataRecord cache;

        private boolean dataRequested = false;

//        private Exception exception = null;
        
        ContourAttributes attr;

        public VectorContourRenderable(IMapDescriptor descriptor,
        		NcGridDataProxy gridData, ContourAttributes attr) {
            super(descriptor);
            this.gridDataPrxy = gridData;
            this.attr = attr;
        }

        @Override
        public IDataRecord getData() {
//            try {
//                if (exception != null) {
//                    throw exception;
//                }
                if (cache == null && !dataRequested) {
                    dataRequested = true;
//                    new Thread() {
//                        @Override
//                        public void run() {
//                            try {
//                                cache = getDataRecord(pdo, attr);
//                                lastTarget.setNeedsRefresh(true);
//                            } catch (Exception e) {
//                                exception = e;
//                            }
//                        }
//                    }.start();
                    try {
                    	cache = getDataRecord( gridDataPrxy, attr);
                    } catch (DgdrivException e) {
                    	e.printStackTrace();
                    }
                    lastTarget.setNeedsRefresh(true);
                }

                return cache;
//   print error message in the console instead of in the alertviz popup, request by DP
//            } catch (VizException e) {
//            	String msg = "Error retrieving " + gridRscData.gdfile.toUpperCase() + 
//            				 " " + attr.getGdpfun().toUpperCase()+ " data\nError is: "+e.getMessage() ;
//                throw new VizException(msg, e);
//            }
        }


        @Override
        public MathTransform getTransform() {
//        		NcgribRecord gribRecord = (NcgribRecord) pdo;

        		GridGeometry2D gridGeometry2D = MapUtil.getGridGeometry( gridDataPrxy
        				.getSpatialObject());
        		return gridGeometry2D.getGridToCRS(PixelInCell.CELL_CENTER);
        }

       

		@Override
		public GeneralGridGeometry getGridGeometry() {
			//NcgribRecord gribRecord = (NcgribRecord) pdo;
			
	        GridGeometry2D gridGeometry2D = MapUtil.getGridGeometry(gridDataPrxy
	                .getSpatialObject());
	        return gridGeometry2D;
		}

		@Override
		public ContourAttributes getContourAttributes() {
			return attr;
		}
    }
    
    */
    
    protected class FrameData extends AbstractFrameData {
    	ContourRenderable[] contourRenderable;
    	NcGridDataProxy gdPrxy=null;
    	GriddedVectorDisplay[] vectorDisplay;
    	GridPointValueDisplay gridPointValueDisplay;
    	GridRelativeHiLoDisplay gridRelativeHiLoDisplay;
    	GridPointMarkerDisplay gridPointMarkerDisplay;
    	GridIndicesDisplay gridIndicesDisplay;
    	
    	String gfunc = "";    	
    	String glevel = "";
    	String gvcord = "";
    	String scale = "";
    	
    	protected FrameData(DataTime time, int interval) {
    		super(time, interval);	
    	}
    	
    	// 
    	public boolean updateFrameData(IRscDataObject rscDataObj ) {
    		if( !(rscDataObj instanceof NcGridDataProxy ) ) {
    			System.out.println("Unexpected rscDataObject type in NcGridResource:updateFrameData:"+
    					rscDataObj.getClass().getName() );
    			return false;
    		}
    		
    		/*
    		 * set navigation
    		 */
    		gdPrxy = (NcGridDataProxy) rscDataObj;
    		
    		if ( gridRscData.getPluginName().equalsIgnoreCase( GempakGrid.gempakPluginName )) {
    			try {
    				String dataLocation = null;
    				try {
//            			dataLocation = NmapCommon.getDatatypeTblColumnValue( gridRscData.getGdfile(), "PATH" );
            			dataLocation = DatatypeTable.getDatatypeTblColumnValue(
            					gridRscData.getGdfile(), "PATH");
            			
    				} catch (VizException e) {
    					throw new VizException ("Unable to specify location for " 
    							+ gridRscData.getPluginName() + " " + gridRscData.getGdfile(), e);
    				}
    				ISpatialObject cov =(ISpatialObject) GempakGrid.getGridNavigation(dataLocation, 
    						rscDataObj.getDataTime().toString());
    				gdPrxy.setSpatialObject( cov );
    			} catch (Exception e) {
    				// TODO Auto-generated catch block
    				System.out.println("Error retrieving GEMPAK grid navigation block: "+e.getMessage() );
                	return false;
    			}
    		}
    		else {
    			HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
        				resourceData.getMetadataMap());
        		
        		if (gridRscData.getGdfile().startsWith("}") && gridRscData.getGdfile().endsWith("}")) {
        			ModelListInfo modelListInfo = new ModelListInfo(gridRscData.getGdfile());
                	String modelName = modelListInfo.getModelList().get(0).getModelName().toUpperCase();
        			queryList.put("modelInfo.modelName", 
                        		new RequestConstraint( modelName, ConstraintType.EQUALS ) );
        		}

                LayerProperty property = new LayerProperty();
                property.setDesiredProduct( ResourceType.PLAN_VIEW );
    			queryList.put("dataTime", new RequestConstraint( gdPrxy.getDataTime().toString() ) );
                
                try {
        			property.setEntryQueryParameters( queryList );			
        			// just need one since all the spatial objs should be the same for this time.
        			property.setNumberOfImages(1); 
        			
        			String script = null;
        			script = ScriptCreator.createScript( property );

        			if (script == null) {
        				gdPrxy = null;
        				throw new VizException( "error creating script.");
        			}

        			Object[] pdoList = Connector.getInstance().connect(script, null, 20000);

        			if( pdoList == null || pdoList.length == 0 ) {
        				gdPrxy = null;
        				throw new VizException( "No records found.");
        			}
//        			System.out.println("retrieving dataTime : "+((NcgribRecord) pdoList[0]).getDataTime().toString() );

        			// update the spatial object in the gdProxy
        			ISpatialObject cov = ((NcgribRecord) pdoList[0]).getSpatialObject();
        			gdPrxy.setSpatialObject( cov );
                }
                catch( VizException e) {
                	System.out.println("Error retrieving ncgrid record for the spatial object: "+e.getMessage() );
                	return false;
                }
    		}
    		
    		
    		
    		/*
    		 * query data
    		 */
    		synchronized (this) {
    			if (contourRenderable == null )
    				contourRenderable = new ContourRenderable[contourAttributes.length];
    			if (vectorDisplay == null)
    				vectorDisplay = new GriddedVectorDisplay[contourAttributes.length];
//  			System.out.println("contourAttributes.length==="+contourAttributes.length);
				
				for (int i = 0; i < contourAttributes.length; i++) {
					
					DisplayType displayType = getVectorType(contourAttributes[i].getType());
					String attrType = contourAttributes[i].getType().toUpperCase();
					
					if ( attrType.contains("M") && gridPointMarkerDisplay == null) {
						gridPointMarkerDisplay = new GridPointMarkerDisplay(contourAttributes[i].getMarker(),descriptor, 
								gdPrxy.getSpatialObject());
					}
					
					if (attrType.contains("G") && gridIndicesDisplay == null) {
						gridIndicesDisplay = new GridIndicesDisplay(GempakColor.convertToRGB(gridRscData.getGrdlbl()), 
								descriptor, gdPrxy.getSpatialObject());
					}
					
					
					/*
					 * Vector data
					 */
					if ( displayType == DisplayType.ARROW || displayType == DisplayType.BARB ) {
						
						boolean isDirectionalArrow = attrType.equals("D");
						
						if (vectorDisplay[i] == null) {
							NcFloatDataRecord rec = null;
							try {
								rec = (NcFloatDataRecord) getDataRecord(gdPrxy, contourAttributes[i]);
							} catch (DgdrivException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
							
							vectorDisplay[i] = new GriddedVectorDisplay(rec, displayType, isDirectionalArrow, descriptor, 
									contourAttributes[i].getWind(), gdPrxy.getSpatialObject());
						}	
						else {
							vectorDisplay[i] = new GriddedVectorDisplay(vectorDisplay[i].getData(), displayType, isDirectionalArrow, descriptor, 
									contourAttributes[i].getWind(), gdPrxy.getSpatialObject());
						}
					}
					else if (displayType == DisplayType.CONTOUR || displayType == DisplayType.STREAMLINE || 
							attrType.contains("P")){
						
						/*
						 * Scalar data
						 */
						String contourName = createContourName(gdPrxy, contourAttributes[i]);
						
						// New creation
						if (contourRenderable[i] == null) {
							IDataRecord gridData = null;
							// Duplicate data
							int index = -1;
							if (i > 0) {
								for (int n = 0; n < i; n++) {
									if (contourAttributes[i].getGdpfun().trim().equalsIgnoreCase(
										contourAttributes[n].getGdpfun().trim()) && 
										contourRenderable[n].getData() instanceof NcFloatDataRecord ) {
										index = n;
										break;
									}
								}
							}
							
							// Avoid duplicate data retrieval
							if (index >= 0) {
								gridData = contourRenderable[index].getData();
							}
							else {
								try {
									gridData = getDataRecord(gdPrxy, contourAttributes[i]);
								} catch (DgdrivException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
							}
							
							contourRenderable[i] = new ContourRenderable(gridData, descriptor, 
									MapUtil.getGridGeometry(gdPrxy.getSpatialObject()), contourAttributes[i], contourName);
						} 
						// Attributes or navigation change
						else {
							contourRenderable[i] = new ContourRenderable(contourRenderable[i].getData(), descriptor, 
									MapUtil.getGridGeometry(gdPrxy.getSpatialObject()), contourAttributes[i], contourName);
						}
						
						
						// Grid point value
						if (attrType.contains("P") && contourRenderable[i] != null && 
								contourRenderable[i].getData() instanceof NcFloatDataRecord ) {
							if (gridPointValueDisplay == null || needsUpdate || 
									!contourAttributes[i].getGdpfun().equalsIgnoreCase(gfunc) ||
									!contourAttributes[i].getGlevel().equalsIgnoreCase(glevel) ||
									!contourAttributes[i].getGvcord().equalsIgnoreCase(gvcord) || 
									!contourAttributes[i].getScale().equalsIgnoreCase(scale)) {
								
								gridPointValueDisplay = createGridPointValueDisplay((NcFloatDataRecord)contourRenderable[i].getData(),
															gdPrxy, contourAttributes[i]);

								gfunc = contourAttributes[i].getGdpfun();
								glevel = contourAttributes[i].getGlevel();
								gvcord = contourAttributes[i].getGvcord();
								scale = contourAttributes[i].getScale();
							}
						}
						
						// create HILO symbols
						if (gridRelativeHiLoDisplay == null && contourRenderable[i] != null && 
								contourRenderable[i].getData() instanceof NcFloatDataRecord) {
    						gridRelativeHiLoDisplay = createGridRelativeHiLoDisplay(
    								(NcFloatDataRecord)contourRenderable[i].getData(), gdPrxy, contourAttributes[i]);
						}
						
					}
					
				} // end of for loop
				
				Collections.sort(NcgridResource.this.dataTimes);
			}  // end of synchronized
    		
			return true;
		}

		private String createContourName(NcGridDataProxy gdPrxy,
				ContourAttributes contourAttributes) {
			StringBuilder contourName = new StringBuilder();
			contourName.append(gridRscData.getGdfile().trim().toUpperCase() + " ");
			contourName.append(contourAttributes.getGdpfun().trim().toUpperCase() + " ");
			contourName.append(" ^" + gdPrxy.getDataTime().toString() + " @" + contourAttributes.getGlevel() + 
					" %" + contourAttributes.getGvcord().toUpperCase());

			return contourName.toString();
		}
		
		public void dispose() {
			gdPrxy=null;
			if ( contourRenderable != null) {
				for (ContourRenderable cr: contourRenderable) {
					if (cr != null) {
						cr.dispose();
					}
					
				}
				contourRenderable = null;
			}
			
			if (vectorDisplay != null) {
				for (GriddedVectorDisplay vd: vectorDisplay) {
					if ( vd != null) {
						vd.dispose();
					}
					
				}
				vectorDisplay = null;
			}
			if (gridPointValueDisplay != null) {
				gridPointValueDisplay.dispose();
				gridPointValueDisplay = null;
			}
    	}

	}

    /**
     * Constructor
     */
    public NcgridResource(NcgridResourceData data, LoadProperties props) {
        super(data, props);
//        data.addChangeListener((IResourceDataChanged) this);
        gridRscData = data;
    }

    // override the base version which queries the db based on the metadatamap since
    // this will return too many objects which won't be used. This will just query 
    // the availableTimes and use them to time match to the frames.
    //
    @Override
	public void queryRecords() throws VizException {
    	ResourceName rscName = getResourceData().getResourceName();
    	
        DataTime   cycleTime = rscName.getCycleTime();

        if( cycleTime == null || rscName.isLatestCycleTime() ) { // latest should already be resolved here.
        	return;
        }
		
		DataTime[] availableTimes;
		if ( gridRscData.getPluginName().equalsIgnoreCase( GempakGrid.gempakPluginName )) {
			try {
				String dataLocation = null;
				try {
        			dataLocation = DatatypeTable.getDatatypeTblColumnValue(
        					gridRscData.getGdfile(), "PATH");
        			
				} catch (VizException e) {
					throw new VizException ("Unable to specify location for " 
							+ gridRscData.getPluginName() + " " + gridRscData.getGdfile(), e);
				}
				String []  gridAvailableTimes = GempakGrid
					.getAvailableGridTimes(dataLocation, cycleTime.toString());
				availableTimes = new DataTime[gridAvailableTimes.length];
				for ( int ii=0; ii<gridAvailableTimes.length; ii++) {
					availableTimes[ii] = new DataTime (gridAvailableTimes[ii]);
				}
			} catch (Exception e) {
				throw new VizException(e);
			}
		}
		else {
			// Note that since this is not constraining the grid parameter, level... there is 
	        // still a potential problem if the db is missing times for the displayed param 
	        // while times for other params exist.
			HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
					resourceData.getMetadataMap());

			if (gridRscData.getGdfile().startsWith("{") && gridRscData.getGdfile().endsWith("}")) {
				ModelListInfo modelListInfo = new ModelListInfo(gridRscData.getGdfile());
	        	String modelName = modelListInfo.getModelList().get(0).getModelName().toUpperCase();
				queryList.put("modelInfo.modelName", 
	                		new RequestConstraint( modelName, ConstraintType.EQUALS ) );
			}
			else {
				queryList.put("modelInfo.modelName", 
		        		new RequestConstraint( gridRscData.getGdfile(), ConstraintType.EQUALS ) );
			}
			LayerProperty property = new LayerProperty();
	        property.setDesiredProduct( ResourceType.PLAN_VIEW );
	        
	        try {
				property.setEntryQueryParameters( queryList );			
		        availableTimes = property.getEntryTimes();
	        }
	        catch( VizException e) {
	        	throw e;
	        }
		}
        
        ArrayList<DataTime> dataTimes = new ArrayList<DataTime>();

        // loop thru all the times for the grib records for this model and 
        //   if the time matches the cycle time for this resource and if
        //   it hasn't already been added, add it to the queue of data objects
        //   to be processed by the abstract class.
        for( DataTime dt : availableTimes ) {
        	// create a dataTime without a possible validPeriod.
        	DataTime availTime = new DataTime( dt.getRefTime(), dt.getFcstTime() );
        	DataTime refTime = new DataTime( dt.getRefTime() );

        	if( cycleTime.equals( refTime ) ) {	        		
        		if( !dataTimes.contains( availTime ) ) {	        			
        			dataTimes.add( availTime );
        			// reuse the same gribRec this is a bit of a hack but hey. 
//        			gribRec.setDataTime(availTime);
                	for( IRscDataObject dataObject : processRecord( availTime ) ) { // gribRec ) )	{	
                		newRscDataObjsQueue.add(dataObject);
                	}
        		}
        	}        	
        }
        setDataTimesForDgdriv (dataTimes);
    }

    private void setDataTimesForDgdriv(ArrayList<DataTime> dataTimes) {
		dataTimesForDgdriv = dataTimes;
	}

	// TODO : if called from the auto update code this will be a Record so add code 
    // to get the time and process as an NcGridDataTimeObj
    //
    @Override
	protected IRscDataObject[] processRecord( Object obj ) {
		if( obj instanceof DataTime ) {
			NcGridDataProxy rscDataObj = new NcGridDataProxy( (DataTime)obj ); 
//			gribRec.getDataTime(), gribRec.getSpatialObject() );	
			return new NcGridDataProxy[]{ rscDataObj };
		}
		else if( obj instanceof NcgribRecord ) {
			NcGridDataProxy rscDataObj = new NcGridDataProxy( ((NcgribRecord)obj).getDataTime() ); 
			return new NcGridDataProxy[]{ rscDataObj };
		}
		else {	
			System.out.println( "Unexpected object in NcGridResource.processRecord ");
			return null;
		}
	}
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
    	FrameData currFrame = (FrameData) getCurrentFrame();
    	if( currFrame == null || currFrame.gdPrxy == null ) {
			return String.format("%s %s", gridRscData.getGdfile(), gridRscData.getTitle())+"-No Data";
		}

    	
		
        String nameStr="";
        TITLE title = new TITLE (gridRscData.getTitle());
        if ( title.getTitleString() !=null) {
        	nameStr = generateTitleInfo (title.getTitleString());
        	//set legend color
        	gridRscData.setLegendColor(title.getTitleColor());
        }

        return String.format("%s", nameStr);
    }

    @Override
	public void disposeInternal() {
    	this.dataTimesForDgdriv.clear();
    	for( AbstractFrameData frameData : frameDataMap.values() ) {
			frameData.dispose();
		}
		frameDataMap.clear();
    }
    

	public void initResource(IGraphicsTarget target) throws VizException {
		synchronized (this) {
			long t0 = System.currentTimeMillis();
			separateAttributes();
			this.lastTarget = target;			
			queryRecords();
			long t1 = System.currentTimeMillis();
			System.out.println("\t\t NcgridResource.initResource took: " + (t1-t0));
		}

    }

    public void paintFrame(AbstractFrameData frmData,
			IGraphicsTarget target, PaintProperties paintProps)
			throws VizException {
    	
    	if( needsUpdate ) {
			initResource(target);
			needsUpdate = false;
		}
    	
    	synchronized (this) {
    		FrameData currFrame = (FrameData) frmData; // will not be null

    		if( currFrame.gdPrxy == null ) {
    			return;
    		}
    		
    		for (int i = 0; i < contourAttributes.length; i++) {
    			
    			String type = contourAttributes[i].getType().toUpperCase();
    			
    			/*
    			 * Plot grid point markers if needed
    			 */
    			if (type.contains("M")) {
    				GridPointMarkerDisplay gridPointMarkerDisplay = currFrame.gridPointMarkerDisplay;
    				if (gridPointMarkerDisplay != null)
    					gridPointMarkerDisplay.paint(target, paintProps);
    				
    			}

    			/*
    			 * Plot grid indices(row/column numbers) if requested
    			 */
    			if (type.contains("G")) {
    				GridIndicesDisplay gridIndicesDisplay = currFrame.gridIndicesDisplay;
    				if (gridIndicesDisplay != null)
    					gridIndicesDisplay.paint(target, paintProps);
    			}
    			
    			
    			/*
    			 * Draw wind barb or wind arrow
    			 */
    			if (type.contains("B") || type.contains("A") ) {

    				GriddedVectorDisplay griddedVectorDisplay = currFrame.vectorDisplay[i];
    				if (griddedVectorDisplay != null) {
    					griddedVectorDisplay.paint(target, paintProps);
    				}
    			}
    			else {
    				
    				/*
    				 * Draw contours or streamlines
    				 */
    				ContourRenderable contourGroup = currFrame.contourRenderable[i];
    				if (contourGroup == null || !(contourGroup.getData() instanceof NcFloatDataRecord)) continue;
    				if ( contourGroup != null && (type.contains("C") ||	type.contains("F") || type.contains("S")) ) {
    					contourGroup.paint(target, paintProps);
    				}

    				/*
    				 *  Plot HILO if needed
    				 */
					if (type.contains("C") || type.contains("F")) {
						GridRelativeHiLoDisplay gridRelativeHiLoDisplay = currFrame.gridRelativeHiLoDisplay;
						if (gridRelativeHiLoDisplay != null ) {
							gridRelativeHiLoDisplay.paint(target, paintProps);
						}
					}
					
    				/*
    				 *  Draw grid point values if needed
    				 */
    				GridPointValueDisplay gridPointValueDisplay = currFrame.gridPointValueDisplay;
    				if (gridPointValueDisplay != null && type.contains("P")) {
    					gridPointValueDisplay.paint(target, paintProps);
    				}
    			}

    			

    		}
    	}
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
    	needsUpdate = true;
    	super.project(mapData);
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        return "Sampling not implemented for ncgridResource";
    }


    @Override
	protected AbstractFrameData createNewFrame( DataTime frameTime, int frameInterval) {
		return new FrameData( frameTime, frameInterval );
	}


    /**
     * Retrieve the data record
     * 
     * @param obj
     * @return
     * @throws FileNotFoundException
     * @throws StorageException
     * @throws VizException
     */
	protected IDataRecord getDataRecord(NcGridDataProxy gdPrxy, ContourAttributes cattr) throws DgdrivException {
		
		if (gdPrxy == null) {
			return null;
		}
		
		/*
		 * Instantiate and populate the object for data retrieval from GEMPAK GD
		 */
		String inputGdfile = gridRscData.getGdfile();
		if (gridRscData.getEventName() != null) {
			inputGdfile = inputGdfile + ":" + gridRscData.getEventName();
		}
		
		Dgdriv aDgdriv = new Dgdriv();
		aDgdriv.setCycleForecastTimes(dataTimesForDgdriv);
		aDgdriv.setSpatialObject(gdPrxy.getSpatialObject());
    	aDgdriv.setGdattim( gdPrxy.getDataTime().toString());
    	aDgdriv.setGarea("dset");
    	aDgdriv.setGdfile(inputGdfile);
    	aDgdriv.setGdpfun(cattr.getGdpfun());
    	aDgdriv.setGlevel(cattr.getGlevel());
    	aDgdriv.setGvcord(cattr.getGvcord());
    	aDgdriv.setScale(cattr.getScale());
    	aDgdriv.setDataSource(gridRscData.getPluginName());

    	DisplayType displayType = getVectorType(cattr.getType());
    	if (displayType == DisplayType.ARROW || displayType == DisplayType.BARB || displayType == DisplayType.STREAMLINE) {
    		/*
        	 *  Specify vector data retrieval from GEMPAK GD
        	 */
        	aDgdriv.setScalar(false);
    	}
    	else {
        	/*
        	 *  Specify scalar data retrieval from GEMPAK GD
        	 */
        	aDgdriv.setScalar(true);
    	}
    	
    	try {
    		return aDgdriv.execute();
		} catch (DgdrivException e) {
			throw new DgdrivException("GEMPAK GD error stack:\n" + e.getMessage(), e);
		}
//		finally {
//			aDgdriv = null;
//		}
    }

    @Override
	public void resourceAttrsModified() {		
		// Repaint the data
		needsUpdate = true;
	}
    

    private GridPointValueDisplay createGridPointValueDisplay(NcFloatDataRecord rec, NcGridDataProxy gdPrxy, ContourAttributes attr) {
    	
    	if (rec == null || rec.getXdata() == null)
    		return null;
    	
    	if (!attr.getType().toUpperCase().contains("P")) return null;
    	
    	FloatBuffer plotData = FloatBuffer.wrap(rec.getXdata());    	
    	COLORS color = new COLORS(gridRscData.getColors());

    	return new GridPointValueDisplay(plotData, color.getFirstColor(), descriptor, 
    			gdPrxy.getSpatialObject());
    } 
    
    private GridRelativeHiLoDisplay createGridRelativeHiLoDisplay (NcFloatDataRecord rec, NcGridDataProxy gdPrxy, ContourAttributes attr) {
    	
    	DisplayType displayType = getVectorType(attr.getType());
    	if (displayType != DisplayType.CONTOUR) return null;
    	
//    	NcFloatDataRecord rec = null;
//		try {
//			rec = (NcFloatDataRecord)getDataRecord(gdPrxy, attr);
//		} catch (DgdrivException e) {
//			e.printStackTrace();
//		}
    	if (rec == null || rec.getXdata() == null)
    		return null;
    	
    	if ( gridRscData.getHilo() == null || gridRscData.getHilo().isEmpty()) {
    		return null;
    	}
    	
    	HILOStringParser hilo = new HILOStringParser(gridRscData.getHilo());
   
    	if ( !(hilo.isHiLoStringParsed())) {
    		//Parse HILO failure
    		return null;
    	}
    	HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
    	
    	GeneralGridGeometry gridGeometryOfGrid = MapUtil.getGridGeometry(gdPrxy.getSpatialObject());
    	
       int nx = gridGeometryOfGrid.getGridRange().getSpan(0);
       int ny = gridGeometryOfGrid.getGridRange().getSpan(1);
       
    	HILORelativeMinAndMaxLocator hiloLocator = new HILORelativeMinAndMaxLocator (rec.getXdata(),
    			nx,ny,hiloBuild.getRadius(),hiloBuild.getInterp(),hiloBuild.getCountHi(),hiloBuild.getCountLo(),
				hiloBuild.getRangeHiMinval(),hiloBuild.getRangeHiMaxval(),hiloBuild.getRangeLoMinval(),hiloBuild.getRangeLoMaxval());

        if ( !(hiloLocator.isHILOMinAndMaxLocated()) ) {
        	//Not locate HILO minima and maxima
        	return null;
        }
        
    	if (gridRscData.getHlsym() == null) {
    		gridRscData.setHlsym(""); 
    	}
    	HLSYM hlsym = new HLSYM(gridRscData.getHlsym());
    	TextStringParser txtMarkerStr = new TextStringParser (hlsym.getMarkerString());
    	TextStringParser txtValueStr = new TextStringParser (hlsym.getValueString());
  
    	return new GridRelativeHiLoDisplay (hiloBuild,hiloLocator,txtMarkerStr,
    			txtValueStr,descriptor,gdPrxy.getSpatialObject());     	
    } 
    
    public DisplayType getVectorType(String type) {
		if (type.toUpperCase().contains("B")) {
			return DisplayType.BARB;
		} else if (type.toUpperCase().contains("A") || 
				   type.toUpperCase().contains("D")) {
			return DisplayType.ARROW;
		} else if (type.toUpperCase().contains("S")) {
			return DisplayType.STREAMLINE;
		}
		else if (type.toUpperCase().contains("C") || type.toUpperCase().contains("F") ) {
			return DisplayType.CONTOUR;
		} else {
		    return null;
		}    
	}
    
    
    private void separateAttributes() {
    	String[] gfuncArray  = gridRscData.getGdpfun().trim().split("!");
    	String[] glevelArray = gridRscData.getGlevel().trim().split("!");
    	String[] gvcordArray = gridRscData.getGvcord().trim().split("!");
    	String[] scaleArray = gridRscData.getScale().trim().split("!");
    	String[] typeArray = gridRscData.getType().trim().split("!");
    	String[] cintArray = gridRscData.getCint().trim().split("!");
    	String[] lineArray = gridRscData.getLineAttributes().trim().split("!");
    	String[] fintArray = gridRscData.getFint().trim().split("!");
    	String[] flineArray = gridRscData.getFline().trim().split("!");
    	String[] hiloArray = gridRscData.getHilo().trim().split("!");
    	String[] hlsymArray = gridRscData.getHlsym().trim().split("!");
    	String[] windArray = gridRscData.getWind().trim().split("!");
    	String[] markerArray = gridRscData.getMarker().trim().split("!");
    	
    	for (int i = 0; i < gfuncArray.length; i++) {
    		if (gfuncArray[i].contains("//")) {
    			String[] tmpstr = gfuncArray[i].trim().split("//", 2);
    			gfuncArray[i] = tmpstr[0];
    			String referencedAlias = tmpstr[1];
    			String referencedFunc = tmpstr[0];
    			/*
    			 * Need to substitute all occurences of referencedAlias
    			 * with referencedFunc
    			 */
    			for (int j = i + 1; j < gfuncArray.length; j++) {
    				/*
    				 * First need to find out if the gfuncArray[i] is a
    				 * derived quantity
    				 */
    				gfuncArray[j] = substituteAlias (referencedAlias, referencedFunc, gfuncArray[j]);
//    				boolean isDerived = gfuncArray[j].trim().contains("(");
//    				boolean isReferenced = gfuncArray[j].trim().contains(referencedAlias);
//    				if ( isReferenced ) {
//    					gfuncArray[j] = gfuncArray[j].replace(referencedAlias, referencedFunc);
//    				}	
    			}
    		}
    	}
    	
    	contourAttributes = new ContourAttributes[gfuncArray.length];
    	
    	for (int i = 0; i < gfuncArray.length; i++) {
    		contourAttributes[i] = new ContourAttributes();
    		contourAttributes[i].setGdpfun(gfuncArray[i]);
    		
    		if (i == 0) {
    			contourAttributes[i].setGlevel(glevelArray[0]);
    			contourAttributes[i].setGvcord(gvcordArray[0]);
    			contourAttributes[i].setScale(scaleArray[0]);
    			contourAttributes[i].setType(typeArray[0]);
    			contourAttributes[i].setCint(cintArray[0]);
    			contourAttributes[i].setLine(lineArray[0]);
    			contourAttributes[i].setFint(fintArray[0]);
    			contourAttributes[i].setFline(flineArray[0]);
    			contourAttributes[i].setHilo(hiloArray[0]);
    			contourAttributes[i].setHlsym(hlsymArray[0]);
    			contourAttributes[i].setWind(windArray[0]);
    			contourAttributes[i].setMarker(markerArray[0]);
    			
    		}
    		else {
    			int idx = (glevelArray.length > i) ? i : (glevelArray.length - 1);
    			contourAttributes[i].setGlevel(glevelArray[idx]);
    			
    			idx = (gvcordArray.length > i) ? i : gvcordArray.length - 1;
    			contourAttributes[i].setGvcord(gvcordArray[idx]);
    			
    			if (i > scaleArray.length - 1) {
    				contourAttributes[i].setScale("0");
    			} else {
    			    contourAttributes[i].setScale(scaleArray[i]);
    			}    
    			
    			idx = (typeArray.length > i) ? i : typeArray.length - 1;
    			contourAttributes[i].setType(typeArray[idx]);
    			
    			idx = (cintArray.length > i) ? i : cintArray.length - 1;
    			contourAttributes[i].setCint(cintArray[idx]);
    			
    			idx = (lineArray.length > i) ? i : lineArray.length - 1;
    			contourAttributes[i].setLine(lineArray[idx]);
    			
    			idx = (fintArray.length > i) ? i : fintArray.length - 1;
    			contourAttributes[i].setFint(fintArray[idx]);
    			
    			idx = (flineArray.length > i) ? i : flineArray.length - 1;
    			contourAttributes[i].setFline(flineArray[idx]);
    			
    			idx = (hiloArray.length > i) ? i : hiloArray.length - 1;
    			contourAttributes[i].setHilo(hiloArray[idx]);
    			
    			idx = (hlsymArray.length > i) ? i : hlsymArray.length - 1;
    			contourAttributes[i].setHlsym(hlsymArray[idx]);
    			
    			idx = (windArray.length > i) ? i : windArray.length - 1;
    			contourAttributes[i].setWind(windArray[idx]);
    			
    			idx = (markerArray.length > i) ? i : markerArray.length - 1;
    			contourAttributes[i].setMarker(markerArray[idx]);
    		}
    	}
    }
    
    /**
     * Substitutes alias referencedAlias in the String returnedFunc with
     * String referencedFunc.
     * 
     * @param referencedAlias
     * @param referencedFunc
     * @param returnedFunc
     * @return
     */
    private String substituteAlias(String referencedAlias,
			String referencedFunc, String aFunc) {
    	
    	String returnedFunc = aFunc;
    	/*
    	 * Process single word functions first
    	 */
    	if ( !returnedFunc.contains("(") ){
    		if (returnedFunc.trim().equalsIgnoreCase(referencedAlias)){
    			return returnedFunc.replace(referencedAlias, referencedFunc);
    		}
    		return returnedFunc;
    	}
    	
    	/*
    	 * Need to make sure that the number of closing and opening 
    	 * parenthesis is the same.
    	 */
    	int openParenthesisNumber  = 0;
    	int closeParenthesisNumber = 0;

		for (char c : returnedFunc.toCharArray()) {
			if (c == '(') {
				openParenthesisNumber++;
			}
			else if (c == ')') {
				closeParenthesisNumber++;
			}
		}
    	
		/*
		 * If the some closing parenthesis are missing add them 
		 * at the end of returnedFunc
		 */
    	if (openParenthesisNumber != closeParenthesisNumber ) {
    		int parenthesisDeficit = openParenthesisNumber - closeParenthesisNumber;
    		for (int idef=0;idef<parenthesisDeficit;idef++) {
    			returnedFunc = returnedFunc + ")";
    			
    		}
    	}
    	
    	/*
    	 * Find all the words that make up our returnedFunc
    	 */
    	String delims = "[ (),]+";
    	String []returnedFuncWords = returnedFunc.split(delims);
    	
    	/*
    	 * Go over each returnedFunc word and replaced each 
    	 * referencedAlias with referencedFunc
    	 */
    	for (String component: returnedFuncWords) {
    		if (component.equalsIgnoreCase(referencedAlias)){
    			/*
    			 * Word that potentially needs de-aliasing found.
    			 */
    			boolean doneDealiasing = false;
    			int startInd = 0;
    			while ( !doneDealiasing ) {
    				int componentBeforePosition = returnedFunc.indexOf(component, startInd) -1;
        			int componentAfterPosition = componentBeforePosition + component.length() + 1;
        			boolean isRoundedBefore = Character.toString(returnedFunc.charAt(componentBeforePosition)).equalsIgnoreCase("(") ||
        			Character.toString(returnedFunc.charAt(componentBeforePosition)).equalsIgnoreCase(")") || 
        			Character.toString(returnedFunc.charAt(componentBeforePosition)).equalsIgnoreCase(",");
        			boolean isRroundedAfter = Character.toString(returnedFunc.charAt(componentAfterPosition)).equalsIgnoreCase("(") ||
        			Character.toString(returnedFunc.charAt(componentAfterPosition)).equalsIgnoreCase(")") || 
        			Character.toString(returnedFunc.charAt(componentAfterPosition)).equalsIgnoreCase(",");
        			if (isRoundedBefore && isRroundedAfter) {
        				/*
        				 * De-alias word since surrounded by '(', ')', or ','
        				 */
        				StringBuilder strb = new StringBuilder();
    					int startIndx = componentBeforePosition + 1;
    					int endIndx = startIndx + component.length();
    					int returnedFuncLen = returnedFunc.length();
    					strb.append(returnedFunc, 0, startIndx);
    					strb.append(referencedFunc);
    					strb.append(returnedFunc, endIndx, returnedFuncLen);
    					returnedFunc = strb.toString();
    					doneDealiasing = true;
        			}
        			else {
        				startInd = componentAfterPosition;
        			}
    			}
    			
    		}
    	}   	
    	
    	System.out.println("returnedFunc="+returnedFunc);
    	return returnedFunc;
//		int parenthesisNumber = returnedFunc.split("\\(").length-1;
//		parenthesisNumber = openParenthesisNumber;
//		System.out.println();
//		System.out.println();
//		
//		/*
//		 * Find the indices of opening parenthesis in the returnedFunc
//		 */
//		int [] openIndxs = new int[parenthesisNumber];		
//		int startOpenIndx = 0;
//		for (int j=0;j<parenthesisNumber; j++){
//			openIndxs[j] = returnedFunc.indexOf("(", startOpenIndx);
//			startOpenIndx = openIndxs[j]+1;
//		}
//		/*
//		 * Find the indices of closing parenthesis in the returnedFunc
//		 */
//		boolean done = false;
//		
//		int [] closeIndxs = new int[parenthesisNumber];
//		int startCloseIndx = 0;
//		int kk = parenthesisNumber-1;
//		int jj=0;
//		int foundCloseParenthesisNumber = 0;
//		while ( !done ) {
//			boolean foundMatchingParenthesis = false;
//			int anIndex = returnedFunc.indexOf(")", startCloseIndx);
//			kk = 0;
//			while ( !foundMatchingParenthesis) {
//				if ( anIndex < openIndxs[kk]) {
//					jj = kk - 1;
//					if ( closeIndxs[jj] == 0 && openIndxs[jj] !=0 ) {
//						foundMatchingParenthesis = true;
//						closeIndxs[jj] = anIndex;
//						foundCloseParenthesisNumber++;
//						kk++;
//					}
//				}
//				else {
//					kk++;
//				}
//			}
//			if ( foundCloseParenthesisNumber != parenthesisNumber) {
//				done = false;
//			}
//			else {
//				done = true;
//			}
//		}
//		for (int j=0;j<parenthesisNumber; j++){
//			openIndxs[j] = returnedFunc.indexOf("(", startOpenIndx);
//			closeIndxs[kk] = returnedFunc.indexOf(")", startCloseIndx);
//			startCloseIndx = closeIndxs[kk]+1;
//			startOpenIndx = openIndxs[j]+1;
//			kk--;
//		}
//		
//		/*
//		 * Traverse the returnedFunc and substitute each occurrence of 
//		 * referencedAlias with referencedFunc, starting with inner most
//		 * argument
//		 */
//		for (int j=parenthesisNumber;j>=1; j--){
//			String theNestedArgument = returnedFunc.substring(openIndxs[j-1]+1, closeIndxs[j-1]);
//			System.out.println("returnedFunc="+returnedFunc);
//			System.out.println("returnedFunc="+returnedFunc);
//			System.out.println("the theNestedArgument="+theNestedArgument);
//			if ( theNestedArgument.contains(",")) {
//				String [] nestedArgArray = theNestedArgument.split(",");
//				for (int nn=0; nn<nestedArgArray.length;nn++){
//					if (nestedArgArray[nn].equalsIgnoreCase(referencedAlias)) {
//						StringBuilder strb = new StringBuilder();
//						int startIndx = returnedFunc.indexOf(nestedArgArray[nn]);
//						int endIndx = startIndx + nestedArgArray[nn].length();
//						int returnedFuncLen = returnedFunc.length();
//						strb.append(returnedFunc, 0, startIndx);
//						strb.append(referencedFunc);
//						strb.append(returnedFunc, endIndx, returnedFuncLen);
//						returnedFunc = strb.toString();
//						closeIndxs[j-2] = closeIndxs[j-2] + referencedFunc.length() - referencedAlias.length();
//					}
//					else {
//						StringBuilder strb = new StringBuilder();
//						int startIndx = returnedFunc.indexOf(nestedArgArray[nn]);
//						int endIndx = startIndx + nestedArgArray[nn].length();
//						int returnedFuncLen = returnedFunc.length();
//						strb.append(returnedFunc, 0, startIndx);
//						strb.append(nestedArgArray[nn]);
//						strb.append(returnedFunc, endIndx, returnedFuncLen);
//						returnedFunc = strb.toString();
//					}
//					 
//				}				
//			}
//			else {
//				if (theNestedArgument.equalsIgnoreCase(referencedAlias)) {
//					StringBuilder strb = new StringBuilder();
//					int startIndx = returnedFunc.indexOf(theNestedArgument);
//					int endIndx = startIndx + theNestedArgument.length();
//					int returnedFuncLen = returnedFunc.length();
//					strb.append(returnedFunc, 0, startIndx);
//					strb.append(referencedFunc);
//					strb.append(returnedFunc, endIndx, returnedFuncLen);
//					returnedFunc = strb.toString();
//					closeIndxs[j-2] = closeIndxs[j-2] + referencedFunc.length() - referencedAlias.length();
//				}
//				else {
//					StringBuilder strb = new StringBuilder();
//					int startIndx = returnedFunc.indexOf(theNestedArgument);
//					int endIndx = startIndx + theNestedArgument.length();
//					int returnedFuncLen = returnedFunc.length();
//					strb.append(returnedFunc, 0, startIndx);
//					strb.append(theNestedArgument);
//					strb.append(returnedFunc, endIndx, returnedFuncLen);
//					returnedFunc = strb.toString();
//				}
//			}
//			System.out.println("returnedFunc="+returnedFunc);
//		}
//		return returnedFunc;
		
	}

	/*
     * Generate Title information
     */
    private String generateTitleInfo ( String title) {
    	String titleInfoStr;
    	String titleStr = title;
    	String shrttlStr = null;

    	/*
    	 * Break title string into title and short title
    	 */
    	int pos = title.indexOf('|');

    	if ( pos == 0){
    		titleStr = title.substring(1, title.length()).trim();
    	}
    	else if ( pos > 0 && pos < title.length() - 1) {
    		titleStr = title.substring(0, pos).trim();
    		shrttlStr = title.substring(pos+1, title.length()).trim();
    	}
    	else if ( pos == title.length() - 1) {
    		titleStr = title.substring(0, pos).trim();
    	}
    	
    	String modelname = gridRscData.getGdfile();
    	if (gridRscData.getEventName() != null) {
    		modelname = modelname + ":" + gridRscData.getEventName();
    	}
    	titleInfoStr = modelname + " " + replaceTitleSpecialCharacters (titleStr);
    	if ( shrttlStr != null) {
    		titleInfoStr = titleInfoStr + " | " + replaceTitleSpecialCharacters (shrttlStr);
    	}
    	return titleInfoStr;
    }
    /*
     *  In the grid display programs, special characters will be replaced
     *  as follows:
     *    		 ^            Forecast date/time
     *    		 ~            Valid date/time
     *    		 @            Vertical level
     *    		 _            Grid function
     *    		 $            Nonzero scaling factor
     *    		 #            Grid point location
     *     		 ?            Day of the week flag
     */
    private String replaceTitleSpecialCharacters ( String title ) {
    	String titleStr = title;
    	boolean daywk = false , daywkF = false, daywkV = false;
    	int pos, posV=-1, posF=-1;

		DataTime currFrameTm = (DataTime) getCurrentFrameTime();

		/*
		 * check '!' and remove it for now ???
		 */
		if ((pos = titleStr.indexOf('!')) > 0) {
			titleStr = titleStr.substring(0, pos -1 );
		}
        /*
         * check '?' flag for day of week
         */
        if ((pos = titleStr.indexOf('?')) > 0) {
        	titleStr = titleStr.substring(0, pos -1 ) + titleStr.substring(pos+1,titleStr.length() );
        	daywk = true;
        }
        /*
         * get '~'/'^' position and decide where to add day of week
         */
        posF = titleStr.indexOf('^');
        posV = titleStr.indexOf('~');
        if ( daywk ) {
        	if ( posF >=0 || posV >= 0) {
        		if ( posF <= posV && posF >=0 ) {
        			daywkF = true;
        		}
        		else if ( posF < 0 ){
        			daywkV = true;
        		}
        		else if ( posV >= 0 ){
        			daywkV = true;
        		}
        		else {
        			daywkF = true;
        		}
        	}       	
        }
        /*
         * check '-' flag for valid date/time
         */
        if ( posV >= 0) {
        	String validTmStr;
        	Calendar cal = currFrameTm.getValidTime();
        	int vTm = currFrameTm.getFcstTime()/3600;
        	String tmStr = String.format("%02d%02d%02d/%02d%02dV%03d", (cal.get(Calendar.YEAR )% 100),(cal.get (Calendar.MONTH)+1),cal.get(Calendar.DAY_OF_MONTH),
        										cal.get(Calendar.HOUR_OF_DAY), cal.get(Calendar.MINUTE), vTm);
        	if ( daywkV ) {
        		validTmStr = String.format("%s %s",cal.getDisplayName(Calendar.DAY_OF_WEEK,Calendar.SHORT,Locale.ENGLISH).toUpperCase(), tmStr );
        	}
        	else {
        		validTmStr = tmStr;
        	}
        	titleStr = titleStr.substring(0, posV) + validTmStr + titleStr.substring(posV+1, titleStr.length());
        }
        posF = titleStr.indexOf('^');
        /*
         * check '^' flag for forecast date/time
         */
        if ( posF >= 0) {
        	String fscTmStr = NmapCommon.getTimeStringFromDataTime(currFrameTm, "/");
        	int vTm = currFrameTm.getFcstTime()/3600;
        	String tmStr = String.format("%sF%03d", fscTmStr, vTm);
        	if ( daywkF ) {
        		Calendar cal = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
        		cal.setTime( currFrameTm.getRefTime() );
        		fscTmStr = String.format("%s %s",cal.getDisplayName(Calendar.DAY_OF_WEEK,Calendar.SHORT,Locale.ENGLISH).toUpperCase(), tmStr );
        	}
        	else {
        		fscTmStr = tmStr;
        	}
        	titleStr = titleStr.substring(0, posF) + fscTmStr + titleStr.substring (posF+1, titleStr.length());
        }  
        /*
         * check '@' for Vertical level
         */
        if ((pos = titleStr.indexOf('@')) >=0 ){
        	titleStr = titleStr.substring(0, pos) + gridRscData.getGlevel() + " " + getVerticalLevelUnits(gridRscData.getGvcord()) + titleStr.substring (pos+1, titleStr.length());
        }
        /*
         * check '_' for Grid function
         */
        if ((pos = titleStr.indexOf('_')) >=0) {
        	titleStr = titleStr.substring(0, pos) + gridRscData.getGdpfun().toUpperCase() + titleStr.substring (pos+1, titleStr.length());
        }
        /*
         * check '$' for Nonzero scaling factor
         */
        if ((pos = titleStr.indexOf('$')) >=0) {
        	if ( gridRscData.getScale().compareTo("0") != 0 ) {
        		titleStr = titleStr.substring(0, pos) + "(*10**" + gridRscData.getScale() +")" + titleStr.substring (pos+1, titleStr.length());
        	}
        	else {
        		titleStr = titleStr.substring(0, pos-1) + titleStr.substring (pos+1, titleStr.length());
        	}
        }
        /*
         * check '#' for Grid point location
         */
        if ((pos = titleStr.indexOf('#')) >=0) {
        	titleStr = titleStr.substring(0, pos-1) + titleStr.substring (pos+1, titleStr.length());
        }
        
    	return titleStr;
    }
    /*
     * Base on GVCORD to get Vertical level Units 
     */
    private String getVerticalLevelUnits (String gVCord ){

    	String tmp = gVCord.toUpperCase();
    	if (tmp.compareTo("PRES") == 0 ){
    		return "MB";
    	} if ( tmp.compareTo ("THTA") == 0) {
    		return "K ";
    	} if ( tmp.compareTo ("HGHT") == 0 ) {
    		return "M ";
    	} if ( tmp.compareTo ("SGMA") == 0 ) {
    		return "SG";
    	} if ( tmp.compareTo ("DPTH") == 0 ) {
    		return "M ";
    	}
    	return "";
    }
    
}
