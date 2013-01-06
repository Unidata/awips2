package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;
import com.raytheon.viz.core.rsc.hdf5.MemoryBasedTileSet;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

 /**
 * Grid Resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb, 2010           		M. Li      Initial creation
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */
public class NcgridContourImageryResource extends AbstractNatlCntrsResource<NcgridResourceData, MapDescriptor> implements 
   		INatlCntrsResource {

	protected NcgridResourceData gridRscData;
	
    //protected Map<DataTime, Map<Float, MemoryBasedTileSet>> tileSet;
	

    protected int displayedIndex;


    protected SingleLevel[] levels;

    protected float displayedLevel;

    protected String parameter;

    protected MemoryBasedTileSet baseTile;

    protected boolean ready = false;

    protected String units;

    protected Geometry baseTileCoverage;

    protected IGraphicsTarget grphTarget;

    protected SingleLevel level;

    protected String levelUnits;

    protected UnitConverter conversion;

    protected int numLevels;

    private UnitConverter levelConverter;

    protected boolean needColorMapInit = true;

    protected String viewType;
    
    private GridCoverage gridCoverage;
    
    private String modelString = null;
    
    private String levelString = null;
    
    protected class FrameData extends AbstractFrameData {
    	Map<Float, MemoryBasedTileSet> tileSet;
    	
    	protected FrameData(DataTime time, int interval) {
    		super(time, interval);	
    	}
    	
		public boolean updateFrameData(IRscDataObject rscDataObj ) {
        	PluginDataObject pdo = ((DfltRecordRscDataObj)rscDataObj).getPDO();
			GribRecord record =(GribRecord)pdo;
			synchronized (this) {
				try {
					Geometry curGeom;
					MemoryBasedTileSet mbts = null;

					modelString = record.getModelInfo().getModelName();
					levelString = record.getModelInfo().getLevelName();
					gridCoverage = record.getModelInfo().getLocation();
					
					curGeom = record.getSpatialObject().getGeometry();

					File file = HDF5Util.findHDF5Location(record);

					Set<SingleLevel> lvlSet = new HashSet<SingleLevel>();
					if (levels != null) {
						for (SingleLevel f : levels) {
							lvlSet.add(f);
						}
					}

					if (tileSet == null) {
						tileSet = new HashMap<Float, MemoryBasedTileSet>();
					}
					
					levelConverter = UnitConverter.IDENTITY;
					
					if (baseTileCoverage == null) {
						baseTileCoverage = curGeom;

						parameter = record.getModelInfo().getParameterName();
						units = record.getModelInfo().getParameterUnit();
						String model = "" + record.getModelInfo().getCenterid();
						String process = "" + record.getModelInfo().getGenprocess();
						levelUnits = record.getModelInfo().getLevelUnit();

						Unit<?> gridUnits = record.getModelInfo().getParameterUnitObject();
						Unit<?> flevelUnits = Unit.valueOf(levelUnits);
						//levelConverter = UnitConverter.IDENTITY;
						if (flevelUnits.isCompatible(SI.MILLI(NonSI.BAR))) {
							levelUnits = "MB";
							levelConverter = flevelUnits.getConverterTo(SI.MILLI(NonSI.BAR));
						}

						float convertedLevel = (float) levelConverter.convert(record
								.getModelInfo().getLevelOneValue());

						level = GridLevelTranslator.construct(record.getModelInfo()
								.getLevelName());
						if (level == null) {
							try {
								throw new VizException("Unhandled layer type: "
										+ record.getModelInfo().getLevelName());
							} catch (VizException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
						}

						level.setValue(convertedLevel);

						ColorMapParameters parameters = null;

						if (record.getMessageData() != null
								/*&& record.getMessageData() instanceof DerivedParameterRequest*/) {
								parameters = ColorMapParameterFactory.build(record, parameter,
										gridUnits, level, model + "" + process);
						} else {
								parameters = ColorMapParameterFactory.build(file, record
										.getDataURI()
										+ "/Data", parameter, gridUnits, level, model + ""
										+ process);
						}
						// Pull the parameters out and use them
						if (parameters.getDisplayUnit() != null) {

							if (gridUnits.isCompatible(parameters.getDisplayUnit())) {

								conversion = gridUnits.getConverterTo(parameters
										.getDisplayUnit());
							} else {
								conversion = UnitConverter.IDENTITY;
								String message = "Data unit: " + gridUnits
								+ " cannot be converted to desired unit: "
								+ parameters.getDisplayUnit()
								+ " Data displayed will be displayed with unit: "
								+ gridUnits;
                                UFStatus.getHandler().handle(Priority.VERBOSE,
                                        message);
							}
							units = parameters.getDisplayUnit().toString();
						}
						getCapability(ColorMapCapability.class).setColorMapParameters(
								parameters);

						mbts = baseTile = createTileSet(record, file.getName(), lvlSet,
								levelConverter);

					} else if (!curGeom.equalsExact(baseTileCoverage)) {
						mbts = new MemoryBasedTileSet(file.getName(), record.getDataURI(),
								"Data", baseTile, conversion, record);

						DataTime dataTime = record.getDataTime();
						System.out.println("&&&& DT: " + dataTime);

						
						
						float convertedLevel = (float) levelConverter.convert(record
								.getModelInfo().getLevelOneValue());
						SingleLevel l = GridLevelTranslator.construct(record.getModelInfo()
								.getLevelName());
						l.setValue(convertedLevel);
						lvlSet.add(l);
						
					} else {
						mbts = createTileSet(record, file.getName(), lvlSet, levelConverter);
					}
					mbts.setMapDescriptor(descriptor);
					//		        Set<DataTime> dateSet = tileSet.keySet();
					//		        dataTimes.clear();
					//		        Iterator<DataTime> dateIterator = dateSet.iterator();
					//		        while (dateIterator.hasNext()) {
					//		            dataTimes.add(dateIterator.next());
					//		        }
					//

					float convertedLevel = (float) levelConverter.convert(record
							.getModelInfo().getLevelOneValue());
					
					if (mbts != null) {
						tileSet.put(convertedLevel, mbts);
						
						if ( grphTarget != null ) {
							//mbts.init(grphTarget);
							System.out.println(">>>> init tileSet ............");
						    tileSet.get(convertedLevel).init(grphTarget);
						}    
					}
					
					if (tileSet != null) {
						System.out.println("tileSet !=nullllllllll size = "+tileSet.size());
					}
					Collections.sort(NcgridContourImageryResource.this.dataTimes);
					//Collections.sort(frameTimes);

					levels = new SingleLevel[lvlSet.size()];
					Iterator<SingleLevel> lvlIterator = lvlSet.iterator();
					for (int i = 0; i < levels.length; i++) {
						levels[i] = lvlIterator.next();
					}

					Arrays.sort(levels);
					displayedIndex = 0;
				}
				catch( VizException e ) {
					System.out.println("Error processing McidasRecord. " + e.getMessage() );
					return false;
				}
			
			
				return true;
			}
		}

	}

    /**
     * Constructor
     */
    public NcgridContourImageryResource(NcgridResourceData data, LoadProperties props) {
        super(data, props);
//        data.addChangeListener((IResourceDataChanged) this);
        gridRscData = data;
        numLevels = 1;
        grphTarget = null;
        //tileSet = new HashMap<DataTime, Map<Float, MemoryBasedTileSet>>();
//        dataTimes = new ArrayList<DataTime>();

//        try {
//            GribRecord[] records = resourceData.getRecords();
//            for (int i = 0; i < records.length; i++) {
//                addRecord(records[i]);
//            }
//        } catch (VizException e) {
//            UFStatus.handle(Priority.SIGNIFICANT, Activator.PLUGIN_ID,
//                    StatusConstants.CATEGORY_WORKSTATION, null,
//                    "Unable to load records", e);
//        }
    }



    private MemoryBasedTileSet createTileSet(GribRecord record,
            String fileName, Set<SingleLevel> lvlSet,
            UnitConverter levelConverter) throws VizException {
        MemoryBasedTileSet mts = createTile(record, fileName);

        Map<Float, MemoryBasedTileSet> tilemap = new HashMap<Float, MemoryBasedTileSet>();

        DataTime dataTime = record.getDataTime();
        float convertedLevel = (float) levelConverter.convert(record
                .getModelInfo().getLevelOneValue());

        SingleLevel l = GridLevelTranslator.construct(record.getModelInfo()
                .getLevelName());
        l.setValue(convertedLevel);
        lvlSet.add(l);
        if (levelUnits.equals("MB")) {
            mts.setElevation(Controller.ptozsa(convertedLevel));
            l.setType(Level.LevelType.PRESSURE);
        } else {
            mts.setElevation(convertedLevel);
        }
        tilemap.put(convertedLevel, mts);
        
        //tileSet.put(dataTime, tilemap);
        return mts;
    }

    private MemoryBasedTileSet createTile(GribRecord record, String fileName)
            throws VizException {
        GridGeometry2D gridGeometry2D = MapUtil.getGridGeometry(record
                .getSpatialObject());
        return new MemoryBasedTileSet(fileName, record.getDataURI(), "Data",
                numLevels, 256, gridGeometry2D, this, conversion,
                PixelInCell.CELL_CORNER, record, viewType);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
//        if (displayedIndex >= frameTimes.size() || displayedIndex < 0) {
//            return "No Data Available";
//        }

        String parameterString = parameter;
//        String modelString = gridRscData.records[0].getModelInfo()
//                .getModelName();
        String unitsString = units;
//        String levelStr = level.getMeasure().toString().trim();
        String legendString = getCurrentFrameTime().getLegendString();

//        if (levelStr.equals("2.0 m") || levelStr.equals("0.0")
//                || levelStr.equals("0.0 m")) {
//            levelStr = this.levelString;
//        }

        return String.format("%s %s %s Img (%s)  %s", modelString, levelString,
                parameterString, unitsString, legendString);
    }

    @Override
	public void disposeInternal() {
//        for (Map.Entry<DataTime, Map<Float, MemoryBasedTileSet>> set : tileSet
//                .entrySet()) {
//            for (Map.Entry<Float, MemoryBasedTileSet> tile : set.getValue()
//                    .entrySet()) {
//                tile.getValue().dispose();
//            }
//        }
    	
    	for( AbstractFrameData frm : frameDataMap.values() ) {
			Map<Float, MemoryBasedTileSet> tile = ((FrameData)frm).tileSet; 
			for (MemoryBasedTileSet ts : tile.values()) {

				if( ts != null )
					ts.dispose();
			}
		}
    }

	public void initResource(IGraphicsTarget target) throws VizException {
		synchronized (this) {
			
			this.grphTarget = target;
			viewType = target.getViewType();

			queryRecords();
			
			if (baseTile != null) {
				baseTile.setMapDescriptor(descriptor);
				baseTile.init(target);
			}

//			for (Map.Entry<DataTime, Map<Float, MemoryBasedTileSet>> set : tileSet
//					.entrySet()) {
//				for (Map.Entry<Float, MemoryBasedTileSet> tile : set.getValue()
//						.entrySet()) {
//					if (tile.getValue().equals(baseTile)) {
//						continue;
//					}
//
//
//					tile.getValue().init(target);
//				}
//			}

//			if (frameDataMap != null && frameDataMap.size() > 0) {
//				for (String t : frameDataMap.keySet()) {
//					System.out.println("key="+t);
//				}
//			}
			
			for( AbstractFrameData frm : frameDataMap.values() ) {
				if (((FrameData)frm).tileSet != null )
					System.out.println("((FrameData)frm).tileSet != null.............");
					
				if (((FrameData)frm).tileSet != null && ((FrameData)frm).tileSet.size() > 0) {
					for (float f : ((FrameData)frm).tileSet.keySet()) {
						System.out.println("key for level float==="+ f);
					}
					
				}
//				Map<Float, MemoryBasedTileSet> tile = ((FrameData)frm).tileSet; 
//				for (MemoryBasedTileSet ts : tile.values()) {
//					for (MemoryBasedTileSet ts : ((FrameData)frm).tileSet.values()) {
//
//					if ( ts == baseTile) continue;
//					
//					if( ts != null )
//						ts.init(target);
//				}
			}
		}

    }

    public void paintFrame(AbstractFrameData frmData,
			IGraphicsTarget target, PaintProperties paintProps)
			throws VizException {
       // this.grphTarget = target;
    	
    	
//        setDisplayedDate(paintProps.getDataTime());
        
//        System.out.println(">>>> paintFrame<< ... paintProps.getDataTime() ="+paintProps.getDataTime()+
//        		"displayedIndex == "+displayedIndex);
//        
//        if (frameTimes != null) {
//        	System.out.println(" >>>>> frameTimes size="+frameTimes.size());
//        }
        
        if (displayedIndex < 0) {
            return;
        }

        
        
        ColorMapParameters parameters = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        if (needColorMapInit && parameters != null) {
            needColorMapInit = false;
            String colorMapName = parameters.getColorMapName();
            
            if (colorMapName == null) {
                colorMapName = "Grid/gridded data";
                parameters.setColorMap(target.buildColorMap(colorMapName));
            }
        }    
        
        Map<Float, MemoryBasedTileSet> tileGroup = ((FrameData)frmData).tileSet; 
        if (tileGroup == null) {
//        	System.out.println("<<<>>>>>paintFrame tileGroup ==== null");
        	
            return;
        }

//        System.out.println("<<<>>>>>paintFrame.....222222222222222222222");
        
        MemoryBasedTileSet tile = tileGroup.get(displayedLevel);
        if (tile == null) {
            // TODO cleanup
            tile = tileGroup.values().iterator().next();
        }
        if (tile == null) {
            return;
        }
        
        tile.init(target);
        
        System.out.println("<<<>>>>>paintFrame.....222222222222222222222");
        
        target.setUseBuiltinColorbar(true);
        tile.paint(target, paintProps);
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        for( AbstractFrameData frm : frameDataMap.values() ) {
			Map<Float, MemoryBasedTileSet> tile = ((FrameData)frm).tileSet; 
			for (MemoryBasedTileSet ts : tile.values()) {
				if( ts != null )
					ts.reproject();
			}
		}
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {        
        Map<Float, MemoryBasedTileSet> map = ((FrameData)getCurrentFrame()).tileSet;
        
        if (map == null) {
            return "No Data";
        }

        MemoryBasedTileSet tile = map.get(displayedLevel);
        if (tile == null) {
            // TODO cleanup
            tile = map.values().iterator().next();
        }

        if (tile == null) {
            return "No Data";
        }

        Coordinate latLon;
        try {
            latLon = coord.asLatLon();
        } catch (Exception e) {
            throw new VizException("Error transforming coordinate to lat/lon",
                    e);
        }

        if (latLon.x < 0.0) {
//            GridCoverage gc = gridRscData.getRecords()[0]
//                    .getModelInfo().getLocation();
        	
            if (gridCoverage instanceof LatLonGridCoverage) {
                LatLonGridCoverage llgc = (LatLonGridCoverage) gridCoverage;
                if (llgc.getLo2() > 180.0) {
                    latLon.x += 360.0;
                }
            }
        }
        return "" + ((int) (tile.interrogate(latLon, false) * 100) / 100.0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IVertSeqResource#getVerticalLevels
     * ()
     */
    public SingleLevel[] getVerticalLevels() {
        return levels;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IVertSeqResource#getVerticalLevelType
     * ()
     */
    public Level.LevelType getVerticalLevelType() {
        return level.getType();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IVertSeqResource#setVerticalLevel
     * (float)
     */
    public void setVerticalLevel(SingleLevel level) {
        displayedLevel = (float) level.getValue();
    }

//    @SuppressWarnings("unchecked")
//	@Override
//    public void remove(DataTime dataTime) {
//    	Map<Float, MemoryBasedTileSet> ts = ((FrameData)this.frameDataMap.get(dataTime)).tileSet;
//        if (ts == null) {
//            return;
//        }
//
//        for (Map.Entry<Float, MemoryBasedTileSet> tile : ts.entrySet()) {
//            tile.getValue().dispose();
//        }
//
//        Set<DataTime> dateSet = (Set<DataTime>) frameTimes;
//    	NcgridContourImageryResource.this.dataTimes.clear();
////    	frameTimes.clear();
//        Iterator<DataTime> dateIterator = dateSet.iterator();
//        while (dateIterator.hasNext()) {
//            if (dateIterator.next() != dataTime ) {
//            	dataTimes.add(dateIterator.next());
//            	frameTimes.add(dateIterator.next());
//            }	
//        }
//
//        Collections.sort(NcgridContourImageryResource.this.dataTimes);
//    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IImpactResource#getParameter()
     */
//    @Override
//    public String getParameter() {
//        return parameter;
//    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IImpactResource#getTile(int[])
     */
//    @Override
//    public ImageTile getTile(int[] id) {
////        Map<Float, MemoryBasedTileSet> tileGroup = tileSet.get(GridResource.this.dataTimes
////                .get(displayedIndex));
////        MemoryBasedTileSet tile = tileGroup.get(displayedLevel);
////        return tile.getTile(id);
//    	
//    	return null;
//    }

//    public void resourceChanged(ChangeType type, Object object) {
//        if (type.equals(ChangeType.DATA_UPDATE)) {
//            PluginDataObject[] pdos = (PluginDataObject[]) object;
//            for (PluginDataObject pdo : pdos) {
//                MemoryBasedTileSet mbts;
//                try {
//                    mbts = addRecord((GribRecord) pdo);
//                    if (mbts != null) {
//                        mbts.setMapDescriptor(descriptor);
//                        mbts.init(target);
//                    }
//                } catch (VizException e) {
//                    UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
//                            StatusConstants.CATEGORY_WORKSTATION, "grid",
//                            "Error updating grid resource", e);
//                }
//            }
//        }
//        issueRefresh();
//    }

    @Override
	protected AbstractFrameData createNewFrame( DataTime frameTime, int frameInterval) {
		return new FrameData( frameTime, frameInterval );
	}


}
