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
package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc.NcgridResourceData;

import java.util.Map.Entry;
import java.util.Queue;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
//import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * An abstract resource for displays where each grid cell is an individual
 * IImage. Handles progressive disclosure algorithm.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2010            bsteffen     Initial creation
 * Nov 22, 2010			   M. Li		modified from RTS for NCGRID
 * Dec 03, 2010			   M. Li		Converted negative longitude
 * Dec 07, 2010			   M. Li		Modified wind plot algorithm
 * Nov 02, 2011            X. Guo       Added nx/ny parameters
 * Feb 06, 2012  #538      Q. Zhou      Changed density to filter. Get filter from resource attribute
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractGriddedDisplay<T>  { //implements IRenderable

    private static final IUFStatusHandler statusHandler = UFStatus.getHandler(AbstractGriddedDisplay.class); 
    
    private final Queue<Coordinate> calculationQueue;

    private CalculationJob calculationJob;

    protected final IMapDescriptor descriptor;

    protected final GeneralGridGeometry gridGeometryOfGrid;

    protected final int[] gridDims;

    protected IGraphicsTarget target;

    protected float size = 64;

    protected RGB color;

    protected int skipx;
    protected int skipy;
    protected double filter;

    protected double magnification = 1.0;

    private boolean async = true;
    
    protected boolean[] isPlotted;

    /**
     * 
     * @param descriptor
     * @param gridGeometryOfGrid
     * @param size
     */
    public AbstractGriddedDisplay(IMapDescriptor descriptor,
            GeneralGridGeometry gridGeometryOfGrid,int nx, int ny) {

        this.calculationQueue = new ConcurrentLinkedQueue<Coordinate>();

        this.descriptor = descriptor;
        this.gridGeometryOfGrid = gridGeometryOfGrid;
        
//        this.size = size;

        this.gridDims = new int[] {
                nx,
                ny };
        
        isPlotted = new boolean[gridDims[0] * gridDims[1]];
        
        
    }

    public void setASync(boolean async) {
        this.async = async;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
   // @Override
    public void paint(NcgridResourceData gridRscData, IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	
    	boolean globalModel = isGlobalModel();    	
    	
    	/**
         *  Get filter attribute
         */
    	String den = gridRscData.getFilter(); 
    	String noFilter = "";
    	if (den != null ){
    		try {
    			if (den.equalsIgnoreCase("YES") || den.equalsIgnoreCase("Y")) {
    				filter = 1.0;
    			}
    			else if (den.equalsIgnoreCase("NO") || den.equalsIgnoreCase("N") || den.equalsIgnoreCase("")) {
    				filter = 0.0;
    				noFilter = "NO";
    			}
    			else {
    				filter = Double.parseDouble(den);  
    			}
    			
    			if (filter == 0)
    				noFilter = "NO";
    			if (filter <0.1)
    				filter = 0.1;
    		}
    		catch (NumberFormatException e) {
    			System.out.println("The filter is not a double number");
    			filter = 1.0;
    		}
    	}
    	else {
    		filter = 1.0;
    	}
    	
//        /**
//         *  Get skip attribute
//         */
//        
//    	String[] skip = null;
//    	int skipx = 0;
//    	int skipy = 0;
//    	
//    	String skipString = gridRscData.getSkip(); //now for positive skip
//    	if (skipString != null && noFilter.equalsIgnoreCase("NO")) {
//    		int ind = skipString.indexOf("/");
//    		if (ind != -1) {
//    			skipString = skipString.substring(ind +1);
//    			
//    			if (skipString.trim().startsWith("-"))  //temp fix for negative value
//    				skipString = skipString.substring(1);
//    			
//    			skip = skipString.split(";");
//    	    	
//    	    	if (skip != null && skip.length !=0){
//    	    		try {
//    	    			skipx = Integer.parseInt(skip[0]);
//    	    		}
//    	    		catch (NumberFormatException e) {
//    	    			System.out.println("The skip is not an interger");
//    	    			skipx = 0;        		
//    	    		}
//    	    			
//    	    		if (skip.length ==1 ) {
//    	    			skipy = skipx;
//    	    		}
//    	    		if (skip.length >1 && skip[0] != skip[1]) {
//    	    			try {
//    	    				skipy = Integer.parseInt(skip[1]);
//    	    			}
//    	    			catch (NumberFormatException e) {
//    	    				System.out.println("The skip is not an interger");    				
//    	    				skipy = skipx;
//    	    			}
//    	    		}
//    	    	}
//    	    	else {
//    	    		skipx = 0;
//    	    		skipy = 0;
//    	    	}
//    		}
//    		else {
//    			skipx = 0;
//    			skipy = 0;
//    		}
//    	}    	
//    	else {
//			skipx = 0;
//			skipy = 0;
//		}	
//           
    	
    	for (int i = 0; i < (gridDims[0] * gridDims[1]); i++)
        	isPlotted[i] = false;
    	
        // Controls whether to draw images or debugging output on the map
//        boolean debug = false;
        this.target = target;

        PaintProperties pp = new PaintProperties(paintProps);
        pp.setAlpha(1.0f);

        IExtent viewPixelExtent = paintProps.getView().getExtent();
        double ratio = viewPixelExtent.getWidth()
                / paintProps.getCanvasBounds().width;
                
        //double interval = size * .75 * ratio / Math.min(2.0, filter);
        double interval = size * .75 * ratio * filter;

        double adjSize = size * ratio * magnification;

        TreeMap<Double, Double> thisRow = new TreeMap<Double, Double>();
        thisRow.put(viewPixelExtent.getMinY(), viewPixelExtent.getMinX()
                - interval);
        boolean stop = false;
        // Only used for debuging.
        int icount = 0;
        int jcount = 0;
        try {
            while (!stop) {
                icount++;
                jcount = 0;
                stop = true;

                TreeMap<Double, Double> lastRow = thisRow;

                thisRow = new TreeMap<Double, Double>();

                for (double j = viewPixelExtent.getMinY(); j < viewPixelExtent
                        .getMaxY(); j += interval) {
                    // find a value of i larger than the last outer loop.
                    Entry<Double, Double> ceilingEntry = lastRow
                            .ceilingEntry(j);
                    Entry<Double, Double> floorEntry = lastRow.floorEntry(j);
                    double floorVal = floorEntry == null ? Double.MAX_VALUE
                            : floorEntry.getValue();
                    double ceilingVal = ceilingEntry == null ? Double.MAX_VALUE
                            : ceilingEntry.getValue();
                    double i = Math.min(floorVal, ceilingVal) + interval;

                    if (!viewPixelExtent.contains(new double[] { i, j })) {
                        continue;
                    } else {
                        stop = false;
                    }
                    jcount++;
                    /*
                     if (debug == true) {
                        // Draw a red labeled square over the area where
                        // we will look for grid points
                        target.drawString(null, icount + "," + jcount, i, j,
                                0.0, TextStyle.NORMAL, new RGB(255, 0, 0),
                                HorizontalAlignment.CENTER,
                                VerticalAlignment.MIDDLE, 0.0);
                        target.drawRect(new PixelExtent(i - halfInterval, i
                                + halfInterval, j - halfInterval, j
                                + halfInterval), new RGB(255, 0, 0), 1, 1);
                    }
                    */
                    // Get a grid coordinate near i, j
                    ReferencedCoordinate coordToTry = new ReferencedCoordinate(
                            this.descriptor.getGridGeometry(), new Coordinate(
                                    i, j));
                    Coordinate gridCell = coordToTry.asGridCell(
                            gridGeometryOfGrid, PixelInCell.CELL_CORNER);
                    gridCell.y = Math.round(gridCell.y);
                    gridCell.x = Math.round(gridCell.x);
                    
                    
                    /*
                     * Convert negative longitude
                     */
                    Coordinate coord = coordToTry.asLatLon();
                    double x = coord.x;
                    if (globalModel && x < 0) {
                    	x = x + 360;
                    }
                    
                    Coordinate newCoord = new Coordinate(x, coord.y);
                    ReferencedCoordinate newrco = new ReferencedCoordinate(newCoord);
                    Coordinate newGridCell = newrco.asGridCell(
                            gridGeometryOfGrid, PixelInCell.CELL_CORNER);
                    newGridCell.x = Math.round(newGridCell.x);
                    
                    /*
                     * Check for bounds
                     */
                    if ((newGridCell.x < 0 || newGridCell.x >= gridDims[0])
                            || (gridCell.y < 0 || gridCell.y >= gridDims[1])) {
                        thisRow.put(j, i);
                        continue;
                        
                    }
                    
                    ReferencedCoordinate rco = new ReferencedCoordinate(
            				new Coordinate((int)gridCell.x, (int)gridCell.y),
            				this.gridGeometryOfGrid, Type.GRID_CORNER);
            		Coordinate plotLoc = rco.asPixel(this.descriptor.getGridGeometry());
            		Coordinate gridCell2 = rco.asGridCell(
                            gridGeometryOfGrid, PixelInCell.CELL_CORNER);
                    
//                    Coordinate plotLoc = coordToTry.asPixel(this.descriptor
//                            .getGridGeometry());
                    
                    
                    /*
                    if (debug == true) {
                        // draw a blue dot where the gridpoints are found.
                        target.drawString(null, ".", plotLoc.x, plotLoc.y, 0.0,
                                TextStyle.NORMAL, new RGB(0, 0, 255),
                                HorizontalAlignment.CENTER,
                                VerticalAlignment.BOTTOM, 0.0);
                    }
                    */
                    // If the real loc of this grid coordinate is close to the
                    // loc we wanted go with it
                    if (Math.abs(plotLoc.y - j) < (interval/2)
                            && Math.abs(plotLoc.x - i) < (interval/2)) {
                        j = plotLoc.y;
                        thisRow.put(j, plotLoc.x);
                    } else {
                        thisRow.put(j, i);
                        continue;
                    }
                    /*
                    if (debug == true) {
                        // Draw a green label where the image will actually be
                        // drawn
                        target.drawString(null, icount + "," + jcount,
                                plotLoc.x, plotLoc.y, 0.0, TextStyle.NORMAL,
                                new RGB(0, 255, 0), HorizontalAlignment.CENTER,
                                VerticalAlignment.MIDDLE, 0.0);
                    }
                    */
                    
                    T oldImage = getImage(gridCell2);
                    if (oldImage != null) {
//                        if (debug == false) {
                            paintImage((int)gridCell.x, (int)gridCell.y, pp, adjSize);
//                        }
                    } else {
                        if (async) {
                            if (!this.calculationQueue.contains(gridCell2)) {
                                this.calculationQueue.add(gridCell2);
                            }
                        } else {
                            T image = createImage(gridCell2);
                            if (image != null /*&& debug == false*/) {
                                paintImage((int)gridCell.x, (int)gridCell.y, pp, adjSize);
                            }
                        }
                    }   
                }
            } //while               
        } catch (Exception e) {
            throw new VizException("Error occured during paint", e);
        }
        
        if (calculationQueue.size() > 0) {
            if (this.calculationJob == null) {
                this.calculationJob = new CalculationJob();
                this.calculationJob.schedule();
            } else if (!this.calculationJob.isRunning()) {
                this.calculationJob.schedule();
            }
        }
    }

    /**
     * Should return a cached image if it is available, if this returns null
     * createImage will be called on the same point(possible asynchronously)
     * 
     * @param coord
     * @return
     */
    protected abstract T getImage(Coordinate coord);

    /**
     * Create an image for the given coordinate.
     * 
     * @param coord
     * @return
     * @throws VizException
     */
    protected abstract T createImage(Coordinate coord) throws VizException;

    /**
     * Should dispose of all images and clear a cache. Called whenever the color
     * is changed, or when the display is disposed.
     */
    protected abstract void disposeImages();

    protected abstract void paintImage(int x, int y, PaintProperties paintProps,
            double adjustedSize) throws VizException;

    public void dispose() {
        disposeImages();
    }

    /**
     * Set the color of the images
     * 
     * @param color
     */
    public boolean setColor(RGB color) {
        if (this.color == null || !this.color.equals(color)) {
            this.color = color;
            return true;
        }
        return false;
    }

    /**
     * @param filter
     *            the filter to set.  Changed from density.
     */
    public boolean setFilter(double filter) {
        if (this.filter != filter) {
            this.filter = filter;
            return true;
        }
        return false;
    }

    
    public float getSize() {
		return size;
	}

	public void setSize(float size) {
		this.size = size;
	}

	/**
     * @param magnification
     *            the magnification to set
     */
    public boolean setMagnification(double magnification) {
        if (this.magnification != magnification) {
            this.magnification = magnification;
            return true;
        }
        return false;
    }

    
    private boolean isGlobalModel() throws VizException {
    	
    	 ReferencedCoordinate newrco0 = new ReferencedCoordinate(
 				new Coordinate(0, 0),
 				this.gridGeometryOfGrid, Type.GRID_CORNER);
    	 ReferencedCoordinate newrco1 = new ReferencedCoordinate(
  				new Coordinate(gridDims[0] - 1, 0),
  				this.gridGeometryOfGrid, Type.GRID_CORNER);
    	 ReferencedCoordinate newrco2 = new ReferencedCoordinate(
  				new Coordinate(1, 0),
  				this.gridGeometryOfGrid, Type.GRID_CORNER);

         try {
             Coordinate latLon0 = newrco0.asLatLon();
             Coordinate latLon1 = newrco1.asLatLon();
             Coordinate latLon2 = newrco2.asLatLon();
             
             double dx1 = latLon2.x - latLon0.x;
             double dx2 = (360 - latLon1.x) + latLon0.x;
             
             int dx = (int) Math.round(dx2/dx1);
             int dlat = (int) Math.round(latLon1.y - latLon0.y);

             if (dx <= 2 && dlat == 0) return true;
             
         } catch (Exception e) {
             throw new VizException(e);
         }
    	
    	return false;
    }
    /**
     * Off UI Thread job for calculating the wind images
     * 
     * @author chammack
     * @version 1.0
     */
    private class CalculationJob extends Job {

        private boolean running = false;

        public CalculationJob() {
            super("Grid Image Calculation");
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            boolean loggedError = false;
            running = true;
            while (!calculationQueue.isEmpty()) {

                Coordinate coord = calculationQueue.remove();

                try {
                    createImage(coord);
                } catch (VizException e) {
                    if (!loggedError) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Generating the grid image failed.", e);
                        loggedError = true;
                    }
                }

            }

            target.setNeedsRefresh(true);
            running = false;
            return Status.OK_STATUS;
        }

        /**
         * @return the running
         */
        public boolean isRunning() {
            return running;
        }

    }

}
