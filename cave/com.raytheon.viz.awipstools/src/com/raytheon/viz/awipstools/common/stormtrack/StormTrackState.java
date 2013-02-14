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
package com.raytheon.viz.awipstools.common.stormtrack;

import java.util.Calendar;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07-14-2010   #6558      bkowal      Added a variable that will be used
 *                                     to indicate when the user manually
 *                                     moves the drag-me point. A new method
 *                                     has been created to calculate the pivot
 *                                     indexes.
 * 10-27-2010   #6964      bkowal      Added a public class member for the LineStyle.
 * 11/29/2012   15571      Qinglu Lin  Added compuateCurrentStormCenter();
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class StormTrackState {

    public enum Mode {
        DRAG_ME, TRACK, NONE
    };

    public enum DisplayType {
        POINT("me"), POLY("line"), CIRCULAR("me");

        public String dragWhat;

        private DisplayType(String text) {
            this.dragWhat = text;
        }
    }

    public enum LabelMode {
        TIME, SPEED;
    }

    public static class StormCoord {
        public Coordinate coord;

        public DataTime time;

        public StormCoord(Coordinate coord, DataTime time) {
            this.coord = coord;
            this.time = time;
        }
    }

    /** Drawing mode, TRACK or DRAG */
    public Mode mode;

    /**
     * Display type, CIRCLE, POINT, or POLY
     */
    public DisplayType displayType;

    /**
     * How to draw the labels (SPEED or TIME)
     */
    public LabelMode labelMode;

    public boolean editable = true;

    public boolean resetAnchor = false;

    public boolean originalTrack = true;

    public boolean isEditable() {
        return editable;
    }

    /** the array of time/loc coordinates for the frames */
    public StormCoord[] timePoints;

    /**
     * The future points, will include timePoints[timePoints.length-1] as
     * element 0
     */
    public StormCoord[] futurePoints;

    /** The Point for the drag me point */
    public Point dragMePoint;

    /** Drag me line */
    public LineString dragMeLine;

    /** Geometry object when mouse is down */
    public Geometry mouseDownGeom;

    /** current geometry object */
    public Geometry dragMeGeom;

    /** Number of drag points for line to start with. when line is created */
    public int numDragMePoints;

    /** index into timePoints where pivot point should be */
    public int pivotIndex;

    /** The pivot index to use when current frame index is on pivotIndex */
    public int otherPivotIndex;

    /** The next pivot index to use */
    public int nextPivotIndex = -1;

    /** The currently displayed pivot index (pivotIndex or otherPivotIndex) */
    public int displayedPivotIndex;

    /** The angle of the line */
    public double angle = Double.NaN;

    /** The speed of the line */
    public double speed = Double.NaN;

    /** "Storm" or "feature" or whatever you want people to drag the point to */
    public String thingToDragTo;

    /**
     * size of line of storms, not sure about units. Used when poly line is
     * first constructed
     */
    public double lineOfStormsLength = 30000;

    /**
     * Some value calculated in StormTrackProperties based on some things, used
     * with lineOfStormsLength
     */
    public double distanceThreshold;

    /** Set true if the geometry has changed and needs to be redrawn */
    public boolean geomChanged = true;

    // Resource capabilities
    /** The width of the line to draw, resource should set before passed on */
    public float lineWidth;

    /** The style of the line to draw */
    public IGraphicsTarget.LineStyle lineStyle;

    /** The color of the lines to draw, resource should set */
    public RGB color = new RGB(255, 255, 255);

    /** Duration of the future time in minutes */
    public int duration = -1;

    /** Set if you want to change the duration */
    public int newDuration = -1;

    public int intialFrame;

    public boolean trackVisible = true;

    public boolean pointMoved = false;

    /** Set if you the duration needs to be calculated from the end time */
    public Calendar endTime = null;
    
    /** Compute the coordinate of the storm center at the time defined by dataTime via interpolation. */
    public boolean compuateCurrentStormCenter(Coordinate coord, DataTime dateTime) {
    	if (futurePoints == null) return false;
    	int length = futurePoints.length;
    	if (length <=1) return false;
    	DataTime[] dt = new DataTime[2];
    	dt[0] = futurePoints[0].time;
    	dt[1] = futurePoints[length-1].time;
    	Coordinate[] cs = new Coordinate[] {futurePoints[0].coord,futurePoints[length-1].coord};
    	boolean crossed180 = false;
        if (cs[0].x>0 && cs[1].x<0 || cs[0].x<0 && cs[1].x>0) {
        	crossed180 = true;
        	if (cs[0].x>0)
        		cs[0].x = -360. + cs[0].x;
        	if (cs[1].x>0)
        		cs[1].x = -360. + cs[1].x;
        }
    	StormTrackUtil trackUtil = new StormTrackUtil();
    	coord.x = cs[0].x + (cs[1].x-cs[0].x)/trackUtil.timeBetweenDataTimes(dt[1],dt[0])
    	    *trackUtil.timeBetweenDataTimes(dateTime,dt[0]);
    	coord.y = cs[0].y + (cs[1].y-cs[0].y)/trackUtil.timeBetweenDataTimes(dt[1],dt[0])
    	    *trackUtil.timeBetweenDataTimes(dateTime,dt[0]);
    	if (crossed180)
    		if (coord.x<-180.0)
    			coord.x = 360. + coord.x;
    	return true;
    }
}
