package gov.noaa.nws.mdl.viz.boundaryTool.common.boundary;

import gov.noaa.nws.mdl.viz.boundaryTool.ui.dialog.BoundaryEditorDialog;

import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * 
 * @author Mamoudou ba
 * @version 1.0
 * 
 *          April 2011: Substantially modified from A2 "StormTrackState" class
 */

public class BoundaryState {

    public enum Mode {
        DRAG_ME, TRACK, NONE
    };

    public enum UserAction {
        SAVE("save me"), INSERT_BOUNDARY("line"), EDIT_BOUNDARY(" edit me"), DELETE_BOUNDARY(
                "delete me"), READ_ACTIVE_BOUNDARIES("read me"), CANCEL_MODIFICATION(
                "cancel modif"), NONE("no action");

        public String dragWhat;

        private UserAction(String text) {
            this.dragWhat = text;
        }
    }

    public enum LabelMode {
        TIME, SPEED;
    }

    public static class BoundaryPolyLine {
        public LineString polyline;

        public DataTime time;

        public BoundaryPolyLine(LineString polyline, DataTime time) {
            this.polyline = polyline;
            this.time = time;
        }
    }

    /** Drawing mode, TRACK or DRAG */
    public Mode mode;

    /**
     * User's actions: Insert, Edit, Save, Delete a boundary, or Select a
     * boundary type ------------- and Read from XML when the tool opens
     */
    public UserAction userAction;

    /**
     * How to draw the labels (SPEED or TIME)
     */
    public LabelMode labelMode;

    public boolean editable = true;

    // public boolean isMoving = false;

    public Map<Integer, Boolean> isMovingMap = new HashMap<Integer, Boolean>();

    public boolean isEditable() {
        return editable;
    }

    /** the array of time/loc coordinates for the frames */

    public BoundaryPolyLine[] timePoints;

    public Map<Integer, BoundaryPolyLine[]> timePointsMap = new HashMap<Integer, BoundaryPolyLine[]>();

    /**
     * The future points, will include timePoints[timePoints.length-1] as
     * element 0
     */
    public BoundaryPolyLine[] futurePoints;

    public Map<Integer, BoundaryPolyLine[]> futurePointsMap = new HashMap<Integer, BoundaryPolyLine[]>();

    /** The Points for the drag me points */
    public Point[] dragMePoint;

    public Point newLineCenter;

    /** Drag me line */
    public LineString dragMeLine;

    public LineString editedLineForMotionComputation;

    public int displayedIndexAtStartMotionCompute;

    public int frameAtCreationTime = 0;

    public int motionIndex;

    public boolean dragingLineNotAllowed = false;

    public boolean lineIsMoving = false;

    public boolean movingEdited = false;

    public boolean loopingWasOn = false;

    public boolean motionIsResetToStationary = false;

    public LineString prevBoundary;

    /** Map to store active boundaries */
    public Map<Integer, LineString> boundariesMap = new HashMap<Integer, LineString>();

    // public Map<String, String> boundaryIdsMap = new HashMap<String,
    // String>();

    /** Drag me Line for the points for the drag me points */
    public Map<Integer, LineString> dragMePointMap = new HashMap<Integer, LineString>();

    /** Map to store the list of forbidden ids (deleted boundaries */
    public Map<Integer, Integer> forbiddenBoundaryIdsMap = new HashMap<Integer, Integer>();

    /** Drag me line creation/modification time */
    // public DataTime createTime;
    public Map<Integer, DataTime> createTimeMap = new HashMap<Integer, DataTime>();

    public Map<Integer, DataTime> editedTimeMap = new HashMap<Integer, DataTime>();

    /** Drag me line expiration time */
    // public DataTime createTime;
    public Map<Integer, DataTime> expirationTimeMap = new HashMap<Integer, DataTime>();

    public Map<Integer, String> logMap = new HashMap<Integer, String>();

    /** Geometry object when mouse is down */
    public Geometry mouseDownGeom;

    /** current geometry object */
    public Geometry dragMeGeom;

    // parameters passed to method saving the data
    public int timeIndex = 0;

    public DataTime[] currentDataTimes = null;

    public BoundaryEditorDialog dialogObject = null;

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

    public float[] vertexAngle = null;

    public Map<Integer, float[]> vertexAngleMap = new HashMap<Integer, float[]>();

    /** The speed of the line */
    public double speed = Double.NaN;

    public float[] vertexSpeed = null;

    public Map<Integer, float[]> vertexSpeedMap = new HashMap<Integer, float[]>();

    /** "Storm" or "feature" or whatever you want people to drag the point to */
    public String thingToDragTo;

    public int editedBoundaryId;

    public String boundaryType;

    public Map<Integer, String> boundaryTypeMap = new HashMap<Integer, String>();

    public int boundaryId;

    public String fileName;

    /**
     * size of line of storms, not sure about units. Used when poly line is
     * first constructed
     */
    public double lineOfStormsLength = 30000;

    /**
     * Some value calculated in BoundaryProperties based on some things, used
     * with lineOfStormsLength
     */
    public double distanceThreshold;

    /** Set true if the geometry has changed and needs to be redrawn */
    public boolean geomChanged = true;

    /** set true if the number of vertex in the line increases/decreases */

    public boolean numberPointChanged = false;

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

    public boolean trackVisible = true;

    public boolean lineMoved = false;

    /** set magnification to default value */
    public float magnification = 1.0f;

    public Map<Integer, Boolean> existingBoundaryNotEmptyMap = new HashMap<Integer, Boolean>();

    public DataTime creationFileTime = null;

    public Map<Integer, Boolean> lineMovedMap = new HashMap<Integer, Boolean>();

    public int boundaryDuration = 8;

    public Map<Integer, Integer> boundaryDurationMap = new HashMap<Integer, Integer>();

    /** Set if you the duration needs to be calculated from the end time */
    public Calendar endTime = null;

}
