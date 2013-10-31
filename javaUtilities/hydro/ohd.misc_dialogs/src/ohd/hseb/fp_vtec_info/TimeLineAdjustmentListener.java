package ohd.hseb.fp_vtec_info;

import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.util.HashMap;
import java.util.Map;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.DataPoint;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.TimeHolder;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;

public class TimeLineAdjustmentListener extends MouseAdapter implements MouseMotionListener {

	private boolean _button1Down = false;
	private boolean _tipShownOnce = false;

	// variables related to keyboard commands
	private boolean _hasFocus = false;
	private Point _mousePosition = null;
	private TimeHolder _beginTimeHolder = null;
	private TimeHolder _endTimeHolder = null;
	private TimeHolder _riseTimeHolder = null;
	private TimeHolder _crestTimeHolder = null;
	private TimeHolder _fallTimeHolder = null;

	/*private TimeHolder[] _timeHolderArray = null;*/
	
	private Map<String, TimeHolder> _timeHolderMap = null;
	private TimeHolder _selectedTimeHolder = null;

	// List _pointList = new ArrayList();
	private TsPaintableCanvas _canvas = null;
	private FpVtecInfo _worker = null;

	/* define constants */
	protected static final int BEGIN_DRAGGED = -2;
	protected static final int END_DRAGGED = -1;
	private static long dragged_line = 0;

	// constructor
	public TimeLineAdjustmentListener(FpVtecInfo worker,
			TsPaintableCanvas canvas, TimeHolder beginTimeHolder,
			TimeHolder endTimeHolder, TimeHolder riseTimeHolder,
			TimeHolder crestTimeHolder, TimeHolder fallTimeHolder) 
	{
		_beginTimeHolder = beginTimeHolder;
		_endTimeHolder   = endTimeHolder;
		_riseTimeHolder  = riseTimeHolder;
		_crestTimeHolder = crestTimeHolder;
		_fallTimeHolder  = fallTimeHolder;
        
		
		/*_timeHolderArray = new TimeHolder[] { beginTimeHolder, endTimeHolder,
				riseTimeHolder, crestTimeHolder, fallTimeHolder };
*/
		/* create a new HashMap _timeHolderMap */
		_timeHolderMap = new HashMap<String, TimeHolder>();
		
		/* initialize the new HashMap object */
		_timeHolderMap.put("begin", beginTimeHolder);
		_timeHolderMap.put("end",   endTimeHolder);
		_timeHolderMap.put("rise",  riseTimeHolder);
		_timeHolderMap.put("crest", crestTimeHolder);
		_timeHolderMap.put("fall",  fallTimeHolder);
		
		
		_canvas = canvas;
		_worker = worker;
	}

	public void mouseDragged(MouseEvent event) {
		String header = "TimeLineAdjustmentListener.mouseDragged(): ";
		System.out.println(header + "activated ");

		if (_button1Down) {
			Point point = event.getPoint();
			// System.out.println("activated mouseDragged, button = " + button
			// );
			// adjustTimeLine(point);
		}

	}

	public void mouseMoved(MouseEvent event) {
		_mousePosition = event.getPoint();
		// do nothing
	}

	public void mousePressed(MouseEvent event) {
		String header = "TimeLineAdjustmentListener.mousePressed(): ";
		System.out.println(header);

		int button = event.getButton();
		if (button == MouseEvent.BUTTON1) {
			_button1Down = true;
			_selectedTimeHolder = findClosestTimeHolder(event.getPoint());
		} else {

		}

		return;

	}

	public void mouseReleased(MouseEvent event) {
		System.out.println("Precip adjust listener: mouseReleased()");
		int button = event.getButton();

		if (button == MouseEvent.BUTTON1) {
			_button1Down = false;

			adjustTimeLine(event.getPoint());
			_selectedTimeHolder = null;
		}

	}

	public void mouseEntered(MouseEvent event) {
		// get the focus on this canvas, so that the keyboard commands will work
		// on it
		// AnalysisWindow.this._precipCanvas.requestFocusInWindow();
		// System.out.println("mouseEntered():" + " requested focus ");
		_hasFocus = true;
	}

	public void mouseExited(MouseEvent event) {
		_hasFocus = false;
	}

	public void keyPressed(KeyEvent e) {

	}

	public void keyReleased(KeyEvent e) {

	}

	public void keyTyped(KeyEvent e) {

	}

	// ----------------------------------------------------------------------------------
	private TimeHolder findClosestTimeHolder(Point point) {
		long timeInMillis = getTimeFromPoint(point);

		long timeWindowInHours = 3;
		final long timeWindowInMillis = timeWindowInHours * TimeHelper.MILLIS_PER_HOUR;

		long timeDiffInMillis = 0;
		long bigTimeInMillis = timeWindowInMillis + 1;
		long minDiffInMillis = bigTimeInMillis;

		TimeHolder closestTimeHolder = null;
			
		for (String key : _timeHolderMap.keySet())
		{	
			TimeHolder timeHolder = _timeHolderMap.get(key);
			long timefromtimeholder = timeHolder.getTime();

			timeDiffInMillis = Math.abs(timeHolder.getTime() - timeInMillis);

			if (timeDiffInMillis < minDiffInMillis) {
				minDiffInMillis = timeDiffInMillis;
				closestTimeHolder = timeHolder;

				/* determine which line is dragged */
				if (key.equals("begin"))					
					dragged_line = BEGIN_DRAGGED;
				else if (key.equals("end"))	
					dragged_line = END_DRAGGED;
				else
					dragged_line = 0;
			}
		}

		// make sure that the closest one is within the time window
		if (minDiffInMillis > timeWindowInMillis) {
			closestTimeHolder = null;
			dragged_line = 0;

		}

		return closestTimeHolder;
	}

	// ----------------------------------------------------------------------------------

	private long getTimeFromPoint(Point point) {
		DataPoint dataPoint = _canvas.getViewport().getDataPoint(point);

		long newTime = (long) Math.floor(dataPoint.getX());

		newTime /= TimeHelper.MILLIS_PER_HALF_HOUR;
		newTime *= TimeHelper.MILLIS_PER_HALF_HOUR;

		return newTime;
	}

	private void adjustTimeLine(Point point) {
		String header = "TimeLineAdjustment.adjustTimeLine(): ";

		TimeHolder timeHolder = _selectedTimeHolder;
		System.out.println(header + "_selectedTimeHolder = "
				+ _selectedTimeHolder);
		if (timeHolder == null) {
			return;
		}

		long newTime = getTimeFromPoint(point);

		long oldTime = timeHolder.getTime();

		System.out.println(header
				+ DbTimeHelper.getDateTimeStringFromLongTime(oldTime)
				+ " new time = "
				+ DbTimeHelper.getDateTimeStringFromLongTime(newTime));

		timeHolder.setTime(newTime);

		System.out.println(header + "time line moved to "
				+ DbTimeHelper.getDateTimeStringFromLongTime(newTime));

		_worker.getFrame().setDraggedLine(dragged_line);
		_worker.getFrame().updateCurDateTimeTextField();
		_worker.getFrame().updateGraph();

	} // end adjustTimeLine

} // end TimeLineAdjustmentListener

// --------------------------------------------------------------------

