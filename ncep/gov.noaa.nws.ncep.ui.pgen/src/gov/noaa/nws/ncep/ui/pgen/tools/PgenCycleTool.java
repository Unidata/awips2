/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenExtrapTool
 * 
 * June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
//import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a modal map tool for PGEN cycle.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer		Description
 * ------------	----------	-------------	--------------------------
 * 03/10		#263		M.Laryukhin		Created
 * </pre>
 * 
 * @author M.Laryukhin
 */
public class PgenCycleTool extends AbstractPgenDrawingTool {
	
	/** Cycle day. */
	private static int cycleDay = 1;
	
	/** Cycle hour. */
	private static int cycleHour = 2;
	
	/** Routine/Update flag (true/false). */
	private static boolean isRoutine = true;
	
	/** Cycle hours available in the dropdown during the summer time. */
	private static final int[] CYCLES_SUMMER = {2, 8, 14, 20}; // must be ascending 
	
	/** Cycle hours available in the dropdown during the winter time. */
	private static final int[] CYCLES_WINTER = {3, 9, 15, 21}; // must be ascending 

	/**
	 * Input handler for mouse events.
	 */
	public PgenCycleTool() {
		
		super();

		Calendar cal = Calendar.getInstance();
		int hour = cal.get(Calendar.HOUR_OF_DAY);
		int[] CYCLES = getCyclesArray(); 
		
		for (int i = 0; i < CYCLES.length; i++) {
			if (hour < CYCLES[i]) {
				cycleHour = CYCLES[i];
				break;
			}
		}
		if(hour >= CYCLES[CYCLES.length-1]) {
			cycleHour = CYCLES[0];
			cal.add(Calendar.DAY_OF_MONTH, 1);
		}
		
		cycleDay = cal.get(Calendar.DAY_OF_MONTH);
				
		isRoutine = true;
		
		updateTitle();
	}

	/**
	 * Returns the current mouse handler.
	 * 
	 * @return
	 */
	public IInputHandler getMouseHandler() {

		if (this.mouseHandler == null) {
			this.mouseHandler = new PgenCycleHandler();
		}

		return this.mouseHandler;

	}

	/**
	 * Implements input handler for mouse events.
	 * 
	 * @author jwu
	 * 
	 */
	public class PgenCycleHandler extends InputHandlerDefaultImpl {

		/*
		 * (non-Javadoc)
		 * 
		 * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
		 * int, int)
		 */
		@Override
		public boolean handleMouseDown(int anX, int aY, int button) {
        	if ( !isResourceEditable() ) return false;

			// Check if mouse is in geographic extent
			Coordinate loc = mapEditor.translateClick(anX, aY);
			if (loc == null || shiftDown )
				return false;

			if (button == 1) {

				mapEditor.refresh();

				return true;

			} else if (button == 3) {

				PgenUtil.setSelectingMode();
				return true;
			} else {
				return false;
			}
		}

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
			if (  !isResourceEditable() || shiftDown ) return false;
			else return true;
		}
		
	}

	public static int getCycleDay() {
		return cycleDay;
	}

	public static void setCycleDay(int day) {
		PgenCycleTool.cycleDay = day;
		updateTitle();
	}

	public static int getCycleHour() {
		return cycleHour;
	}
	
	public static String getCycleHourStr() {
		return pad(cycleHour);
	}

	public static void setCycleHour(int hour) {
		PgenCycleTool.cycleHour = hour;
		updateTitle();
	}

	public static int[] getCyclesArray() {
		// check local computer time zone for day light time
		if(TimeZone.getDefault().inDaylightTime(new Date())){
			return CYCLES_SUMMER;
		}
		return CYCLES_WINTER;
	}

	public static boolean isRoutine(){
		return isRoutine;
	}
	
	public static void setCycleRoutine(boolean isRoutine) {
		PgenCycleTool.isRoutine = isRoutine;
		updateTitle();
	}

	/**
	 * Pads 0, 1, ... or 9 with a leading zero and returns as a string. For example, 7 will become 07,
	 * 11 will remain 11, 123 will remain 123...
	 * 
	 * @param in
	 * @return
	 */
	public static String pad(int in){
		return (in >= 0 &&  in < 10) ? "0" + in: "" + in;
	}

	public static void updateTitle(){
		String title = "Day/Cycle:  " + pad(cycleDay) + "/" + pad(cycleHour) + "Z ";
		title += isRoutine? "Routine": "Update";
		PgenUtil.setCaveTitle(title);
	}

}
