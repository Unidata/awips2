package gov.noaa.nws.ncep.metparameters.util;

import java.util.Calendar;
import java.util.Date;

public class SelectedFrameTimeUtil {
	private static Calendar calendar = Calendar.getInstance(); 
	
	private static int frameTimeMonthValue; 
	private static int frameTimeDayOfMonthValue; 
	
	public static void setSelectedFrameTime(Date frameDate) {
		if(frameDate != null) {
			calendar.setTime(frameDate); 
			frameTimeMonthValue = calendar.get(Calendar.MONTH) + 1; 
			frameTimeDayOfMonthValue = calendar.get(Calendar.DAY_OF_MONTH); 
//			displayDateInfo(calendar); 
		}
	}

	public static int getFrameTimeMonthIntValue() {
		return frameTimeMonthValue;
	}

	public static String getFrameTimeMonthStringValue() {
		return String.valueOf(frameTimeMonthValue);
	}

	public static int getFrameTimeDayOfMonthIntValue() {
		return frameTimeDayOfMonthValue;
	}

	public static String getFrameTimeDayOfMonthStringValue() {
		return String.valueOf(frameTimeDayOfMonthValue);
	}

	private static void displayDateInfo(Calendar cal) {
		System.out.println("===after being set in Class SelectedFrameTimeUtil, month=" 
				+ cal.get(Calendar.MONTH)); 
		System.out.println("===after being set in Class SelectedFrameTimeUtil, dayOfMonth=" 
				+ cal.get(Calendar.DAY_OF_MONTH)); 
	}
}
