/*
 * Created on Sep 26, 2003
 *
 * 
 */
package ohd.hseb.util.gui.drawing;


import java.awt.*;
import java.text.*;
import java.util.*;

/**
 * @author GobsC
 *
 * 
 */
public class CanvasHelper
{

	public static Stroke createDashedStroke()
	{		
		float[] fArray = {4.0f, 4.0f};
		return new BasicStroke(1.0f, BasicStroke.CAP_ROUND,
									 BasicStroke.JOIN_ROUND, 5.0f, fArray, 0.0f);
	}

//		--------------------------------------------------------------------------	
	public static void drawDashedLine(Graphics g, int x1, int y1, int x2, int y2)
	{
		Graphics2D g2 = (Graphics2D) g;
		Stroke origStroke = g2.getStroke();
		Stroke stroke = createDashedStroke(); 
			
		g2.setStroke(stroke);
		g2.drawLine(x1, y1, x2, y2); 
		g2.setStroke(origStroke);

		return;
	} //end drawDashedLine
	
//	--------------------------------------------------------------------------	
	
	public static String getDateString(long timeInMillis)
	{
		SimpleDateFormat sdf = new SimpleDateFormat("MM/dd");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		String timeString = sdf.format(new Date(timeInMillis));
		
		return timeString;   
	}
//	 --------------------------------------------------------------------------

	public static String getHourString(long timeInMillis)
	{
		SimpleDateFormat sdf = new SimpleDateFormat("HH");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		String timeString = sdf.format(new Date(timeInMillis));
		
		return timeString;   
	}
//	--------------------------------------------------------------------------
	public static String getDateTimeStringFromLongTime(long time)
	{
	    String timeString  = null;
	
		//System.out.println("timeString = !" + timeString + "!");
		SimpleDateFormat utcSdf2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
		timeString = utcSdf2.format(new java.util.Date(time));
	
		return timeString;
	}

//	--------------------------------------------------------------------------

} //end CanvasHelper
