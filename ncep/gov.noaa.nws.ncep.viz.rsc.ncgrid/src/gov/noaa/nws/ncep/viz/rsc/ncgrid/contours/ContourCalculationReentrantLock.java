package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import java.util.concurrent.locks.*;
/**
 * @author xguo
 *
 */
public class ContourCalculationReentrantLock {
	public static ReentrantLock contourLock = new ReentrantLock ();

	public static void getReentrantLock () {
		contourLock.lock();
	}
	
	public static void releaseReentrantLock () {
		contourLock.unlock();
	}
}
