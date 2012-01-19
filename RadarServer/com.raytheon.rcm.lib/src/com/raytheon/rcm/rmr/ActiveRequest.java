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
package com.raytheon.rcm.rmr;

import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;

public class ActiveRequest implements Delayed {
	private MultipleRequest multipleRequest;
	private long nextTime;
	private int remainingTime;
		
	public ActiveRequest() {
		nextTime = Long.MAX_VALUE;
	}
	
	public ActiveRequest(MultipleRequest mr) {
		this.multipleRequest = mr;
		this.nextTime = System.currentTimeMillis() + 
			mr.getInterval() * 1000;
		this.remainingTime = mr.getDuration();
	}
	public MultipleRequest getMultipleRequest() {
		return multipleRequest;
	}
	/* This method supports serialization.  It should not be called from normal
	 * code because the effects on nextTime are not defined.
	 */ 
	public void setMultipleRequest(MultipleRequest multipleRequest) {
		this.multipleRequest = multipleRequest;
	}
	public long getNextTime() {
		return nextTime;
	}
	public void setNextTime(long nextTime) {
		this.nextTime = nextTime;
	}
	public int getRemainingTime() {
		return remainingTime;
	}
	public void setRemainingTime(int remainingTime) {
		this.remainingTime = remainingTime;
	}
	@Override
	public long getDelay(TimeUnit unit) {
		long delay = unit.convert(nextTime - System.currentTimeMillis(),
				TimeUnit.MILLISECONDS);
		return delay;
	}
	@Override
	public int compareTo(Delayed o) {
		return (int)(getDelay(TimeUnit.SECONDS) - o.getDelay(TimeUnit.SECONDS));
	}
}
