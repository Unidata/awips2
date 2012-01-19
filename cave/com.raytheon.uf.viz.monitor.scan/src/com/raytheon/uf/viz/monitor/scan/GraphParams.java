package com.raytheon.uf.viz.monitor.scan;

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


/**
* 
* Covenient storage for graph params read from Localization
* 
* <pre>
* SOFTWARE HISTORY
* Date         Ticket#    Engineer    Description
* ------------ ---------- ----------- --------------------------
* 05/11/2009   2307      dhladky    Initial Creation.
* 
* </pre>
* 
* @author dhladky
* @version 1.0
*/


public class GraphParams {
	
	@SuppressWarnings("unchecked")
	Enum fieldName = null;
	Double min = 0.0;
	Double interval = 0.0;
	Double range = 0.0;
	
	@SuppressWarnings("unchecked")
	public GraphParams(Enum fieldName, Double min, Double interval, Double range) {
		this.fieldName = fieldName;
		this.min = min;
		this.interval = interval;
		this.range = range;
	}
	
	public Double getMin() {
		return min;
	}
	public void setMin(Double min) {
		this.min = min;
	}
	public Double getInterval() {
		return interval;
	}
	public void setInterval(Double interval) {
		this.interval = interval;
	}
	public Double getRange() {
		return range;
	}
	public void setRange(Double range) {
		this.range = range;
	}

}
