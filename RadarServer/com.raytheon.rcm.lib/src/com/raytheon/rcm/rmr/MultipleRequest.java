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

import java.util.Arrays;

import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.rcm.request.Request;

@XmlRootElement
@XmlType(propOrder={})
public class MultipleRequest implements Cloneable {

	@XmlRootElement
	@XmlType(propOrder={})
	public static class Element implements Cloneable {

		private String[] radarIDs = new String[0];
		private Request request;

		@XmlList
		public String[] getRadarIDs() {
			return radarIDs;
		}
		public void setRadarIDs(String[] radarIDs) {
			this.radarIDs = radarIDs;
		}
		public Request getRequest() {
			return request;
		}
		public void setRequest(Request request) {
			this.request = request;
		}
		
		public Element duplicate() {
			Element dup;
			try {
				dup = (Element) clone();
			} catch (CloneNotSupportedException e) {
				throw new RuntimeException(e);
			}
			dup.radarIDs = Arrays.copyOf(radarIDs, radarIDs.length);
			dup.request = (Request) request.clone();
			return dup;
		}
	}
	
	private String name;
	private int duration;
	private int interval;
	private Element[] elements = new Element[0];

	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public int getDuration() {
		return duration;
	}
	public void setDuration(int duration) {
		this.duration = duration;
	}
	public int getInterval() {
		return interval;
	}
	public void setInterval(int interval) {
		this.interval = interval;
	}
	public Element[] getElements() {
		return elements;
	}
	public void setElements(Element[] elements) {
		this.elements = elements;
	}
	
	public boolean isSingle() {
		return interval <= 0 || duration <= 0;
	}
	
	public MultipleRequest duplicate() {
		MultipleRequest dup;
		try {
			dup = (MultipleRequest) clone();
		} catch (CloneNotSupportedException e) {
			throw new RuntimeException(e);
		}
		dup.elements = new Element[elements.length];
		for (int i = 0; i < elements.length; ++i)
			dup.elements[i] = elements[i].duplicate();
		return dup;
	}
}
