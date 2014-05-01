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
package com.raytheon.uf.viz.radarapps.alertreq.impl;

import java.util.BitSet;
import java.util.List;

import org.eclipse.core.databinding.observable.AbstractObservable;
import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.list.WritableList;

import com.raytheon.rcm.alertreq.AlertRequestDefinition;


public class AlertRequestDoc extends AbstractObservable {
	
	public AlertRequestDoc() {
		this(Realm.getDefault());
		thresholds = new WritableList(getRealm());
		thresholds.addChangeListener(new IChangeListener() {
			public void handleChange(ChangeEvent event) {
				modified = true;
			}
		});
	}
	
	public AlertRequestDoc(Realm realm) {
		super(realm);
	}

	private boolean modified;
	private WritableList thresholds;
	private BitSet boxBits = new BitSet(58*58);
	
	public void set(AlertRequestDefinition ard) {
		// stale = true;
		modified = true;
		thresholds.clear();
		thresholds.addAll(ard.elements);
		boxBits.clear();
		boxBits.or(ard.boxBits);
	}
	
	public AlertRequestDefinition toAlertRequestDefinition() {
		AlertRequestDefinition ard = new AlertRequestDefinition();
		ard.elements.addAll((List<AlertRequestDefinition.Threshold>) thresholds);
		ard.boxBits.or(boxBits);
		return ard;
	}
	
	public boolean isModified() {
		return modified;
	}

	public void setModified(boolean modified) {
		this.modified = modified;
	}

	public WritableList getThresholds() {
		return thresholds;
	}

	public void setThresholds(WritableList thresholds) {
		this.thresholds = thresholds;
	}

	public BitSet getBoxBits() {
		return boxBits;
	}

	public void setBoxBits(BitSet boxBits) {
		this.boxBits = boxBits;
	}
	
	@Override
	public boolean isStale() {
		// "stale" not currently used
		return false;
	}

	public void handleBoxBitsChanged() {
		modified = true;
		this.fireChange();
	}

}
