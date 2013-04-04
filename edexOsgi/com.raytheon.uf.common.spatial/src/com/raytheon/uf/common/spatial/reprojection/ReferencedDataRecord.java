/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.common.spatial.reprojection;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datastorage.records.IDataRecord;

public class ReferencedDataRecord{	
	
	protected IDataRecord record;
	
	protected ReferencedEnvelope envelope;
	
	public ReferencedDataRecord(IDataRecord record, ReferencedEnvelope envlope) {
		this.record = record;
		this.envelope = envlope;
	}

	public IDataRecord getRecord() {
		return record;
	}

	public void setRecord(IDataRecord record) {
		this.record = record;
	}

	public ReferencedEnvelope getEnvelope() {
		return envelope;
	}

	public void setEnvelope(ReferencedEnvelope envelope) {
		this.envelope = envelope;
	}
	
}
