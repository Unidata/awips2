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
 * Jun 3, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.format;

import java.io.IOException;
import java.nio.channels.WritableByteChannel;

import ucar.ma2.Array;
import ucar.ma2.InvalidRangeException;
import ucar.ma2.Section;
import ucar.ma2.StructureDataIterator;
import ucar.nc2.NetcdfFile;
import ucar.nc2.ParsedSectionSpec;
import ucar.nc2.Structure;
import ucar.nc2.Variable;
import ucar.nc2.iosp.IOServiceProvider;
import ucar.nc2.util.CancelTask;
import ucar.unidata.io.RandomAccessFile;

import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class DataRecordIOSP implements IOServiceProvider {

	protected IDataRecord dataRecord;

	public DataRecordIOSP(IDataRecord dataRecord) {
		this.dataRecord = dataRecord;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * ucar.nc2.iosp.IOServiceProvider#isValidFile(ucar.unidata.io.RandomAccessFile
	 * )
	 */
	@Override
	public boolean isValidFile(RandomAccessFile arg0) throws IOException {
		System.out.println();
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * ucar.nc2.iosp.IOServiceProvider#open(ucar.unidata.io.RandomAccessFile,
	 * ucar.nc2.NetcdfFile, ucar.nc2.util.CancelTask)
	 */
	@Override
	public void open(RandomAccessFile arg0, NetcdfFile arg1, CancelTask arg2)
			throws IOException {
		System.out.println();

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#readData(ucar.nc2.Variable,
	 * ucar.ma2.Section)
	 */
	@Override
	public Array readData(Variable arg0, Section arg1) throws IOException,
			InvalidRangeException {
		System.out.println();
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#close()
	 */
	@Override
	public void close() throws IOException {
		System.out.println();

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#getDetailInfo()
	 */
	@Override
	public String getDetailInfo() {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#getFileTypeDescription()
	 */
	@Override
	public String getFileTypeDescription() {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#getFileTypeId()
	 */
	@Override
	public String getFileTypeId() {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#getFileTypeVersion()
	 */
	@Override
	public String getFileTypeVersion() {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * ucar.nc2.iosp.IOServiceProvider#getStructureIterator(ucar.nc2.Structure,
	 * int)
	 */
	@Override
	public StructureDataIterator getStructureIterator(Structure arg0, int arg1)
			throws IOException {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * ucar.nc2.iosp.IOServiceProvider#readSection(ucar.nc2.ParsedSectionSpec)
	 */
	@Override
	public Array readSection(ParsedSectionSpec arg0) throws IOException,
			InvalidRangeException {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#readToByteChannel(ucar.nc2.Variable,
	 * ucar.ma2.Section, java.nio.channels.WritableByteChannel)
	 */
	@Override
	public long readToByteChannel(Variable arg0, Section arg1,
			WritableByteChannel arg2) throws IOException, InvalidRangeException {
		// TODO Auto-generated method stub
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#sendIospMessage(java.lang.Object)
	 */
	@Override
	public Object sendIospMessage(Object arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#sync()
	 */
	@Override
	public boolean sync() throws IOException {
		// TODO Auto-generated method stub
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#syncExtend()
	 */
	@Override
	public boolean syncExtend() throws IOException {
		// TODO Auto-generated method stub
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ucar.nc2.iosp.IOServiceProvider#toStringDebug(java.lang.Object)
	 */
	@Override
	public String toStringDebug(Object arg0) {
		// TODO Auto-generated method stub
		return null;
	}

}
