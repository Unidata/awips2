// @(#) Sandia: $Header$
// @(#) Sandia: $Source$
// @(#) Sandia: $Revision$
// @(#) Sandia: $Author$
// @(#) Sandia: $Date$
//
// Copyright 2006 Sandia National Laboratories.  All rights reserved.

import java.util.Vector;

import ncsa.hdf.object.h5.H5CompoundDS;
import ncsa.hdf.object.h5.H5File;

public class TestHDF5ReadString
{

  public static void main (String[] args) throws Exception
  {
    H5File file = new H5File("./TestHDF5ReadString.h5", H5File.READ);
    H5CompoundDS dataset = (H5CompoundDS)file.get("/Table0");
    dataset.init();
    
    // get the data; the first column should be of type String, length=25
    Vector columns = (Vector)dataset.getData();
    String[] columnData = (String[])columns.get(0);

    System.out.println("columnData.length should equal 1");
    System.out.println("columnData.length == " + columnData.length);
    
    System.out.println("OK");
  }

}
