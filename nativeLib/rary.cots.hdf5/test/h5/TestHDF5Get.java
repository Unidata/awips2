// @(#) Sandia: $Header$
// @(#) Sandia: $Source$
// @(#) Sandia: $Revision$
// @(#) Sandia: $Author$
// @(#) Sandia: $Date$
//
// Copyright 2006 Sandia National Laboratories.  All rights reserved.

import ncsa.hdf.object.h5.H5File;

public class TestHDF5Get
{

  public static void main (String[] args) throws Exception
  {
    H5File file = new H5File("./TestHDF5Get.h5", H5File.READ);
    file.get("/Group0");
    file.close();
  }

}
