// @(#) Sandia: $Header$
// @(#) Sandia: $Source$
// @(#) Sandia: $Revision$
// @(#) Sandia: $Author$
// @(#) Sandia: $Date$
//
// Copyright 2006 Sandia National Laboratories.  All rights reserved.

import java.util.Date;
import java.util.Vector;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.Dataset;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.Group;
import ncsa.hdf.object.h5.H5Datatype;
import ncsa.hdf.object.h5.H5File;

public class TestHDF5Create
{

  public static void main (String[] args) throws Exception
  {
    H5File file = new H5File("./TestHDF5Create.h5", H5File.WRITE);
    
    // get the root group
    Group rootgroup = (Group)file.get("/");
    if (rootgroup == null)
    {
      throw new Exception("Could not get group: /");
    }
    
    // make a subgroup using the current timestamp (milliseconds since epoch)
    Date now = new Date();
    String nowString = new Long(now.getTime()).toString();
    String newGroupPath = "/" + nowString;
    Group newGroup = file.createGroup(nowString, rootgroup);
    if (newGroup == null)
    {
      throw new Exception("Could not create new group: " + newGroupPath);
    }
    
    // flush the changes
    H5.H5Fflush(file.getFID(), HDF5Constants.H5F_SCOPE_GLOBAL);
    
    // reopen the file
    file = new H5File("./TestHDF5Create.h5", H5File.WRITE);
    
    // read the subgroup
    newGroup = (Group)file.get(newGroupPath);
    if (newGroup == null)
    {
      throw new Exception("Could not get newly created group: " + newGroupPath);
    }
    
    // make a table in the new group
    String newTablePath = newGroupPath + "/CreatedTable";
    long[] dims = {3};
    String[] memberNames = {"Column"};
    Datatype[] datatypes = {
        new H5Datatype(
          Datatype.CLASS_STRING,
          256,
          Datatype.NATIVE,
          Datatype.NATIVE)};
    int[] memberOrders = {256};
    Vector<String[]> data = new Vector<String[]>();
    String[] columnValues = {"One", "Two", "Three"};
    data.add(columnValues);
    
    Dataset dataset = file.createCompoundDS(
      "CreatedTable", newGroup, dims, memberNames,
      datatypes, memberOrders, data);
    
    // flush the changes
    H5.H5Fflush(file.getFID(), HDF5Constants.H5F_SCOPE_GLOBAL);
    
    // reopen the file
    file = new H5File("./TestHDF5Create.h5", H5File.WRITE);
    
    // read the new table
    dataset = (Dataset)file.get(newTablePath);
    if (dataset == null)
    {
      throw new Exception("Could not get newly created table: " + newTablePath);
    }
    
    System.out.println("OK");
  }

}
