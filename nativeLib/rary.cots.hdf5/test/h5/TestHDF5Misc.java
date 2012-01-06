// @(#) Sandia: $Header$
// @(#) Sandia: $Source$
// @(#) Sandia: $Revision$
// @(#) Sandia: $Author$
// @(#) Sandia: $Date$
//
// Copyright 2006 Sandia National Laboratories.  All rights reserved.

import java.util.List;

import ncsa.hdf.object.CompoundDS;
import ncsa.hdf.object.Dataset;
import ncsa.hdf.object.Group;
import ncsa.hdf.object.HObject;
import ncsa.hdf.object.h5.H5File;

public class TestHDF5Misc
{
  public static void main (String[] args) throws Exception
  {
    H5File file = new H5File("./TestHDF5Misc.h5", H5File.READ);
    testGetMemberList(file);
    testGetPath(file);
    testIsRoot(file);
  }
  
  /**
   * This is a bug.  The objects that are returned by the
   * 'Group.getMemberList()' method don't appear to be initialized
   * properly.  The objects returned are typed correctly (the list
   * returned seems to contain 'Group', 'CompoundDS', or 'Dataset' objects),
   * but not all the methods available for these classes seem
   * to work propertly.
   */
  public static void testGetMemberList (H5File file) throws Exception
  {
    Group group = (Group)file.get("/");
    printHObject(group);
  }
  
  private static void printHObject (HObject hObject) throws Exception
  {
    if (hObject instanceof Group) printHObject((Group)hObject);
    if (hObject instanceof CompoundDS) printHObject((CompoundDS)hObject);
    if (hObject instanceof Dataset) printHObject((Dataset)hObject);
  }
  
  private static void printHObject (Group group) throws Exception
  {
    List<HObject> list = (List<HObject>)group.getMemberList();
    for (HObject hObject : list)
    {
      System.out.println(
        hObject.getPath() + hObject.getName() + " : " + hObject.getClass().getName());
      try
      {
        printHObject(hObject);
      }
      catch (Exception e)
      {
        System.out.println(e);
      }
    }
  }
  
  private static void printHObject (CompoundDS compoundDS) throws Exception
  {
    System.out.println(compoundDS.getData());
  }

  private static void printHObject (Dataset dataset) throws Exception
  {
    System.out.println(dataset.getData());
  }
  
  /**
   * This is probably not a bug, but is inconvenient.  Normally, if you
   * have an HObject, you could use 'getPath() + getName()' to obtain
   * the full path, but the root HObject is a special case because 'getPath()'
   * returns a null object, rather than an empty string.  So, you have
   * to check if your HObject is the root.  See next method!
   */
  public static void testGetPath (H5File file) throws Exception
  {
    HObject hObject = file.get("/");
    System.out.println("Next line should print '':");
    System.out.println(hObject.getPath());
  }
  
  /**
   * This is a bug.  The 'Group.isRoot()' operation operation seems
   * to return 'true' no matter what... 
   */
  public static void testIsRoot (H5File file) throws Exception
  {
    Group group = (Group)file.get("/");
    System.out.println("Next line should print 'true':");
    System.out.println(group.isRoot());
    
    group = (Group)file.get("/Group0");
    System.out.println("Next line should print 'false':");
    System.out.println(group.isRoot());
    
    group = (Group)file.get("/Group0/SubGroup0");
    System.out.println("Next line should print 'false':");
    System.out.println(group.isRoot());
  }
  
}
