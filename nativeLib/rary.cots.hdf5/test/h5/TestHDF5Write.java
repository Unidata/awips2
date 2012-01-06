import ncsa.hdf.object.Dataset;
import ncsa.hdf.object.h5.*;


public class TestHDF5Write
{
  public static void main (String[] args)
  {
    try
    {
      H5File file = new H5File("./TestHDF5Write.h5", H5File.WRITE);
      Dataset ds;


      // retrieve the dataset
      ds = (Dataset)file.get("/Dataset0");
      // init the dataset
      ds.init();
      String[] stringArray = (String[])ds.getData();
      System.out.println("Dataset0 == " + stringArray[0]);
      stringArray[0] += "!";
      // write the dataset to the file
      ds.write();
      // reopen the file
      file.close();
      file.open();
      // reread the dataset
      ds = (Dataset)file.get("/Dataset0");
      stringArray = (String[])ds.getData();
      System.out.println("Dataset0 == " + stringArray[0]);


      // retrieve the dataset
      ds = (Dataset)file.get("/Table0");
      // init the dataset
      ds.init();
      java.util.List list1 = (java.util.List)ds.getData();
      int[] intArray = (int[])list1.get(1);
      System.out.println("Member1 == " + intArray[0]);
      intArray[0]++;
      // write the dataset to the file
      ds.write();
      // reopen the file
      file.close();
      file.open();
      // reread the dataset
      ds = (H5CompoundDS)file.get("/Table0");
      list1 = (java.util.List)ds.getData();
      intArray = (int[])list1.get(1);
      System.out.println("Member1 == " + intArray[0]);      
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
  }
}

