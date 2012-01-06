import ncsa.hdf.object.h5.*;

public class TestHDF5OpenClose 
{ 
  public static void main (String[] args) throws Exception 
  { 
    H5File file = new H5File("./TestHDF5OpenClose.h5", H5File.READ); 
    H5Group grp = (H5Group)file.get("/Group0"); 
    System.out.println(grp);
    file.close(); 
  } 
} 


