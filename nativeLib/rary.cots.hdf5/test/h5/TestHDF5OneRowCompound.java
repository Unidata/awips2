
import java.lang.reflect.Array;
import java.util.List;
import ncsa.hdf.object.h5.*;
import ncsa.hdf.hdf5lib.*;

public class TestHDF5OneRowCompound
{
    //private static final String FILE_NAME = "hdf5_test.h5";
    //private static final String DSET_NAME = "/arrays/Vdata with mixed types";
    private static final String FILE_NAME = "TestHDF5Misc.h5";
    private static final String DSET_NAME = "/Table0";
  
    public static void main (String[] args) throws Exception
    {
        List data=null;
        
        // Get the source dataset
        H5File file = new H5File(FILE_NAME, H5File.READ);
        file.open();

        H5CompoundDS dset = (H5CompoundDS)file.get(DSET_NAME);
        System.out.println(dset);
        
        int rank = dset.getRank();
        try { if (rank<=0) dset.init(); } catch (Exception ex) {}
        rank = dset.getRank(); 

        
        // 1)  I read a table from an H5 file; and use the 'select subset' code 
        //     to get only one row's worth of data before calling 'getData()
        long[] count = dset.getSelectedDims();
        long[] start = dset.getStartDims();
        for (int i=0; i<rank; i++) {
            start[i] = 2; // start the third data point
            count[i] = 1; // select only one row (the third row)
        }
        
        // 2)  I call 'Dataset.init()' to clear the selection
        dset.init();
        
        // 3)  I call 'Dataset.clearData()' to clear the file data from memory
        dset.clearData();
        
        // 4)  I call 'Dataset.getData()' to get the entire table's worth of data
        try { data = (List)dset.read(); }
        catch (Exception ex) { ex.printStackTrace();}
        
        int n = dset.getMemberCount();
        for (int i=0; i<n; i++)
            System.out.print(Array.get(data.get(i), 0)+",\t");
       System.out.println("\n");
       

        file.close();
    }
} 


