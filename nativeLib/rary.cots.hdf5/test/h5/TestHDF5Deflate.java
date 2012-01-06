import ncsa.hdf.hdf5lib.*;

public class TestHDF5Deflate
{
    public static void main (String[] args) throws Exception
    {
        int COMPRESSION_LEVEL = 6;
        int iPlistID=-1, status=-1;

        try {
            iPlistID = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            status = H5.H5Pset_deflate(iPlistID, COMPRESSION_LEVEL);
        } catch (Exception ex) {
            ex.printStackTrace(); 
        }

        if (iPlistID<0 | status <0)
            System.out.println("H5Pset_deflate() failed");
        else
            System.out.println("H5Pset_deflate() worked");
    }
}


