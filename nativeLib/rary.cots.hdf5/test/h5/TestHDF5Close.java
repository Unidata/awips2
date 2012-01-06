import ncsa.hdf.object.h5.*;
import ncsa.hdf.hdf5lib.*;

public class TestHDF5Close 
{ 
    public static void main (String[] args) throws Exception
    {
        H5File file = new H5File("./TestHDF5OpenClose.h5", H5File.READ); 
        H5Group grp = (H5Group)file.get("/Group0"); 
        System.out.println(grp);

        int fid = file.getFID();
        try {
            int n=0, type=-1, oids[];
            n = H5.H5Fget_obj_count(fid, HDF5Constants.H5F_OBJ_ALL);
            if ( n>0)
            {
                oids = new int[n];
                H5.H5Fget_obj_ids(fid, HDF5Constants.H5F_OBJ_ALL, n, oids);
                for (int i=0; i<n; i++)
                {
                    type = H5.H5Iget_type(oids[i]);
                    if (HDF5Constants.H5I_DATASET == type) {
                        try { H5.H5Dclose(oids[i]); } catch (Exception ex2) {}
                    } else if (HDF5Constants.H5I_GROUP == type) {
                        try { H5.H5Gclose(oids[i]); } catch (Exception ex2) {}
                    } else if (HDF5Constants.H5I_DATATYPE == type) {
                        try { H5.H5Tclose(oids[i]); } catch (Exception ex2) {}
                    } else if (HDF5Constants.H5I_ATTR == type) {
                        try { H5.H5Aclose(oids[i]); } catch (Exception ex2) {}
                    }
                } // for (int i=0; i<n; i++)
            } // if ( n>0)
        } catch (Exception ex) {}

        try { H5.H5Fflush(fid, HDF5Constants.H5F_SCOPE_GLOBAL); } catch (Exception ex) {ex.printStackTrace();}
        try { H5.H5Fclose(fid); } catch (Exception ex) {}

    }
} 


