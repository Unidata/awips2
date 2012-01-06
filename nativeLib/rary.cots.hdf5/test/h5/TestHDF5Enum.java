import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;

public class TestHDF5Enum 
{
    public static void main(String[] args) throws Exception
    {
        int[] itrue = {1};
        int[] ifalse = {0};

        int booleanEnum = H5.H5Tenum_create(HDF5Constants.H5T_NATIVE_INT);
        int status = H5.H5Tenum_insert(booleanEnum, "true", itrue);
        status = H5.H5Tenum_insert(booleanEnum, "false", ifalse);

        System.out.println(H5.H5Tget_member_name(booleanEnum, 0));
        System.out.println(H5.H5Tget_member_name(booleanEnum, 1));
    }
}
