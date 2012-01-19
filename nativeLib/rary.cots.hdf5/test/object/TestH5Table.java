package test.object;

import java.util.Vector;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.Dataset;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.h5.H5Datatype;
import ncsa.hdf.object.h5.H5File;

public class TestH5Table {
    
    public static void main(final String[] args) {
        long dim1=1000, dim2=50;
        
        if (args.length > 1) 
        {
            try {
                dim1 = Long.parseLong(args[0]);
                dim2 = Long.parseLong(args[1]);
            } catch (final Exception ex) {
                ex.printStackTrace();
                System.exit(0);
            }
        }
        
        try { 
            testTable(dim1, dim2); 
        } catch (final Exception ex) {
            ex.printStackTrace();
            System.exit(0);
        }
    }
    
    /**
     * Test the performance of reading a small table data.
     */
    public static final void testTable(final long dim1, final long dim2) throws Exception 
    {
        int nObjs = 0; // number of object left open
        Dataset dset =null;
        long t0=0, t1=0, time=0, nbytes=0, readKBS=0, writeKBS=0;
        final String dname = "/table";
        
        final String[] COMPOUND_MEMBER_NAMES = {"int32", "float32"};
        final H5Datatype[] COMPOUND_MEMBER_DATATYPES = {
            new H5Datatype(Datatype.CLASS_INTEGER, 4, -1, -1), 
            new H5Datatype(Datatype.CLASS_FLOAT, 4, -1, -1)};
        
        final long DIM1 = dim1;
        final long DIM2 = dim2;
        final long[] DIMs = {DIM1, DIM2};
        final int DIM_SIZE = (int)(DIM1*DIM2);
        
        final int[] DATA_INT = new int[DIM_SIZE];
        final float[] DATA_FLOAT = new float[DIM_SIZE];
        final Vector DATA_COMP = new Vector(2);
        
        for (int i=0; i<DIM_SIZE; i++) {
            DATA_INT[i] = i;
            DATA_FLOAT[i] = i+i/100.0f;
        }
        DATA_COMP.add(0, DATA_INT);
        DATA_COMP.add(1, DATA_FLOAT);
        
        final H5File file = new H5File("testH5Table.h5", FileFormat.CREATE);
        file.open();
        
        try {
            dset = file.createCompoundDS(dname, null, DIMs, null, null, -1, 
                    COMPOUND_MEMBER_NAMES, COMPOUND_MEMBER_DATATYPES, null, DATA_COMP);
        } catch (final Exception ex) { 
            System.out.println("file.createCompoundDS() failed. "+ ex);
        }
        
        try {
            time = 0;
            nbytes = (DIM_SIZE*8L*1000000L);            
            dset = (Dataset)file.get(dname);
            dset.clearData();
            collectGarbage();
            
            // test reading
            t0 = System.nanoTime();
            try {
                dset.getData();
            } catch (final Exception ex) { 
                 System.out.println("dset.getData() failed. "+ ex);
            }
            t1 = System.nanoTime();
            time = (t1-t0);
            readKBS = (nbytes)/time;

            // test writing
            t0 = System.nanoTime();
            try {
                dset.write();
            } catch (final Exception ex) { 
                 System.out.println("dset.write() failed. "+ ex);
            }
            t1 = System.nanoTime();
            time = (t1-t0);
            writeKBS = (nbytes)/time;
            System.out.println("\nReading/writing a "+DIM1+"x"+DIM2+" table [KB/S]: \t"+ readKBS+"\t"+writeKBS);
            
            try {
                nObjs = H5.H5Fget_obj_count(file.getFID(), HDF5Constants.H5F_OBJ_ALL);
            } catch (final Exception ex) { 
                 System.out.println("H5.H5Fget_obj_count() failed. "+ ex);
            }
            
            try {            
                file.close();
            } catch (final Exception ex) { 
                System.out.println("file.close() failed. "+ ex);
            }
        } finally {
            // delete the testing file
            file.deleteOnExit();
        }
    }

    private static void collectGarbage() {
        try {
            System.gc();
            Thread.sleep(100);
            System.runFinalization();
            Thread.sleep(100);
        }
        catch (final Exception ex){
            ex.printStackTrace();
        }
    }
}
   