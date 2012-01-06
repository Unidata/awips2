import ncsa.hdf.hdf5lib.*;

public class TestHDF5Perf
{
    final static String DNAME = "dset";    

    public static void main (String[] args) throws Exception
    {
        int fid=-1, plist=-1, did=-1, msid=-1, fsid=-1, tid=HDF5Constants.H5T_NATIVE_INT;
        int i=0, buf[], argc=0;
        long t0, t1, dim1, dsize=0;
        long start[] = {0};
        long count[] = {1};

	argc = args.length;
        if (argc < 2) {
            System.out.println("\nUsage: TestHDF5Perf file_name selection_size(MB)\n");
            System.exit(0);
        }
    
        plist = H5.H5Pcreate (HDF5Constants.H5P_FILE_ACCESS);
        H5.H5Pset_fclose_degree (plist, HDF5Constants.H5F_CLOSE_STRONG);

        fid = H5.H5Fopen(args[0], HDF5Constants.H5F_ACC_RDONLY, plist);
        did = H5.H5Dopen(fid, DNAME);
        fsid = H5.H5Dget_space(did);
        dim1 = H5.H5Sget_simple_extent_npoints(fsid);
 
        dsize = (long)(Float.parseFloat(args[1])*256)*1024;
        if (dsize > dim1) dsize = dim1;
        buf = new int[(int)dsize];

        long[] dims = {dsize};
        msid = H5.H5Screate_simple(1, dims, null);
    
        /* read the last block into memory */
        start[0] = dim1-dsize; count[0] = dsize;
        H5.H5Sselect_hyperslab(fsid, HDF5Constants.H5S_SELECT_SET, start, null, count, null);
        t0 = System.currentTimeMillis()/1000;
        H5.H5Dread(did, tid, msid, fsid, HDF5Constants.H5P_DEFAULT, buf);
        t1 = System.currentTimeMillis()/1000;
        System.out.println("\n Time to read "+args[1]+" MB data: " + (t1-t0));

        /* close and reopen file for write */
        if (fsid > 0) H5.H5Sclose(fsid);
        if (did > 0) H5.H5Dclose(did);
        if (fid > 0) H5.H5Fclose(fid);
        fid = H5.H5Fopen(args[0], HDF5Constants.H5F_ACC_RDWR, plist);
        did = H5.H5Dopen(fid, DNAME);

        /* write data to the first block */
        fsid = H5.H5Dget_space(did);
        start[0] = 0;
        H5.H5Sselect_hyperslab(fsid, HDF5Constants.H5S_SELECT_SET, start, null, count, null);
        t0 = System.currentTimeMillis()/1000;
        H5.H5Dwrite(did, tid, msid, fsid, HDF5Constants.H5P_DEFAULT, buf);
        t1 = System.currentTimeMillis()/1000;
        System.out.println("\n Time to write "+args[1]+" MB data: " + (t1-t0));
    
    done:
        try { if (plist > 0) H5.H5Pclose(plist); } catch (Exception ex) {}
        try { if (fsid > 0) H5.H5Sclose(fsid); } catch (Exception ex) {}
        try { if (msid > 0) H5.H5Sclose(msid); } catch (Exception ex) {}
        try { if (did > 0) H5.H5Dclose(did); } catch (Exception ex) {}
        try { if (fid > 0) H5.H5Fclose(fid); } catch (Exception ex) {}
    }
}
