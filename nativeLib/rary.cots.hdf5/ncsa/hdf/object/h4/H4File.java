/*****************************************************************************
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF Java Products distribution.                  *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the files COPYING and Copyright.html. *
 * COPYING can be found at the root of the source code distribution tree.    *
 * Or, see http://hdfgroup.org/products/hdf-java/doc/Copyright.html.         *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 ****************************************************************************/

package ncsa.hdf.object.h4;

import java.util.*;
import java.io.File;

import java.lang.reflect.Array;
import javax.swing.tree.*;
import ncsa.hdf.hdflib.*;
import ncsa.hdf.object.*;

/**
 * This class provides file level APIs. File access APIs include retrieving the
 * file hierarchy, opening and closing file, and writing file content to disk.
 * <p>
 * @version 2.4 9/4/2007
 * @author Peter X. Cao
 */
public class H4File extends FileFormat
{
    public static final long serialVersionUID = HObject.serialVersionUID;

    // make sure that the library is loaded.
    static { HDFLibrary.loadH4Lib(); }

    /**
     * the file access flag.
     */
    private int flag;

    /**
     * The root node of the tree structure of this file.
     */
    private DefaultMutableTreeNode rootNode;

    /**
     * The list of unique (tag, ref) pairs.
     * It is used to avoid duplicate objects in memory.
     */
    private List objList;

    /**
     * The GR interface identifier.
     * The identifier is returned by GRstart(fid), which initializes the GR
     * interface for the file specified by the parameter. GRstart(fid) is an
     * expensive call. It should be called only once. Calling GRstart(fid) in
     * a loop should be avoided.
     */
    private int grid;

    private boolean isNetCDF = false;

    /**
     * The SDS interface identifier.
     * The identifier is returned by SDstart(fname, flag), which initializes the
     * SD interface for the file specified by the parameter. 
     * SDstart(fname, flag) is an expensive call. It should be called only once
     * Calling SDstart(fname, flag) in a loop should be avoided.
     */
    private int sdid;

    /* secret flag: show CDF0.0, etc., to help debug
     */
    private boolean showAll = false;

    /**
     * Creates an H4File with read only access.
     */
    public H4File()
    {
        this("", WRITE);
    }

    /**
     * Creates an H4File with read only access.
     */
    public H4File(String pathname)
    {
        this(pathname, WRITE);
    }

    /**
     * Creates an H4File instance with specified file name and access.
     * <p>
     * The access parameter values and corresponding behaviors:
     * <ul>
     * <li> READ: Read-only access; open() will fail file doesn't exist.
     * <li> WRITE: Read/Write access; if file doesn't exist, open() will 
     *      create it; open() will fail if read/write access not allowed.
     * <li> CREATE: Read/Write access; create a new file or truncate
     *      an existing one; open() will fail if file can't be created or if
     *      file exists but can't be opened read/write.
     * </ul>
     * <p>
     * This constructor does not open the file for access, nor does it
     * confirm that the file can later be opened read/write or created.
     * <p>
     * The flag returned by {@link #isReadOnly()} is set to true if the access
     * parameter value is READ, even though the file isn't yet open.
     *
     * @param fileName A valid file name, with a relative or absolute path.
     * @param access The file access flag, which determines behavior when file
     *               is opened.
     *               Acceptable values are <code> READ, WRITE, </code>
     *               and <code>CREATE</code>.
     * @throws NullPointerException If the <code>fileName</code> argument is
     *         <code>null</code>.
     */
    public H4File(String fileName, int access)
    {
        super(fileName);
        isReadOnly = (access == READ);
        objList = new Vector();

        this.fid = -1;

        if (access == READ) {
            flag = HDFConstants.DFACC_READ;
        } else if (access == WRITE) {
            flag = HDFConstants.DFACC_WRITE;
        } else if (access == CREATE) {
            flag = HDFConstants.DFACC_CREATE;
        } else {
            flag = access;
        }

        String shwAll = System.getProperty("h4showall");
        if (shwAll != null) {
            showAll = true;
            //System.err.println("show all is on");
        } else {
            //System.err.println("show all is off");
        }
    }

    /**
     * Checks if the given file format is an HDF4 file.
     * <p>
     * @param fileformat the fileformat to be checked.
     * @return true if the given file is an HDF4 file; otherwise returns false.
     */
    public boolean isThisType(FileFormat fileformat)
    {
        return (fileformat instanceof H4File);
    }

    /**
     * Checks if the given file is an HDF4 file or netCDF.
     * HDF4 library supports netCDF version 2.3.2. It only supports SDS APIs.
     * <p>
     * @param filename the file to be checked.
     * @return true if the given file is an HDF4 file; otherwise returns false.
     */
    public boolean isThisType(String filename)
    {
        boolean isH4 = false;

        try {
            isH4 = HDFLibrary.Hishdf(filename);
        } catch (HDFException ex)
        {
            isH4 = false;
        }

        if (!isH4) {
            isH4 = isNetCDF(filename);
        }

        return isH4;
    }


    /**
     * Creates an HDF4 file with the specified name and returns a new
     * H4File instance associated with the file.
     *
     * @throws HDFException If the file cannot be created or if createFlag
     *          has unexpected value.
     * @see ncsa.hdf.object.FileFormat#createFile(java.lang.String, int)
     * @see #H4File(String, int)
     */
    public FileFormat createFile(String filename, int createFlag)
                                                        throws Exception
    {
        // Flag if we need to create or truncate the file.
        Boolean doCreateFile = true;

        // Won't create or truncate if CREATE_OPEN specified and file exists
        if (createFlag == FILE_CREATE_OPEN) {
            File f = new File( filename );
            if ( f.exists() ) {
                doCreateFile = false;
            }
        }

        if (doCreateFile) {
            int fileid = HDFLibrary.Hopen(filename, HDFConstants.DFACC_CREATE);
            try { 
		HDFLibrary.Hclose(fileid); 
	    } catch (HDFException ex) {}
	}

        return new H4File(filename, WRITE);
    }

    /**
     * Creates an H4File instance with specified file name and access.
     * <p>
     * @see ncsa.hdf.object.FileFormat#createInstance(java.lang.String, int)
     * @see #H4File(String, int)
     */
    public FileFormat createInstance(String filename, int access) 
							throws Exception
    {
        return new H4File(filename, access);
    }

    // Implementing FileFormat
    public int open() throws Exception
    {
        if ( fid >=0 ) {
            return fid; // file is openned already
        }

        // check for valid file access permission
        if (flag < 0) // invalid access id
        {
            throw new HDFException("Invalid access identifer -- "+flag);
        }
        else if (flag == HDFConstants.DFACC_READ)
        {
           if (!exists()) {
            throw new HDFException("File does not exist -- "+fullFileName);
        } else if (exists() && !canRead()) {
                throw new HDFException("Cannot read file -- "+fullFileName);
            }
        }
        else if ((flag == HDFConstants.DFACC_WRITE) ||
            (flag == HDFConstants.DFACC_CREATE))
        {
            if (exists() && !canWrite()) {
                throw new HDFException("Cannot write file, try open as read-only -- "+fullFileName);
            }
        }

        isNetCDF = isNetCDF(fullFileName);
        if (isNetCDF) {
            isReadOnly = true; // read only for netCDF
        }

        // only support SDS APIs for netCDF
        if (isNetCDF) {
            fid = 0;
        } else {
            fid = HDFLibrary.Hopen( fullFileName, flag);
            HDFLibrary.Vstart(fid);
            grid = HDFLibrary.GRstart(fid);
        }
        sdid = HDFLibrary.SDstart(fullFileName, flag);
        
        // load the file hierarchy
        rootNode = loadTree();

        return fid;
    }

    // Implementing FileFormat
    public void close() throws HDFException
    {
        // clean unused objects
        if (rootNode != null)
        {
            DefaultMutableTreeNode theNode = null;
            HObject theObj = null;
            Enumeration local_enum = (rootNode).breadthFirstEnumeration();
            while(local_enum.hasMoreElements())
            {
                theNode = (DefaultMutableTreeNode)local_enum.nextElement();
                theObj = (HObject)theNode.getUserObject();
                if (theObj instanceof Dataset) {
                    ((Dataset)theObj).clearData();
                }
                theObj = null;
                theNode = null;
            }
        }

        try { HDFLibrary.GRend(grid); } catch (HDFException ex) {}
        try { HDFLibrary.SDend(sdid); } catch (HDFException ex) {}
        try { HDFLibrary.Vend(fid); } catch (HDFException ex) {}

        HDFLibrary.Hclose(fid);

        fid = -1;
        objList = null;
    }

    // Implementing FileFormat
    public TreeNode getRootNode()
    {
        return rootNode;
    }


    public Group createGroup(String name, Group pgroup) throws Exception
    {
        return H4Group.create(name, pgroup);
    }

    public Datatype createDatatype(
        int tclass,
        int tsize,
        int torder,
        int tsign) throws Exception
    {
        return new H4Datatype(tclass, tsize, torder, tsign);
    }

    public Datatype createDatatype(
        int tclass,
        int tsize,
        int torder,
        int tsign,
        String name) throws Exception
    {
        throw new UnsupportedOperationException("HDF4 does not support named datatype.");
    }

    public Dataset createScalarDS(
        String name,
        Group pgroup,
        Datatype type,
        long[] dims,
        long[] maxdims,
        long[] chunks,
        int gzip,
        Object data) throws Exception
    {
        return H4SDS.create(name, pgroup, type, dims, maxdims, chunks, gzip, data);
    }


    public Dataset createImage(
        String name,
        Group pgroup,
        Datatype type,
        long[] dims,
        long[] maxdims,
        long[] chunks,
        int gzip,
        int ncomp,
        int interlace,
        Object data) throws Exception
    {
        H4GRImage dataset = H4GRImage.create(name, pgroup, type, dims, maxdims, chunks, gzip, ncomp, interlace, data);

        return dataset;
    }

    /**
     * Delete an object from the file.
     * @param obj the data object to delete.
     */
    public void delete(HObject obj) throws Exception
    {
        throw (new UnsupportedOperationException(
            "Cannot delete HDF4 object."));
    }

    /**
     * Copy an object to a group.
     * @param srcObj   the object to copy.
     * @param dstGroup the destination group.
     * @return the new node containing the new object.
     */
    public TreeNode copy(HObject srcObj, Group dstGroup, String dstName) throws Exception
    {
        TreeNode newNode = null;

        if ((srcObj == null) || (dstGroup == null)) {
            return null;
        }

        if (dstName == null) {
            dstName = srcObj.getName();
        }

        if (srcObj instanceof H4SDS)
        {
            newNode = new DefaultMutableTreeNode(
            ((H4SDS)srcObj).copy(dstGroup, dstName, null, null));
        }
        else if (srcObj instanceof H4GRImage)
        {
            newNode = new DefaultMutableTreeNode(
            ((H4GRImage)srcObj).copy(dstGroup, dstName, null, null));
        }
        else if (srcObj instanceof H4Vdata)
        {
           newNode = new DefaultMutableTreeNode(((H4Vdata)srcObj).copy(dstGroup, null, null, null));
        }
        else if (srcObj instanceof H4Group)
        {
            newNode = copyGroup((H4Group)srcObj, (H4Group)dstGroup);
        }

        return newNode;
    }

    // implementign FileFormat
    public HObject createLink(Group parentGroup, String name, HObject currentObj) throws Exception
    {
        throw new UnsupportedOperationException("createLink() is not supported");
    }

    /**
     * Creates a new attribute and attached to the object if attribute does
     * not exist. Otherwise, just update the value of the attribute.
     *
     * <p>
     * @param obj the object which the attribute is to be attached to.
     * @param attr the atribute to attach.
     * @param isSDglobalAttr The indicator if the given attribute exists.
     */
    public void writeAttribute(HObject obj, Attribute attr,
        boolean isSDglobalAttr) throws HDFException
    {
        String attrName = attr.getName();
        int attrType = attr.getType().toNative();
        long[] dims = attr.getDataDims();
        int count = 1;
        if (dims != null)
        {
            for (int i=0; i<dims.length; i++) {
                count *= (int)dims[i];
            }
        }

        Object attrValue = attr.getValue();
         if (Array.get(attrValue, 0) instanceof String)
        {
            String strValue = (String)Array.get(attrValue, 0);

            if (strValue.length() > count)
            {
                // truncate the extra characters
                strValue = strValue.substring(0, count);
                Array.set(attrValue, 0, strValue);
            }
            else
            {
                // pad space to the unused space
                for (int i=strValue.length(); i<count; i++) {
                    strValue += " ";
                }
            }

            byte[] bval = strValue.getBytes();
            // add null to the end to get rid of the junks
            bval[(strValue.length() - 1)] = 0;
            attrValue = bval;
        }

        if ( (obj instanceof H4Group) &&
             ((H4Group)obj).isRoot())
        {
            if (isSDglobalAttr) {
                HDFLibrary.SDsetattr(sdid, attrName, attrType, count, attrValue);
            } else {
                HDFLibrary.GRsetattr(grid, attrName, attrType, count, attrValue);
            }
            return;
        }

        int id = obj.open();
        if (obj instanceof H4Group) {
            HDFLibrary.Vsetattr(id, attrName, attrType, count, attrValue);
        } else if (obj instanceof H4SDS) {
            HDFLibrary.SDsetattr(id, attrName, attrType, count, attrValue);
        } else if (obj instanceof H4GRImage) {
            HDFLibrary.GRsetattr(id, attrName, attrType, count, attrValue);
        } else if (obj instanceof H4Vdata) {
            HDFLibrary.VSsetattr(id, -1, attrName, attrType, count, attrValue);
        }
        obj.close(id);
    }

    private TreeNode copyGroup(H4Group srcGroup, H4Group pgroup) throws Exception
    {
        H4Group group = null;
        int srcgid, dstgid;
        String gname=null, path=null;

        dstgid = HDFLibrary.Vattach(fid, -1, "w");
        if (dstgid < 0) {
            return null;
        }

        gname = srcGroup.getName();
        srcgid = srcGroup.open();

        HDFLibrary.Vsetname(dstgid, gname);
        int ref = HDFLibrary.VQueryref(dstgid);
        int tag = HDFLibrary.VQuerytag(dstgid);

        if (pgroup.isRoot())
        {
           path = HObject.separator;
        }
        else
        {
            // add the dataset to the parent group
            path = pgroup.getPath()+pgroup.getName()+HObject.separator;
            int pid = pgroup.open();
            HDFLibrary.Vinsert(pid, dstgid);
            pgroup.close(pid);
        }

        // copy attributes
        int numberOfAttributes = 0;
        try {
            numberOfAttributes = HDFLibrary.Vnattrs(srcgid);
        } catch (Exception ex) { numberOfAttributes = 0; }

        String[] attrName = new String[1];
        byte[] attrBuff = null;
        int[] attrInfo = new int[3]; //data_type,  count, size
        for (int i=0; i< numberOfAttributes; i++)
        {
            try {
                attrName[0] = "";
                HDFLibrary.Vattrinfo(srcgid, i, attrName, attrInfo);
                attrBuff = new byte[attrInfo[2]];
                HDFLibrary.Vgetattr(srcgid, i, attrBuff);
                HDFLibrary.Vsetattr(dstgid, attrName[0], attrInfo[0], attrInfo[2], attrBuff);
            } catch (Exception ex) { continue; }
        }

        long[] oid = {tag, ref};
        group = new H4Group(this, gname, path, pgroup, oid);

        DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(group)
        {
        	public static final long serialVersionUID = HObject.serialVersionUID;

            public boolean isLeaf() { return false; }
        };
        pgroup.addToMemberList(group);

        // copy members of the source group to the new group
        List members = srcGroup.getMemberList();
        if ((members != null) && (members.size()>0))
        {
            Iterator iterator = members.iterator();
            while (iterator.hasNext())
            {
                HObject mObj = (HObject)iterator.next();
                try {
                    newNode.add((MutableTreeNode)copy(mObj, group));
                } catch (Exception ex) {}
            }
        }

        srcGroup.close(srcgid);
        try { HDFLibrary.Vdetach(dstgid); }
        catch (Exception ex) { ; }

        return newNode;
    }

    /**
     * Retrieves and returns the file structure from disk.
     * <p>
     * First gets the top level objects or objects that do not belong to any groups.
     * If a top level object is a group, call the depth_first() to retrieve
     * the sub-tree of that group, recursively.
     *
     */
    private DefaultMutableTreeNode loadTree()
    {
        if (fid <0 ) {
            return null;
        }

        long[] oid = {0, 0};
        int n=0, ref=-1;
        int[] argv = null;
        MutableTreeNode node = null;

        H4Group rootGroup = new H4Group(
            this,
            getName(), // set the node name to the file name
            null, // root node does not have a parent path
            null, // root node does not have a parent node
            oid);

        DefaultMutableTreeNode root = new DefaultMutableTreeNode(rootGroup)
        {
        	public static final long serialVersionUID = HObject.serialVersionUID;

            public boolean isLeaf() { return false; }
        };

        // get top level VGroup
        int[] tmpN = new int[1];
        int[] refs = null;
        try {
            // first call to get the number of lone Vgroup
            n = HDFLibrary.Vlone(fid, tmpN, 0);
            refs = new int[n];
            // second call to get the references of all lone Vgroup
            n = HDFLibrary.Vlone(fid, refs, n);
        } catch (HDFException ex) { n = 0; }

        int i0 = Math.max(0, getStartMembers());
        int i1 = getMaxMembers();
        if (i1 >= n)
        {
            i1 = n;
            i0 = 0; // load all members
        }
        i1 += i0;
        i1 = Math.min(i1, n);

        //Iterate through the file to see members of the group
        for ( int i = i0; i < i1; i++)
        {
            ref = refs[i];
            H4Group g = getVGroup(HDFConstants.DFTAG_VG, ref, HObject.separator, rootGroup, false);

            if (g != null)
            {
                node = new DefaultMutableTreeNode(g)
                {
                	public static final long serialVersionUID = HObject.serialVersionUID;

                    public boolean isLeaf() { return false; }
                };
                root.add( node );
                rootGroup.addToMemberList(g);

                // recursively get the sub-tree
                depth_first(node, null);
            }
        } // for (int i=0; i<n; i++)

        // get the top level GR images
        argv = new int[2];
        boolean b = false;
        try {
            b = HDFLibrary.GRfileinfo(grid, argv);
        } catch (HDFException ex)
        {
            b = false;
        }

        if ( b )
        {
            n = argv[0];

            for (int i=0; i<n; i++)
            {
                // no duplicate object at top level
                H4GRImage gr = getGRImage(HDFConstants.DFTAG_RIG, i, HObject.separator, false);
                if (gr != null)
                {
                    node = new DefaultMutableTreeNode(gr);
                    root.add( node );
                    rootGroup.addToMemberList(gr);
                }
            } // for (int i=0; i<n; i++)
        } // if ( grid!=HDFConstants.FAIL && HDFLibrary.GRfileinfo(grid,argv) )

        // get top level SDS
        try {
            b = HDFLibrary.SDfileinfo(sdid, argv);
        } catch (HDFException ex)
        {
            b = false;
        }

        if (b)
        {
            n = argv[0];
            for (int i=0; i<n; i++)
            {
                // no duplicate object at top level
                H4SDS sds = getSDS(HDFConstants.DFTAG_NDG, i, HObject.separator, false);
                if (sds != null)
                {
                    node = new DefaultMutableTreeNode(sds);
                    root.add( node );
                    rootGroup.addToMemberList(sds);
                }
            } // for (int i=0; i<n; i++)
        } // if (sdid != HDFConstants.FAIL && HDFLibrary.SDfileinfo(sdid, argv))

        // get top level VData
        try {
            n = HDFLibrary.VSlone(fid, tmpN, 0);
            refs = new int[n];
            n = HDFLibrary.VSlone(fid, refs, n);
        } catch (HDFException ex)
        {
            n = 0;
        }
        for (int i=0; i<n; i++)
        {
            ref = refs[i];

            // no duplicate object at top level
            H4Vdata vdata = getVdata(HDFConstants.DFTAG_VS, ref, HObject.separator, false);

            if (vdata != null)
            {
                node = new DefaultMutableTreeNode(vdata);
                root.add( node );
                rootGroup.addToMemberList(vdata);
            }
        } // for (int i=0; i<n; i++)

        if (rootGroup != null)
        {
            // retrieve file annotation, GR and SDS globle attributes
            List attributeList = null;
            try { attributeList = rootGroup.getMetadata(); }
            catch (HDFException ex) {}

            if (attributeList != null)
            {
                try { getFileAnnotation(fid, attributeList); }
                catch (HDFException ex) {}
                try { getGRglobleAttribute(grid, attributeList); }
                catch (HDFException ex) {}
                try { getSDSglobleAttribute(sdid, attributeList); }
                catch (HDFException ex) {}
            }
        }

        return root;
    }

    /**
     * Retrieves the tree structure of the file by depth-first order.
     * The current implementation only retrieves group and dataset. It does
     * not include named datatype and soft links.
     * <p>
     * @param parentNode the parent node.
     */
    private void depth_first(MutableTreeNode parentNode, H4Group pgroup)
    {
        if ((pgroup == null) && (parentNode == null)) {
            return;
        }

        //System.out.println("H4File.depth_first() pnode = "+parentNode);
        int nelems=0, ref=-1, tag=-1, index=-1;
        int[] tags = null;
        int[] refs = null;
        MutableTreeNode node = null;
        DefaultMutableTreeNode pnode = null;

        if (parentNode != null) {
            pnode = (DefaultMutableTreeNode)parentNode;
            pgroup = (H4Group)(pnode.getUserObject());
        }

        String fullPath = pgroup.getPath()+pgroup.getName()+HObject.separator;
        int gid = pgroup.open();
        if (gid == HDFConstants.FAIL)
        {
            return;
        }

        try
        {
            nelems = HDFLibrary.Vntagrefs(gid);
            tags = new int[nelems];
            refs = new int[nelems];
            nelems = HDFLibrary.Vgettagrefs(gid, tags, refs, nelems);
        } catch (HDFException ex)
        {
            nelems = 0;
        } finally
        {
            pgroup.close(gid);
        }

        int i0 = Math.max(0, getStartMembers());
        int i1 = getMaxMembers();
        if (i1 >= nelems)
        {
            i1 = nelems;
            i0 = 0; // load all members
        }
        i1 += i0;
        i1 = Math.min(i1, nelems);

        //Iterate through the file to see members of the group
        for ( int i = i0; i < i1; i++)
        {
            tag = tags[i];
            ref = refs[i];

            switch (tag)
            {
                case HDFConstants.DFTAG_RIG:
                case HDFConstants.DFTAG_RI:
                case HDFConstants.DFTAG_RI8:
                    try {
                        index = HDFLibrary.GRreftoindex(grid, (short)ref);
                    } catch (HDFException ex)
                    {
                        index = HDFConstants.FAIL;
                    }
                    if (index != HDFConstants.FAIL)
                    {
                        H4GRImage gr = getGRImage(tag, index, fullPath, true);
                        pgroup.addToMemberList(gr);
                        if ((gr != null) && (pnode != null))
                        {
                            node = new DefaultMutableTreeNode(gr);
                            pnode.add( node );
                        }
                    }
                    break;
                case HDFConstants.DFTAG_SD:
                case HDFConstants.DFTAG_SDG:
                case HDFConstants.DFTAG_NDG:
                    try {
                        index = HDFLibrary.SDreftoindex(sdid, ref);
                    } catch (HDFException ex)
                    {
                        index = HDFConstants.FAIL;
                    }
                    if (index != HDFConstants.FAIL)
                    {
                        H4SDS sds = getSDS(tag, index, fullPath, true);
                        pgroup.addToMemberList(sds);
                        if ((sds != null)  && (pnode != null))
                        {
                            node = new DefaultMutableTreeNode(sds);
                            pnode.add( node );
                        }
                    }
                    break;
                case HDFConstants.DFTAG_VH:
                case HDFConstants.DFTAG_VS:
                    H4Vdata vdata = getVdata(tag, ref, fullPath, true);
                    pgroup.addToMemberList(vdata);
                    if ((vdata != null) && (pnode != null))
                    {
                        node = new DefaultMutableTreeNode(vdata);
                        pnode.add( node );
                    }
                    break;
                case HDFConstants.DFTAG_VG:
                    H4Group vgroup = getVGroup(tag, ref, fullPath, pgroup, true);
                    pgroup.addToMemberList(vgroup);
                    if ((vgroup != null) && (pnode != null))
                    {
                        node = new DefaultMutableTreeNode(vgroup)
                        {
                        	public static final long serialVersionUID = HObject.serialVersionUID;

                            public boolean isLeaf() { return false; }
                        };

                        pnode.add( node );

                        // check for loops
                        boolean looped = false;
                        DefaultMutableTreeNode theNode = pnode;
                        while ((theNode != null) && !looped)
                        {
                            H4Group theGroup = (H4Group)theNode.getUserObject();
                            long[] oid = {tag, ref};
                            if (theGroup.equalsOID(oid)) {
                                looped = true;
                            } else {
                                theNode = (DefaultMutableTreeNode)theNode.getParent();
                            }
                        }
                        if (!looped) {
                            depth_first(node, null);
                        }
                    }
                    break;
                default:
                    break;
            } //switch (tag)

        } //for (int i=0; i<nelms; i++)

    } // private depth_first()

    /**
     * Retrieve an GR image for the given GR image identifier and index.
     * <p>
     * @param index the index of the image.
     * @param path the path of the image.
     * @param copyAllowed The indicator if multiple copies of an object is allowed.
     * @return the new H5GRImage if successful; otherwise returns null.
     */
    private final H4GRImage getGRImage(int tag, int index, String path, boolean copyAllowed)
    {
        int id=-1, ref=-1;
        H4GRImage gr = null;
        String[] objName = {""};
        int[] imgInfo = new int[4];
        int[] dim_sizes = {0, 0};
        //int tag = HDFConstants.DFTAG_RIG;

        try
        {
            id = HDFLibrary.GRselect(grid, index);
            ref = HDFLibrary.GRidtoref(id);
            HDFLibrary.GRgetiminfo(id, objName, imgInfo, dim_sizes);
        } catch (HDFException ex)
        {
            id = HDFConstants.FAIL;
        }
        finally
        {
            try { HDFLibrary.GRendaccess(id); }
            catch (HDFException ex ) {}
        }

        if (id != HDFConstants.FAIL)
        {
            long oid[] = {tag, ref};

            if (copyAllowed)
            {
                objList.add(oid);
            } else if (find(oid))
            {
                return null;
            }

            gr = new H4GRImage(
                this,
                objName[0],
                path,
                oid);
        }

        return gr;
    }

    /**
     * Retrieve a SDS for the given sds identifier and index.
     * <p>
     * @param sdid the SDS idendifier.
     * @param index the index of the SDS.
     * @param path the path of the SDS.
     * @param copyAllowed The indicator if multiple copies of an object is allowed.
     * @return the new H4SDS if successful; otherwise returns null.
     */
    private final H4SDS getSDS(int tag, int index, String path, boolean copyAllowed)
    {
        int id=-1, ref=-1;
        H4SDS sds = null;
        String[] objName = {""};
        int[] tmpInfo = new int[HDFConstants.MAX_VAR_DIMS];
        int[] sdInfo = {0, 0, 0};
        //int tag = HDFConstants.DFTAG_NDG;

        boolean isCoordvar = false;
        try
        {
            id = HDFLibrary.SDselect(sdid, index);
            if (isNetCDF)
            {
                ref = index; //HDFLibrary.SDidtoref(id) fails for netCDF
                tag = H4SDS.DFTAG_NDG_NETCDF;
            } else {
                ref = HDFLibrary.SDidtoref(id);
            }
            HDFLibrary.SDgetinfo(id, objName, tmpInfo, sdInfo);
            isCoordvar = HDFLibrary.SDiscoordvar(id);
        } catch (HDFException ex)
        {
            id = HDFConstants.FAIL;
        }
        finally
        {
            try { HDFLibrary.SDendaccess(id); }
            catch (HDFException ex) {}
        }

        // check if the given SDS has dimension metadata
        // Coordinate variables are not displayed. They are created to store
        // metadata associated with dimensions. To ensure compatibility with
        // netCDF, coordinate variables are implemented as data sets

        if (isCoordvar)
        {
            objName[0] += " (dimension)";
        }

        if (id != HDFConstants.FAIL)// && !isCoordvar)
        {
            long oid[] = {tag, ref};

            if (copyAllowed)
            {
                objList.add(oid);
            } else if (find(oid))
            {
                return null;
            }

            sds = new H4SDS(
                this,
                objName[0],
                path,
                oid);
        }

        return sds;
    }

    /**
     * Retrieve a Vdata for the given Vdata identifier and index.
     * <p>
     * @param ref the reference idendifier of the Vdata.
     * @param path the path of the Vdata.
     * @param copyAllowed The indicator if multiple copies of an object is allowed.
     * @return the new H4Vdata if successful; otherwise returns null.
     */
    private final H4Vdata getVdata(int tag, int ref, String path, boolean copyAllowed)
    {
        int id=-1;
        H4Vdata vdata = null;
        String[] objName = {""};
        String[] vClass = {""};
        //int tag = HDFConstants.DFTAG_VS;
        long oid[] = {tag, ref};

        if (copyAllowed)
        {
            objList.add(oid);
        } else if (find(oid))
        {
            return null;
        }

        try
        {
            id = HDFLibrary.VSattach(fid, ref, "r");
            HDFLibrary.VSgetclass(id, vClass);
            vClass[0] = vClass[0].trim();
            HDFLibrary.VSgetname(id, objName);
        } catch (HDFException ex)
        {
            id = HDFConstants.FAIL;
        }
        finally
        {
            try { HDFLibrary.VSdetach(id); }
            catch (HDFException ex) {}
        }

        if (showAll || ((id != HDFConstants.FAIL) &&
            // do not display Vdata named "Attr0.0"
            !vClass[0].equalsIgnoreCase(HDFConstants.HDF_ATTRIBUTE) &&
            // do not display internal Vdata, "_HDF_CHK_TBL_"
            !vClass[0].startsWith(HDFConstants.HDF_CHK_TBL) &&
            // do not display internal vdata for CDF, "CDF0.0"
            !vClass[0].equalsIgnoreCase(HDFConstants.HDF_CDF)))
        {
            vdata = new H4Vdata(
                this,
                objName[0],
                path,
                oid);
        }

        return vdata;
    }

    /**
     * Retrieve a VGroup for the given VGroup identifier and index.
     * <p>
     * @param ref the reference idendifier of the VGroup.
     * @param path the path of the VGroup.
     * @param pgroup the parent group.
     * @param copyAllowed The indicator if multiple copies of an object is allowed.
     * @return the new H4VGroup if successful; otherwise returns null.
     */
    private final H4Group getVGroup(int tag, int ref, String path, H4Group pgroup, boolean copyAllowed)
    {
        int id=-1;
        H4Group vgroup  = null;
        String[] objName = {""};
        String[] vClass = {""};
        //int tag = HDFConstants.DFTAG_VG;
        long oid[] = {tag, ref};

        if (copyAllowed)
        {
            objList.add(oid);
        } else if (find(oid))
        {
            return null;
        }

        try
        {
            id = HDFLibrary.Vattach(fid, ref, "r");
            HDFLibrary.Vgetclass(id, vClass);
            vClass[0] = vClass[0].trim();
            HDFLibrary.Vgetname(id, objName);
        } catch (HDFException ex)
        {
            id = HDFConstants.FAIL;
        }
        finally
        {
            try { HDFLibrary.Vdetach(id); }
            catch (HDFException ex) {}
        }

        // ignore the Vgroups created by the GR interface
        if (showAll || ((id != HDFConstants.FAIL) &&
            // do not display Vdata named "Attr0.0"
            !vClass[0].equalsIgnoreCase(HDFConstants.GR_NAME) &&
            !vClass[0].equalsIgnoreCase(HDFConstants.RI_NAME) &&
            !vClass[0].equalsIgnoreCase(HDFConstants.RIGATTRNAME) &&
            !vClass[0].equalsIgnoreCase(HDFConstants.RIGATTRCLASS) &&
            !vClass[0].equalsIgnoreCase(HDFConstants.HDF_CDF)))
        {
            vgroup = new H4Group( this, objName[0], path, pgroup, oid);
        }

        return vgroup;
    }

    /**
     * Check if object already exists in memory by match the (tag, ref) pairs.
     */
    private final boolean find(long[] oid)
    {
        boolean existed = false;

        if (objList == null) {
            return false;
        }

        int n = objList.size();
        long[] theOID = null;

        for (int i=0; i<n; i++)
        {
            theOID = (long[])objList.get(i);
            if ((theOID[0]==oid[0]) && (theOID[1]==oid[1]))
            {
                existed = true;
                break;
            }
        }

        if (!existed)
        {
            objList.add(oid);
        }

        return existed;
    }

    /**
     * Returns the GR identifier, which is returned from GRstart(fid).
     */
    int getGRAccessID()
    {
        return grid;
    }

    /**
     * Returns the SDS identifier, which is returned from SDstart(fname, flag).
     */
    int getSDAccessID()
    {
        return sdid;
    }

    /**
     *  Reads HDF file annontation (file labels and descriptions) into memory.
     *  The file annotation is stroed as attribute of the root group.
     *  <p>
     *  @param fid the file identifier.
     *  @param attrList the list of attributes.
     *  @return the updated attribute list.
     */
    private List getFileAnnotation(int fid, List attrList)
    throws HDFException
    {
        if (fid < 0 ) {
            return attrList;
        }

        int anid = HDFConstants.FAIL;
        try
        {
            anid = HDFLibrary.ANstart(fid);
            // fileInfo[0] = n_file_label, fileInfo[1] = n_file_desc,
            // fileInfo[2] = n_data_label, fileInfo[3] = n_data_desc
            int[] fileInfo = new int[4];
            HDFLibrary.ANfileinfo(anid, fileInfo);

            if (fileInfo[0]+fileInfo[1] <= 0)
            {
                try { HDFLibrary.ANend(anid); } catch (HDFException ex) {}
                return attrList;
            }

            if (attrList == null) {
                attrList = new Vector(fileInfo[0]+fileInfo[1], 5);
            }

            // load file labels and descriptions
            int id = -1;
            int[] annTypes = {HDFConstants.AN_FILE_LABEL, HDFConstants.AN_FILE_DESC};
            for (int j=0; j<2; j++)
            {
                String annName = null;
                if (j == 0) {
                    annName = "File Label";
                } else {
                    annName = "File Description";
                }

                for (int i=0; i < fileInfo[j]; i++)
                {
                    try {
                        id = HDFLibrary.ANselect(anid, i, annTypes[j]);
                    } catch (HDFException ex)
                    {
                        id = HDFConstants.FAIL;
                    }

                    if (id == HDFConstants.FAIL)
                    {
                        try { HDFLibrary.ANendaccess(id); } catch (HDFException ex) {}
                        continue;
                    }

                    int length = 0;
                    try {
                        length = HDFLibrary.ANannlen(id)+1;
                    } catch (HDFException ex)
                    {
                        length = 0;
                    }

                    if (length > 0)
                    {
                        boolean b = false;
                        String str[] = {""};
                        try { b = HDFLibrary.ANreadann(id, str, length);
                        } catch ( HDFException ex) { b = false; }

                        if (b && (str[0].length()>0))
                        {
                            long attrDims[] = {str[0].length()};
                            Attribute newAttr = new Attribute(
                                annName +" #"+i,
                                new H4Datatype(HDFConstants.DFNT_CHAR),
                                attrDims);
                            attrList.add(newAttr);
                            newAttr.setValue(str[0]);
                        }
                    }

                    try { HDFLibrary.ANendaccess(id); } catch (HDFException ex) {}
                } // for (int i=0; i < fileInfo[annTYpe]; i++)
            } // for (int annType=0; annType<2; annType++)
        } finally
        {
            try { HDFLibrary.ANend(anid); } catch (HDFException ex) {}
        }

        return attrList;
    }

    /**
     *  Reads GR globle attributes into memory.
     *  The attributes sre stroed as attributes of the root group.
     *  <p>
     *  @param grid the GR identifier.
     *  @param attrList the list of attributes.
     *  @return the updated attribute list.
     */
    private List getGRglobleAttribute(int grid, List attrList)
    throws HDFException
    {
        if (grid == HDFConstants.FAIL) {
            return attrList;
        }

        int[] attrInfo = {0, 0};
        HDFLibrary.GRfileinfo(grid, attrInfo);
        int numberOfAttributes = attrInfo[1];

        if (numberOfAttributes>0)
        {
            if (attrList == null) {
                attrList = new Vector(numberOfAttributes, 5);
            }

            String[] attrName = new String[1];
            for (int i=0; i<numberOfAttributes; i++)
            {
                attrName[0] = "";
                boolean b = false;
                try {
                    b =  HDFLibrary.GRattrinfo(grid, i, attrName, attrInfo);
                    // mask off the litend bit
                    attrInfo[0] = attrInfo[0] & (~HDFConstants.DFNT_LITEND);
                } catch (HDFException ex)
                {
                    b = false;
                }

                if (!b) {
                    continue;
                }

                long[] attrDims = {attrInfo[1]};
                Attribute attr = new Attribute(attrName[0], new H4Datatype(attrInfo[0]), attrDims);;
                attrList.add(attr);

                Object buf = H4Datatype.allocateArray(attrInfo[0], attrInfo[1]);
                try {
                    HDFLibrary.GRgetattr(grid, i, buf);
                } catch (HDFException ex)
                {
                    buf = null;
                }

                if (buf != null)
                {
                    if ((attrInfo[0] == HDFConstants.DFNT_CHAR) ||
                        (attrInfo[0] ==  HDFConstants.DFNT_UCHAR8))
                    {
                        buf = Dataset.byteToString((byte[])buf, attrInfo[1]);
                    }

                    attr.setValue(buf);
                }

            } // for (int i=0; i<numberOfAttributes; i++)
        } // if (b && numberOfAttributes>0)

        return attrList;
    }

    /**
     *  Reads SDS globle attributes into memory.
     *  The attributes sre stroed as attributes of the root group.
     *  <p>
     *  @param sdid the SD identifier.
     *  @param attrList the list of attributes.
     *  @return the updated attribute list.
     */
    private List getSDSglobleAttribute(int sdid, List attrList)
    throws HDFException
    {
        if (sdid == HDFConstants.FAIL) {
            return attrList;
        }

        int[] attrInfo = {0, 0};
        HDFLibrary.SDfileinfo(sdid, attrInfo);

        int numberOfAttributes = attrInfo[1];
        if (numberOfAttributes>0)
        {
            if (attrList == null) {
                attrList = new Vector(numberOfAttributes, 5);
            }

            String[] attrName = new String[1];
            for (int i=0; i<numberOfAttributes; i++)
            {
                attrName[0] = "";
                boolean b = false;
                try {
                    b =  HDFLibrary.SDattrinfo(sdid, i, attrName, attrInfo);
                    // mask off the litend bit
                    attrInfo[0] = attrInfo[0] & (~HDFConstants.DFNT_LITEND);
                } catch (HDFException ex)
                {
                    b = false;
                }

                if (!b) {
                    continue;
                }

                long[] attrDims = {attrInfo[1]};
                Attribute attr = new Attribute(attrName[0], new H4Datatype(attrInfo[0]), attrDims);;
                attrList.add(attr);

                Object buf = H4Datatype.allocateArray(attrInfo[0], attrInfo[1]);
                try {
                    HDFLibrary.SDreadattr(sdid, i, buf);
                } catch (HDFException ex)
                {
                    buf = null;
                }

                if (buf != null)
                {
                    if ((attrInfo[0] == HDFConstants.DFNT_CHAR) ||
                        (attrInfo[0] ==  HDFConstants.DFNT_UCHAR8))
                    {
                        buf = Dataset.byteToString((byte[])buf, attrInfo[1]);
                    }

                    attr.setValue(buf);
                }

            } // for (int i=0; i<numberOfAttributes; i++)
        } // if (b && numberOfAttributes>0)

        return attrList;
    }

    /**
     *  Returns the version of the HDF4 library.
     */
    public String getLibversion()
    {
        int[] vers = new int[3];
        String ver = "HDF ";
        String[] verStr = {""};

        try { HDFLibrary.Hgetlibversion(vers, verStr); }
        catch (HDFException ex) {}

        ver += vers[0] + "." + vers[1] +"."+vers[2];

        return ver;
    }

    /** HDF4 library supports netCDF version 2.3.2. It only supports SDS APIs.*/
    private boolean isNetCDF(String filename)
    {
        boolean isnetcdf = false;
        java.io.RandomAccessFile raf = null;

        try { raf = new java.io.RandomAccessFile(filename, "r"); }
        catch (Exception ex)
        {
            try { raf.close();} catch (Exception ex2) {}
            raf = null;
        }

        if (raf == null) {
            return false;
        }

        byte[] header = new byte[4];
        try { raf.read(header); }
        catch (Exception ex) { header = null; }

        if (header != null)
        {
            if (
                // netCDF
               ((header[0]==67) &&
                (header[1]==68) &&
                (header[2]==70) &&
                (header[3]==1)) ) {
                isnetcdf = true;
            } else {
                isnetcdf = false;
            }
        }

        try { raf.close();} catch (Exception ex) {}

        return isnetcdf;
    }

    /**
     * Get an individual HObject with a given path. It deoes not load the whole
     * file structure.
     */
    public HObject get(String path) throws Exception
    {
        if (objList == null) {
            objList = new Vector();
        }

        if ((path == null) || (path.length() <= 0)) {
            return null;
        }

        path = path.replace('\\', '/');
        if (!path.startsWith("/")) {
            path = "/"+path;
        }

        String name=null, pPath=null;
        boolean isRoot = false;

        if (path.equals("/"))
        {
            name = "/"; // the root
            isRoot = true;
        } else
        {
            if (path.endsWith("/")) {
                path = path.substring(0, path.length()-2);
            }
            int idx = path.lastIndexOf('/');
            name = path.substring(idx+1);
            if (idx == 0) {
                pPath = "/";
            } else {
                pPath = path.substring(0, idx);
            }
        }

        HObject obj = null;
        isReadOnly = false;

        if (fid < 0)
        {
            fid = HDFLibrary.Hopen( fullFileName, HDFConstants.DFACC_WRITE);
            if (fid < 0)
            {
                isReadOnly = true;
                fid = HDFLibrary.Hopen( fullFileName, HDFConstants.DFACC_READ);
            }
            HDFLibrary.Vstart(fid);
            grid = HDFLibrary.GRstart(fid);
            sdid = HDFLibrary.SDstart(fullFileName, flag);
        }

        if (isRoot) {
            obj = getRootGroup();
        } else {
            obj = getAttachedObject(pPath, name);
        }

        return obj;
    }

    /** get the root group and all the alone objects */
    private H4Group getRootGroup()
    {
        H4Group rootGroup = null;

        long[] oid = {0, 0};
        int n=0, ref=-1;
        int[] argv = null;

        rootGroup = new H4Group(this,"/",  null, null, oid);

        // get top level VGroup
        int[] tmpN = new int[1];
        int[] refs = null;
        try {
            // first call to get the number of lone Vgroup
            n = HDFLibrary.Vlone(fid, tmpN, 0);
            refs = new int[n];
            // second call to get the references of all lone Vgroup
            n = HDFLibrary.Vlone(fid, refs, n);
        } catch (HDFException ex) { n = 0; }

        //Iterate through the file to see members of the group
        for ( int i = 0; i < n; i++)
        {
            ref = refs[i];
            H4Group g = getVGroup(HDFConstants.DFTAG_VG, ref, HObject.separator, rootGroup, false);
            if (g != null) {
                rootGroup.addToMemberList(g);
            }
        } // for (int i=0; i<n; i++)

        // get the top level GR images
        argv = new int[2];
        boolean b = false;
        try { b = HDFLibrary.GRfileinfo(grid, argv);
        } catch (HDFException ex) { b = false; }

        if ( b )
        {
            n = argv[0];
            for (int i=0; i<n; i++)
            {
                // no duplicate object at top level
                H4GRImage gr = getGRImage(HDFConstants.DFTAG_RIG, i, HObject.separator, false);
                if (gr != null) {
                    rootGroup.addToMemberList(gr);
                }
            } // for (int i=0; i<n; i++)
        } // if ( grid!=HDFConstants.FAIL && HDFLibrary.GRfileinfo(grid,argv) )

        // get top level SDS
        try { b = HDFLibrary.SDfileinfo(sdid, argv);
        } catch (HDFException ex) { b = false; }

        if (b)
        {
            n = argv[0];

            for (int i=0; i<n; i++)
            {
                // no duplicate object at top level
                H4SDS sds = getSDS(HDFConstants.DFTAG_NDG, i, HObject.separator, false);
                if (sds != null) {
                    rootGroup.addToMemberList(sds);
                }
            } // for (int i=0; i<n; i++)
        } // if (sdid != HDFConstants.FAIL && HDFLibrary.SDfileinfo(sdid, argv))

        // get top level VData
        try {
            n = HDFLibrary.VSlone(fid, tmpN, 0);
            refs = new int[n];
            n = HDFLibrary.VSlone(fid, refs, n);
        } catch (HDFException ex) {  n = 0; }

        for (int i=0; i<n; i++)
        {
            ref = refs[i];

            // no duplicate object at top level
            H4Vdata vdata = getVdata(HDFConstants.DFTAG_VS, ref, HObject.separator, false);

            if (vdata != null) {
                rootGroup.addToMemberList(vdata);
            }
        } // for (int i=0; i<n; i++)

        if (rootGroup != null)
        {
            // retrieve file annotation, GR and SDS globle attributes
            List attributeList = null;
            try { attributeList = rootGroup.getMetadata(); }
            catch (HDFException ex) {}

            if (attributeList != null)
            {
                try { getFileAnnotation(fid, attributeList); }
                catch (HDFException ex) {}
                try { getGRglobleAttribute(grid, attributeList); }
                catch (HDFException ex) {}
                try { getSDSglobleAttribute(sdid, attributeList); }
                catch (HDFException ex) {}
            }
        }

        return rootGroup;
    }

    /** get the object attached to a vgroup */
    private HObject getAttachedObject(String path, String name)
    {
        if ((name== null) || (name.length()<=0)) {
            return null;
        }

        HObject obj = null;

        // get top level VGroup
        String[] objName = {""};
        // check if it is an image
        int idx = -1;
        try {
            idx = HDFLibrary.GRnametoindex(grid, name);
        } catch (HDFException ex) { idx = -1; }

        if ( idx >= 0 ) {
            return getGRImage(HDFConstants.DFTAG_RIG, idx, HObject.separator, false);
        }

        // get top level SDS
        try {
            idx = HDFLibrary.SDnametoindex(sdid, name);
        } catch (HDFException ex) { idx = -1; }

        if ( idx >= 0 )
        {
            return getSDS(HDFConstants.DFTAG_NDG, idx, HObject.separator, false);
        } // if (sdid != HDFConstants.FAIL && HDFLibrary.SDfileinfo(sdid, argv))

        int ref = 0;
        try {
            ref = HDFLibrary.Vfind(fid, name);
        } catch (HDFException ex) { ref = -1; }

        if (ref > 0)
        {
            long oid[] = {HDFConstants.DFTAG_VG, ref};
            H4Group g = new H4Group( this, objName[0], path, null, oid);
            depth_first(null, g);
            return g;
        }

        // get top level VData
        try {
            ref = HDFLibrary.VSfind(fid, name);
        } catch (HDFException ex) {  ref = -1; }

        if (ref > 0)
        {
            return getVdata(HDFConstants.DFTAG_VS, ref, HObject.separator, false);
        } // for (int i=0; i<n; i++)

        return obj;
    }
}
