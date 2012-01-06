
/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 *
 * The h5gen tool uses the Apache Java Xerces classes to parse
 * XML.  The xerces classes (xerces.jar) are included in the Java
 * HDF5 product.  See the Apache license (LICENSE.html) for
 * terms and conditions of use of Xerces.
 *
 * For conditions of distribution and use, see the accompanying             *
 * java-hdf5/COPYING file.                                                  *
 *                                                                          *
 ****************************************************************************/
package ncsa.hdf.tools.h5gen;


import java.util.Hashtable;
import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;
import ncsa.hdf.hdf5lib.*;

import ncsa.hdf.hdf5lib.exceptions.*;
import org.xml.sax.SAXException;

public class H5gGroup extends H5Group
{
   private Hashtable xmlAttributes;

    public H5gGroup(FileFormat fileFormat, String name, String path, Group parent)
    {
        this(fileFormat, name, path, parent, null);
    }

    /**
     * Constructs an HDF5 group with specific name, path, and parent.
     * <p>
     * @param fileFormat the file which containing the group.
     * @param name the name of this group.
     * @param path the full path of this group.
     * @param parent the parent of this group.
     * @param oid the unique identifier of this data object.
     */
    public H5gGroup(
        FileFormat fileFormat,
        String name,
        String path,
        Group parent,
        long[] theID)
    {
        super (fileFormat, name, path, parent, ((theID == null) ? DEFAULT_OID : theID));

        int gid = open();
        try { hasAttribute = (H5.H5Aget_num_attrs(gid)>0); }
        catch (Exception ex ) {}
        close(gid);

        xmlAttributes = new Hashtable();
    }

    public Hashtable getAttributes() { return xmlAttributes; }

    public void setParent(H5gGroup pGroup) { parent = pGroup; };

    /**
     * Creates a new group.
     */
    public void create() throws Exception
    {
        String fullPath = null;
        String name = getName();

        if (parent == null ||
            name == null)
            return ;

        H5File file = (H5File)getFileFormat();

        if (file == null)
            return;

        String path = HObject.separator;
        if (!parent.isRoot())
            path = parent.getPath()+parent.getName()+HObject.separator;
        fullPath = path +  name;

         // create a new group and add ot to the parent node
        int gid = H5.H5Gcreate(file.open(), fullPath, -1);
        this.close(gid);
    }
}

