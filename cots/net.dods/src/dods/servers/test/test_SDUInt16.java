/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1999, COAS, Oregon State University  
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged. 
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Nathan Potter (ndp@oce.orst.edu)
//
//                        College of Oceanic and Atmospheric Scieneces
//                        Oregon State University
//                        104 Ocean. Admin. Bldg.
//                        Corvallis, OR 97331-5503
//         
/////////////////////////////////////////////////////////////////////////////

package dods.servers.test;
import dods.dap.Server.*;
import dods.dap.*;
import java.io.*;

/**
 * Holds a DODS Server <code>UInt16</code> value.
 *
 * @version $Revision: 1.2 $
 * @author ndp
 * @see BaseType
 */
public class test_SDUInt16 extends SDUInt16 {
    
    /** Constructs a new <code>test_SDUInt16</code>. */
    public test_SDUInt16() { 
        super(); 
    }

    /**
    * Constructs a new <code>test_SDUInt16</code> with name <code>n</code>.
    * @param n the name of the variable.
    */
    public test_SDUInt16(String n) { 
        super(n); 
    }


    // --------------- FileIO Interface
    /** Read a value from the named dataset for this variable. 
    *  @param datasetName String identifying the file or other data store
    *  from which to read a vaue for this variable.
    *  @param specialO This <code>Object</code> is a goody that is used by Server implementations
    *  to deliver important, and as yet unknown, stuff to the read method. If you
    *  don't need it, make it a <code>null</code>.
    *  @return <code>true</code> if more data remains to be read, otherwise
    *  <code>false</code>. This is an abtsract method that must be implemented
    *  as part of the installation/localization of a DODS server.
    *  @exception IOException
    *  @exception EOFException
    */
    public boolean read(String datasetName, Object specialO)
                            throws NoSuchVariableException, IOException, EOFException {

        testEngine te = (testEngine)specialO;

        setValue(te.nextUint16());
        setRead(true);
        return (false);
    }
    
    
}
