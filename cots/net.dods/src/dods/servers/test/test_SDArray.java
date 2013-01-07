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
 * Holds a DODS Server <code>Array</code> value.
 *
 * @version $Revision: 1.3 $
 * @author ndp 
 * @see BaseType
 */
public class test_SDArray extends SDArray  {

    private boolean Debug = false;
    private int origShape[];

    /** Constructs a new <code>test_SDArray</code>. */
    public test_SDArray() { 
        super();       
    }

    /**
    * Constructs a new <code>test_SDArray</code> with name <code>n</code>.
    * @param n the name of the variable.
    */
    public test_SDArray(String n) { 
        super(n); 
    }
    
    
    
    
    public void cacheShape() {
    
	origShape = new int[numDimensions()];
	
	try {
	    for(int dim=0; dim<numDimensions() ;dim++){
                DArrayDimension dad = getDimension(dim);			
                origShape[dim] = dad.getSize();
	    }
	}
	catch (InvalidParameterException e){
	    System.out.println("ERROR! Unresolved problem in test_SDArray.cacheShape!");
	}
	
    }
    
    public int getCachedShape(int dim) {
        if(dim < origShape.length)
	    return(origShape[dim]);
	else
	    return(-1);
    }
    
    /** Read a value from the named dataset for this variable. 
    @param datasetName String identifying the file or other data store
    from which to read a vaue for this variable.
    @param specialO This <code>Object</code> is a goody that is used by Server implementations
    to deliver important, and as yet unknown, stuff to the read method. If you
    don't need it, make it a <code>null</code>.
    @return <code>true</code> if more data remains to be read, otherwise
    <code>false</code>. This is an abtsract method that must be implemented
    as part of the installation/localization of a DODS server.
    @exception IOException
    @exception EOFException
    */
    public boolean read(String datasetName, Object specialO)
                                    throws NoSuchVariableException, IOException, EOFException {

        testEngine te = (testEngine)specialO;

        te.newLoadTestArray(datasetName, this);

        setRead(true);
        return (false);
    }
}
